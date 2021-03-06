use std::collections::{HashMap, HashSet};
use std::fs;
use std::process::Command;
use std::rc::Rc;
use std::sync::Arc;

use inkwell::{
    memory_buffer::MemoryBuffer,
    passes::{PassManager, PassManagerBuilder},
    targets::*,
    OptimizationLevel,
};
use nac3core::codegen::gen_func_impl;
use nac3core::toplevel::builtins::get_exn_constructor;
use nac3core::typecheck::typedef::{TypeEnum, Unifier};
use nac3parser::{
    ast::{self, ExprKind, Stmt, StmtKind, StrRef},
    parser::{self, parse_program},
};
use pyo3::prelude::*;
use pyo3::{exceptions, types::PyBytes, types::PyDict, types::PySet};
use pyo3::create_exception;

use parking_lot::{Mutex, RwLock};

use nac3core::{
    codegen::irrt::load_irrt,
    codegen::{concrete_type::ConcreteTypeStore, CodeGenTask, WithCall, WorkerRegistry},
    symbol_resolver::SymbolResolver,
    toplevel::{
        composer::{ComposerConfig, TopLevelComposer},
        DefinitionId, GenCall, TopLevelDef,
    },
    typecheck::typedef::{FunSignature, FuncArg},
    typecheck::{type_inferencer::PrimitiveStore, typedef::Type},
};

use tempfile::{self, TempDir};

use crate::codegen::attributes_writeback;
use crate::{
    codegen::{rpc_codegen_callback, ArtiqCodeGenerator},
    symbol_resolver::{InnerResolver, PythonHelper, Resolver, DeferredEvaluationStore},
};

mod codegen;
mod symbol_resolver;
mod timeline;

use timeline::TimeFns;

#[derive(PartialEq, Clone, Copy)]
enum Isa {
    Host,
    RiscV32G,
    RiscV32IMA,
    CortexA9,
}

#[derive(Clone)]
pub struct PrimitivePythonId {
    int: u64,
    int32: u64,
    int64: u64,
    uint32: u64,
    uint64: u64,
    float: u64,
    float64: u64,
    bool: u64,
    list: u64,
    tuple: u64,
    typevar: u64,
    none: u64,
    exception: u64,
    generic_alias: (u64, u64),
    virtual_id: u64,
    option: u64,
}

type TopLevelComponent = (Stmt, String, PyObject);

// TopLevelComposer is unsendable as it holds the unification table, which is
// unsendable due to Rc. Arc would cause a performance hit.
#[pyclass(unsendable, name = "NAC3")]
struct Nac3 {
    isa: Isa,
    time_fns: &'static (dyn TimeFns + Sync),
    primitive: PrimitiveStore,
    builtins: Vec<(StrRef, FunSignature, Arc<GenCall>)>,
    pyid_to_def: Arc<RwLock<HashMap<u64, DefinitionId>>>,
    primitive_ids: PrimitivePythonId,
    working_directory: TempDir,
    top_levels: Vec<TopLevelComponent>,
    string_store: Arc<RwLock<HashMap<String, i32>>>,
    exception_ids: Arc<RwLock<HashMap<usize, usize>>>,
    deferred_eval_store: DeferredEvaluationStore
}

create_exception!(nac3artiq, CompileError, exceptions::PyException);

impl Nac3 {
    fn register_module(
        &mut self,
        module: PyObject,
        registered_class_ids: &HashSet<u64>,
    ) -> PyResult<()> {
        let (module_name, source_file) = Python::with_gil(|py| -> PyResult<(String, String)> {
            let module: &PyAny = module.extract(py)?;
            Ok((module.getattr("__name__")?.extract()?, module.getattr("__file__")?.extract()?))
        })?;

        let source = fs::read_to_string(&source_file).map_err(|e| {
            exceptions::PyIOError::new_err(format!("failed to read input file: {}", e))
        })?;
        let parser_result = parser::parse_program(&source, source_file.into())
            .map_err(|e| exceptions::PySyntaxError::new_err(format!("parse error: {}", e)))?;

        for mut stmt in parser_result.into_iter() {
            let include = match stmt.node {
                ast::StmtKind::ClassDef {
                    ref decorator_list, ref mut body, ref mut bases, ..
                } => {
                    let nac3_class = decorator_list.iter().any(|decorator| {
                        if let ast::ExprKind::Name { id, .. } = decorator.node {
                            id.to_string() == "nac3"
                        } else {
                            false
                        }
                    });
                    if !nac3_class {
                        continue;
                    }
                    // Drop unregistered (i.e. host-only) base classes.
                    bases.retain(|base| {
                        Python::with_gil(|py| -> PyResult<bool> {
                            let id_fn = PyModule::import(py, "builtins")?.getattr("id")?;
                            match &base.node {
                                ast::ExprKind::Name { id, .. } => {
                                    if *id == "Exception".into() {
                                        Ok(true)
                                    } else {
                                        let base_obj = module.getattr(py, id.to_string())?;
                                        let base_id = id_fn.call1((base_obj,))?.extract()?;
                                        Ok(registered_class_ids.contains(&base_id))
                                    }
                                }
                                _ => Ok(true),
                            }
                        })
                        .unwrap()
                    });
                    body.retain(|stmt| {
                        if let ast::StmtKind::FunctionDef { ref decorator_list, .. } = stmt.node {
                            decorator_list.iter().any(|decorator| {
                                if let ast::ExprKind::Name { id, .. } = decorator.node {
                                    id.to_string() == "kernel"
                                        || id.to_string() == "portable"
                                        || id.to_string() == "rpc"
                                } else {
                                    false
                                }
                            })
                        } else {
                            true
                        }
                    });
                    true
                }
                ast::StmtKind::FunctionDef { ref decorator_list, .. } => {
                    decorator_list.iter().any(|decorator| {
                        if let ast::ExprKind::Name { id, .. } = decorator.node {
                            let id = id.to_string();
                            id == "extern" || id == "portable" || id == "kernel" || id == "rpc"
                        } else {
                            false
                        }
                    })
                }
                _ => false,
            };

            if include {
                self.top_levels.push((stmt, module_name.clone(), module.clone()));
            }
        }
        Ok(())
    }

    fn report_modinit(
        arg_names: &[String],
        method_name: &str,
        resolver: Arc<dyn SymbolResolver + Send + Sync>,
        top_level_defs: &[Arc<RwLock<TopLevelDef>>],
        unifier: &mut Unifier,
        primitives: &PrimitiveStore,
    ) -> Option<String> {
        let base_ty =
            match resolver.get_symbol_type(unifier, top_level_defs, primitives, "base".into()) {
                Ok(ty) => ty,
                Err(e) => return Some(format!("type error inside object launching kernel: {}", e)),
            };

        let fun_ty = if method_name.is_empty() {
            base_ty
        } else if let TypeEnum::TObj { fields, .. } = &*unifier.get_ty(base_ty) {
            match fields.get(&(*method_name).into()) {
                Some(t) => t.0,
                None => {
                    return Some(format!(
                        "object launching kernel does not have method `{}`",
                        method_name
                    ))
                }
            }
        } else {
            return Some("cannot launch kernel by calling a non-callable".into());
        };

        if let TypeEnum::TFunc(FunSignature { args, .. }) = &*unifier.get_ty(fun_ty) {
            if arg_names.len() > args.len() {
                return Some(format!(
                    "launching kernel function with too many arguments (expect {}, found {})",
                    args.len(),
                    arg_names.len(),
                ));
            }
            for (i, FuncArg { ty, default_value, name }) in args.iter().enumerate() {
                let in_name = match arg_names.get(i) {
                    Some(n) => n,
                    None if default_value.is_none() => {
                        return Some(format!(
                            "argument `{}` not provided when launching kernel function",
                            name
                        ))
                    }
                    _ => break,
                };
                let in_ty = match resolver.get_symbol_type(
                    unifier,
                    top_level_defs,
                    primitives,
                    in_name.clone().into(),
                ) {
                    Ok(t) => t,
                    Err(e) => {
                        return Some(format!(
                            "type error ({}) at parameter #{} when calling kernel function",
                            e, i
                        ))
                    }
                };
                if let Err(e) = unifier.unify(in_ty, *ty) {
                    return Some(format!(
                        "type error ({}) at parameter #{} when calling kernel function",
                        e.to_display(unifier).to_string(),
                        i
                    ));
                }
            }
        } else {
            return Some("cannot launch kernel by calling a non-callable".into());
        }
        None
    }
}

fn add_exceptions(
    composer: &mut TopLevelComposer,
    builtin_def: &mut HashMap<StrRef, DefinitionId>,
    builtin_ty: &mut HashMap<StrRef, Type>,
    error_names: &[&str]
) -> Vec<Type> {
    let mut types = Vec::new();
    // note: this is only for builtin exceptions, i.e. the exception name is "0:{exn}"
    for name in error_names {
        let def_id = composer.definition_ast_list.len();
        let (exception_fn, exception_class, exception_cons, exception_type) = get_exn_constructor(
            name,
            // class id
            def_id,
            // constructor id
            def_id + 1,
            &mut composer.unifier,
            &composer.primitives_ty
        );
        composer.definition_ast_list.push((Arc::new(RwLock::new(exception_class)), None));
        composer.definition_ast_list.push((Arc::new(RwLock::new(exception_fn)), None));
        builtin_ty.insert((*name).into(), exception_cons);
        builtin_def.insert((*name).into(), DefinitionId(def_id));
        types.push(exception_type);
    }
    types
}

#[pymethods]
impl Nac3 {
    #[new]
    fn new(isa: &str, py: Python) -> PyResult<Self> {
        let isa = match isa {
            "host" => Isa::Host,
            "rv32g" => Isa::RiscV32G,
            "rv32ima" => Isa::RiscV32IMA,
            "cortexa9" => Isa::CortexA9,
            _ => return Err(exceptions::PyValueError::new_err("invalid ISA")),
        };
        let time_fns: &(dyn TimeFns + Sync) = match isa {
            Isa::Host => &timeline::EXTERN_TIME_FNS,
            Isa::RiscV32G => &timeline::NOW_PINNING_TIME_FNS_64,
            Isa::RiscV32IMA => &timeline::NOW_PINNING_TIME_FNS,
            Isa::CortexA9 => &timeline::EXTERN_TIME_FNS,
        };
        let primitive: PrimitiveStore = TopLevelComposer::make_primitives().0;
        let builtins = vec![
            (
                "now_mu".into(),
                FunSignature { args: vec![], ret: primitive.int64, vars: HashMap::new() },
                Arc::new(GenCall::new(Box::new(move |ctx, _, _, _, _| {
                    Ok(Some(time_fns.emit_now_mu(ctx)))
                }))),
            ),
            (
                "at_mu".into(),
                FunSignature {
                    args: vec![FuncArg {
                        name: "t".into(),
                        ty: primitive.int64,
                        default_value: None,
                    }],
                    ret: primitive.none,
                    vars: HashMap::new(),
                },
                Arc::new(GenCall::new(Box::new(move |ctx, _, fun, args, generator| {
                    let arg_ty = fun.0.args[0].ty;
                    let arg = args[0].1.clone().to_basic_value_enum(ctx, generator, arg_ty).unwrap();
                    time_fns.emit_at_mu(ctx, arg);
                    Ok(None)
                }))),
            ),
            (
                "delay_mu".into(),
                FunSignature {
                    args: vec![FuncArg {
                        name: "dt".into(),
                        ty: primitive.int64,
                        default_value: None,
                    }],
                    ret: primitive.none,
                    vars: HashMap::new(),
                },
                Arc::new(GenCall::new(Box::new(move |ctx, _, fun, args, generator| {
                    let arg_ty = fun.0.args[0].ty;
                    let arg = args[0].1.clone().to_basic_value_enum(ctx, generator, arg_ty).unwrap();
                    time_fns.emit_delay_mu(ctx, arg);
                    Ok(None)
                }))),
            ),
        ];

        let builtins_mod = PyModule::import(py, "builtins").unwrap();
        let id_fn = builtins_mod.getattr("id").unwrap();
        let numpy_mod = PyModule::import(py, "numpy").unwrap();
        let typing_mod = PyModule::import(py, "typing").unwrap();
        let types_mod = PyModule::import(py, "types").unwrap();

        let get_id = |x| id_fn.call1((x,)).unwrap().extract().unwrap();
        let get_attr_id = |obj: &PyModule, attr| id_fn.call1((obj.getattr(attr).unwrap(),))
            .unwrap().extract().unwrap();
        let primitive_ids = PrimitivePythonId {
            virtual_id: get_id(
                            builtins_mod
                            .getattr("globals")
                            .unwrap()
                            .call0()
                            .unwrap()
                            .get_item("virtual")
                            .unwrap(
                        )),
            generic_alias: (
                get_attr_id(typing_mod, "_GenericAlias"),
                get_attr_id(types_mod, "GenericAlias"),
            ),
            none: id_fn
                .call1((builtins_mod
                    .getattr("globals")
                    .unwrap()
                    .call0()
                    .unwrap()
                    .get_item("none")
                    .unwrap(),))
                .unwrap()
                .extract()
                .unwrap(),
            typevar: get_attr_id(typing_mod, "TypeVar"),
            int: get_attr_id(builtins_mod, "int"),
            int32: get_attr_id(numpy_mod, "int32"),
            int64: get_attr_id(numpy_mod, "int64"),
            uint32: get_attr_id(numpy_mod, "uint32"),
            uint64: get_attr_id(numpy_mod, "uint64"),
            bool: get_attr_id(builtins_mod, "bool"),
            float: get_attr_id(builtins_mod, "float"),
            float64: get_attr_id(numpy_mod, "float64"),
            list: get_attr_id(builtins_mod, "list"),
            tuple: get_attr_id(builtins_mod, "tuple"),
            exception: get_attr_id(builtins_mod, "Exception"),
            option: id_fn
                .call1((builtins_mod
                    .getattr("globals")
                    .unwrap()
                    .call0()
                    .unwrap()
                    .get_item("Option")
                    .unwrap(),))
                .unwrap()
                .extract()
                .unwrap(),
        };

        let working_directory = tempfile::Builder::new().prefix("nac3-").tempdir().unwrap();
        fs::write(working_directory.path().join("kernel.ld"), include_bytes!("kernel.ld")).unwrap();

        Ok(Nac3 {
            isa,
            time_fns,
            primitive,
            builtins,
            primitive_ids,
            top_levels: Default::default(),
            pyid_to_def: Default::default(),
            working_directory,
            string_store: Default::default(),
            exception_ids: Default::default(),
            deferred_eval_store: DeferredEvaluationStore::new(),
        })
    }

    fn analyze(&mut self, functions: &PySet, classes: &PySet) -> PyResult<()> {
        let (modules, class_ids) =
            Python::with_gil(|py| -> PyResult<(HashMap<u64, PyObject>, HashSet<u64>)> {
                let mut modules: HashMap<u64, PyObject> = HashMap::new();
                let mut class_ids: HashSet<u64> = HashSet::new();

                let id_fn = PyModule::import(py, "builtins")?.getattr("id")?;
                let getmodule_fn = PyModule::import(py, "inspect")?.getattr("getmodule")?;

                for function in functions.iter() {
                    let module = getmodule_fn.call1((function,))?.extract()?;
                    modules.insert(id_fn.call1((&module,))?.extract()?, module);
                }
                for class in classes.iter() {
                    let module = getmodule_fn.call1((class,))?.extract()?;
                    modules.insert(id_fn.call1((&module,))?.extract()?, module);
                    class_ids.insert(id_fn.call1((class,))?.extract()?);
                }
                Ok((modules, class_ids))
            })?;

        for module in modules.into_values() {
            self.register_module(module, &class_ids)?;
        }
        Ok(())
    }

    fn compile_method_to_file(
        &mut self,
        obj: &PyAny,
        method_name: &str,
        args: Vec<&PyAny>,
        filename: &str,
        embedding_map: &PyAny,
        py: Python,
    ) -> PyResult<()> {
        let (mut composer, mut builtins_def, mut builtins_ty) = TopLevelComposer::new(
            self.builtins.clone(),
            ComposerConfig { kernel_ann: Some("Kernel"), kernel_invariant_ann: "KernelInvariant" },
        );

        let builtins = PyModule::import(py, "builtins")?;
        let typings = PyModule::import(py, "typing")?;
        let id_fn = builtins.getattr("id")?;
        let issubclass = builtins.getattr("issubclass")?;
        let exn_class = builtins.getattr("Exception")?;
        let store_obj = embedding_map.getattr("store_object").unwrap().to_object(py);
        let store_str = embedding_map.getattr("store_str").unwrap().to_object(py);
        let store_fun = embedding_map.getattr("store_function").unwrap().to_object(py);
        let host_attributes = embedding_map.getattr("attributes_writeback").unwrap().to_object(py);
        let global_value_ids: Arc<RwLock<HashMap<_, _>>> = Arc::new(RwLock::new(HashMap::new()));
        let helper = PythonHelper {
            id_fn: builtins.getattr("id").unwrap().to_object(py),
            len_fn: builtins.getattr("len").unwrap().to_object(py),
            type_fn: builtins.getattr("type").unwrap().to_object(py),
            origin_ty_fn: typings.getattr("get_origin").unwrap().to_object(py),
            args_ty_fn: typings.getattr("get_args").unwrap().to_object(py),
            store_obj: store_obj.clone(),
            store_str,
        };

        let pyid_to_type = Arc::new(RwLock::new(HashMap::<u64, Type>::new()));
        let exception_names = [
            "ZeroDivisionError",
            "IndexError",
            "ValueError",
            "RuntimeError",
            "AssertionError",
            "KeyError",
            "NotImplementedError",
            "OverflowError",
            "IOError",
            "UnwrapNoneError",
        ];
        add_exceptions(&mut composer, &mut builtins_def, &mut builtins_ty, &exception_names);

        let mut module_to_resolver_cache: HashMap<u64, _> = HashMap::new();

        let mut rpc_ids = vec![];
        for (stmt, path, module) in self.top_levels.iter() {
            let py_module: &PyAny = module.extract(py)?;
            let module_id: u64 = id_fn.call1((py_module,))?.extract()?;
            let helper = helper.clone();
            let class_obj;
            if let StmtKind::ClassDef { name, .. } = &stmt.node {
                let class = py_module.getattr(name.to_string()).unwrap();
                if issubclass.call1((class, exn_class)).unwrap().extract().unwrap() &&
                    class.getattr("artiq_builtin").is_err() {
                    class_obj = Some(class);
                } else {
                    class_obj = None;
                }
            } else {
                class_obj = None;
            }
            let (name_to_pyid, resolver) =
                module_to_resolver_cache.get(&module_id).cloned().unwrap_or_else(|| {
                    let mut name_to_pyid: HashMap<StrRef, u64> = HashMap::new();
                    let members: &PyDict =
                        py_module.getattr("__dict__").unwrap().cast_as().unwrap();
                    for (key, val) in members.iter() {
                        let key: &str = key.extract().unwrap();
                        let val = id_fn.call1((val,)).unwrap().extract().unwrap();
                        name_to_pyid.insert(key.into(), val);
                    }
                    let resolver = Arc::new(Resolver(Arc::new(InnerResolver {
                        id_to_type: builtins_ty.clone().into(),
                        id_to_def: builtins_def.clone().into(),
                        pyid_to_def: self.pyid_to_def.clone(),
                        pyid_to_type: pyid_to_type.clone(),
                        primitive_ids: self.primitive_ids.clone(),
                        global_value_ids: global_value_ids.clone(),
                        class_names: Default::default(),
                        name_to_pyid: name_to_pyid.clone(),
                        module: module.clone(),
                        id_to_pyval: Default::default(),
                        id_to_primitive: Default::default(),
                        field_to_val: Default::default(),
                        helper,
                        string_store: self.string_store.clone(),
                        exception_ids: self.exception_ids.clone(),
                        deferred_eval_store: self.deferred_eval_store.clone(),
                    })))
                        as Arc<dyn SymbolResolver + Send + Sync>;
                    let name_to_pyid = Rc::new(name_to_pyid);
                    module_to_resolver_cache
                        .insert(module_id, (name_to_pyid.clone(), resolver.clone()));
                    (name_to_pyid, resolver)
                });

            let (name, def_id, ty) = composer
                .register_top_level(stmt.clone(), Some(resolver.clone()), path.clone())
                .map_err(|e| {
                    CompileError::new_err(format!(
                        "compilation failed\n----------\n{}",
                        e
                    ))
                })?;
            if let Some(class_obj) = class_obj {
                self.exception_ids.write().insert(def_id.0, store_obj.call1(py, (class_obj, ))?.extract(py)?);
            }

            match &stmt.node {
                StmtKind::FunctionDef { decorator_list, .. } => {
                    if decorator_list.iter().any(|decorator| matches!(decorator.node, ExprKind::Name { id, .. } if id == "rpc".into())) {
                        store_fun.call1(py, (def_id.0.into_py(py), module.getattr(py, name.to_string()).unwrap())).unwrap();
                        rpc_ids.push((None, def_id));
                    }
                }
                StmtKind::ClassDef { name, body, .. } => {
                    let class_obj = module.getattr(py, name.to_string()).unwrap();
                    for stmt in body.iter() {
                        if let StmtKind::FunctionDef { name, decorator_list, .. } = &stmt.node {
                            if decorator_list.iter().any(|decorator| matches!(decorator.node, ExprKind::Name { id, .. } if id == "rpc".into())) {
                                rpc_ids.push((Some((class_obj.clone(), *name)), def_id));
                            }
                        }
                    }
                }
                _ => ()
            }

            let id = *name_to_pyid.get(&name).unwrap();
            self.pyid_to_def.write().insert(id, def_id);
            {
                let mut pyid_to_ty = pyid_to_type.write();
                if let Some(ty) = ty {
                    pyid_to_ty.insert(id, ty);
                }
            }
        }

        let id_fun = PyModule::import(py, "builtins")?.getattr("id")?;
        let mut name_to_pyid: HashMap<StrRef, u64> = HashMap::new();
        let module = PyModule::new(py, "tmp")?;
        module.add("base", obj)?;
        name_to_pyid.insert("base".into(), id_fun.call1((obj,))?.extract()?);
        let mut arg_names = vec![];
        for (i, arg) in args.into_iter().enumerate() {
            let name = format!("tmp{}", i);
            module.add(&name, arg)?;
            name_to_pyid.insert(name.clone().into(), id_fun.call1((arg,))?.extract()?);
            arg_names.push(name);
        }
        let synthesized = if method_name.is_empty() {
            format!("def __modinit__():\n    base({})", arg_names.join(", "))
        } else {
            format!("def __modinit__():\n    base.{}({})", method_name, arg_names.join(", "))
        };
        let mut synthesized =
            parse_program(&synthesized, "<nac3_synthesized_modinit>".to_string().into()).unwrap();
        let inner_resolver = Arc::new(InnerResolver {
            id_to_type: builtins_ty.clone().into(),
            id_to_def: builtins_def.clone().into(),
            pyid_to_def: self.pyid_to_def.clone(),
            pyid_to_type: pyid_to_type.clone(),
            primitive_ids: self.primitive_ids.clone(),
            global_value_ids: global_value_ids.clone(),
            class_names: Default::default(),
            id_to_pyval: Default::default(),
            id_to_primitive: Default::default(),
            field_to_val: Default::default(),
            name_to_pyid,
            module: module.to_object(py),
            helper,
            string_store: self.string_store.clone(),
            exception_ids: self.exception_ids.clone(),
            deferred_eval_store: self.deferred_eval_store.clone(),
        });
        let resolver = Arc::new(Resolver(inner_resolver.clone())) as Arc<dyn SymbolResolver + Send + Sync>;
        let (_, def_id, _) = composer
            .register_top_level(synthesized.pop().unwrap(), Some(resolver.clone()), "".into())
            .unwrap();

        let fun_signature =
            FunSignature { args: vec![], ret: self.primitive.none, vars: HashMap::new() };
        let mut store = ConcreteTypeStore::new();
        let mut cache = HashMap::new();
        let signature =
            store.from_signature(&mut composer.unifier, &self.primitive, &fun_signature, &mut cache);
        let signature = store.add_cty(signature);

        if let Err(e) = composer.start_analysis(true) {
            // report error of __modinit__ separately
            if !e.contains("<nac3_synthesized_modinit>") {
                return Err(CompileError::new_err(format!(
                    "compilation failed\n----------\n{}",
                    e
                )));
            } else {
                let msg = Self::report_modinit(
                    &arg_names,
                    method_name,
                    resolver.clone(),
                    &composer.extract_def_list(),
                    &mut composer.unifier,
                    &self.primitive,
                );
                return Err(CompileError::new_err(msg.unwrap_or(e)));
            }
        }
        let top_level = Arc::new(composer.make_top_level_context());

        {
            let rpc_codegen = rpc_codegen_callback();
            let defs = top_level.definitions.read();
            for (class_data, id) in rpc_ids.iter() {
                let mut def = defs[id.0].write();
                match &mut *def {
                    TopLevelDef::Function { codegen_callback, .. } => {
                        *codegen_callback = Some(rpc_codegen.clone());
                    }
                    TopLevelDef::Class { methods, .. } => {
                        let (class_def, method_name) = class_data.as_ref().unwrap();
                        for (name, _, id) in methods.iter() {
                            if name != method_name {
                                continue;
                            }
                            if let TopLevelDef::Function { codegen_callback, .. } =
                                &mut *defs[id.0].write()
                            {
                                *codegen_callback = Some(rpc_codegen.clone());
                                store_fun
                                    .call1(
                                        py,
                                        (
                                            id.0.into_py(py),
                                            class_def.getattr(py, name.to_string()).unwrap(),
                                        ),
                                    )
                                    .unwrap();
                            }
                        }
                    }
                }
            }
        }

        let instance = {
            let defs = top_level.definitions.read();
            let mut definition = defs[def_id.0].write();
            if let TopLevelDef::Function { instance_to_stmt, instance_to_symbol, .. } =
                &mut *definition
            {
                instance_to_symbol.insert("".to_string(), "__modinit__".into());
                instance_to_stmt[""].clone()
            } else {
                unreachable!()
            }
        };

        let task = CodeGenTask {
            subst: Default::default(),
            symbol_name: "__modinit__".to_string(),
            body: instance.body,
            signature,
            resolver: resolver.clone(),
            store,
            unifier_index: instance.unifier_id,
            calls: instance.calls,
            id: 0,
        };

        let mut store = ConcreteTypeStore::new();
        let mut cache = HashMap::new();
        let signature =
            store.from_signature(&mut composer.unifier, &self.primitive, &fun_signature, &mut cache);
        let signature = store.add_cty(signature);
        let attributes_writeback_task = CodeGenTask {
            subst: Default::default(),
            symbol_name: "attributes_writeback".to_string(),
            body: Arc::new(Default::default()),
            signature,
            resolver,
            store,
            unifier_index: instance.unifier_id,
            calls: Arc::new(Default::default()),
            id: 0,
        };
        let isa = self.isa;
        let working_directory = self.working_directory.path().to_owned();

        let membuffers: Arc<Mutex<Vec<Vec<u8>>>> = Default::default();

        let membuffer = membuffers.clone();

        let f = Arc::new(WithCall::new(Box::new(move |module| {
            let buffer = module.write_bitcode_to_memory();
            let buffer = buffer.as_slice().into();
            membuffer.lock().push(buffer);
        })));
        let size_t = if self.isa == Isa::Host { 64 } else { 32 };
        let thread_names: Vec<String> = (0..4).map(|_| "main".to_string()).collect();
        let threads: Vec<_> = thread_names
            .iter()
            .map(|s| Box::new(ArtiqCodeGenerator::new(s.to_string(), size_t, self.time_fns)))
            .collect();

        let membuffer = membuffers.clone();
        py.allow_threads(|| {
            let (registry, handles) = WorkerRegistry::create_workers(threads, top_level.clone(), f);
            registry.add_task(task);
            registry.wait_tasks_complete(handles);

            let mut generator = ArtiqCodeGenerator::new("attributes_writeback".to_string(), size_t, self.time_fns);
            let context = inkwell::context::Context::create();
            let module = context.create_module("attributes_writeback");
            let builder = context.create_builder();
            let (_, module, _) = gen_func_impl(&context, &mut generator, &registry, builder, module,
                attributes_writeback_task, |generator, ctx| {
                    attributes_writeback(ctx, generator, inner_resolver.as_ref(), host_attributes)
                }).unwrap();
            let buffer = module.write_bitcode_to_memory();
            let buffer = buffer.as_slice().into();
            membuffer.lock().push(buffer);
        });

        let context = inkwell::context::Context::create();
        let buffers = membuffers.lock();
        let main = context
            .create_module_from_ir(MemoryBuffer::create_from_memory_range(&buffers[0], "main"))
            .unwrap();
        for buffer in buffers.iter().skip(1) {
            let other = context
                .create_module_from_ir(MemoryBuffer::create_from_memory_range(buffer, "main"))
                .unwrap();

            main.link_in_module(other)
                .map_err(|err| CompileError::new_err(err.to_string()))?;
        }
        let builder = context.create_builder();
        let modinit_return = main.get_function("__modinit__").unwrap().get_last_basic_block().unwrap().get_terminator().unwrap();
        builder.position_before(&modinit_return);
        builder.build_call(main.get_function("attributes_writeback").unwrap(), &[], "attributes_writeback");

        main.link_in_module(load_irrt(&context))
            .map_err(|err| CompileError::new_err(err.to_string()))?;

        let mut function_iter = main.get_first_function();
        while let Some(func) = function_iter {
            if func.count_basic_blocks() > 0 && func.get_name().to_str().unwrap() != "__modinit__" {
                func.set_linkage(inkwell::module::Linkage::Private);
            }
            function_iter = func.get_next_function();
        }

        let builder = PassManagerBuilder::create();
        builder.set_optimization_level(OptimizationLevel::Aggressive);
        let passes = PassManager::create(());
        builder.set_inliner_with_threshold(255);
        builder.populate_module_pass_manager(&passes);
        passes.run_on(&main);

        let (triple, features) = match isa {
            Isa::Host => (
                TargetMachine::get_default_triple(),
                TargetMachine::get_host_cpu_features().to_string(),
            ),
            Isa::RiscV32G => {
                (TargetTriple::create("riscv32-unknown-linux"), "+a,+m,+f,+d".to_string())
            }
            Isa::RiscV32IMA => (TargetTriple::create("riscv32-unknown-linux"), "+a,+m".to_string()),
            Isa::CortexA9 => (
                TargetTriple::create("armv7-unknown-linux-gnueabihf"),
                "+dsp,+fp16,+neon,+vfp3".to_string(),
            ),
        };
        let target =
            Target::from_triple(&triple).expect("couldn't create target from target triple");
        let target_machine = target
            .create_target_machine(
                &triple,
                "",
                &features,
                OptimizationLevel::Default,
                RelocMode::PIC,
                CodeModel::Default,
            )
            .expect("couldn't create target machine");
        target_machine
            .write_to_file(&main, FileType::Object, &working_directory.join("module.o"))
            .expect("couldn't write module to file");

        let mut linker_args = vec![
            "-shared".to_string(),
            "--eh-frame-hdr".to_string(),
            "-x".to_string(),
            "-o".to_string(),
            filename.to_string(),
            working_directory.join("module.o").to_string_lossy().to_string(),
        ];
        if isa != Isa::Host {
            linker_args.push(
                "-T".to_string()
                    + self.working_directory.path().join("kernel.ld").to_str().unwrap(),
            );
        }

        #[cfg(not(windows))]
        let lld_command = "ld.lld";
        #[cfg(windows)]
        let lld_command = "ld.lld.exe";
        if let Ok(linker_status) = Command::new(lld_command).args(linker_args).status() {
            if !linker_status.success() {
                return Err(CompileError::new_err("failed to start linker"));
            }
        } else {
            return Err(CompileError::new_err(
                "linker returned non-zero status code",
            ));
        }

        Ok(())
    }

    fn compile_method_to_mem(
        &mut self,
        obj: &PyAny,
        method_name: &str,
        args: Vec<&PyAny>,
        embedding_map: &PyAny,
        py: Python,
    ) -> PyResult<PyObject> {
        let filename_path = self.working_directory.path().join("module.elf");
        let filename = filename_path.to_str().unwrap();
        self.compile_method_to_file(obj, method_name, args, filename, embedding_map, py)?;
        Ok(PyBytes::new(py, &fs::read(filename).unwrap()).into())
    }
}

#[cfg(feature = "init-llvm-profile")]
extern "C" {
    fn __llvm_profile_initialize();
}

#[pymodule]
fn nac3artiq(py: Python, m: &PyModule) -> PyResult<()> {
    #[cfg(feature = "init-llvm-profile")]
    unsafe {
        __llvm_profile_initialize();
    }

    Target::initialize_all(&InitializationConfig::default());
    m.add("CompileError", py.get_type::<CompileError>())?;
    m.add_class::<Nac3>()?;
    Ok(())
}
