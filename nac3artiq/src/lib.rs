use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::Path;
use std::process::Command;
use std::sync::Arc;

use inkwell::{
    passes::{PassManager, PassManagerBuilder},
    targets::*,
    OptimizationLevel,
};
use pyo3::prelude::*;
use pyo3::{exceptions, types::PyList};
use rustpython_parser::{
    ast::{self, StrRef},
    parser::{self, parse_program},
};

use parking_lot::{Mutex, RwLock};

use nac3core::{
    codegen::{CodeGenTask, WithCall, WorkerRegistry},
    symbol_resolver::SymbolResolver,
    toplevel::{composer::TopLevelComposer, DefinitionId, GenCall, TopLevelContext, TopLevelDef},
    typecheck::typedef::{FunSignature, FuncArg},
    typecheck::{type_inferencer::PrimitiveStore, typedef::Type},
};

use crate::symbol_resolver::Resolver;

mod symbol_resolver;
mod timeline;

use timeline::TimeFns;

#[derive(PartialEq, Clone, Copy)]
enum Isa {
    RiscV,
    CortexA9,
}

#[derive(Clone)]
pub struct PrimitivePythonId {
    int: u64,
    int32: u64,
    int64: u64,
    float: u64,
    bool: u64,
    list: u64,
    tuple: u64,
}

// TopLevelComposer is unsendable as it holds the unification table, which is
// unsendable due to Rc. Arc would cause a performance hit.
#[pyclass(unsendable, name = "NAC3")]
struct Nac3 {
    isa: Isa,
    primitive: PrimitiveStore,
    builtins_ty: HashMap<StrRef, Type>,
    builtins_def: HashMap<StrRef, DefinitionId>,
    pyid_to_def: Arc<RwLock<HashMap<u64, DefinitionId>>>,
    pyid_to_type: Arc<RwLock<HashMap<u64, Type>>>,
    composer: TopLevelComposer,
    top_level: Option<Arc<TopLevelContext>>,
    to_be_registered: Vec<PyObject>,
    primitive_ids: PrimitivePythonId,
    global_value_ids: Arc<Mutex<HashSet<u64>>>,
}

impl Nac3 {
    fn register_module_impl(&mut self, obj: PyObject) -> PyResult<()> {
        let mut name_to_pyid: HashMap<StrRef, u64> = HashMap::new();
        let (module_name, source_file) = Python::with_gil(|py| -> PyResult<(String, String)> {
            let obj: &PyAny = obj.extract(py)?;
            let builtins = PyModule::import(py, "builtins")?;
            let id_fn = builtins.getattr("id")?;
            let members: &PyList = PyModule::import(py, "inspect")?
                .getattr("getmembers")?
                .call1((obj,))?
                .cast_as()?;
            for member in members.iter() {
                let key: &str = member.get_item(0)?.extract()?;
                let val = id_fn.call1((member.get_item(1)?,))?.extract()?;
                name_to_pyid.insert(key.into(), val);
            }
            Ok((
                obj.getattr("__name__")?.extract()?,
                obj.getattr("__file__")?.extract()?,
            ))
        })?;

        let source = fs::read_to_string(source_file).map_err(|e| {
            exceptions::PyIOError::new_err(format!("failed to read input file: {}", e))
        })?;
        let parser_result = parser::parse_program(&source)
            .map_err(|e| exceptions::PySyntaxError::new_err(format!("parse error: {}", e)))?;
        let resolver = Arc::new(Box::new(Resolver {
            id_to_type: self.builtins_ty.clone().into(),
            id_to_def: self.builtins_def.clone().into(),
            pyid_to_def: self.pyid_to_def.clone(),
            pyid_to_type: self.pyid_to_type.clone(),
            primitive_ids: self.primitive_ids.clone(),
            global_value_ids: self.global_value_ids.clone(),
            class_names: Default::default(),
            name_to_pyid: name_to_pyid.clone(),
            module: obj,
        }) as Box<dyn SymbolResolver + Send + Sync>);
        let mut name_to_def = HashMap::new();
        let mut name_to_type = HashMap::new();

        for mut stmt in parser_result.into_iter() {
            let include = match stmt.node {
                ast::StmtKind::ClassDef {
                    ref decorator_list,
                    ref mut body,
                    ..
                } => {
                    let kernels = decorator_list.iter().any(|decorator| {
                        if let ast::ExprKind::Name { id, .. } = decorator.node {
                            id.to_string() == "kernel" || id.to_string() == "portable"
                        } else {
                            false
                        }
                    });
                    body.retain(|stmt| {
                        if let ast::StmtKind::FunctionDef {
                            ref decorator_list, ..
                        } = stmt.node
                        {
                            decorator_list.iter().any(|decorator| {
                                if let ast::ExprKind::Name { id, .. } = decorator.node {
                                    id.to_string() == "kernel" || id.to_string() == "portable"
                                } else {
                                    false
                                }
                            })
                        } else {
                            true
                        }
                    });
                    kernels
                }
                ast::StmtKind::FunctionDef {
                    ref decorator_list, ..
                } => decorator_list.iter().any(|decorator| {
                    if let ast::ExprKind::Name { id, .. } = decorator.node {
                        let id = id.to_string();
                        id == "extern" || id == "portable" || id == "kernel"
                    } else {
                        false
                    }
                }),
                _ => false,
            };

            if include {
                let (name, def_id, ty) = self
                    .composer
                    .register_top_level(stmt, Some(resolver.clone()), module_name.clone())
                    .unwrap();
                name_to_def.insert(name, def_id);
                if let Some(ty) = ty {
                    name_to_type.insert(name, ty);
                }
            }
        }
        let mut map = self.pyid_to_def.write();
        for (name, def) in name_to_def.into_iter() {
            map.insert(*name_to_pyid.get(&name).unwrap(), def);
        }
        let mut map = self.pyid_to_type.write();
        for (name, ty) in name_to_type.into_iter() {
            map.insert(*name_to_pyid.get(&name).unwrap(), ty);
        }
        Ok(())
    }
}

#[pymethods]
impl Nac3 {
    #[new]
    fn new(isa: &str, py: Python) -> PyResult<Self> {
        let isa = match isa {
            "riscv" => Isa::RiscV,
            "cortexa9" => Isa::CortexA9,
            _ => return Err(exceptions::PyValueError::new_err("invalid ISA")),
        };
        let time_fns: &(dyn TimeFns + Sync) = match isa {
            Isa::RiscV => &timeline::NOW_PINNING_TIME_FNS,
            Isa::CortexA9 => &timeline::EXTERN_TIME_FNS,
        };
        let primitive: PrimitiveStore = TopLevelComposer::make_primitives().0;
        let builtins = vec![
            (
                "now_mu".into(),
                FunSignature {
                    args: vec![],
                    ret: primitive.int64,
                    vars: HashMap::new(),
                },
                Arc::new(GenCall::new(Box::new(move |ctx, _, _, _| {
                    Some(time_fns.emit_now_mu(ctx))
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
                Arc::new(GenCall::new(Box::new(move |ctx, _, _, args| {
                    time_fns.emit_at_mu(ctx, args[0].1);
                    None
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
                Arc::new(GenCall::new(Box::new(move |ctx, _, _, args| {
                    time_fns.emit_delay_mu(ctx, args[0].1);
                    None
                }))),
            ),
        ];
        let (composer, builtins_def, builtins_ty) = TopLevelComposer::new(builtins);

        let builtins_mod = PyModule::import(py, "builtins").unwrap();
        let id_fn = builtins_mod.getattr("id").unwrap();
        let numpy_mod = PyModule::import(py, "numpy").unwrap();
        let primitive_ids = PrimitivePythonId {
            int: id_fn
                .call1((builtins_mod.getattr("int").unwrap(),))
                .unwrap()
                .extract()
                .unwrap(),
            int32: id_fn
                .call1((numpy_mod.getattr("int32").unwrap(),))
                .unwrap()
                .extract()
                .unwrap(),
            int64: id_fn
                .call1((numpy_mod.getattr("int64").unwrap(),))
                .unwrap()
                .extract()
                .unwrap(),
            bool: id_fn
                .call1((builtins_mod.getattr("bool").unwrap(),))
                .unwrap()
                .extract()
                .unwrap(),
            float: id_fn
                .call1((builtins_mod.getattr("float").unwrap(),))
                .unwrap()
                .extract()
                .unwrap(),
            list: id_fn
                .call1((builtins_mod.getattr("list").unwrap(),))
                .unwrap()
                .extract()
                .unwrap(),
            tuple: id_fn
                .call1((builtins_mod.getattr("tuple").unwrap(),))
                .unwrap()
                .extract()
                .unwrap(),
        };

        Ok(Nac3 {
            isa,
            primitive,
            builtins_ty,
            builtins_def,
            composer,
            primitive_ids,
            top_level: None,
            pyid_to_def: Default::default(),
            pyid_to_type: Default::default(),
            to_be_registered: Default::default(),
            global_value_ids: Default::default(),
        })
    }

    fn register_module(&mut self, obj: PyObject) {
        // Delay registration until all referenced variables are supposed to exist on the CPython side
        self.to_be_registered.push(obj);
    }

    fn analyze(&mut self) -> PyResult<()> {
        for obj in std::mem::take(&mut self.to_be_registered).into_iter() {
            self.register_module_impl(obj)?;
        }
        Ok(())
    }

    fn compile_method(
        &mut self,
        obj: &PyAny,
        method_name: String,
        args: Vec<&PyAny>,
        py: Python,
    ) -> PyResult<()> {
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
            format!(
                "def __modinit__():\n    base.{}({})",
                method_name,
                arg_names.join(", ")
            )
        };
        let mut synthesized = parse_program(&synthesized).unwrap();
        let resolver = Arc::new(Box::new(Resolver {
            id_to_type: self.builtins_ty.clone().into(),
            id_to_def: self.builtins_def.clone().into(),
            pyid_to_def: self.pyid_to_def.clone(),
            pyid_to_type: self.pyid_to_type.clone(),
            primitive_ids: self.primitive_ids.clone(),
            global_value_ids: self.global_value_ids.clone(),
            class_names: Default::default(),
            name_to_pyid,
            module: module.to_object(py),
        }) as Box<dyn SymbolResolver + Send + Sync>);
        let (_, def_id, _) = self
            .composer
            .register_top_level(
                synthesized.pop().unwrap(),
                Some(resolver.clone()),
                "".into(),
            )
            .unwrap();

        self.composer.start_analysis(true).unwrap();
        self.top_level = Some(Arc::new(self.composer.make_top_level_context()));
        let top_level = self.top_level.as_ref().unwrap();
        let instance = {
            let defs = top_level.definitions.read();
            let mut definition = defs[def_id.0].write();
            if let TopLevelDef::Function {
                instance_to_stmt,
                instance_to_symbol,
                ..
            } = &mut *definition
            {
                instance_to_symbol.insert("".to_string(), "__modinit__".into());
                instance_to_stmt[""].clone()
            } else {
                unreachable!()
            }
        };

        let signature = FunSignature {
            args: vec![],
            ret: self.primitive.none,
            vars: HashMap::new(),
        };
        let task = CodeGenTask {
            subst: Default::default(),
            symbol_name: "__modinit__".to_string(),
            body: instance.body,
            signature,
            resolver,
            unifier: top_level.unifiers.read()[instance.unifier_id].clone(),
            calls: instance.calls,
        };
        let isa = self.isa;
        let f = Arc::new(WithCall::new(Box::new(move |module| {
            let builder = PassManagerBuilder::create();
            builder.set_optimization_level(OptimizationLevel::Default);
            let passes = PassManager::create(());
            builder.populate_module_pass_manager(&passes);
            passes.run_on(module);

            let (triple, features) = match isa {
                Isa::RiscV => (TargetTriple::create("riscv32-unknown-linux"), "+a,+m"),
                Isa::CortexA9 => (
                    TargetTriple::create("armv7-unknown-linux-gnueabihf"),
                    "+dsp,+fp16,+neon,+vfp3",
                ),
            };
            let target =
                Target::from_triple(&triple).expect("couldn't create target from target triple");
            let target_machine = target
                .create_target_machine(
                    &triple,
                    "",
                    features,
                    OptimizationLevel::Default,
                    RelocMode::PIC,
                    CodeModel::Default,
                )
                .expect("couldn't create target machine");
            target_machine
                .write_to_file(
                    module,
                    FileType::Object,
                    Path::new(&format!("{}.o", module.get_name().to_str().unwrap())),
                )
                .expect("couldn't write module to file");
        })));
        let thread_names: Vec<String> = (0..4).map(|i| format!("module{}", i)).collect();
        let threads: Vec<_> = thread_names.iter().map(|s| s.as_str()).collect();

        py.allow_threads(|| {
            let (registry, handles) =
                WorkerRegistry::create_workers(&threads, top_level.clone(), f);
            registry.add_task(task);
            registry.wait_tasks_complete(handles);
        });

        let mut linker_args = vec![
            "-shared".to_string(),
            "--eh-frame-hdr".to_string(),
            "-Tkernel.ld".to_string(),
            "-x".to_string(),
            "-o".to_string(),
            "module.elf".to_string(),
        ];
        linker_args.extend(thread_names.iter().map(|name| name.to_owned() + ".o"));
        if let Ok(linker_status) = Command::new("ld.lld").args(linker_args).status() {
            if !linker_status.success() {
                return Err(exceptions::PyRuntimeError::new_err(
                    "failed to start linker",
                ));
            }
        } else {
            return Err(exceptions::PyRuntimeError::new_err(
                "linker returned non-zero status code",
            ));
        }

        Ok(())
    }
}

#[pymodule]
fn nac3artiq(_py: Python, m: &PyModule) -> PyResult<()> {
    Target::initialize_all(&InitializationConfig::default());
    m.add_class::<Nac3>()?;
    Ok(())
}
