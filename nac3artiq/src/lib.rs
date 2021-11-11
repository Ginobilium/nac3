use std::collections::{HashMap, HashSet};
use std::fs;
use std::process::Command;
use std::sync::Arc;

use inkwell::{
    passes::{PassManager, PassManagerBuilder},
    targets::*,
    OptimizationLevel,
};
use pyo3::prelude::*;
use pyo3::{exceptions, types::PyList, types::PySet, types::PyBytes};
use nac3parser::{
    ast::{self, StrRef},
    parser::{self, parse_program},
};

use parking_lot::{Mutex, RwLock};

use nac3core::{
    codegen::{concrete_type::ConcreteTypeStore, CodeGenTask, WithCall, WorkerRegistry},
    symbol_resolver::SymbolResolver,
    toplevel::{composer::TopLevelComposer, DefinitionId, GenCall, TopLevelContext, TopLevelDef},
    typecheck::typedef::{FunSignature, FuncArg},
    typecheck::{type_inferencer::PrimitiveStore, typedef::Type},
};

use tempfile::{self, TempDir};

use crate::{codegen::ArtiqCodeGenerator, symbol_resolver::Resolver};

mod codegen;
mod symbol_resolver;
mod timeline;

use timeline::TimeFns;

#[derive(PartialEq, Clone, Copy)]
enum Isa {
    Host,
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
    time_fns: &'static (dyn TimeFns + Sync),
    primitive: PrimitiveStore,
    builtins_ty: HashMap<StrRef, Type>,
    builtins_def: HashMap<StrRef, DefinitionId>,
    pyid_to_def: Arc<RwLock<HashMap<u64, DefinitionId>>>,
    pyid_to_type: Arc<RwLock<HashMap<u64, Type>>>,
    composer: TopLevelComposer,
    top_level: Option<Arc<TopLevelContext>>,
    primitive_ids: PrimitivePythonId,
    global_value_ids: Arc<Mutex<HashSet<u64>>>,
    working_directory: TempDir,
}

impl Nac3 {
    fn register_module(&mut self, module: PyObject, registered_class_ids: &HashSet<u64>) -> PyResult<()> {
        let mut name_to_pyid: HashMap<StrRef, u64> = HashMap::new();
        let (module_name, source_file) = Python::with_gil(|py| -> PyResult<(String, String)> {
            let module: &PyAny = module.extract(py)?;
            let builtins = PyModule::import(py, "builtins")?;
            let id_fn = builtins.getattr("id")?;
            let members: &PyList = PyModule::import(py, "inspect")?
                .getattr("getmembers")?
                .call1((module,))?
                .cast_as()?;
            for member in members.iter() {
                let key: &str = member.get_item(0)?.extract()?;
                let val = id_fn.call1((member.get_item(1)?,))?.extract()?;
                name_to_pyid.insert(key.into(), val);
            }
            Ok((
                module.getattr("__name__")?.extract()?,
                module.getattr("__file__")?.extract()?,
            ))
        })?;

        let source = fs::read_to_string(source_file).map_err(|e| {
            exceptions::PyIOError::new_err(format!("failed to read input file: {}", e))
        })?;
        let parser_result = parser::parse_program(&source)
            .map_err(|e| exceptions::PySyntaxError::new_err(format!("parse error: {}", e)))?;
        let resolver = Arc::new(Resolver {
            id_to_type: self.builtins_ty.clone().into(),
            id_to_def: self.builtins_def.clone().into(),
            pyid_to_def: self.pyid_to_def.clone(),
            pyid_to_type: self.pyid_to_type.clone(),
            primitive_ids: self.primitive_ids.clone(),
            global_value_ids: self.global_value_ids.clone(),
            class_names: Default::default(),
            name_to_pyid: name_to_pyid.clone(),
            module: module.clone(),
        }) as Arc<dyn SymbolResolver + Send + Sync>;
        let mut name_to_def = HashMap::new();
        let mut name_to_type = HashMap::new();

        for mut stmt in parser_result.into_iter() {
            let include = match stmt.node {
                ast::StmtKind::ClassDef {
                    ref decorator_list,
                    ref mut body,
                    ref mut bases,
                    ..
                } => {
                    let kernels = decorator_list.iter().any(|decorator| {
                        if let ast::ExprKind::Name { id, .. } = decorator.node {
                            id.to_string() == "nac3"
                        } else {
                            false
                        }
                    });
                    // Drop unregistered (i.e. host-only) base classes.
                    bases.retain(|base| {
                        Python::with_gil(|py| -> PyResult<bool> {
                            let id_fn = PyModule::import(py, "builtins")?.getattr("id")?;
                            match &base.node {
                                ast::ExprKind::Name { id, .. } => {
                                    let base_obj = module.getattr(py, id.to_string())?;
                                    let base_id = id_fn.call1((base_obj,))?.extract()?;
                                    Ok(registered_class_ids.contains(&base_id))
                                },
                                _ => Ok(true)
                            }
                        }).unwrap()
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
            "host" => Isa::Host,
            "riscv" => Isa::RiscV,
            "cortexa9" => Isa::CortexA9,
            _ => return Err(exceptions::PyValueError::new_err("invalid ISA")),
        };
        let time_fns: &(dyn TimeFns + Sync) = match isa {
            Isa::Host => &timeline::EXTERN_TIME_FNS,
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

        let working_directory = tempfile::Builder::new().prefix("nac3-").tempdir().unwrap();
        fs::write(working_directory.path().join("kernel.ld"), include_bytes!("kernel.ld")).unwrap();

        Ok(Nac3 {
            isa,
            time_fns,
            primitive,
            builtins_ty,
            builtins_def,
            composer,
            primitive_ids,
            top_level: None,
            pyid_to_def: Default::default(),
            pyid_to_type: Default::default(),
            global_value_ids: Default::default(),
            working_directory
        })
    }

    fn analyze(&mut self, functions: &PySet, classes: &PySet) -> PyResult<()> {
        let (modules, class_ids) = Python::with_gil(|py| -> PyResult<(HashMap<u64, PyObject>, HashSet<u64>)> {
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
        let resolver = Arc::new(Resolver {
            id_to_type: self.builtins_ty.clone().into(),
            id_to_def: self.builtins_def.clone().into(),
            pyid_to_def: self.pyid_to_def.clone(),
            pyid_to_type: self.pyid_to_type.clone(),
            primitive_ids: self.primitive_ids.clone(),
            global_value_ids: self.global_value_ids.clone(),
            class_names: Default::default(),
            name_to_pyid,
            module: module.to_object(py),
        }) as Arc<dyn SymbolResolver + Send + Sync>;
        let (_, def_id, _) = self
            .composer
            .register_top_level(
                synthesized.pop().unwrap(),
                Some(resolver.clone()),
                "".into(),
            )
            .unwrap();

        let signature = FunSignature {
            args: vec![],
            ret: self.primitive.none,
            vars: HashMap::new(),
        };
        let mut store = ConcreteTypeStore::new();
        let mut cache = HashMap::new();
        let signature = store.from_signature(
            &mut self.composer.unifier,
            &self.primitive,
            &signature,
            &mut cache,
        );
        let signature = store.add_cty(signature);

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

        let task = CodeGenTask {
            subst: Default::default(),
            symbol_name: "__modinit__".to_string(),
            body: instance.body,
            signature,
            resolver,
            store,
            unifier_index: instance.unifier_id,
            calls: instance.calls,
        };
        let isa = self.isa;
        let working_directory = self.working_directory.path().to_owned();
        let f = Arc::new(WithCall::new(Box::new(move |module| {
            let builder = PassManagerBuilder::create();
            builder.set_optimization_level(OptimizationLevel::Default);
            let passes = PassManager::create(());
            builder.populate_module_pass_manager(&passes);
            passes.run_on(module);

            let (triple, features) = match isa {
                Isa::Host => (TargetMachine::get_default_triple(), TargetMachine::get_host_cpu_features().to_string()),
                Isa::RiscV => (TargetTriple::create("riscv32-unknown-linux"), "+a,+m".to_string()),
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
                .write_to_file(
                    module,
                    FileType::Object,
                    &working_directory.join(&format!("{}.o", module.get_name().to_str().unwrap())),
                )
                .expect("couldn't write module to file");
        })));
        let thread_names: Vec<String> = (0..4).map(|i| format!("module{}", i)).collect();
        let threads: Vec<_> = thread_names
            .iter()
            .map(|s| Box::new(ArtiqCodeGenerator::new(s.to_string(), self.time_fns)))
            .collect();

        py.allow_threads(|| {
            let (registry, handles) = WorkerRegistry::create_workers(threads, top_level.clone(), f);
            registry.add_task(task);
            registry.wait_tasks_complete(handles);
        });

        let mut linker_args = vec![
            "-shared".to_string(),
            "--eh-frame-hdr".to_string(),
            "-x".to_string(),
            "-o".to_string(),
            filename.to_string(),
        ];
        if isa != Isa::Host {
            linker_args.push("-T".to_string() + self.working_directory.path().join("kernel.ld").to_str().unwrap());
        }
        linker_args.extend(thread_names.iter().map(|name| {
            let name_o = name.to_owned() + ".o";
            self.working_directory.path().join(name_o.as_str()).to_str().unwrap().to_string()
        }));
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

    fn compile_method_to_mem(
        &mut self,
        obj: &PyAny,
        method_name: &str,
        args: Vec<&PyAny>,
        py: Python,
    ) -> PyResult<PyObject> {
        let filename_path = self.working_directory.path().join("module.elf");
        let filename = filename_path.to_str().unwrap();
        self.compile_method_to_file(obj, method_name, args, filename, py)?;
        Ok(PyBytes::new(py, &fs::read(filename).unwrap()).into())
    }
}

#[pymodule]
fn nac3artiq(_py: Python, m: &PyModule) -> PyResult<()> {
    Target::initialize_all(&InitializationConfig::default());
    m.add_class::<Nac3>()?;
    Ok(())
}
