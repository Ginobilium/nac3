use std::fs;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use std::path::Path;
use std::process::Command;

use pyo3::prelude::*;
use pyo3::exceptions;
use rustpython_parser::{ast, parser};
use inkwell::{
    passes::{PassManager, PassManagerBuilder},
    targets::*,
    OptimizationLevel,
};

use nac3core::typecheck::type_inferencer::PrimitiveStore;
use nac3core::{
    codegen::{CodeGenTask, WithCall, WorkerRegistry, GenCall},
    symbol_resolver::SymbolResolver,
    toplevel::{composer::TopLevelComposer, TopLevelContext, TopLevelDef},
    typecheck::typedef::{FunSignature, FuncArg},
};

mod symbol_resolver;
use symbol_resolver::*;

#[derive(Clone, Copy)]
enum Isa {
    RiscV,
    CortexA9
}

// TODO: do we really want unsendable?
// TopLevelComposer causes a lot of problems for Send.
#[pyclass(unsendable,name="NAC3")]
struct Nac3 {
    isa: Isa,
    primitive: PrimitiveStore,
    internal_resolver: Arc<ResolverInternal>,
    resolver: Arc<Box<dyn SymbolResolver + Send + Sync>>,
    composer: TopLevelComposer,
    top_level: Option<Arc<TopLevelContext>>,
    registered_module_ids: HashSet<u64>
}

#[pymethods]
impl Nac3 {
    #[new]
    fn new(isa: &str) -> PyResult<Self> {
        let isa = match isa {
            "riscv" => Isa::RiscV,
            "cortexa9" => Isa::CortexA9,
            _ => return Err(exceptions::PyValueError::new_err("invalid ISA"))
        };
        let primitive: PrimitiveStore = TopLevelComposer::make_primitives().0;
        let (composer, builtins_def, builtins_ty) = TopLevelComposer::new(vec![
            ("output_int".into(), FunSignature {
                args: vec![FuncArg {
                    name: "x".into(),
                    ty: primitive.int32,
                    default_value: None,
                }],
                ret: primitive.none,
                vars: HashMap::new(),
            }),
        ]);
        let internal_resolver: Arc<ResolverInternal> = ResolverInternal {
            id_to_type: builtins_ty.into(),
            id_to_def: builtins_def.into(),
            class_names: Default::default(),
        }.into();
        let resolver = Arc::new(
            Box::new(Resolver(internal_resolver.clone())) as Box<dyn SymbolResolver + Send + Sync>
        );
        Ok(Nac3 {
            isa,
            primitive,
            internal_resolver,
            resolver,
            composer,
            top_level: None,
            registered_module_ids: HashSet::new()
        })
    }

    fn register_module(&mut self, obj: PyObject) -> PyResult<()> {
        let module_info = Python::with_gil(|py| -> PyResult<Option<(String, String)>> {
            let obj: &PyAny = obj.extract(py)?;
            let builtins = PyModule::import(py, "builtins")?;
            let id = builtins.getattr("id")?.call1((obj, ))?.extract()?;
            if self.registered_module_ids.insert(id) {
                Ok(Some((obj.getattr("__name__")?.extract()?, obj.getattr("__file__")?.extract()?)))
            } else {
                Ok(None)
            }
        })?;

        if let Some((module_name, source_file)) = module_info {
            let source = fs::read_to_string(source_file).map_err(|e|
                exceptions::PyIOError::new_err(format!("failed to read input file: {}", e)))?;
            let parser_result = parser::parse_program(&source).map_err(|e|
                exceptions::PySyntaxError::new_err(format!("failed to parse host object source: {}", e)))?;

            for mut stmt in parser_result.into_iter() {
                let include = match stmt.node {
                    ast::StmtKind::ClassDef { ref decorator_list, ref mut body, .. } => {
                        let kernels = decorator_list.iter().any(|decorator| if let ast::ExprKind::Name { id, .. } = decorator.node
                            { id.to_string() == "kernel" || id.to_string() == "portable" } else { false });
                        body.retain(|stmt| {
                            if let ast::StmtKind::FunctionDef { ref decorator_list, .. } = stmt.node {
                                decorator_list.iter().any(|decorator| if let ast::ExprKind::Name { id, .. } = decorator.node
                                    { id.to_string() == "kernel" || id.to_string() == "portable" } else { false })
                            } else {
                                true
                            }
                        });
                        kernels
                    },
                    ast::StmtKind::FunctionDef { ref decorator_list, .. } => {
                        decorator_list.iter().any(|decorator| if let ast::ExprKind::Name { id, .. } = decorator.node
                            { id.to_string() == "extern" || id.to_string() == "portable" } else { false })
                    },
                    _ => false
                };

                if include {
                    let (name, def_id, ty) = self.composer.register_top_level(
                        stmt,
                        Some(self.resolver.clone()),
                        module_name.clone(),
                    ).unwrap();

                    self.internal_resolver.add_id_def(name.clone(), def_id);
                    if let Some(ty) = ty {
                        self.internal_resolver.add_id_type(name, ty);
                    }
                }
            }
        }
        Ok(())
    }

    fn analyze(&mut self) -> PyResult<()> {
        self.composer.start_analysis(true).unwrap();
        self.top_level = Some(Arc::new(self.composer.make_top_level_context()));
        Ok(())
    }

    fn compile_method(&mut self, class_name: String, method_name: String) -> PyResult<()> {
        let top_level = self.top_level.as_ref().unwrap();
        let instance = {
            let defs = top_level.definitions.read();
            let class_def = defs[self.resolver.get_identifier_def(class_name.into()).unwrap().0].write();
            let mut method_def = if let TopLevelDef::Class { methods, .. } = &*class_def {
                if let Some((_name, _unification_key, definition_id)) = methods.iter().find(|method| method.0.to_string() == method_name) {
                    defs[definition_id.0].write()
                } else {
                    return Err(exceptions::PyValueError::new_err("method not found"));
                }
            } else {
                return Err(exceptions::PyTypeError::new_err("parent object is not a class"));
            };

            // FIXME: what is this for? What happens if the kernel is called twice?
            if let TopLevelDef::Function {
                instance_to_stmt,
                instance_to_symbol,
                ..
            } = &mut *method_def
            {
                instance_to_symbol.insert("".to_string(), method_name.clone());
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
            resolver: self.resolver.clone(),
            unifier: top_level.unifiers.read()[instance.unifier_id].clone(),
            calls: instance.calls,
        };
        let isa = self.isa;
        let f = Arc::new(WithCall::new(Box::new(move |module| {
            let builder = PassManagerBuilder::create();
            builder.set_optimization_level(OptimizationLevel::Aggressive);
            let passes = PassManager::create(());
            builder.populate_module_pass_manager(&passes);
            passes.run_on(module);

            let (triple, features) = match isa {
                Isa::RiscV => (TargetTriple::create("riscv32-unknown-linux"), "+a,+m"),
                Isa::CortexA9 => (TargetTriple::create("armv7-unknown-linux-gnueabihf"), "+dsp,+fp16,+neon,+vfp3"),
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
                .write_to_file(module, FileType::Object, Path::new(&format!("{}.o", module.get_name().to_str().unwrap())))
                .expect("couldn't write module to file");
        })));
        let external_codegen = Arc::new(GenCall::new(Box::new(|_, _, _, _| unimplemented!()), HashSet::new()));
        let thread_names: Vec<String> = (0..4).map(|i| format!("module{}", i)).collect();
        let threads: Vec<_> = thread_names.iter().map(|s| s.as_str()).collect();
        let (registry, handles) = WorkerRegistry::create_workers(&threads, top_level.clone(), f, external_codegen);
        registry.add_task(task);
        registry.wait_tasks_complete(handles);

        let mut linker_args = vec![
            "-shared".to_string(),
            "--eh-frame-hdr".to_string(),
            "-Tkernel.ld".to_string(),
            "-x".to_string(),
            "-o".to_string(),
            "module.elf".to_string()
        ];
        linker_args.extend(thread_names.iter().map(|name| name.to_owned() + ".o"));
        if let Ok(linker_status) = Command::new("ld.lld").args(linker_args).status() {
            if !linker_status.success() {
                return Err(exceptions::PyRuntimeError::new_err("failed to start linker"));
            }
        } else {
            return Err(exceptions::PyRuntimeError::new_err("linker returned non-zero status code"));
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
