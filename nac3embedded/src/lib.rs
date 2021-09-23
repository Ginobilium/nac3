use std::collections::HashMap;
use std::sync::Arc;
use std::path::Path;

use pyo3::prelude::*;
use pyo3::exceptions;
use rustpython_parser::parser;
use inkwell::{
    passes::{PassManager, PassManagerBuilder},
    targets::*,
    OptimizationLevel,
};

use nac3core::typecheck::type_inferencer::PrimitiveStore;
use nac3core::{
    codegen::{CodeGenTask, WithCall, WorkerRegistry},
    symbol_resolver::SymbolResolver,
    toplevel::{composer::TopLevelComposer, TopLevelContext, TopLevelDef},
    typecheck::typedef::{FunSignature, FuncArg},
};

mod symbol_resolver;
use symbol_resolver::*;

// TODO: do we really want unsendable?
// TopLevelComposer causes a lot of problems for Send.
#[pyclass(unsendable,name="NAC3")]
struct Nac3 {
    primitive: PrimitiveStore,
    internal_resolver: Arc<ResolverInternal>,
    resolver: Arc<Box<dyn SymbolResolver + Send + Sync>>,
    composer: TopLevelComposer,
    top_level: Option<Arc<TopLevelContext>>
}

#[pymethods]
impl Nac3 {
    #[new]
    fn new() -> Self {
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
        Nac3 {
            primitive: primitive,
            internal_resolver: internal_resolver,
            resolver: resolver,
            composer: composer,
            top_level: None
        }
    }

    fn register_class(&mut self, obj: PyObject) -> PyResult<()> {
        Python::with_gil(|py| -> PyResult<()> {
            let obj: &PyAny = obj.extract(py)?;

            let source = PyModule::import(py, "inspect")?.getattr("getsource")?.call1((obj, ))?.extract()?;
            let parser_result = parser::parse_program(source).map_err(|e|
                exceptions::PySyntaxError::new_err(format!("failed to parse host object source: {}", e)))?;

            for stmt in parser_result.into_iter() {
                let (name, def_id, ty) = self.composer.register_top_level(
                    stmt,
                    Some(self.resolver.clone()),
                    "__main__".into(),
                ).unwrap();

                self.internal_resolver.add_id_def(name.clone(), def_id);
                if let Some(ty) = ty {
                    self.internal_resolver.add_id_type(name, ty);
                }
            }

            Ok(())
        })
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
            let class_def = defs[self.resolver.get_identifier_def(&class_name).unwrap().0].write();
            let mut method_def = if let TopLevelDef::Class { methods, .. } = &*class_def {
                if let Some((_name, _unification_key, definition_id)) = methods.iter().find(|method| method.0 == method_name) {
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
            symbol_name: method_name,
            body: instance.body,
            signature,
            resolver: self.resolver.clone(),
            unifier: top_level.unifiers.read()[instance.unifier_id].clone(),
            calls: instance.calls,
        };
        let f = Arc::new(WithCall::new(Box::new(move |module| {
            let builder = PassManagerBuilder::create();
            builder.set_optimization_level(OptimizationLevel::Aggressive);
            let passes = PassManager::create(());
            builder.populate_module_pass_manager(&passes);
            passes.run_on(module);

            let triple = TargetTriple::create("riscv32-unknown-linux");
            let target =
                Target::from_triple(&triple).expect("couldn't create target from target triple");
            let target_machine = target
                .create_target_machine(
                    &triple,
                    "",
                    "+a,+m",
                    OptimizationLevel::Default,
                    RelocMode::PIC,
                    CodeModel::Default,
                )
                .expect("couldn't create target machine");
            target_machine
                .write_to_file(module, FileType::Object, Path::new(&format!("{}.o", module.get_name().to_str().unwrap())))
                .expect("couldn't write module to file");
        })));
        let threads: Vec<String> = (0..4).map(|i| format!("module{}", i)).collect();
        let threads: Vec<_> = threads.iter().map(|s| s.as_str()).collect();
        let (registry, handles) = WorkerRegistry::create_workers(&threads, top_level.clone(), f);
        registry.add_task(task);
        registry.wait_tasks_complete(handles);
        Ok(())
    }
}

#[pymodule]
fn nac3embedded(_py: Python, m: &PyModule) -> PyResult<()> {
    Target::initialize_all(&InitializationConfig::default());
    m.add_class::<Nac3>()?;
    Ok(())
}
