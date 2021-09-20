use std::fs;
use inkwell::{
    passes::{PassManager, PassManagerBuilder},
    targets::*,
    OptimizationLevel,
};
use nac3core::typecheck::type_inferencer::PrimitiveStore;
use rustpython_parser::parser;
use std::{collections::HashMap, path::Path, sync::Arc};

use nac3core::{
    codegen::{CodeGenTask, WithCall, WorkerRegistry},
    symbol_resolver::SymbolResolver,
    toplevel::{composer::TopLevelComposer, TopLevelDef},
    typecheck::typedef::{FunSignature, FuncArg},
};

mod basic_symbol_resolver;
use basic_symbol_resolver::*;

fn main() {
    Target::initialize_all(&InitializationConfig::default());

    let program = match fs::read_to_string("mandelbrot.py") {
        Ok(program) => program,
        Err(err) => {
            println!("Cannot open input file: {}", err);
            return;
        }
    };

    let primitive: PrimitiveStore = TopLevelComposer::make_primitives().0;
    let (mut composer, builtins_def, builtins_ty) = TopLevelComposer::new(vec![
        ("output".into(), FunSignature {
            args: vec![FuncArg {
                name: "c".into(),
                ty: primitive.int32,
                default_value: None,
            }],
            ret: primitive.none,
            vars: HashMap::new(),
        })
    ]);

    let internal_resolver: Arc<ResolverInternal> = ResolverInternal {
        id_to_type: builtins_ty.into(),
        id_to_def: builtins_def.into(),
        class_names: Default::default(),
    }.into();
    let resolver = Arc::new(
        Box::new(Resolver(internal_resolver.clone())) as Box<dyn SymbolResolver + Send + Sync>
    );

    for stmt in parser::parse_program(&program).unwrap().into_iter() {
        let (name, def_id, ty) = composer.register_top_level(
            stmt,
            Some(resolver.clone()),
            "__main__".into(),
        ).unwrap();

        internal_resolver.add_id_def(name.clone(), def_id);
        if let Some(ty) = ty {
            internal_resolver.add_id_type(name, ty);
        }
    }

    composer.start_analysis(true).unwrap();

    let top_level = Arc::new(composer.make_top_level_context());

    let instance = {
        let defs = top_level.definitions.read();
        let mut instance = defs[resolver.get_identifier_def("run").unwrap().0].write();
        if let TopLevelDef::Function {
            instance_to_stmt,
            instance_to_symbol,
            ..
        } = &mut *instance
        {
            instance_to_symbol.insert("".to_string(), "run".to_string());
            instance_to_stmt[""].clone()
        } else {
            unreachable!()
        }
    };
    let signature = FunSignature {
        args: vec![],
        ret: primitive.int32,
        vars: HashMap::new(),
    };

    let task = CodeGenTask {
        subst: Default::default(),
        symbol_name: "run".to_string(),
        body: instance.body,
        signature,
        resolver,
        unifier: top_level.unifiers.read()[instance.unifier_id].clone(),
        calls: instance.calls,
    };
    let f = Arc::new(WithCall::new(Box::new(move |module| {
        let builder = PassManagerBuilder::create();
        builder.set_optimization_level(OptimizationLevel::Aggressive);
        let passes = PassManager::create(());
        builder.populate_module_pass_manager(&passes);
        passes.run_on(module);

        let triple = TargetMachine::get_default_triple();
        let target =
            Target::from_triple(&triple).expect("couldn't create target from target triple");
        let target_machine = target
            .create_target_machine(
                &triple,
                "",
                "",
                OptimizationLevel::Default,
                RelocMode::Default,
                CodeModel::Default,
            )
            .expect("couldn't create target machine");
        target_machine
            .write_to_file(module, FileType::Object, Path::new("mandelbrot.o"))
            .expect("couldn't write module to file");

        println!("IR:\n{}", module.print_to_string().to_str().unwrap());

    })));
    let threads = ["test"];
    let (registry, handles) = WorkerRegistry::create_workers(&threads, top_level, f);
    registry.add_task(task);
    registry.wait_tasks_complete(handles);
    println!("object file is in mandelbrot.o");
}
