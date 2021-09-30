use std::env;
use std::fs;
use inkwell::{OptimizationLevel, passes::{PassManager, PassManagerBuilder}, targets::*};
use nac3core::typecheck::type_inferencer::PrimitiveStore;
use rustpython_parser::parser;
use std::{collections::HashMap, path::Path, sync::Arc, time::SystemTime};

use nac3core::{
    codegen::{CodeGenTask, WithCall, WorkerRegistry},
    symbol_resolver::SymbolResolver,
    toplevel::{composer::TopLevelComposer, TopLevelDef},
    typecheck::typedef::{FunSignature, FuncArg},
};

mod basic_symbol_resolver;
use basic_symbol_resolver::*;

fn main() {
    let demo_name = env::args().nth(1).unwrap();
    let threads: u32 = env::args().nth(2).map(|s| str::parse(&s).unwrap()).unwrap_or(1);

    let start = SystemTime::now();

    Target::initialize_all(&InitializationConfig::default());

    let program = match fs::read_to_string(demo_name + ".py") {
        Ok(program) => program,
        Err(err) => {
            println!("Cannot open input file: {}", err);
            return;
        }
   };

    let primitive: PrimitiveStore = TopLevelComposer::make_primitives().0;
    let (mut composer, builtins_def, builtins_ty) = TopLevelComposer::new(vec![
        ("output_int".into(), FunSignature {
            args: vec![FuncArg {
                name: "x".into(),
                ty: primitive.int32,
                default_value: None,
            }],
            ret: primitive.none,
            vars: HashMap::new(),
        }),
        ("output_asciiart".into(), FunSignature {
            args: vec![FuncArg {
                name: "x".into(),
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
    let setup_time = SystemTime::now();
    println!("setup time: {}ms", setup_time.duration_since(start).unwrap().as_millis());

    let parser_result = parser::parse_program(&program).unwrap();
    let parse_time = SystemTime::now();
    println!("parse time: {}ms", parse_time.duration_since(setup_time).unwrap().as_millis());

    for stmt in parser_result.into_iter() {
        let (name, def_id, ty) = composer.register_top_level(
            stmt,
            Some(resolver.clone()),
            "__main__".into(),
        ).unwrap();

        internal_resolver.add_id_def(name, def_id);
        if let Some(ty) = ty {
            internal_resolver.add_id_type(name, ty);
        }
    }

    composer.start_analysis(true).unwrap();
    let analysis_time = SystemTime::now();
    println!("analysis time: {}ms", analysis_time.duration_since(parse_time).unwrap().as_millis());

    let top_level = Arc::new(composer.make_top_level_context());

    let instance = {
        let defs = top_level.definitions.read();
        let mut instance = defs[resolver.get_identifier_def("run".into()).unwrap().0].write();
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
            .write_to_file(module, FileType::Object, Path::new(&format!("{}.o", module.get_name().to_str().unwrap())))
            .expect("couldn't write module to file");

        // println!("IR:\n{}", module.print_to_string().to_str().unwrap());

    })));
    let threads: Vec<String> = (0..threads).map(|i| format!("module{}", i)).collect();
    let threads: Vec<_> = threads.iter().map(|s| s.as_str()).collect();
    let (registry, handles) = WorkerRegistry::create_workers(&threads, top_level, f);
    registry.add_task(task);
    registry.wait_tasks_complete(handles);

    let final_time = SystemTime::now();
    println!("codegen time (including LLVM): {}ms", final_time.duration_since(analysis_time).unwrap().as_millis());
    println!("total time: {}ms", final_time.duration_since(start).unwrap().as_millis());
}
