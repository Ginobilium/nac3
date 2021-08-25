use std::fs;

use inkwell::{targets::*, OptimizationLevel};
use parking_lot::RwLock;
use rustpython_parser::{
    ast::{fold::Fold, StmtKind},
    parser,
};
use std::{cell::RefCell, collections::HashMap, path::Path, sync::Arc};

use nac3core::{
    codegen::{CodeGenTask, WithCall, WorkerRegistry},
    symbol_resolver::SymbolResolver,
    toplevel::{DefinitionId, TopLevelComposer, TopLevelContext, TopLevelDef},
    typecheck::{
        type_inferencer::{FunctionData, Inferencer},
        typedef::{FunSignature, FuncArg, TypeEnum},
    },
};

mod basic_symbol_resolver;

fn main() {
    Target::initialize_all(&InitializationConfig::default());

    let program = match fs::read_to_string("mandelbrot.py") {
        Ok(program) => program,
        Err(err) => {
            println!("Cannot open input file: {}", err);
            return;
        }
    };

    let statements = match parser::parse_program(&program) {
        Ok(mut ast) => {
            let first = ast.remove(0);
            if let StmtKind::FunctionDef { name, body, .. } = first.node {
                if name != "run" {
                    panic!("Parse error: expected function \"run\" but got {}", name);
                }
                body
            } else {
                panic!(
                    "Parse error: expected function \"run\" but got {:?}",
                    first.node
                );
            }
        }
        Err(err) => {
            panic!("Parse error: {}", err);
        }
    };

    let composer = TopLevelComposer::new();
    let mut unifier = composer.unifier.clone();
    let primitives = composer.primitives_ty;
    let top_level = Arc::new(composer.make_top_level_context());
    unifier.top_level = Some(top_level.clone());
    let output_fun = unifier.add_ty(TypeEnum::TFunc(RefCell::new(FunSignature {
        args: vec![FuncArg {
            name: "c".into(),
            ty: primitives.int32,
            default_value: None,
        }],
        ret: primitives.none,
        vars: HashMap::new(),
    })));
    let def_id = top_level.definitions.read().len();
    top_level
        .definitions
        .write()
        .push(Arc::new(RwLock::new(TopLevelDef::Function {
            name: "output".into(),
            signature: output_fun,
            instance_to_stmt: HashMap::new(),
            instance_to_symbol: [("".to_string(), "output".to_string())]
                .iter()
                .cloned()
                .collect(),
            var_id: Default::default(),
            resolver: None,
        })));

    let resolver = Arc::new(Box::new(basic_symbol_resolver::Resolver {
        id_to_type: [("output".into(), output_fun)].iter().cloned().collect(),
        id_to_def: [("output".into(), DefinitionId(def_id))]
            .iter()
            .cloned()
            .collect(),
        class_names: Default::default(),
    }) as Box<dyn SymbolResolver + Send + Sync>);

    let threads = ["test"];
    let signature = FunSignature {
        args: vec![],
        ret: primitives.int32,
        vars: HashMap::new(),
    };

    let mut function_data = FunctionData {
        resolver: resolver.clone(),
        bound_variables: Vec::new(),
        return_type: Some(primitives.int32),
    };
    let mut virtual_checks = Vec::new();
    let mut calls = HashMap::new();
    let mut inferencer = Inferencer {
        top_level: &top_level,
        function_data: &mut function_data,
        unifier: &mut unifier,
        variable_mapping: Default::default(),
        primitives: &primitives,
        virtual_checks: &mut virtual_checks,
        calls: &mut calls,
    };

    let statements = statements
        .into_iter()
        .map(|v| inferencer.fold_stmt(v))
        .collect::<Result<Vec<_>, _>>()
        .unwrap();

    let mut identifiers = vec!["output".to_string()];
    inferencer
        .check_block(&statements, &mut identifiers)
        .unwrap();
    let top_level = Arc::new(TopLevelContext {
        definitions: Arc::new(RwLock::new(std::mem::take(
            &mut *top_level.definitions.write(),
        ))),
        unifiers: Arc::new(RwLock::new(vec![(
            unifier.get_shared_unifier(),
            primitives,
        )])),
    });

    let unifier = (unifier.get_shared_unifier(), primitives);

    let task = CodeGenTask {
        subst: Default::default(),
        symbol_name: "run".to_string(),
        body: statements,
        resolver,
        unifier,
        calls,
        signature,
    };
    let f = Arc::new(WithCall::new(Box::new(|module| {
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
    })));
    let (registry, handles) = WorkerRegistry::create_workers(&threads, top_level, f);
    registry.add_task(task);
    registry.wait_tasks_complete(handles);
    println!("object file is in mandelbrot.o")
}
