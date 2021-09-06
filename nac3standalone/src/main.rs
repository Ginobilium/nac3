use std::time::SystemTime;
use std::{collections::HashSet, fs};

use inkwell::{
    passes::{PassManager, PassManagerBuilder},
    targets::*,
    OptimizationLevel,
};
use parking_lot::{Mutex, RwLock};
use rustpython_parser::{
    ast::{fold::Fold, StmtKind},
    parser,
};
use std::{cell::RefCell, collections::HashMap, path::Path, sync::Arc};

use nac3core::{
    codegen::{CodeGenTask, WithCall, WorkerRegistry},
    symbol_resolver::SymbolResolver,
    toplevel::{DefinitionId, FunInstance, TopLevelComposer, TopLevelContext, TopLevelDef},
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

    let start = SystemTime::now();

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
    let output_id = top_level.definitions.read().len();
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

    // dummy resolver...
    let resolver = Arc::new(Box::new(basic_symbol_resolver::Resolver {
        id_to_type: HashMap::new(),
        id_to_def: HashMap::new(),
        class_names: Default::default(),
    }) as Box<dyn SymbolResolver + Send + Sync>);
    let mut functions = HashMap::new();

    for stmt in parser::parse_program(&program).unwrap().into_iter() {
        if let StmtKind::FunctionDef {
            name,
            body,
            args,
            returns,
            ..
        } = stmt.node
        {
            let args = args
                .args
                .into_iter()
                .map(|arg| FuncArg {
                    name: arg.node.arg.to_string(),
                    ty: resolver
                        .parse_type_annotation(
                            &top_level.definitions.read(),
                            &mut unifier,
                            &primitives,
                            &arg.node
                                .annotation
                                .expect("expected type annotation in parameters"),
                        )
                        .unwrap(),
                    default_value: None,
                })
                .collect();
            let ret = returns
                .map(|r| {
                    resolver
                        .parse_type_annotation(
                            &top_level.definitions.read(),
                            &mut unifier,
                            &primitives,
                            &r,
                        )
                        .unwrap()
                })
                .unwrap_or(primitives.none);
            let signature = FunSignature {
                args,
                ret,
                vars: Default::default(),
            };
            let fun_ty = unifier.add_ty(TypeEnum::TFunc(RefCell::new(signature.clone())));
            let id = top_level.definitions.read().len();
            top_level
                .definitions
                .write()
                .push(Arc::new(RwLock::new(TopLevelDef::Function {
                    name: name.clone(),
                    signature: fun_ty,
                    var_id: vec![],
                    instance_to_stmt: HashMap::new(),
                    instance_to_symbol: HashMap::new(),
                    resolver: None,
                })));
            functions.insert(name, (id, body, signature));
        } else {
            panic!("unsupported statement type");
        }
    }

    let setup_time = SystemTime::now();
    println!(
        "Setup time: {}ms",
        setup_time
            .duration_since(start)
            .unwrap()
            .as_millis()
    );

    let mut id_to_def: HashMap<_, _> = functions
        .iter()
        .map(|(k, v)| (k.clone(), DefinitionId(v.0)))
        .collect();
    id_to_def.insert("output".into(), DefinitionId(output_id));
    let mut id_to_type: HashMap<_, _> = functions
        .iter()
        .map(|(k, v)| {
            (
                k.clone(),
                unifier.add_ty(TypeEnum::TFunc(RefCell::new(v.2.clone()))),
            )
        })
        .collect();
    id_to_type.insert("output".into(), output_fun);

    let resolver = Arc::new(Mutex::new(Box::new(basic_symbol_resolver::Resolver {
        class_names: Default::default(),
        id_to_type,
        id_to_def,
    }) as Box<dyn SymbolResolver + Send + Sync>));

    for (_, (id, ast, signature)) in functions.into_iter() {
        if let TopLevelDef::Function {
            resolver: r,
            instance_to_stmt,
            ..
        } = &mut *top_level.definitions.read()[id].write()
        {
            *r = Some(resolver.clone());

            let return_type = if unifier.unioned(primitives.none, signature.ret) {
                None
            } else {
                Some(signature.ret)
            };
            let mut function_data = FunctionData {
                resolver: resolver.clone(),
                bound_variables: Vec::new(),
                return_type,
            };
            let mut virtual_checks = Vec::new();
            let mut calls = HashMap::new();
            let mut identifiers = HashSet::new();
            let mut variable_mapping = HashMap::new();
            for arg in signature.args.iter() {
                identifiers.insert(arg.name.clone());
                variable_mapping.insert(arg.name.clone(), arg.ty);
            }
            let mut inferencer = Inferencer {
                top_level: &top_level,
                function_data: &mut function_data,
                unifier: &mut unifier,
                variable_mapping,
                primitives: &primitives,
                virtual_checks: &mut virtual_checks,
                calls: &mut calls,
                defined_identifiers: identifiers.clone(),
            };
            let statements = ast
                .into_iter()
                .map(|v| inferencer.fold_stmt(v))
                .collect::<Result<Vec<_>, _>>()
                .unwrap();

            let returned = inferencer
                .check_block(&statements, &mut identifiers)
                .unwrap();
            if return_type.is_some() && !returned {
                panic!("expected return");
            }

            instance_to_stmt.insert(
                "".to_string(),
                FunInstance {
                    body: statements,
                    unifier_id: 0,
                    calls,
                    subst: Default::default(),
                },
            );
        }
    }

    let inference_time = SystemTime::now();
    println!(
        "Type inference time: {}ms",
        inference_time
            .duration_since(setup_time)
            .unwrap()
            .as_millis()
    );

    let top_level = Arc::new(TopLevelContext {
        definitions: Arc::new(RwLock::new(std::mem::take(
            &mut *top_level.definitions.write(),
        ))),
        unifiers: Arc::new(RwLock::new(vec![(
            unifier.get_shared_unifier(),
            primitives,
        )])),
    });

    let instance = {
        let defs = top_level.definitions.read();
        let mut instance = defs[resolver.lock().get_identifier_def("run").unwrap().0].write();
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
        ret: primitives.int32,
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
        let codegen_time = SystemTime::now();
        println!(
            "Code generation time: {}ms",
            codegen_time
                .duration_since(inference_time)
                .unwrap()
                .as_millis()
        );
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

        println!(
            "LLVM time: {}ms",
            SystemTime::now()
                .duration_since(codegen_time)
                .unwrap()
                .as_millis()
        );
        println!("IR:\n{}", module.print_to_string().to_str().unwrap());

    })));
    let threads = ["test"];
    let (registry, handles) = WorkerRegistry::create_workers(&threads, top_level, f);
    registry.add_task(task);
    registry.wait_tasks_complete(handles);
    println!("object file is in mandelbrot.o");
}
