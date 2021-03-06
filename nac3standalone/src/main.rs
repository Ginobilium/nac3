use inkwell::{
    memory_buffer::MemoryBuffer,
    passes::{PassManager, PassManagerBuilder},
    targets::*,
    OptimizationLevel,
};
use parking_lot::{Mutex, RwLock};
use std::{borrow::Borrow, collections::HashMap, env, fs, path::Path, sync::Arc};

use nac3core::{
    codegen::{
        concrete_type::ConcreteTypeStore, irrt::load_irrt, CodeGenTask, DefaultCodeGenerator,
        WithCall, WorkerRegistry,
    },
    symbol_resolver::SymbolResolver,
    toplevel::{
        composer::TopLevelComposer, helper::parse_parameter_default_value, type_annotation::*,
        TopLevelDef,
    },
    typecheck::{
        type_inferencer::PrimitiveStore,
        typedef::{FunSignature, Type, Unifier},
    },
};
use nac3parser::{
    ast::{Expr, ExprKind, StmtKind},
    parser,
};

mod basic_symbol_resolver;
use basic_symbol_resolver::*;

fn handle_typevar_definition(
    var: &Expr,
    resolver: &(dyn SymbolResolver + Send + Sync),
    def_list: &[Arc<RwLock<TopLevelDef>>],
    unifier: &mut Unifier,
    primitives: &PrimitiveStore,
) -> Result<Type, String> {
    if let ExprKind::Call { func, args, .. } = &var.node {
        if matches!(&func.node, ExprKind::Name { id, .. } if id == &"TypeVar".into()) {
            let constraints = args
                .iter()
                .skip(1)
                .map(|x| -> Result<Type, String> {
                    let ty = parse_ast_to_type_annotation_kinds(
                        resolver,
                        def_list,
                        unifier,
                        primitives,
                        x,
                        Default::default(),
                    )?;
                    get_type_from_type_annotation_kinds(
                        def_list, unifier, primitives, &ty, &mut None
                    )
                })
                .collect::<Result<Vec<_>, _>>()?;
            Ok(unifier.get_fresh_var_with_range(&constraints, None, None).0)
        } else {
            Err(format!(
                "expression {:?} cannot be handled as a TypeVar in global scope",
                var
            ))
        }
    } else {
        Err(format!(
            "expression {:?} cannot be handled as a TypeVar in global scope",
            var
        ))
    }
}

fn handle_assignment_pattern(
    targets: &[Expr],
    value: &Expr,
    resolver: &(dyn SymbolResolver + Send + Sync),
    internal_resolver: &ResolverInternal,
    def_list: &[Arc<RwLock<TopLevelDef>>],
    unifier: &mut Unifier,
    primitives: &PrimitiveStore,
) -> Result<(), String> {
    if targets.len() == 1 {
        match &targets[0].node {
            ExprKind::Name { id, .. } => {
                if let Ok(var) = handle_typevar_definition(
                    value.borrow(),
                    resolver,
                    def_list,
                    unifier,
                    primitives,
                ) {
                    internal_resolver.add_id_type(*id, var);
                    Ok(())
                } else if let Ok(val) =
                    parse_parameter_default_value(value.borrow(), resolver)
                {
                    internal_resolver.add_module_global(*id, val);
                    Ok(())
                } else {
                    Err(format!("fails to evaluate this expression `{:?}` as a constant or TypeVar at {}",
                        targets[0].node,
                        targets[0].location,
                    ))
                }
            }
            ExprKind::List { elts, .. } | ExprKind::Tuple { elts, .. } => {
                handle_assignment_pattern(
                    elts,
                    value,
                    resolver,
                    internal_resolver,
                    def_list,
                    unifier,
                    primitives,
                )?;
                Ok(())
            }
            _ => Err(format!(
                "assignment to {:?} is not supported at {}",
                targets[0], targets[0].location
            )),
        }
    } else {
        match &value.node {
            ExprKind::List { elts, .. } | ExprKind::Tuple { elts, .. } => {
                if elts.len() != targets.len() {
                    Err(format!(
                        "number of elements to unpack does not match (expect {}, found {}) at {}",
                        targets.len(),
                        elts.len(),
                        value.location
                    ))
                } else {
                    for (tar, val) in targets.iter().zip(elts) {
                        handle_assignment_pattern(
                            std::slice::from_ref(tar),
                            val,
                            resolver,
                            internal_resolver,
                            def_list,
                            unifier,
                            primitives,
                        )?;
                    }
                    Ok(())
                }
            }
            _ => Err(format!(
                "unpack of this expression is not supported at {}",
                value.location
            )),
        }
    }
}

fn main() {
    let file_name = env::args().nth(1).unwrap();
    let threads: u32 = env::args().nth(2).map(|s| str::parse(&s).unwrap()).unwrap_or(1);

    Target::initialize_all(&InitializationConfig::default());

    let program = match fs::read_to_string(file_name.clone()) {
        Ok(program) => program,
        Err(err) => {
            println!("Cannot open input file: {}", err);
            return;
        }
    };

    let primitive: PrimitiveStore = TopLevelComposer::make_primitives().0;
    let (mut composer, builtins_def, builtins_ty) =
        TopLevelComposer::new(vec![], Default::default());

    let internal_resolver: Arc<ResolverInternal> = ResolverInternal {
        id_to_type: builtins_ty.into(),
        id_to_def: builtins_def.into(),
        class_names: Default::default(),
        module_globals: Default::default(),
        str_store: Default::default(),
    }.into();
    let resolver =
        Arc::new(Resolver(internal_resolver.clone())) as Arc<dyn SymbolResolver + Send + Sync>;

    let parser_result = parser::parse_program(&program, file_name.into()).unwrap();

    for stmt in parser_result.into_iter() {
        match &stmt.node {
            StmtKind::Assign { targets, value, .. } => {
                let def_list = composer.extract_def_list();
                let unifier = &mut composer.unifier;
                let primitives = &composer.primitives_ty;
                if let Err(err) = handle_assignment_pattern(
                    targets,
                    value,
                    resolver.as_ref(),
                    internal_resolver.as_ref(),
                    &def_list,
                    unifier,
                    primitives,
                ) {
                    eprintln!("{}", err);
                    return;
                }
            },
            // allow (and ignore) "from __future__ import annotations"
            StmtKind::ImportFrom { module, names, .. }
                if module == &Some("__future__".into()) && names.len() == 1 && names[0].name == "annotations".into() => (),
            _ => {
                let (name, def_id, ty) =
                    composer.register_top_level(stmt, Some(resolver.clone()), "__main__".into()).unwrap();
                internal_resolver.add_id_def(name, def_id);
                if let Some(ty) = ty {
                    internal_resolver.add_id_type(name, ty);
                }
            }
        }
    }

    let signature = FunSignature { args: vec![], ret: primitive.int32, vars: HashMap::new() };
    let mut store = ConcreteTypeStore::new();
    let mut cache = HashMap::new();
    let signature = store.from_signature(&mut composer.unifier, &primitive, &signature, &mut cache);
    let signature = store.add_cty(signature);

    composer.start_analysis(true).unwrap();

    let top_level = Arc::new(composer.make_top_level_context());

    let instance = {
        let defs = top_level.definitions.read();
        let mut instance = defs[resolver
            .get_identifier_def("run".into())
            .unwrap_or_else(|_| panic!("cannot find run() entry point"))
            .0]
            .write();
        if let TopLevelDef::Function { instance_to_stmt, instance_to_symbol, .. } = &mut *instance {
            instance_to_symbol.insert("".to_string(), "run".to_string());
            instance_to_stmt[""].clone()
        } else {
            unreachable!()
        }
    };

    let task = CodeGenTask {
        subst: Default::default(),
        symbol_name: "run".to_string(),
        body: instance.body,
        signature,
        resolver,
        store,
        unifier_index: instance.unifier_id,
        calls: instance.calls,
        id: 0,
    };

    let membuffers: Arc<Mutex<Vec<Vec<u8>>>> = Default::default();
    let membuffer = membuffers.clone();

    let f = Arc::new(WithCall::new(Box::new(move |module| {
        let buffer = module.write_bitcode_to_memory();
        let buffer = buffer.as_slice().into();
        membuffer.lock().push(buffer);
    })));
    let threads = (0..threads)
        .map(|i| Box::new(DefaultCodeGenerator::new(format!("module{}", i), 64)))
        .collect();
    let (registry, handles) = WorkerRegistry::create_workers(threads, top_level, f);
    registry.add_task(task);
    registry.wait_tasks_complete(handles);

    let buffers = membuffers.lock();
    let context = inkwell::context::Context::create();
    let main = context
        .create_module_from_ir(MemoryBuffer::create_from_memory_range(&buffers[0], "main"))
        .unwrap();
    for buffer in buffers.iter().skip(1) {
        let other = context
            .create_module_from_ir(MemoryBuffer::create_from_memory_range(buffer, "main"))
            .unwrap();

        main.link_in_module(other).unwrap();
    }
    main.link_in_module(load_irrt(&context)).unwrap();

    let mut function_iter = main.get_first_function();
    while let Some(func) = function_iter {
        if func.count_basic_blocks() > 0 && func.get_name().to_str().unwrap() != "run" {
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

    let triple = TargetMachine::get_default_triple();
    let target = Target::from_triple(&triple).expect("couldn't create target from target triple");
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
        .write_to_file(&main, FileType::Object, Path::new("module.o"))
        .expect("couldn't write module to file");
}
