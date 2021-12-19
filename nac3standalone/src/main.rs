use inkwell::{
    passes::{PassManager, PassManagerBuilder},
    targets::*,
    OptimizationLevel,
};
use std::{borrow::Borrow, collections::HashMap, env, fs, path::Path, sync::Arc, time::SystemTime};
use parking_lot::RwLock;

use nac3parser::{ast::{Expr, ExprKind, StmtKind}, parser};
use nac3core::{
    codegen::{
        concrete_type::ConcreteTypeStore, CodeGenTask, DefaultCodeGenerator, WithCall,
        WorkerRegistry,
    },
    symbol_resolver::SymbolResolver,
    toplevel::{
        composer::{TopLevelComposer, ComposerConfig},
        TopLevelDef, helper::parse_parameter_default_value,
        type_annotation::*,
    },
    typecheck::{type_inferencer::PrimitiveStore, typedef::{Type, Unifier, FunSignature}}
};

mod basic_symbol_resolver;
use basic_symbol_resolver::*;

fn main() {
    let demo_name = env::args().nth(1).unwrap();
    let threads: u32 = env::args()
        .nth(2)
        .map(|s| str::parse(&s).unwrap())
        .unwrap_or(1);

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
    let (mut composer, builtins_def, builtins_ty) = TopLevelComposer::new(
        vec![],
        Default::default()
    );

    let internal_resolver: Arc<ResolverInternal> = ResolverInternal {
        id_to_type: builtins_ty.into(),
        id_to_def: builtins_def.into(),
        class_names: Default::default(),
        module_globals: Default::default(),
    }
    .into();
    let resolver =
        Arc::new(Resolver(internal_resolver.clone())) as Arc<dyn SymbolResolver + Send + Sync>;
    let setup_time = SystemTime::now();
    println!(
        "setup time: {}ms",
        setup_time.duration_since(start).unwrap().as_millis()
    );

    let parser_result = parser::parse_program(&program).unwrap();
    let parse_time = SystemTime::now();
    println!(
        "parse time: {}ms",
        parse_time.duration_since(setup_time).unwrap().as_millis()
    );

    for stmt in parser_result.into_iter() {
        if let StmtKind::Assign { targets, value, .. } = &stmt.node {
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
                                get_type_from_type_annotation_kinds(def_list, unifier, primitives, &ty)
                            })
                            .collect::<Result<Vec<_>, _>>()?;
                        Ok(unifier.get_fresh_var_with_range(&constraints).0)
                    } else {
                        Err(format!("expression {:?} cannot be handled as a TypeVar in global scope", var))
                    }
                } else {
                    Err(format!("expression {:?} cannot be handled as a TypeVar in global scope", var))
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
                            } else if let Ok(val) = parse_parameter_default_value(value.borrow(), resolver) {
                                internal_resolver.add_module_global(*id, val);
                                Ok(())
                            } else {
                                Err(format!("fails to evaluate this expression `{:?}` as a constant or TypeVar at {}",
                                    targets[0].node,
                                    targets[0].location,
                                ))
                            }
                        }
                        ExprKind::List { elts, .. }
                        | ExprKind::Tuple { elts, .. } => {
                            handle_assignment_pattern(
                                elts,
                                value,
                                resolver,
                                internal_resolver,
                                def_list,
                                unifier,
                                primitives
                            )?;
                            Ok(())
                        }
                        _ => Err(format!("assignment to {:?} is not supported at {}", targets[0], targets[0].location))
                    }
                } else {
                    match &value.node {
                        ExprKind::List { elts, .. }
                        | ExprKind::Tuple { elts, .. } => {
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
                                        primitives
                                    )?;
                                }
                                Ok(())
                            }
                        },
                        _ => Err(format!("unpack of this expression is not supported at {}", value.location))
                    }
                }
            }

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
            continue;
        }

        let (name, def_id, ty) = composer
            .register_top_level(stmt, Some(resolver.clone()), "__main__".into())
            .unwrap();

        internal_resolver.add_id_def(name, def_id);
        if let Some(ty) = ty {
            internal_resolver.add_id_type(name, ty);
        }
    }

    let signature = FunSignature {
        args: vec![],
        ret: primitive.int32,
        vars: HashMap::new(),
    };
    let mut store = ConcreteTypeStore::new();
    let mut cache = HashMap::new();
    let signature = store.from_signature(&mut composer.unifier, &primitive, &signature, &mut cache);
    let signature = store.add_cty(signature);

    composer.start_analysis(true).unwrap();
    let analysis_time = SystemTime::now();
    println!(
        "analysis time: {}ms",
        analysis_time
            .duration_since(parse_time)
            .unwrap()
            .as_millis()
    );

    let top_level = Arc::new(composer.make_top_level_context());

    let instance = {
        let defs = top_level.definitions.read();
        let mut instance =
            defs[resolver
                .get_identifier_def("run".into())
                .unwrap_or_else(|| panic!("cannot find run() entry point")).0
            ].write();
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
            .write_to_file(
                module,
                FileType::Object,
                Path::new(&format!("{}.o", module.get_name().to_str().unwrap())),
            )
            .expect("couldn't write module to file");

        // println!("IR:\n{}", module.print_to_string().to_str().unwrap());
    })));
    let threads = (0..threads)
        .map(|i| Box::new(DefaultCodeGenerator::new(format!("module{}", i))))
        .collect();
    let (registry, handles) = WorkerRegistry::create_workers(threads, top_level, f);
    registry.add_task(task);
    registry.wait_tasks_complete(handles);

    let final_time = SystemTime::now();
    println!(
        "codegen time (including LLVM): {}ms",
        final_time
            .duration_since(analysis_time)
            .unwrap()
            .as_millis()
    );
    println!(
        "total time: {}ms",
        final_time.duration_since(start).unwrap().as_millis()
    );
}
