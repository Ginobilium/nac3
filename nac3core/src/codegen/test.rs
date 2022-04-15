use crate::{
    codegen::{
        concrete_type::ConcreteTypeStore, CodeGenContext, CodeGenTask, DefaultCodeGenerator,
        WithCall, WorkerRegistry,
    },
    symbol_resolver::{SymbolResolver, ValueEnum},
    toplevel::{
        composer::TopLevelComposer, DefinitionId, FunInstance, TopLevelContext, TopLevelDef,
    },
    typecheck::{
        type_inferencer::{FunctionData, Inferencer, PrimitiveStore},
        typedef::{FunSignature, FuncArg, Type, TypeEnum, Unifier},
    },
};
use indoc::indoc;
use nac3parser::{
    ast::{fold::Fold, StrRef},
    parser::parse_program,
};
use parking_lot::RwLock;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

struct Resolver {
    id_to_type: HashMap<StrRef, Type>,
    id_to_def: RwLock<HashMap<StrRef, DefinitionId>>,
    class_names: HashMap<StrRef, Type>,
}

impl Resolver {
    pub fn add_id_def(&self, id: StrRef, def: DefinitionId) {
        self.id_to_def.write().insert(id, def);
    }
}

impl SymbolResolver for Resolver {
    fn get_default_param_value(
        &self,
        _: &nac3parser::ast::Expr,
    ) -> Option<crate::symbol_resolver::SymbolValue> {
        unimplemented!()
    }

    fn get_symbol_type(
        &self,
        _: &mut Unifier,
        _: &[Arc<RwLock<TopLevelDef>>],
        _: &PrimitiveStore,
        str: StrRef,
    ) -> Result<Type, String> {
        self.id_to_type.get(&str).cloned().ok_or_else(|| format!("cannot find symbol `{}`", str))
    }

    fn get_symbol_value<'ctx, 'a>(
        &self,
        _: StrRef,
        _: &mut CodeGenContext<'ctx, 'a>,
    ) -> Option<ValueEnum<'ctx>> {
        unimplemented!()
    }

    fn get_identifier_def(&self, id: StrRef) -> Result<DefinitionId, String> {
        self.id_to_def
            .read()
            .get(&id)
            .cloned()
            .ok_or_else(|| format!("cannot find symbol `{}`", id))
    }

    fn get_string_id(&self, _: &str) -> i32 {
        unimplemented!()
    }

    fn get_exception_id(&self, _tyid: usize) -> usize {
        unimplemented!()
    }
}

#[test]
fn test_primitives() {
    let source = indoc! { "
        c = a + b
        d = a if c == 1 else 0
        return d
        "};
    let statements = parse_program(source, Default::default()).unwrap();

    let composer: TopLevelComposer = Default::default();
    let mut unifier = composer.unifier.clone();
    let primitives = composer.primitives_ty;
    let top_level = Arc::new(composer.make_top_level_context());
    unifier.top_level = Some(top_level.clone());

    let resolver = Arc::new(Resolver {
        id_to_type: HashMap::new(),
        id_to_def: RwLock::new(HashMap::new()),
        class_names: Default::default(),
    }) as Arc<dyn SymbolResolver + Send + Sync>;

    let threads = vec![DefaultCodeGenerator::new("test".into(), 32).into()];
    let signature = FunSignature {
        args: vec![
            FuncArg { name: "a".into(), ty: primitives.int32, default_value: None },
            FuncArg { name: "b".into(), ty: primitives.int32, default_value: None },
        ],
        ret: primitives.int32,
        vars: HashMap::new(),
    };

    let mut store = ConcreteTypeStore::new();
    let mut cache = HashMap::new();
    let signature = store.from_signature(&mut unifier, &primitives, &signature, &mut cache);
    let signature = store.add_cty(signature);

    let mut function_data = FunctionData {
        resolver: resolver.clone(),
        bound_variables: Vec::new(),
        return_type: Some(primitives.int32),
    };
    let mut virtual_checks = Vec::new();
    let mut calls = HashMap::new();
    let mut identifiers: HashSet<_> = ["a".into(), "b".into()].iter().cloned().collect();
    let mut inferencer = Inferencer {
        top_level: &top_level,
        function_data: &mut function_data,
        unifier: &mut unifier,
        variable_mapping: Default::default(),
        primitives: &primitives,
        virtual_checks: &mut virtual_checks,
        calls: &mut calls,
        defined_identifiers: identifiers.clone(),
        in_handler: false,
    };
    inferencer.variable_mapping.insert("a".into(), inferencer.primitives.int32);
    inferencer.variable_mapping.insert("b".into(), inferencer.primitives.int32);

    let statements = statements
        .into_iter()
        .map(|v| inferencer.fold_stmt(v))
        .collect::<Result<Vec<_>, _>>()
        .unwrap();

    inferencer.check_block(&statements, &mut identifiers).unwrap();
    let top_level = Arc::new(TopLevelContext {
        definitions: Arc::new(RwLock::new(std::mem::take(&mut *top_level.definitions.write()))),
        unifiers: Arc::new(RwLock::new(vec![(unifier.get_shared_unifier(), primitives)])),
        personality_symbol: None,
    });

    let task = CodeGenTask {
        subst: Default::default(),
        symbol_name: "testing".into(),
        body: Arc::new(statements),
        unifier_index: 0,
        calls: Arc::new(calls),
        resolver,
        store,
        signature,
        id: 0,
    };
    let f = Arc::new(WithCall::new(Box::new(|module| {
        // the following IR is equivalent to
        // ```
        // ; ModuleID = 'test.ll'
        // source_filename = "test"
        //
        // ; Function Attrs: norecurse nounwind readnone
        // define i32 @testing(i32 %0, i32 %1) local_unnamed_addr #0 {
        // init:
        //   %add = add i32 %1, %0
        //   %cmp = icmp eq i32 %add, 1
        //   %ifexpr = select i1 %cmp, i32 %0, i32 0
        //   ret i32 %ifexpr
        // }
        //
        // attributes #0 = { norecurse nounwind readnone }
        // ```
        // after O2 optimization

        let expected = indoc! {"
            ; ModuleID = 'test'
            source_filename = \"test\"
            
            define i32 @testing(i32 %0, i32 %1) !dbg !4 {
            init:
              %add = add i32 %0, %1, !dbg !9
              %cmp = icmp eq i32 %add, 1, !dbg !10
              br i1 %cmp, label %then, label %else, !dbg !10
            
            then:                                             ; preds = %init
              br label %cont, !dbg !11
            
            else:                                             ; preds = %init
              br label %cont, !dbg !12
            
            cont:                                             ; preds = %else, %then
              %if_exp_result.0 = phi i32 [ %0, %then ], [ 0, %else ], !dbg !13
              ret i32 %if_exp_result.0, !dbg !14
            }

            !llvm.module.flags = !{!0, !1}
            !llvm.dbg.cu = !{!2}

            !0 = !{i32 2, !\"Debug Info Version\", i32 3}
            !1 = !{i32 2, !\"Dwarf Version\", i32 4}
            !2 = distinct !DICompileUnit(language: DW_LANG_Python, file: !3, producer: \"NAC3\", isOptimized: true, runtimeVersion: 0, emissionKind: FullDebug)
            !3 = !DIFile(filename: \"unknown\", directory: \"\")
            !4 = distinct !DISubprogram(name: \"testing\", linkageName: \"testing\", scope: null, file: !3, line: 1, type: !5, scopeLine: 1, flags: DIFlagPublic, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !8)
            !5 = !DISubroutineType(flags: DIFlagPublic, types: !6)
            !6 = !{!7}
            !7 = !DIBasicType(name: \"_\", flags: DIFlagPublic)
            !8 = !{}
            !9 = !DILocation(line: 1, column: 9, scope: !4)
            !10 = !DILocation(line: 2, column: 15, scope: !4)
            !11 = !DILocation(line: 2, column: 5, scope: !4)
            !12 = !DILocation(line: 2, column: 22, scope: !4)
            !13 = !DILocation(line: 0, scope: !4)
            !14 = !DILocation(line: 3, column: 8, scope: !4)
        "}
        .trim();
        assert_eq!(expected, module.print_to_string().to_str().unwrap().trim());
    })));
    let (registry, handles) = WorkerRegistry::create_workers(threads, top_level, f);
    registry.add_task(task);
    registry.wait_tasks_complete(handles);
}

#[test]
fn test_simple_call() {
    let source_1 = indoc! { "
        a = foo(a)
        return a * 2
        "};
    let statements_1 = parse_program(source_1, Default::default()).unwrap();

    let source_2 = indoc! { "
        return a + 1
        "};
    let statements_2 = parse_program(source_2, Default::default()).unwrap();

    let composer: TopLevelComposer = Default::default();
    let mut unifier = composer.unifier.clone();
    let primitives = composer.primitives_ty;
    let top_level = Arc::new(composer.make_top_level_context());
    unifier.top_level = Some(top_level.clone());

    let signature = FunSignature {
        args: vec![FuncArg { name: "a".into(), ty: primitives.int32, default_value: None }],
        ret: primitives.int32,
        vars: HashMap::new(),
    };
    let fun_ty = unifier.add_ty(TypeEnum::TFunc(signature.clone()));
    let mut store = ConcreteTypeStore::new();
    let mut cache = HashMap::new();
    let signature = store.from_signature(&mut unifier, &primitives, &signature, &mut cache);
    let signature = store.add_cty(signature);

    let foo_id = top_level.definitions.read().len();
    top_level.definitions.write().push(Arc::new(RwLock::new(TopLevelDef::Function {
        name: "foo".to_string(),
        simple_name: "foo".into(),
        signature: fun_ty,
        var_id: vec![],
        instance_to_stmt: HashMap::new(),
        instance_to_symbol: HashMap::new(),
        resolver: None,
        codegen_callback: None,
        loc: None,
    })));

    let resolver = Resolver {
        id_to_type: HashMap::new(),
        id_to_def: RwLock::new(HashMap::new()),
        class_names: Default::default(),
    };
    resolver.add_id_def("foo".into(), DefinitionId(foo_id));
    let resolver = Arc::new(resolver) as Arc<dyn SymbolResolver + Send + Sync>;

    if let TopLevelDef::Function { resolver: r, .. } =
        &mut *top_level.definitions.read()[foo_id].write()
    {
        *r = Some(resolver.clone());
    } else {
        unreachable!()
    }

    let threads = vec![DefaultCodeGenerator::new("test".into(), 32).into()];
    let mut function_data = FunctionData {
        resolver: resolver.clone(),
        bound_variables: Vec::new(),
        return_type: Some(primitives.int32),
    };
    let mut virtual_checks = Vec::new();
    let mut calls = HashMap::new();
    let mut identifiers: HashSet<_> = ["a".into(), "foo".into()].iter().cloned().collect();
    let mut inferencer = Inferencer {
        top_level: &top_level,
        function_data: &mut function_data,
        unifier: &mut unifier,
        variable_mapping: Default::default(),
        primitives: &primitives,
        virtual_checks: &mut virtual_checks,
        calls: &mut calls,
        defined_identifiers: identifiers.clone(),
        in_handler: false,
    };
    inferencer.variable_mapping.insert("a".into(), inferencer.primitives.int32);
    inferencer.variable_mapping.insert("foo".into(), fun_ty);

    let statements_1 = statements_1
        .into_iter()
        .map(|v| inferencer.fold_stmt(v))
        .collect::<Result<Vec<_>, _>>()
        .unwrap();

    let calls1 = inferencer.calls.clone();
    inferencer.calls.clear();

    let statements_2 = statements_2
        .into_iter()
        .map(|v| inferencer.fold_stmt(v))
        .collect::<Result<Vec<_>, _>>()
        .unwrap();

    if let TopLevelDef::Function { instance_to_stmt, .. } =
        &mut *top_level.definitions.read()[foo_id].write()
    {
        instance_to_stmt.insert(
            "".to_string(),
            FunInstance {
                body: Arc::new(statements_2),
                calls: Arc::new(inferencer.calls.clone()),
                subst: Default::default(),
                unifier_id: 0,
            },
        );
    } else {
        unreachable!()
    }

    inferencer.check_block(&statements_1, &mut identifiers).unwrap();
    let top_level = Arc::new(TopLevelContext {
        definitions: Arc::new(RwLock::new(std::mem::take(&mut *top_level.definitions.write()))),
        unifiers: Arc::new(RwLock::new(vec![(unifier.get_shared_unifier(), primitives)])),
        personality_symbol: None,
    });

    let task = CodeGenTask {
        subst: Default::default(),
        symbol_name: "testing".to_string(),
        body: Arc::new(statements_1),
        calls: Arc::new(calls1),
        unifier_index: 0,
        resolver,
        signature,
        store,
        id: 0,
    };
    let f = Arc::new(WithCall::new(Box::new(|module| {
        let expected = indoc! {"
            ; ModuleID = 'test'
            source_filename = \"test\"

            define i32 @testing(i32 %0) !dbg !5 {
            init:
              %call = call i32 @foo.0(i32 %0), !dbg !10
              %mul = mul i32 %call, 2, !dbg !11
              ret i32 %mul, !dbg !11
            }
            
            define i32 @foo.0(i32 %0) !dbg !12 {
            init:
              %add = add i32 %0, 1, !dbg !13
              ret i32 %add, !dbg !13
            }
            
            !llvm.module.flags = !{!0, !1}
            !llvm.dbg.cu = !{!2, !4}
            
            !0 = !{i32 2, !\"Debug Info Version\", i32 3}
            !1 = !{i32 2, !\"Dwarf Version\", i32 4}
            !2 = distinct !DICompileUnit(language: DW_LANG_Python, file: !3, producer: \"NAC3\", isOptimized: true, runtimeVersion: 0, emissionKind: FullDebug)
            !3 = !DIFile(filename: \"unknown\", directory: \"\")
            !4 = distinct !DICompileUnit(language: DW_LANG_Python, file: !3, producer: \"NAC3\", isOptimized: true, runtimeVersion: 0, emissionKind: FullDebug)
            !5 = distinct !DISubprogram(name: \"testing\", linkageName: \"testing\", scope: null, file: !3, line: 1, type: !6, scopeLine: 1, flags: DIFlagPublic, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !2, retainedNodes: !9)
            !6 = !DISubroutineType(flags: DIFlagPublic, types: !7)
            !7 = !{!8}
            !8 = !DIBasicType(name: \"_\", flags: DIFlagPublic)
            !9 = !{}
            !10 = !DILocation(line: 1, column: 9, scope: !5)
            !11 = !DILocation(line: 2, column: 12, scope: !5)
            !12 = distinct !DISubprogram(name: \"foo.0\", linkageName: \"foo.0\", scope: null, file: !3, line: 1, type: !6, scopeLine: 1, flags: DIFlagPublic, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !4, retainedNodes: !9)
            !13 = !DILocation(line: 1, column: 12, scope: !12)
        "}
        .trim();
        assert_eq!(expected, module.print_to_string().to_str().unwrap().trim());
    })));
    let (registry, handles) = WorkerRegistry::create_workers(threads, top_level, f);
    registry.add_task(task);
    registry.wait_tasks_complete(handles);
}
