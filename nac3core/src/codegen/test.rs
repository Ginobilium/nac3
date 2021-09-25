use crate::{
    codegen::{CodeGenTask, WithCall, WorkerRegistry},
    location::Location,
    symbol_resolver::{SymbolResolver, SymbolValue},
    toplevel::{
        composer::TopLevelComposer, DefinitionId, FunInstance, TopLevelContext, TopLevelDef,
    },
    typecheck::{
        type_inferencer::{FunctionData, Inferencer, PrimitiveStore},
        typedef::{FunSignature, FuncArg, Type, TypeEnum, Unifier},
    },
};
use indoc::indoc;
use parking_lot::RwLock;
use rustpython_parser::{ast::{StrRef, fold::Fold}, parser::parse_program};
use std::cell::RefCell;
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
    fn get_symbol_type(&self, _: &mut Unifier, _: &PrimitiveStore, str: StrRef) -> Option<Type> {
        self.id_to_type.get(&str).cloned()
    }

    fn get_symbol_value(&self, _: StrRef) -> Option<SymbolValue> {
        unimplemented!()
    }

    fn get_symbol_location(&self, _: StrRef) -> Option<Location> {
        unimplemented!()
    }

    fn get_identifier_def(&self, id: StrRef) -> Option<DefinitionId> {
        self.id_to_def.read().get(&id).cloned()
    }
}

#[test]
fn test_primitives() {
    let source = indoc! { "
        c = a + b
        d = a if c == 1 else 0
        return d
        "};
    let statements = parse_program(source).unwrap();

    let composer: TopLevelComposer = Default::default();
    let mut unifier = composer.unifier.clone();
    let primitives = composer.primitives_ty;
    let top_level = Arc::new(composer.make_top_level_context());
    unifier.top_level = Some(top_level.clone());

    // let resolver = Arc::new(Mutex::new(Resolver {
    //     id_to_type: HashMap::new(),
    //     id_to_def: RwLock::new(HashMap::new()),
    //     class_names: Default::default(),
    // }) as Mutex<dyn SymbolResolver + Send + Sync>);

    let resolver = Arc::new(Box::new(Resolver {
        id_to_type: HashMap::new(),
        id_to_def: RwLock::new(HashMap::new()),
        class_names: Default::default(),
    }) as Box<dyn SymbolResolver + Send + Sync>);

    let threads = ["test"];
    let signature = FunSignature {
        args: vec![
            FuncArg { name: "a".into(), ty: primitives.int32, default_value: None },
            FuncArg { name: "b".into(), ty: primitives.int32, default_value: None },
        ],
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
        personality_symbol: None
    });

    let unifier = (unifier.get_shared_unifier(), primitives);

    let task = CodeGenTask {
        subst: Default::default(),
        symbol_name: "testing".into(),
        body: Arc::new(statements),
        resolver,
        unifier,
        calls: Arc::new(calls),
        signature,
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

            define i32 @testing(i32 %0, i32 %1) {
            init:
              %a = alloca i32, align 4
              store i32 %0, i32* %a, align 4
              %b = alloca i32, align 4
              store i32 %1, i32* %b, align 4
              %tmp = alloca i32, align 4
              %tmp4 = alloca i32, align 4
              br label %body

            body:                                             ; preds = %init
              %load = load i32, i32* %a, align 4
              %load1 = load i32, i32* %b, align 4
              %add = add i32 %load, %load1
              store i32 %add, i32* %tmp, align 4
              %load2 = load i32, i32* %tmp, align 4
              %cmp = icmp eq i32 %load2, 1
              br i1 %cmp, label %then, label %else

            then:                                             ; preds = %body
              %load3 = load i32, i32* %a, align 4
              br label %cont

            else:                                             ; preds = %body
              br label %cont

            cont:                                             ; preds = %else, %then
              %ifexpr = phi i32 [ %load3, %then ], [ 0, %else ]
              store i32 %ifexpr, i32* %tmp4, align 4
              %load5 = load i32, i32* %tmp4, align 4
              ret i32 %load5
            }
       "}
        .trim();
        assert_eq!(expected, module.print_to_string().to_str().unwrap().trim());
    })));
    let (registry, handles) = WorkerRegistry::create_workers(&threads, top_level, f);
    registry.add_task(task);
    registry.wait_tasks_complete(handles);
}

#[test]
fn test_simple_call() {
    let source_1 = indoc! { "
        a = foo(a)
        return a * 2
        "};
    let statements_1 = parse_program(source_1).unwrap();

    let source_2 = indoc! { "
        return a + 1
        "};
    let statements_2 = parse_program(source_2).unwrap();

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
    let fun_ty = unifier.add_ty(TypeEnum::TFunc(RefCell::new(signature.clone())));

    let foo_id = top_level.definitions.read().len();
    top_level.definitions.write().push(Arc::new(RwLock::new(TopLevelDef::Function {
        name: "foo".to_string(),
        simple_name: "foo".into(),
        signature: fun_ty,
        var_id: vec![],
        instance_to_stmt: HashMap::new(),
        instance_to_symbol: HashMap::new(),
        resolver: None,
    })));

    let resolver = Box::new(Resolver {
        id_to_type: HashMap::new(),
        id_to_def: RwLock::new(HashMap::new()),
        class_names: Default::default(),
    });
    resolver.add_id_def("foo".into(), DefinitionId(foo_id));
    let resolver = Arc::new(resolver as Box<dyn SymbolResolver + Send + Sync>);

    if let TopLevelDef::Function { resolver: r, .. } =
        &mut *top_level.definitions.read()[foo_id].write()
    {
        *r = Some(resolver.clone());
    } else {
        unreachable!()
    }

    let threads = ["test"];
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
        personality_symbol: None
    });

    let unifier = (unifier.get_shared_unifier(), primitives);

    let task = CodeGenTask {
        subst: Default::default(),
        symbol_name: "testing".to_string(),
        body: Arc::new(statements_1),
        resolver,
        unifier,
        calls: Arc::new(calls1),
        signature,
    };
    let f = Arc::new(WithCall::new(Box::new(|module| {
        let expected = indoc! {"
            ; ModuleID = 'test'
            source_filename = \"test\"

            define i32 @testing(i32 %0) {
            init:
              %a = alloca i32, align 4
              store i32 %0, i32* %a, align 4
              br label %body

            body:                                             ; preds = %init
              %load = load i32, i32* %a, align 4
              %call = call i32 @foo.0(i32 %load)
              store i32 %call, i32* %a, align 4
              %load1 = load i32, i32* %a, align 4
              %mul = mul i32 %load1, 2
              ret i32 %mul
            }

            define i32 @foo.0(i32 %0) {
            init:
              %a = alloca i32, align 4
              store i32 %0, i32* %a, align 4
              br label %body

            body:                                             ; preds = %init
              %load = load i32, i32* %a, align 4
              %add = add i32 %load, 1
              ret i32 %add
            }
       "}
        .trim();
        assert_eq!(expected, module.print_to_string().to_str().unwrap().trim());
    })));
    let (registry, handles) = WorkerRegistry::create_workers(&threads, top_level, f);
    registry.add_task(task);
    registry.wait_tasks_complete(handles);
}
