use super::{CodeGenTask, WorkerRegistry};
use crate::{
    codegen::WithCall,
    location::Location,
    symbol_resolver::{SymbolResolver, SymbolValue},
    top_level::{DefinitionId, TopLevelContext},
    typecheck::{
        magic_methods::set_primitives_magic_methods,
        type_inferencer::{CodeLocation, FunctionData, Inferencer, PrimitiveStore},
        typedef::{CallId, FunSignature, FuncArg, Type, TypeEnum, Unifier},
    },
};
use indoc::indoc;
use parking_lot::RwLock;
use rustpython_parser::{ast::fold::Fold, parser::parse_program};
use std::collections::HashMap;
use std::sync::Arc;

#[derive(Clone)]
struct Resolver {
    id_to_type: HashMap<String, Type>,
    id_to_def: HashMap<String, DefinitionId>,
    class_names: HashMap<String, Type>,
}

impl SymbolResolver for Resolver {
    fn get_symbol_type(&self, _: &mut Unifier, _: &PrimitiveStore, str: &str) -> Option<Type> {
        self.id_to_type.get(str).cloned()
    }

    fn get_symbol_value(&self, _: &str) -> Option<SymbolValue> {
        unimplemented!()
    }

    fn get_symbol_location(&self, _: &str) -> Option<Location> {
        unimplemented!()
    }

    fn get_identifier_def(&self, id: &str) -> Option<DefinitionId> {
        self.id_to_def.get(id).cloned()
    }
}

struct TestEnvironment {
    pub unifier: Unifier,
    pub function_data: FunctionData,
    pub primitives: PrimitiveStore,
    pub id_to_name: HashMap<usize, String>,
    pub identifier_mapping: HashMap<String, Type>,
    pub virtual_checks: Vec<(Type, Type)>,
    pub calls: HashMap<CodeLocation, CallId>,
    pub top_level: TopLevelContext,
}

impl TestEnvironment {
    pub fn basic_test_env() -> TestEnvironment {
        let mut unifier = Unifier::new();

        let int32 = unifier.add_ty(TypeEnum::TObj {
            obj_id: DefinitionId(0),
            fields: HashMap::new().into(),
            params: HashMap::new().into(),
        });
        let int64 = unifier.add_ty(TypeEnum::TObj {
            obj_id: DefinitionId(1),
            fields: HashMap::new().into(),
            params: HashMap::new().into(),
        });
        let float = unifier.add_ty(TypeEnum::TObj {
            obj_id: DefinitionId(2),
            fields: HashMap::new().into(),
            params: HashMap::new().into(),
        });
        let bool = unifier.add_ty(TypeEnum::TObj {
            obj_id: DefinitionId(3),
            fields: HashMap::new().into(),
            params: HashMap::new().into(),
        });
        let none = unifier.add_ty(TypeEnum::TObj {
            obj_id: DefinitionId(4),
            fields: HashMap::new().into(),
            params: HashMap::new().into(),
        });
        let primitives = PrimitiveStore { int32, int64, float, bool, none };
        set_primitives_magic_methods(&primitives, &mut unifier);

        let id_to_name = [
            (0, "int32".to_string()),
            (1, "int64".to_string()),
            (2, "float".to_string()),
            (3, "bool".to_string()),
            (4, "none".to_string()),
        ]
        .iter()
        .cloned()
        .collect();

        let mut identifier_mapping = HashMap::new();
        identifier_mapping.insert("None".into(), none);

        let resolver = Arc::new(Resolver {
            id_to_type: identifier_mapping.clone(),
            id_to_def: Default::default(),
            class_names: Default::default(),
        }) as Arc<dyn SymbolResolver + Send + Sync>;

        TestEnvironment {
            unifier,
            top_level: TopLevelContext {
                definitions: Default::default(),
                unifiers: Default::default(),
                // conetexts: Default::default(),
            },
            function_data: FunctionData {
                resolver,
                bound_variables: Vec::new(),
                return_type: Some(primitives.int32),
            },
            primitives,
            id_to_name,
            identifier_mapping,
            virtual_checks: Vec::new(),
            calls: HashMap::new(),
        }
    }

    fn get_inferencer(&mut self) -> Inferencer {
        Inferencer {
            top_level: &self.top_level,
            function_data: &mut self.function_data,
            unifier: &mut self.unifier,
            variable_mapping: Default::default(),
            primitives: &mut self.primitives,
            virtual_checks: &mut self.virtual_checks,
            calls: &mut self.calls,
        }
    }
}

#[test]
fn test_primitives() {
    let mut env = TestEnvironment::basic_test_env();
    let threads = ["test"];
    let signature = FunSignature {
        args: vec![
            FuncArg { name: "a".to_string(), ty: env.primitives.int32, default_value: None },
            FuncArg { name: "b".to_string(), ty: env.primitives.int32, default_value: None },
        ],
        ret: env.primitives.int32,
        vars: HashMap::new(),
    };

    let mut inferencer = env.get_inferencer();
    inferencer.variable_mapping.insert("a".into(), inferencer.primitives.int32);
    inferencer.variable_mapping.insert("b".into(), inferencer.primitives.int32);
    let source = indoc! { "
        c = a + b
        d = a if c == 1 else 0
        return d
        "};
    let statements = parse_program(source).unwrap();

    let statements = statements
        .into_iter()
        .map(|v| inferencer.fold_stmt(v))
        .collect::<Result<Vec<_>, _>>()
        .unwrap();
    let mut identifiers = vec!["a".to_string(), "b".to_string()];
    inferencer.check_block(&statements, &mut identifiers).unwrap();

    let top_level = Arc::new(TopLevelContext {
        definitions: Default::default(),
        unifiers: Arc::new(RwLock::new(vec![(env.unifier.get_shared_unifier(), env.primitives)])),
        // conetexts: Default::default(),
    });
    let task = CodeGenTask {
        subst: Default::default(),
        symbol_name: "testing".to_string(),
        body: statements,
        unifier_index: 0,
        resolver: env.function_data.resolver.clone(),
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
