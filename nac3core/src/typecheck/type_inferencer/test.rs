use super::super::typedef::*;
use super::*;
use crate::symbol_resolver::*;
use crate::top_level::DefinitionId;
use crate::{location::Location, top_level::TopLevelDef};
use indoc::indoc;
use itertools::zip;
use parking_lot::RwLock;
use rustpython_parser::parser::parse_program;
use test_case::test_case;

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
            top_level: TopLevelContext {
                definitions: Default::default(),
                unifiers: Default::default(),
            },
            unifier,
            function_data: FunctionData {
                resolver,
                bound_variables: Vec::new(),
                return_type: None,
            },
            primitives,
            id_to_name,
            identifier_mapping,
            virtual_checks: Vec::new(),
            calls: HashMap::new(),
        }
    }

    fn new() -> TestEnvironment {
        let mut unifier = Unifier::new();
        let mut identifier_mapping = HashMap::new();
        let mut top_level_defs = Vec::new();
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
        identifier_mapping.insert("None".into(), none);
        for i in 0..5 {
            top_level_defs.push(RwLock::new(TopLevelDef::Class {
                object_id: DefinitionId(i),
                type_vars: Default::default(),
                fields: Default::default(),
                methods: Default::default(),
                ancestors: Default::default(),
                resolver: None,
            }));
        }

        let primitives = PrimitiveStore { int32, int64, float, bool, none };

        let (v0, id) = unifier.get_fresh_var();

        let foo_ty = unifier.add_ty(TypeEnum::TObj {
            obj_id: DefinitionId(5),
            fields: [("a".into(), v0)].iter().cloned().collect::<HashMap<_, _>>().into(),
            params: [(id, v0)].iter().cloned().collect::<HashMap<_, _>>().into(),
        });
        top_level_defs.push(RwLock::new(TopLevelDef::Class {
            object_id: DefinitionId(5),
            type_vars: vec![v0],
            fields: [("a".into(), v0)].into(),
            methods: Default::default(),
            ancestors: Default::default(),
            resolver: None,
        }));

        identifier_mapping.insert(
            "Foo".into(),
            unifier.add_ty(TypeEnum::TFunc(FunSignature {
                args: vec![],
                ret: foo_ty,
                vars: [(id, v0)].iter().cloned().collect(),
            }.into())),
        );

        let fun = unifier.add_ty(TypeEnum::TFunc(FunSignature {
            args: vec![],
            ret: int32,
            vars: Default::default(),
        }.into()));
        let bar = unifier.add_ty(TypeEnum::TObj {
            obj_id: DefinitionId(6),
            fields: [("a".into(), int32), ("b".into(), fun)]
                .iter()
                .cloned()
                .collect::<HashMap<_, _>>()
                .into(),
            params: Default::default(),
        });
        top_level_defs.push(RwLock::new(TopLevelDef::Class {
            object_id: DefinitionId(6),
            type_vars: Default::default(),
            fields: [("a".into(), int32), ("b".into(), fun)].into(),
            methods: Default::default(),
            ancestors: Default::default(),
            resolver: None,
        }));
        identifier_mapping.insert(
            "Bar".into(),
            unifier.add_ty(TypeEnum::TFunc(FunSignature {
                args: vec![],
                ret: bar,
                vars: Default::default(),
            }.into())),
        );

        let bar2 = unifier.add_ty(TypeEnum::TObj {
            obj_id: DefinitionId(7),
            fields: [("a".into(), bool), ("b".into(), fun)]
                .iter()
                .cloned()
                .collect::<HashMap<_, _>>()
                .into(),
            params: Default::default(),
        });
        top_level_defs.push(RwLock::new(TopLevelDef::Class {
            object_id: DefinitionId(7),
            type_vars: Default::default(),
            fields: [("a".into(), bool), ("b".into(), fun)].into(),
            methods: Default::default(),
            ancestors: Default::default(),
            resolver: None,
        }));
        identifier_mapping.insert(
            "Bar2".into(),
            unifier.add_ty(TypeEnum::TFunc(FunSignature {
                args: vec![],
                ret: bar2,
                vars: Default::default(),
            }.into())),
        );
        let class_names = [("Bar".into(), bar), ("Bar2".into(), bar2)].iter().cloned().collect();

        let id_to_name = [
            (0, "int32".to_string()),
            (1, "int64".to_string()),
            (2, "float".to_string()),
            (3, "bool".to_string()),
            (4, "none".to_string()),
            (5, "Foo".to_string()),
            (6, "Bar".to_string()),
            (7, "Bar2".to_string()),
        ]
        .iter()
        .cloned()
        .collect();

        let top_level = TopLevelContext {
            definitions: Arc::new(RwLock::new(top_level_defs)),
            unifiers: Default::default(),
        };

        let resolver = Arc::new(Resolver {
            id_to_type: identifier_mapping.clone(),
            id_to_def: [
                ("Foo".into(), DefinitionId(5)),
                ("Bar".into(), DefinitionId(6)),
                ("Bar2".into(), DefinitionId(7)),
            ]
            .iter()
            .cloned()
            .collect(),
            class_names,
        }) as Arc<dyn SymbolResolver + Send + Sync>;

        TestEnvironment {
            unifier,
            top_level,
            function_data: FunctionData {
                resolver,
                bound_variables: Vec::new(),
                return_type: None,
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

#[test_case(indoc! {"
        a = 1234
        b = int64(2147483648)
        c = 1.234
        d = True
    "},
    [("a", "int32"), ("b", "int64"), ("c", "float"), ("d", "bool")].iter().cloned().collect(),
    &[]
    ; "primitives test")]
#[test_case(indoc! {"
        a = lambda x, y: x
        b = lambda x: a(x, x)
        c = 1.234
        d = b(c)
    "},
    [("a", "fn[[x=float, y=float], float]"), ("b", "fn[[x=float], float]"), ("c", "float"), ("d", "float")].iter().cloned().collect(),
    &[]
    ; "lambda test")]
#[test_case(indoc! {"
        a = lambda x: x
        b = lambda x: x

        foo1 = Foo()
        foo2 = Foo()
        c = a(foo1.a)
        d = b(foo2.a)

        a(True)
        b(123)

    "},
    [("a", "fn[[x=bool], bool]"), ("b", "fn[[x=int32], int32]"), ("c", "bool"),
     ("d", "int32"), ("foo1", "Foo[bool]"), ("foo2", "Foo[int32]")].iter().cloned().collect(),
    &[]
    ; "obj test")]
#[test_case(indoc! {"
        f = lambda x: True
        a = [1, 2, 3]
        b = [f(x) for x in a if f(x)]
    "},
    [("a", "list[int32]"), ("b", "list[bool]"), ("f", "fn[[x=int32], bool]")].iter().cloned().collect(),
    &[]
    ; "listcomp test")]
#[test_case(indoc! {"
        a = virtual(Bar(), Bar)
        b = a.b()
        a = virtual(Bar2())
    "},
    [("a", "virtual[Bar]"), ("b", "int32")].iter().cloned().collect(),
    &[("Bar", "Bar"), ("Bar2", "Bar")]
    ; "virtual test")]
#[test_case(indoc! {"
        a = [virtual(Bar(), Bar), virtual(Bar2())]
        b = [x.b() for x in a]
    "},
    [("a", "list[virtual[Bar]]"), ("b", "list[int32]")].iter().cloned().collect(),
    &[("Bar", "Bar"), ("Bar2", "Bar")]
    ; "virtual list test")]
fn test_basic(source: &str, mapping: HashMap<&str, &str>, virtuals: &[(&str, &str)]) {
    println!("source:\n{}", source);
    let mut env = TestEnvironment::new();
    let id_to_name = std::mem::take(&mut env.id_to_name);
    let mut defined_identifiers: Vec<_> = env.identifier_mapping.keys().cloned().collect();
    defined_identifiers.push("virtual".to_string());
    let mut inferencer = env.get_inferencer();
    let statements = parse_program(source).unwrap();
    let statements = statements
        .into_iter()
        .map(|v| inferencer.fold_stmt(v))
        .collect::<Result<Vec<_>, _>>()
        .unwrap();

    inferencer.check_block(&statements, &mut defined_identifiers).unwrap();

    for (k, v) in inferencer.variable_mapping.iter() {
        let name = inferencer.unifier.stringify(
            *v,
            &mut |v| id_to_name.get(&v).unwrap().clone(),
            &mut |v| format!("v{}", v),
        );
        println!("{}: {}", k, name);
    }
    for (k, v) in mapping.iter() {
        let ty = inferencer.variable_mapping.get(*k).unwrap();
        let name = inferencer.unifier.stringify(
            *ty,
            &mut |v| id_to_name.get(&v).unwrap().clone(),
            &mut |v| format!("v{}", v),
        );
        assert_eq!(format!("{}: {}", k, v), format!("{}: {}", k, name));
    }
    assert_eq!(inferencer.virtual_checks.len(), virtuals.len());
    for ((a, b), (x, y)) in zip(inferencer.virtual_checks.iter(), virtuals) {
        let a = inferencer.unifier.stringify(
            *a,
            &mut |v| id_to_name.get(&v).unwrap().clone(),
            &mut |v| format!("v{}", v),
        );
        let b = inferencer.unifier.stringify(
            *b,
            &mut |v| id_to_name.get(&v).unwrap().clone(),
            &mut |v| format!("v{}", v),
        );

        assert_eq!(&a, x);
        assert_eq!(&b, y);
    }
}

#[test_case(indoc! {"
        a = 2
        b = 2
        c = a + b
        d = a - b
        e = a * b
        f = a / b
        g = a // b
        h = a % b
    "},
    [("a", "int32"),
    ("b", "int32"),
    ("c", "int32"),
    ("d", "int32"),
    ("e", "int32"),
    ("f", "float"),
    ("g", "int32"),
    ("h", "int32")].iter().cloned().collect()
    ; "int32")]
#[test_case(
    indoc! {"
        a = 2.4
        b = 3.6
        c = a + b
        d = a - b
        e = a * b
        f = a / b
        g = a // b
        h = a % b
        i = a ** b
        ii = 3
        j = a ** b
    "},
    [("a", "float"),
    ("b", "float"),
    ("c", "float"),
    ("d", "float"),
    ("e", "float"),
    ("f", "float"),
    ("g", "float"),
    ("h", "float"),
    ("i", "float"),
    ("ii", "int32"),
    ("j", "float")].iter().cloned().collect()
    ; "float"
)]
#[test_case(
    indoc! {"
        a = int64(12312312312)
        b = int64(24242424424)
        c = a + b
        d = a - b
        e = a * b
        f = a / b
        g = a // b
        h = a % b
        i = a == b
        j = a > b
        k = a < b
        l = a != b
    "},
    [("a", "int64"),
    ("b", "int64"),
    ("c", "int64"),
    ("d", "int64"),
    ("e", "int64"),
    ("f", "float"),
    ("g", "int64"),
    ("h", "int64"),
    ("i", "bool"),
    ("j", "bool"),
    ("k", "bool"),
    ("l", "bool")].iter().cloned().collect()
    ; "int64"
)]
#[test_case(
    indoc! {"
        a = True
        b = False
        c = a == b
        d = not a
        e = a != b
    "},
    [("a", "bool"),
    ("b", "bool"),
    ("c", "bool"),
    ("d", "bool"),
    ("e", "bool")].iter().cloned().collect()
    ; "boolean"
)]
fn test_primitive_magic_methods(source: &str, mapping: HashMap<&str, &str>) {
    println!("source:\n{}", source);
    let mut env = TestEnvironment::basic_test_env();
    let id_to_name = std::mem::take(&mut env.id_to_name);
    let mut defined_identifiers: Vec<_> = env.identifier_mapping.keys().cloned().collect();
    defined_identifiers.push("virtual".to_string());
    let mut inferencer = env.get_inferencer();
    let statements = parse_program(source).unwrap();
    let statements = statements
        .into_iter()
        .map(|v| inferencer.fold_stmt(v))
        .collect::<Result<Vec<_>, _>>()
        .unwrap();

    inferencer.check_block(&statements, &mut defined_identifiers).unwrap();

    for (k, v) in inferencer.variable_mapping.iter() {
        let name = inferencer.unifier.stringify(
            *v,
            &mut |v| id_to_name.get(&v).unwrap().clone(),
            &mut |v| format!("v{}", v),
        );
        println!("{}: {}", k, name);
    }
    for (k, v) in mapping.iter() {
        let ty = inferencer.variable_mapping.get(*k).unwrap();
        let name = inferencer.unifier.stringify(
            *ty,
            &mut |v| id_to_name.get(&v).unwrap().clone(),
            &mut |v| format!("v{}", v),
        );
        assert_eq!(format!("{}: {}", k, v), format!("{}: {}", k, name));
    }
}
