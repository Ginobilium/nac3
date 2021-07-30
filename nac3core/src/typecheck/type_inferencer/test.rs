use super::super::location::Location;
use super::super::symbol_resolver::*;
use super::super::typedef::*;
use super::*;
use indoc::indoc;
use itertools::zip;
use rustpython_parser::ast;
use rustpython_parser::parser::parse_program;
use test_case::test_case;

struct Resolver {
    identifier_mapping: HashMap<String, Type>,
    class_names: HashMap<String, Type>,
}

impl SymbolResolver for Resolver {
    fn get_symbol_type(&mut self, str: &str) -> Option<Type> {
        self.identifier_mapping.get(str).cloned()
    }

    fn parse_type_name(&mut self, ty: &ast::Expr<()>) -> Option<Type> {
        if let ExprKind::Name { id, .. } = &ty.node {
            self.class_names.get(id).cloned()
        } else {
            unimplemented!()
        }
    }

    fn get_symbol_value(&mut self, _: &str) -> Option<SymbolValue> {
        unimplemented!()
    }

    fn get_symbol_location(&mut self, _: &str) -> Option<Location> {
        unimplemented!()
    }
}

struct TestEnvironment {
    pub unifier: Unifier,
    pub function_data: FunctionData,
    pub primitives: PrimitiveStore,
    pub id_to_name: HashMap<usize, String>,
    pub identifier_mapping: HashMap<String, Type>,
    pub virtual_checks: Vec<(Type, Type)>,
    pub calls: HashMap<CodeLocation, Rc<Call>>,
}

impl TestEnvironment {
    pub fn basic_test_env() -> TestEnvironment {
        let mut unifier = Unifier::new();
        
        let int32 = unifier.add_ty(TypeEnum::TObj {
            obj_id: 0,
            fields: HashMap::new(),
            params: HashMap::new(),
        });
        let int64 = unifier.add_ty(TypeEnum::TObj {
            obj_id: 1,
            fields: HashMap::new(),
            params: HashMap::new(),
        });
        let float = unifier.add_ty(TypeEnum::TObj {
            obj_id: 2,
            fields: HashMap::new(),
            params: HashMap::new(),
        });
        let bool = unifier.add_ty(TypeEnum::TObj {
            obj_id: 3,
            fields: HashMap::new(),
            params: HashMap::new(),
        });
        let none = unifier.add_ty(TypeEnum::TObj {
            obj_id: 4,
            fields: HashMap::new(),
            params: HashMap::new(),
        });
        // identifier_mapping.insert("None".into(), none);
        let primitives = PrimitiveStore { int32, int64, float, bool, none };
        set_primirives_magic_methods(&primitives, &mut unifier);
        
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

        let mut identifier_mapping = HashMap::new();
        identifier_mapping.insert("None".into(), none);
       
        let resolver =
            Box::new(Resolver { identifier_mapping: identifier_mapping.clone(), class_names: Default::default() })
                as Box<dyn SymbolResolver>;

        TestEnvironment {
            unifier,
            function_data: FunctionData {
                resolver,
                bound_variables: Vec::new(),
                return_type: None
            },
            primitives,
            id_to_name,
            identifier_mapping,
            virtual_checks: Vec::new(),
        }
    }

    fn new() -> TestEnvironment {
        let mut unifier = Unifier::new();
        let mut identifier_mapping = HashMap::new();
        let int32 = unifier.add_ty(TypeEnum::TObj {
            obj_id: 0,
            fields: HashMap::new(),
            params: HashMap::new(),
        });
        let int64 = unifier.add_ty(TypeEnum::TObj {
            obj_id: 1,
            fields: HashMap::new(),
            params: HashMap::new(),
        });
        let float = unifier.add_ty(TypeEnum::TObj {
            obj_id: 2,
            fields: HashMap::new(),
            params: HashMap::new(),
        });
        let bool = unifier.add_ty(TypeEnum::TObj {
            obj_id: 3,
            fields: HashMap::new(),
            params: HashMap::new(),
        });
        let none = unifier.add_ty(TypeEnum::TObj {
            obj_id: 4,
            fields: HashMap::new(),
            params: HashMap::new(),
        });
        identifier_mapping.insert("None".into(), none);

        let primitives = PrimitiveStore { int32, int64, float, bool, none };

        let (v0, id) = unifier.get_fresh_var();

        let foo_ty = unifier.add_ty(TypeEnum::TObj {
            obj_id: 5,
            fields: [("a".into(), v0)].iter().cloned().collect(),
            params: [(id, v0)].iter().cloned().collect(),
        });

        identifier_mapping.insert(
            "Foo".into(),
            unifier.add_ty(TypeEnum::TFunc(FunSignature {
                args: vec![],
                ret: foo_ty,
                vars: [(id, v0)].iter().cloned().collect(),
            })),
        );

        let fun = unifier.add_ty(TypeEnum::TFunc(FunSignature {
            args: vec![],
            ret: int32,
            vars: Default::default(),
        }));
        let bar = unifier.add_ty(TypeEnum::TObj {
            obj_id: 6,
            fields: [("a".into(), int32), ("b".into(), fun)].iter().cloned().collect(),
            params: Default::default(),
        });
        identifier_mapping.insert(
            "Bar".into(),
            unifier.add_ty(TypeEnum::TFunc(FunSignature {
                args: vec![],
                ret: bar,
                vars: Default::default(),
            })),
        );

        let bar2 = unifier.add_ty(TypeEnum::TObj {
            obj_id: 7,
            fields: [("a".into(), bool), ("b".into(), fun)].iter().cloned().collect(),
            params: Default::default(),
        });
        identifier_mapping.insert(
            "Bar2".into(),
            unifier.add_ty(TypeEnum::TFunc(FunSignature {
                args: vec![],
                ret: bar2,
                vars: Default::default(),
            })),
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

        let resolver =
            Box::new(Resolver { identifier_mapping: identifier_mapping.clone(), class_names })
                as Box<dyn SymbolResolver>;

        TestEnvironment {
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

    fn get_inferencer(&mut self) -> Inferencer {
        Inferencer {
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
