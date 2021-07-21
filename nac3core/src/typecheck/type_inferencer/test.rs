use super::super::location::Location;
use super::super::symbol_resolver::*;
use super::super::typedef::*;
use super::*;
use indoc::indoc;
use rustpython_parser::ast;
use rustpython_parser::parser::parse_program;
use test_case::test_case;

struct Resolver {
    type_mapping: HashMap<String, Type>,
}

impl SymbolResolver for Resolver {
    fn get_symbol_type(&mut self, str: &str) -> Option<Type> {
        self.type_mapping.get(str).cloned()
    }

    fn parse_type_name(&mut self, _: &ast::Expr<()>) -> Option<Type> {
        unimplemented!()
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
    pub resolver: Box<dyn SymbolResolver>,
    pub calls: Vec<Rc<Call>>,
    pub primitives: PrimitiveStore,
    pub id_to_name: HashMap<usize, String>,
}

impl TestEnvironment {
    fn new() -> TestEnvironment {
        let mut unifier = Unifier::new();
        let mut type_mapping = HashMap::new();
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
        type_mapping.insert("int32".into(), int32);
        type_mapping.insert("int64".into(), int64);
        type_mapping.insert("float".into(), float);
        type_mapping.insert("bool".into(), bool);
        type_mapping.insert("none".into(), none);

        let primitives = PrimitiveStore {
            int32,
            int64,
            float,
            bool,
            none,
        };

        let (v0, id) = unifier.get_fresh_var();
        type_mapping.insert(
            "foo".into(),
            unifier.add_ty(TypeEnum::TObj {
                obj_id: 5,
                fields: [("a".into(), v0)].iter().cloned().collect(),
                params: [(id, v0)].iter().cloned().collect(),
            }),
        );

        let id_to_name = [
            (0, "int32".to_string()),
            (1, "int64".to_string()),
            (2, "float".to_string()),
            (3, "bool".to_string()),
            (4, "none".to_string()),
            (5, "Foo".to_string()),
        ]
        .iter()
        .cloned()
        .collect();

        let resolver = Box::new(Resolver { type_mapping }) as Box<dyn SymbolResolver>;

        TestEnvironment {
            unifier,
            resolver,
            primitives,
            id_to_name,
            calls: Vec::new(),
        }
    }

    fn get_inferencer(&mut self) -> Inferencer {
        Inferencer {
            resolver: &mut self.resolver,
            unifier: &mut self.unifier,
            variable_mapping: Default::default(),
            calls: &mut self.calls,
            primitives: &mut self.primitives,
        }
    }
}

#[test_case(indoc! {"
        a = 1234
        b = int64(2147483648)
        c = 1.234
        d = True
    "},
    [("a", "int32"), ("b", "int64"), ("c", "float"), ("d", "bool")].iter().cloned().collect()
    ; "primitives test")]
#[test_case(indoc! {"
        a = lambda x, y: x
        b = lambda x: a(x, x)
        c = 1.234
        d = b(c)
    "},
    [("a", "fn[[x=float, y=float], float]"), ("b", "fn[[x=float], float]"), ("c", "float"), ("d", "float")].iter().cloned().collect()
    ; "lambda test")]
fn test_basic(source: &str, mapping: HashMap<&str, &str>) {
    let mut env = TestEnvironment::new();
    let id_to_name = std::mem::take(&mut env.id_to_name);
    let mut inferencer = env.get_inferencer();
    let statements = parse_program(source).unwrap();
    statements
        .into_iter()
        .map(|v| inferencer.fold_stmt(v))
        .collect::<Result<Vec<_>, _>>()
        .unwrap();
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

