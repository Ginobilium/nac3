use super::super::typedef::*;
use super::*;
use crate::{
    codegen::CodeGenContext,
    location::Location,
    symbol_resolver::ValueEnum,
    toplevel::{DefinitionId, TopLevelDef},
};
use indoc::indoc;
use itertools::zip;
use nac3parser::parser::parse_program;
use parking_lot::RwLock;
use test_case::test_case;

struct Resolver {
    id_to_type: HashMap<StrRef, Type>,
    id_to_def: HashMap<StrRef, DefinitionId>,
    class_names: HashMap<StrRef, Type>,
}

impl SymbolResolver for Resolver {
    fn get_symbol_type(
        &self,
        _: &mut Unifier,
        _: &[Arc<RwLock<TopLevelDef>>],
        _: &PrimitiveStore,
        str: StrRef,
    ) -> Option<Type> {
        self.id_to_type.get(&str).cloned()
    }

    fn get_symbol_value<'ctx, 'a>(
        &self,
        _: StrRef,
        _: &mut CodeGenContext<'ctx, 'a>,
    ) -> Option<ValueEnum<'ctx>> {
        unimplemented!()
    }

    fn get_symbol_location(&self, _: StrRef) -> Option<Location> {
        unimplemented!()
    }

    fn get_identifier_def(&self, id: StrRef) -> Option<DefinitionId> {
        self.id_to_def.get(&id).cloned()
    }
}

struct TestEnvironment {
    pub unifier: Unifier,
    pub function_data: FunctionData,
    pub primitives: PrimitiveStore,
    pub id_to_name: HashMap<usize, StrRef>,
    pub identifier_mapping: HashMap<StrRef, Type>,
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
        if let TypeEnum::TObj { fields, .. } = &*unifier.get_ty(int32) {
            let add_ty = unifier.add_ty(TypeEnum::TFunc(
                FunSignature {
                    args: vec![FuncArg { name: "other".into(), ty: int32, default_value: None }],
                    ret: int32,
                    vars: HashMap::new(),
                }
                .into(),
            ));
            fields.borrow_mut().insert("__add__".into(), (add_ty, false));
        }
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
        let range = unifier.add_ty(TypeEnum::TObj {
            obj_id: DefinitionId(5),
            fields: HashMap::new().into(),
            params: HashMap::new().into(),
        });
        let str = unifier.add_ty(TypeEnum::TObj {
            obj_id: DefinitionId(6),
            fields: HashMap::new().into(),
            params: HashMap::new().into(),
        });
        let primitives = PrimitiveStore { int32, int64, float, bool, none, range, str };
        set_primitives_magic_methods(&primitives, &mut unifier);

        let id_to_name = [
            (0, "int32".into()),
            (1, "int64".into()),
            (2, "float".into()),
            (3, "bool".into()),
            (4, "none".into()),
            (5, "range".into()),
            (6, "str".into()),
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
                personality_symbol: None,
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
        let mut top_level_defs: Vec<Arc<RwLock<TopLevelDef>>> = Vec::new();
        let int32 = unifier.add_ty(TypeEnum::TObj {
            obj_id: DefinitionId(0),
            fields: HashMap::new().into(),
            params: HashMap::new().into(),
        });
        if let TypeEnum::TObj { fields, .. } = &*unifier.get_ty(int32) {
            let add_ty = unifier.add_ty(TypeEnum::TFunc(
                FunSignature {
                    args: vec![FuncArg { name: "other".into(), ty: int32, default_value: None }],
                    ret: int32,
                    vars: HashMap::new(),
                }
                .into(),
            ));
            fields.borrow_mut().insert("__add__".into(), (add_ty, false));
        }
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
        let range = unifier.add_ty(TypeEnum::TObj {
            obj_id: DefinitionId(5),
            fields: HashMap::new().into(),
            params: HashMap::new().into(),
        });
        let str = unifier.add_ty(TypeEnum::TObj {
            obj_id: DefinitionId(6),
            fields: HashMap::new().into(),
            params: HashMap::new().into(),
        });
        identifier_mapping.insert("None".into(), none);
        for (i, name) in
            ["int32", "int64", "float", "bool", "none", "range", "str"].iter().enumerate()
        {
            top_level_defs.push(
                RwLock::new(TopLevelDef::Class {
                    name: (*name).into(),
                    object_id: DefinitionId(i),
                    type_vars: Default::default(),
                    fields: Default::default(),
                    methods: Default::default(),
                    ancestors: Default::default(),
                    resolver: None,
                    constructor: None,
                })
                .into(),
            );
        }

        let primitives = PrimitiveStore { int32, int64, float, bool, none, range, str };

        let (v0, id) = unifier.get_fresh_var();

        let foo_ty = unifier.add_ty(TypeEnum::TObj {
            obj_id: DefinitionId(7),
            fields: [("a".into(), (v0, true))].iter().cloned().collect::<HashMap<_, _>>().into(),
            params: [(id, v0)].iter().cloned().collect::<HashMap<_, _>>().into(),
        });
        top_level_defs.push(
            RwLock::new(TopLevelDef::Class {
                name: "Foo".into(),
                object_id: DefinitionId(7),
                type_vars: vec![v0],
                fields: [("a".into(), v0, true)].into(),
                methods: Default::default(),
                ancestors: Default::default(),
                resolver: None,
                constructor: None,
            })
            .into(),
        );

        identifier_mapping.insert(
            "Foo".into(),
            unifier.add_ty(TypeEnum::TFunc(
                FunSignature {
                    args: vec![],
                    ret: foo_ty,
                    vars: [(id, v0)].iter().cloned().collect(),
                }
                .into(),
            )),
        );

        let fun = unifier.add_ty(TypeEnum::TFunc(
            FunSignature { args: vec![], ret: int32, vars: Default::default() }.into(),
        ));
        let bar = unifier.add_ty(TypeEnum::TObj {
            obj_id: DefinitionId(8),
            fields: [("a".into(), (int32, true)), ("b".into(), (fun, true))]
                .iter()
                .cloned()
                .collect::<HashMap<_, _>>()
                .into(),
            params: Default::default(),
        });
        top_level_defs.push(
            RwLock::new(TopLevelDef::Class {
                name: "Bar".into(),
                object_id: DefinitionId(8),
                type_vars: Default::default(),
                fields: [("a".into(), int32, true), ("b".into(), fun, true)].into(),
                methods: Default::default(),
                ancestors: Default::default(),
                resolver: None,
                constructor: None,
            })
            .into(),
        );
        identifier_mapping.insert(
            "Bar".into(),
            unifier.add_ty(TypeEnum::TFunc(
                FunSignature { args: vec![], ret: bar, vars: Default::default() }.into(),
            )),
        );

        let bar2 = unifier.add_ty(TypeEnum::TObj {
            obj_id: DefinitionId(9),
            fields: [("a".into(), (bool, true)), ("b".into(), (fun, false))]
                .iter()
                .cloned()
                .collect::<HashMap<_, _>>()
                .into(),
            params: Default::default(),
        });
        top_level_defs.push(
            RwLock::new(TopLevelDef::Class {
                name: "Bar2".into(),
                object_id: DefinitionId(9),
                type_vars: Default::default(),
                fields: [("a".into(), bool, true), ("b".into(), fun, false)].into(),
                methods: Default::default(),
                ancestors: Default::default(),
                resolver: None,
                constructor: None,
            })
            .into(),
        );
        identifier_mapping.insert(
            "Bar2".into(),
            unifier.add_ty(TypeEnum::TFunc(
                FunSignature { args: vec![], ret: bar2, vars: Default::default() }.into(),
            )),
        );
        let class_names = [("Bar".into(), bar), ("Bar2".into(), bar2)].iter().cloned().collect();

        let id_to_name = [
            (0, "int32".into()),
            (1, "int64".into()),
            (2, "float".into()),
            (3, "bool".into()),
            (4, "none".into()),
            (5, "range".into()),
            (6, "str".into()),
            (7, "Foo".into()),
            (8, "Bar".into()),
            (9, "Bar2".into()),
        ]
        .iter()
        .cloned()
        .collect();

        let top_level = TopLevelContext {
            definitions: Arc::new(top_level_defs.into()),
            unifiers: Default::default(),
            personality_symbol: None,
        };

        let resolver = Arc::new(Resolver {
            id_to_type: identifier_mapping.clone(),
            id_to_def: [
                ("Foo".into(), DefinitionId(7)),
                ("Bar".into(), DefinitionId(8)),
                ("Bar2".into(), DefinitionId(9)),
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
            defined_identifiers: Default::default(),
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
        a = lambda x: x + x
        b = lambda x: a(x) + x
        a = b
        c = b(1)
    "},
    [("a", "fn[[x=int32], int32]"), ("b", "fn[[x=int32], int32]"), ("c", "int32")].iter().cloned().collect(),
    &[]
    ; "lambda test 2")]
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
     ("d", "int32"), ("foo1", "Foo[1->bool]"), ("foo2", "Foo[1->int32]")].iter().cloned().collect(),
    &[]
    ; "obj test")]
#[test_case(indoc! {"
        a = [1, 2, 3]
        b = [x + x for x in a]
    "},
    [("a", "list[int32]"), ("b", "list[int32]")].iter().cloned().collect(),
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
    let mut defined_identifiers: HashSet<_> = env.identifier_mapping.keys().cloned().collect();
    defined_identifiers.insert("virtual".into());
    let mut inferencer = env.get_inferencer();
    inferencer.defined_identifiers = defined_identifiers.clone();
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
            &mut |v| (*id_to_name.get(&v).unwrap()).into(),
            &mut |v| format!("v{}", v),
        );
        println!("{}: {}", k, name);
    }
    for (k, v) in mapping.iter() {
        let ty = inferencer.variable_mapping.get(&(*k).into()).unwrap();
        let name = inferencer.unifier.stringify(
            *ty,
            &mut |v| (*id_to_name.get(&v).unwrap()).into(),
            &mut |v| format!("v{}", v),
        );
        assert_eq!(format!("{}: {}", k, v), format!("{}: {}", k, name));
    }
    assert_eq!(inferencer.virtual_checks.len(), virtuals.len());
    for ((a, b), (x, y)) in zip(inferencer.virtual_checks.iter(), virtuals) {
        let a = inferencer.unifier.stringify(
            *a,
            &mut |v| (*id_to_name.get(&v).unwrap()).into(),
            &mut |v| format!("v{}", v),
        );
        let b = inferencer.unifier.stringify(
            *b,
            &mut |v| (*id_to_name.get(&v).unwrap()).into(),
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
    let mut defined_identifiers: HashSet<_> = env.identifier_mapping.keys().cloned().collect();
    defined_identifiers.insert("virtual".into());
    let mut inferencer = env.get_inferencer();
    inferencer.defined_identifiers = defined_identifiers.clone();
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
            &mut |v| (*id_to_name.get(&v).unwrap()).into(),
            &mut |v| format!("v{}", v),
        );
        println!("{}: {}", k, name);
    }
    for (k, v) in mapping.iter() {
        let ty = inferencer.variable_mapping.get(&(*k).into()).unwrap();
        let name = inferencer.unifier.stringify(
            *ty,
            &mut |v| (*id_to_name.get(&v).unwrap()).into(),
            &mut |v| format!("v{}", v),
        );
        assert_eq!(format!("{}: {}", k, v), format!("{}: {}", k, name));
    }
}
