use crate::{
    location::Location,
    symbol_resolver::{SymbolResolver, SymbolValue},
    toplevel::DefinitionId,
    typecheck::{
        type_inferencer::PrimitiveStore,
        typedef::{Type, Unifier},
    },
};
use indoc::indoc;
use parking_lot::Mutex;
use rustpython_parser::parser::parse_program;
use std::{collections::HashMap, sync::Arc};
use test_case::test_case;

use super::*;

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

    fn add_id_def(&mut self, id: String, def: DefinitionId) {
        self.id_to_def.insert(id, def);
    }
}

#[test_case(
    vec![
        indoc! {"
            def fun(a: int32) -> int32:
                return a
        "},
        indoc! {"
            class A:
                def __init__(self):
                    self.a: int32 = 3
        "},
        indoc! {"
            class B:
                def __init__(self):
                    self.b: float = 4.3
                
                def fun(self):
                    self.b = self.b + 3.0
        "},
        indoc! {"
            def foo(a: float):
                a + 1.0
        "},
        indoc! {"
            class C(B):
                def __init__(self):
                    self.c: int32 = 4
                    self.a: bool = True
        "}
    ]
)]
fn test_simple_register(source: Vec<&str>) {
    let mut composer = TopLevelComposer::new();

    for s in source {
        let ast = parse_program(s).unwrap();
        let ast = ast[0].clone();

        composer.register_top_level(ast, None).unwrap();
    }
}

#[test_case(
    vec![
        indoc! {"
            def fun(a: int32) -> int32:
                return a
        "},
        indoc! {"
            def foo(a: float):
                a + 1.0
        "},
        indoc! {"
            def f(b: int64) -> int32:
                return 3
        "},
    ],
    vec![
        "fn[[a=0], 0]",
        "fn[[a=2], 4]",
        "fn[[b=1], 0]",
    ],
    vec![
        "fun",
        "foo",
        "f"
    ]
)]
fn test_simple_function_analyze(source: Vec<&str>, tys: Vec<&str>, names: Vec<&str>) {
    let mut composer = TopLevelComposer::new();

    let resolver = Arc::new(Mutex::new(Box::new(Resolver {
        id_to_def: Default::default(),
        id_to_type: Default::default(),
        class_names: Default::default(),
    }) as Box<dyn SymbolResolver + Send + Sync>));

    for s in source {
        let ast = parse_program(s).unwrap();
        let ast = ast[0].clone();

        let (id, def_id) = composer.register_top_level(ast, Some(resolver.clone())).unwrap();
        resolver.lock().add_id_def(id, def_id);
    }

    composer.start_analysis().unwrap();

    for (i, (def, _)) in composer.definition_ast_list.iter().skip(5).enumerate() {
        let def = &*def.read();
        if let TopLevelDef::Function { signature, name, .. } = def {
            let ty_str =
                composer
                    .unifier
                    .stringify(*signature, &mut |id| id.to_string(), &mut |id| id.to_string());
            assert_eq!(ty_str, tys[i]);
            assert_eq!(name, names[i]);
        }
    }
}

#[test_case(
    vec![
        indoc! {"
            class A():
                def __init__(self):
                    self.a: int32 = 3
                def fun(self, b: B):
                    pass
                def foo(self, a: T, b: V):
                    pass
        "},
        indoc! {"
            class B(C):
                def __init__(self):
                    pass
        "},
        indoc! {"
            class C(A):
                def __init__(self):
                    pass
                def fun(self, b: B):
                    a = 1
                    pass
        "},
        indoc! {"
            def foo(a: A):
                pass
        "},
        indoc! {"
            def ff(a: T) -> V:
                pass
        "}
    ],
    vec![
        indoc! {"5: Class {
        name: \"A\",
        def_id: DefinitionId(5),
        ancestors: [CustomClassKind { id: DefinitionId(5), params: [] }],
        fields: [(\"a\", \"0\")],
        methods: [(\"__init__\", \"fn[[self=5], 5]\", DefinitionId(6)), (\"fun\", \"fn[[self=5, b=9], 4]\", DefinitionId(7))],
        type_vars: []
        }"},

        indoc! {"6: Function {
        name: \"A__init__\",
        sig: \"fn[[self=5], 5]\",
        var_id: []
        }"},

        indoc! {"7: Function {
        name: \"Afun\",
        sig: \"fn[[self=5, b=9], 4]\",
        var_id: []
        }"},

        indoc! {"8: Initializer { DefinitionId(5) }"},

        indoc! {"9: Class {
        name: \"B\",
        def_id: DefinitionId(9),
        ancestors: [CustomClassKind { id: DefinitionId(9), params: [] }, CustomClassKind { id: DefinitionId(12), params: [] }, CustomClassKind { id: DefinitionId(5), params: [] }],
        fields: [(\"a\", \"0\")],
        methods: [(\"__init__\", \"fn[[self=9], 9]\", DefinitionId(10)), (\"fun\", \"fn[[self=12, b=9], 4]\", DefinitionId(14))],
        type_vars: []
        }"},

        indoc! {"10: Function {
        name: \"B__init__\",
        sig: \"fn[[self=9], 9]\",
        var_id: []
        }"},

        indoc! {"11: Initializer { DefinitionId(9) }"},

        indoc! {"12: Class {
        name: \"C\",
        def_id: DefinitionId(12),
        ancestors: [CustomClassKind { id: DefinitionId(12), params: [] }, CustomClassKind { id: DefinitionId(5), params: [] }],
        fields: [(\"a\", \"0\")],
        methods: [(\"__init__\", \"fn[[self=12], 12]\", DefinitionId(13)), (\"fun\", \"fn[[self=12, b=9], 4]\", DefinitionId(14))],
        type_vars: []
        }"},

        indoc! {"13: Function {
        name: \"C__init__\",
        sig: \"fn[[self=12], 12]\",
        var_id: []
        }"},

        indoc! {"14: Function {
        name: \"Cfun\",
        sig: \"fn[[self=12, b=9], 4]\",
        var_id: []
        }"},

        indoc! {"15: Initializer { DefinitionId(12) }"},

        indoc! {"16: Function {
        name: \"foo\",
        sig: \"fn[[a=5], 4]\",
        var_id: []
        }"},
    ]
)]
fn test_simple_class_analyze(source: Vec<&str>, res: Vec<&str>) {
    let mut composer = TopLevelComposer::new();

    let tvar_t = composer.unifier.get_fresh_var();
    let tvar_v = composer
        .unifier
        .get_fresh_var_with_range(&[composer.primitives_ty.bool, composer.primitives_ty.int32]);
    println!("t: {}", tvar_t.1);
    println!("v: {}\n", tvar_v.1);

    let resolver = Arc::new(Mutex::new(Box::new(Resolver {
        id_to_def: Default::default(),
        id_to_type: vec![("T".to_string(), tvar_t.0), ("V".to_string(), tvar_v.0)]
            .into_iter()
            .collect(),
        class_names: Default::default(),
    }) as Box<dyn SymbolResolver + Send + Sync>));

    for s in source {
        let ast = parse_program(s).unwrap();
        let ast = ast[0].clone();

        let (id, def_id) = composer.register_top_level(ast, Some(resolver.clone())).unwrap();
        resolver.lock().add_id_def(id, def_id);
    }

    composer.start_analysis().unwrap();

    // skip 5 to skip primitives
    for (i, (def, _)) in composer.definition_ast_list.iter().skip(5).enumerate() {
        let def = &*def.read();
        println!(
            "{}: {}\n",
            i + 5,
            def.to_string(
                composer.unifier.borrow_mut(),
                &mut |id| format!("class{}", id),
                &mut |id| format!("tvar{}", id)
            )
        );
        // assert_eq!(
        //     format!(
        //         "{}: {}",
        //         i + 5,
        //         def.to_string(
        //             composer.unifier.borrow_mut(),
        //             &mut |id| id.to_string(),
        //             &mut |id| id.to_string()
        //         )
        //     ),
        //     res[i]
        // )
    }
}
