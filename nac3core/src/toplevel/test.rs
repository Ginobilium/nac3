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

struct ResolverInternal {
    id_to_type: Mutex<HashMap<String, Type>>,
    id_to_def: Mutex<HashMap<String, DefinitionId>>,
    class_names: Mutex<HashMap<String, Type>>,
}

impl ResolverInternal {
    fn add_id_def(&self, id: String, def: DefinitionId) {
        self.id_to_def.lock().insert(id, def);
    }
}

struct Resolver(Arc<ResolverInternal>);

impl SymbolResolver for Resolver {
    fn get_symbol_type(&self, _: &mut Unifier, _: &PrimitiveStore, str: &str) -> Option<Type> {
        self.0.id_to_type.lock().get(str).cloned()
    }

    fn get_symbol_value(&self, _: &str) -> Option<SymbolValue> {
        unimplemented!()
    }

    fn get_symbol_location(&self, _: &str) -> Option<Location> {
        unimplemented!()
    }

    fn get_identifier_def(&self, id: &str) -> Option<DefinitionId> {
        self.0.id_to_def.lock().get(id).cloned()
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
    ];
    "register"
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
    ];
    "function compose"
)]
fn test_simple_function_analyze(source: Vec<&str>, tys: Vec<&str>, names: Vec<&str>) {
    let mut composer = TopLevelComposer::new();

    let internal_resolver = Arc::new(ResolverInternal {
        id_to_def: Default::default(),
        id_to_type: Default::default(),
        class_names: Default::default(),
    });
    let resolver = Arc::new(
        Box::new(Resolver(internal_resolver.clone())) as Box<dyn SymbolResolver + Send + Sync>
    );

    for s in source {
        let ast = parse_program(s).unwrap();
        let ast = ast[0].clone();

        let (id, def_id) = composer.register_top_level(ast, Some(resolver.clone())).unwrap();
        internal_resolver.add_id_def(id, def_id);
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
                def __init__():
                    self.a: int32 = 3
                def fun(b: B):
                    pass
                def foo(a: T, b: V):
                    pass
        "},
        indoc! {"
            class B(C):
                def __init__():
                    pass
        "},
        indoc! {"
            class C(A):
                def __init__():
                    pass
                def fun(b: B):
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
        fields: [(\"a\", \"class0\")],
        methods: [(\"__init__\", \"fn[[], class4]\", DefinitionId(6)), (\"fun\", \"fn[[b=class10], class4]\", DefinitionId(7)), (\"foo\", \"fn[[a=tvar2, b=tvar3], class4]\", DefinitionId(8))],
        type_vars: []
        }"},

        indoc! {"6: Function {
        name: \"A__init__\",
        sig: \"fn[[], class4]\",
        var_id: []
        }"},

        indoc! {"7: Function {
        name: \"Afun\",
        sig: \"fn[[b=class10], class4]\",
        var_id: []
        }"},

        indoc! {"8: Function {
        name: \"Afoo\",
        sig: \"fn[[a=tvar2, b=tvar3], class4]\",
        var_id: [2, 3]
        }"},

        indoc! {"9: Initializer { DefinitionId(5) }"},

        indoc! {"10: Class {
        name: \"B\",
        def_id: DefinitionId(10),
        ancestors: [CustomClassKind { id: DefinitionId(10), params: [] }, CustomClassKind { id: DefinitionId(13), params: [] }, CustomClassKind { id: DefinitionId(5), params: [] }],
        fields: [(\"a\", \"class0\")],
        methods: [(\"__init__\", \"fn[[], class4]\", DefinitionId(11)), (\"fun\", \"fn[[b=class10], class4]\", DefinitionId(15)), (\"foo\", \"fn[[a=tvar2, b=tvar3], class4]\", DefinitionId(8))],
        type_vars: []
        }"},

        indoc! {"11: Function {
        name: \"B__init__\",
        sig: \"fn[[], class4]\",
        var_id: []
        }"},

        indoc! {"12: Initializer { DefinitionId(10) }"},

        indoc! {"13: Class {
        name: \"C\",
        def_id: DefinitionId(13),
        ancestors: [CustomClassKind { id: DefinitionId(13), params: [] }, CustomClassKind { id: DefinitionId(5), params: [] }],
        fields: [(\"a\", \"class0\")],
        methods: [(\"__init__\", \"fn[[], class4]\", DefinitionId(14)), (\"fun\", \"fn[[b=class10], class4]\", DefinitionId(15)), (\"foo\", \"fn[[a=tvar2, b=tvar3], class4]\", DefinitionId(8))],
        type_vars: []
        }"},

        indoc! {"14: Function {
        name: \"C__init__\",
        sig: \"fn[[], class4]\",
        var_id: []
        }"},

        indoc! {"15: Function {
        name: \"Cfun\",
        sig: \"fn[[b=class10], class4]\",
        var_id: []
        }"},

        indoc! {"16: Initializer { DefinitionId(13) }"},

        indoc! {"17: Function {
        name: \"foo\",
        sig: \"fn[[a=class5], class4]\",
        var_id: []
        }"},

        indoc! {"18: Function {
        name: \"ff\",
        sig: \"fn[[a=tvar2], tvar3]\",
        var_id: [2, 3]
        }"},
    ];
    "simple class compose"
)]
#[test_case(
    vec![
        indoc! {"
            class Generic_A(Generic[V], B):
                def __init__():
                    self.a: int64 = 123123123123
                def fun(a: int32) -> V:
                    pass
        "},
        indoc! {"
            class B:
                def __init__():
                    self.aa: bool = False
                def foo(b: T):
                    pass
        "}
    ],
    vec![
        indoc! {"5: Class {
        name: \"Generic_A\",
        def_id: DefinitionId(5),
        ancestors: [CustomClassKind { id: DefinitionId(5), params: [TypeVarKind(UnificationKey(101))] }, CustomClassKind { id: DefinitionId(9), params: [] }],
        fields: [(\"aa\", \"class3\"), (\"a\", \"class1\")],
        methods: [(\"__init__\", \"fn[[], class4]\", DefinitionId(6)), (\"foo\", \"fn[[b=tvar2], class4]\", DefinitionId(11)), (\"fun\", \"fn[[a=class0], tvar3]\", DefinitionId(7))],
        type_vars: [UnificationKey(101)]
        }"},

        indoc! {"6: Function {
        name: \"Generic_A__init__\",
        sig: \"fn[[], class4]\",
        var_id: [3]
        }"},

        indoc! {"7: Function {
        name: \"Generic_Afun\",
        sig: \"fn[[a=class0], tvar3]\",
        var_id: [3]
        }"},

        indoc! {"8: Initializer { DefinitionId(5) }"},

        indoc! {"9: Class {
        name: \"B\",
        def_id: DefinitionId(9),
        ancestors: [CustomClassKind { id: DefinitionId(9), params: [] }],
        fields: [(\"aa\", \"class3\")],
        methods: [(\"__init__\", \"fn[[], class4]\", DefinitionId(10)), (\"foo\", \"fn[[b=tvar2], class4]\", DefinitionId(11))],
        type_vars: []
        }"},

        indoc! {"10: Function {
        name: \"B__init__\",
        sig: \"fn[[], class4]\",
        var_id: []
        }"},

        indoc! {"11: Function {
        name: \"Bfoo\",
        sig: \"fn[[b=tvar2], class4]\",
        var_id: [2]
        }"},

        indoc! {"12: Initializer { DefinitionId(9) }"},
    ];
    "generic class"
)]
fn test_simple_class_analyze(source: Vec<&str>, res: Vec<&str>) {
    let mut composer = TopLevelComposer::new();

    let tvar_t = composer.unifier.get_fresh_var();
    let tvar_v = composer
        .unifier
        .get_fresh_var_with_range(&[composer.primitives_ty.bool, composer.primitives_ty.int32]);
    
    println!("t: {}, {:?}", tvar_t.1, tvar_t.0);
    println!("v: {}, {:?}\n", tvar_v.1, tvar_v.0);

    let internal_resolver = Arc::new(ResolverInternal {
        id_to_def: Default::default(),
        id_to_type: Mutex::new(
            vec![("T".to_string(), tvar_t.0), ("V".to_string(), tvar_v.0)].into_iter().collect(),
        ),
        class_names: Default::default(),
    });
    let resolver = Arc::new(
        Box::new(Resolver(internal_resolver.clone())) as Box<dyn SymbolResolver + Send + Sync>
    );

    for s in source {
        let ast = parse_program(s).unwrap();
        let ast = ast[0].clone();

        let (id, def_id) = composer.register_top_level(ast, Some(resolver.clone())).unwrap();
        internal_resolver.add_id_def(id, def_id);
    }

    composer.start_analysis().unwrap();

    // skip 5 to skip primitives
    for (i, (def, _)) in composer.definition_ast_list.iter().skip(5).enumerate() {
        let def = &*def.read();
        // println!(
        //     "{}: {}\n",
        //     i + 5,
        //     def.to_string(
        //         composer.unifier.borrow_mut(),
        //         &mut |id| format!("class{}", id),
        //         &mut |id| format!("tvar{}", id)
        //     )
        // );
        assert_eq!(
            format!(
                "{}: {}",
                i + 5,
                def.to_string(
                    composer.unifier.borrow_mut(),
                    &mut |id| format!("class{}", id.to_string()),
                    &mut |id| format!("tvar{}", id.to_string()),
                )
            ),
            res[i]
        )
    }
}
