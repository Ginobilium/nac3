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
                a: int32
                def __init__(self):
                    self.a = 3
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
                a: int64
                def __init__(self):
                    self.a = 123123123123
                def fun(self, a: int32) -> V:
                    pass
        "},
        indoc! {"
            class B:
                aa: bool
                def __init__(self):
                    self.aa = False
                def foo(self, b: T):
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
#[test_case(
    vec![
        indoc! {"
            def foo(a: list[int32], b: tuple[T, float]) -> A[B, bool]:
                pass
        "},
        indoc! {"
            class A(Generic[T, V]):
                a: T
                b: V
                def __init__(self, v: V):
                    self.a = 1
                    self.b = v
                def fun(self, a: T) -> V:
                    pass
        "},
        indoc! {"
            def gfun(a: A[list[float], int32]):
                pass
        "},
        indoc! {"
            class B:
                def __init__(self):
                    pass
        "}
    ],
    vec![
        indoc! {"5: Function {
        name: \"foo\",
        sig: \"fn[[a=list[class0], b=tuple[tvar2, class2]], class6[2->class11, 3->class3]]\",
        var_id: [2]
        }"},

        indoc! {"6: Class {
        name: \"A\",
        def_id: DefinitionId(6),
        ancestors: [CustomClassKind { id: DefinitionId(6), params: [TypeVarKind(UnificationKey(100)), TypeVarKind(UnificationKey(101))] }],
        fields: [(\"a\", \"tvar2\"), (\"b\", \"tvar3\")],
        methods: [(\"__init__\", \"fn[[v=tvar3], class4]\", DefinitionId(7)), (\"fun\", \"fn[[a=tvar2], tvar3]\", DefinitionId(8))],
        type_vars: [UnificationKey(100), UnificationKey(101)]
        }"},

        indoc! {"7: Function {
        name: \"A__init__\",
        sig: \"fn[[v=tvar3], class4]\",
        var_id: [2, 3]
        }"},

        indoc! {"8: Function {
        name: \"Afun\",
        sig: \"fn[[a=tvar2], tvar3]\",
        var_id: [2, 3]
        }"},

        indoc! {"9: Initializer { DefinitionId(6) }"},

        indoc! {"10: Function {
        name: \"gfun\",
        sig: \"fn[[a=class6[2->list[class2], 3->class0]], class4]\",
        var_id: []
        }"},

        indoc! {"11: Class {
        name: \"B\",
        def_id: DefinitionId(11),
        ancestors: [CustomClassKind { id: DefinitionId(11), params: [] }],
        fields: [],
        methods: [(\"__init__\", \"fn[[], class4]\", DefinitionId(12))],
        type_vars: []
        }"},

        indoc! {"12: Function {
        name: \"B__init__\",
        sig: \"fn[[], class4]\",
        var_id: []
        }"},

        indoc! {"13: Initializer { DefinitionId(11) }"},
    ];
    "list tuple generic"
)]
#[test_case(
    vec![
        indoc! {"
            class A(Generic[T, V]):
                a: A[float, bool]
                b: B
                def __init__(self, a: A[float, bool], b: B):
                    self.a = a
                    self.b = b
                def fun(self, a: A[float, bool]) -> A[bool, int32]:
                    pass
        "},
        indoc! {"
            class B(A[int64, bool]):
                def __init__(self):
                    pass
                def foo(self, b: B) -> B:
                    pass
                def bar(self, a: A[list[B], int32]) -> tuple[A[virtual[A[B, int32]], bool], B]:
                    pass
        "}
    ],
    vec![
        indoc! {"5: Class {
        name: \"A\",
        def_id: DefinitionId(5),
        ancestors: [CustomClassKind { id: DefinitionId(5), params: [TypeVarKind(UnificationKey(100)), TypeVarKind(UnificationKey(101))] }],
        fields: [(\"a\", \"class5[2->class2, 3->class3]\"), (\"b\", \"class9\")],
        methods: [(\"__init__\", \"fn[[a=class5[2->class2, 3->class3], b=class9], class4]\", DefinitionId(6)), (\"fun\", \"fn[[a=class5[2->class2, 3->class3]], class5[2->class3, 3->class0]]\", DefinitionId(7))],
        type_vars: [UnificationKey(100), UnificationKey(101)]
        }"},
        
        indoc! {"6: Function {
        name: \"A__init__\",
        sig: \"fn[[a=class5[2->class2, 3->class3], b=class9], class4]\",
        var_id: [2, 3]
        }"},
        
        indoc! {"7: Function {
        name: \"Afun\",
        sig: \"fn[[a=class5[2->class2, 3->class3]], class5[2->class3, 3->class0]]\",
        var_id: [2, 3]
        }"},
        
        indoc! {"8: Initializer { DefinitionId(5) }"},
        
        indoc! {"9: Class {
        name: \"B\",
        def_id: DefinitionId(9),
        ancestors: [CustomClassKind { id: DefinitionId(9), params: [] }, CustomClassKind { id: DefinitionId(5), params: [PrimitiveKind(UnificationKey(1)), PrimitiveKind(UnificationKey(3))] }],
        fields: [(\"a\", \"class5[2->class2, 3->class3]\"), (\"b\", \"class9\")],
        methods: [(\"__init__\", \"fn[[], class4]\", DefinitionId(10)), (\"fun\", \"fn[[a=class5[2->class2, 3->class3]], class5[2->class3, 3->class0]]\", DefinitionId(7)), (\"foo\", \"fn[[b=class9], class9]\", DefinitionId(11)), (\"bar\", \"fn[[a=class5[2->list[class9], 3->class0]], tuple[class5[2->virtual[class5[2->class9, 3->class0]], 3->class3], class9]]\", DefinitionId(12))],
        type_vars: []
        }"},
        
        indoc! {"10: Function {
        name: \"B__init__\",
        sig: \"fn[[], class4]\",
        var_id: []
        }"},
        
        indoc! {"11: Function {
        name: \"Bfoo\",
        sig: \"fn[[b=class9], class9]\",
        var_id: []
        }"},
        
        indoc! {"12: Function {
        name: \"Bbar\",
        sig: \"fn[[a=class5[2->list[class9], 3->class0]], tuple[class5[2->virtual[class5[2->class9, 3->class0]], 3->class3], class9]]\",
        var_id: []
        }"},
        
        indoc! {"13: Initializer { DefinitionId(9) }"},
    ];
    "self1"
)]
#[test_case(
    vec![
        indoc! {"
            class A(Generic[T]):
                def __init__(self):
                    pass
                def fun(self, a: A[T]) -> A[T]:
                    pass
        "}
    ],
    vec!["application of type vars to generic class is not currently supported"];
    "err no type var in generic app"
)]
#[test_case(
    vec![
        indoc! {"
            class A(B):
                def __init__(self):
                    pass
        "},
        indoc! {"
            class B(A):
                def __init__(self):
                    pass
        "}
    ],
    vec!["cyclic inheritance detected"];
    "cyclic1"
)]
#[test_case(
    vec![
        indoc! {"
            class A(B[bool, int64]):
                def __init__(self):
                    pass
        "},
        indoc! {"
            class B(Generic[V, T], C[int32]):
                def __init__(self):
                    pass
        "},
        indoc! {"
            class C(Generic[T], A):
                def __init__(self):
                    pass
        "},
    ],
    vec!["cyclic inheritance detected"];
    "cyclic2"
)]
#[test_case(
    vec![indoc! {"
        class A:
            pass
    "}],
    vec!["class def must have __init__ method defined"];
    "err no __init__"
)]
#[test_case(
    vec![indoc! {"
        class A:
            def __init__():
                pass
    "}],
    vec!["__init__ function must have a `self` parameter"];
    "err no self_1"
)]
#[test_case(
    vec![
        indoc! {"
            class A(B, Generic[T], C):
                def __init__(self):
                    pass
        "},
        indoc! {"
            class B:
                def __init__(self):
                    pass
        "},
        indoc! {"
            class C:
                def __init__(self):
                    pass
        "}

    ],
    vec!["a class def can only have at most one base class declaration and one generic declaration"];
    "err multiple inheritance"
)]
fn test_analyze(source: Vec<&str>, res: Vec<&str>) {
    let print = false;
    let mut composer = TopLevelComposer::new();

    let tvar_t = composer.unifier.get_fresh_var();
    let tvar_v = composer
        .unifier
        .get_fresh_var_with_range(&[composer.primitives_ty.bool, composer.primitives_ty.int32]);

    if print {
        println!("t: {}, {:?}", tvar_t.1, tvar_t.0);
        println!("v: {}, {:?}\n", tvar_v.1, tvar_v.0);
    }

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

        let (id, def_id) = {
            match composer.register_top_level(ast, Some(resolver.clone())) {
                Ok(x) => x,
                Err(msg) => {
                    if print {
                        println!("{}", msg);
                    } else {
                        assert_eq!(res[0], msg);
                    }
                    return
                }
            }
        };
        internal_resolver.add_id_def(id, def_id);
    }

    if let Err(msg) = composer.start_analysis() {
        if print {
            println!("{}", msg);
        } else {
            assert_eq!(res[0], msg);
        }
    } else {
        // skip 5 to skip primitives
        for (i, (def, _)) in composer.definition_ast_list.iter().skip(5).enumerate() {
            let def = &*def.read();
            
            if print {
                println!(
                    "{}: {}\n",
                    i + 5,
                    def.to_string(
                        composer.unifier.borrow_mut(),
                        &mut |id| format!("class{}", id),
                        &mut |id| format!("tvar{}", id)
                    )
                );
            } else {
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
    }
}
