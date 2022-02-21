use crate::{
    codegen::CodeGenContext,
    symbol_resolver::{SymbolResolver, ValueEnum},
    toplevel::DefinitionId,
    typecheck::{
        type_inferencer::PrimitiveStore,
        typedef::{Type, Unifier},
    },
};
use indoc::indoc;
use nac3parser::{ast::fold::Fold, parser::parse_program};
use parking_lot::Mutex;
use std::{collections::HashMap, sync::Arc};
use test_case::test_case;

use super::*;

struct ResolverInternal {
    id_to_type: Mutex<HashMap<StrRef, Type>>,
    id_to_def: Mutex<HashMap<StrRef, DefinitionId>>,
    class_names: Mutex<HashMap<StrRef, Type>>,
}

impl ResolverInternal {
    fn add_id_def(&self, id: StrRef, def: DefinitionId) {
        self.id_to_def.lock().insert(id, def);
    }

    fn add_id_type(&self, id: StrRef, ty: Type) {
        self.id_to_type.lock().insert(id, ty);
    }
}

struct Resolver(Arc<ResolverInternal>);

impl SymbolResolver for Resolver {
    fn get_default_param_value(&self, _: &nac3parser::ast::Expr) -> Option<crate::symbol_resolver::SymbolValue> {
        unimplemented!()
    }

    fn get_symbol_type(
        &self,
        _: &mut Unifier,
        _: &[Arc<RwLock<TopLevelDef>>],
        _: &PrimitiveStore,
        str: StrRef,
    ) -> Result<Type, String> {
        self.0
            .id_to_type
            .lock()
            .get(&str)
            .cloned()
            .ok_or_else(|| format!("cannot find symbol `{}`", str))
    }

    fn get_symbol_value<'ctx, 'a>(
        &self,
        _: StrRef,
        _: &mut CodeGenContext<'ctx, 'a>,
    ) -> Option<ValueEnum<'ctx>> {
        unimplemented!()
    }

    fn get_identifier_def(&self, id: StrRef) -> Result<DefinitionId, String> {
        self.0.id_to_def.lock().get(&id).cloned().ok_or("Unknown identifier".to_string())
    }

    fn get_string_id(&self, _: &str) -> i32 {
        unimplemented!()
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
    let mut composer: TopLevelComposer = Default::default();

    for s in source {
        let ast = parse_program(s, Default::default()).unwrap();
        let ast = ast[0].clone();

        composer.register_top_level(ast, None, "".into()).unwrap();
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
        "fn[[a:0], 0]",
        "fn[[a:2], 4]",
        "fn[[b:1], 0]",
    ],
    vec![
        "fun",
        "foo",
        "f"
    ];
    "function compose"
)]
fn test_simple_function_analyze(source: Vec<&str>, tys: Vec<&str>, names: Vec<&str>) {
    let mut composer: TopLevelComposer = Default::default();

    let internal_resolver = Arc::new(ResolverInternal {
        id_to_def: Default::default(),
        id_to_type: Default::default(),
        class_names: Default::default(),
    });
    let resolver =
        Arc::new(Resolver(internal_resolver.clone())) as Arc<dyn SymbolResolver + Send + Sync>;

    for s in source {
        let ast = parse_program(s, Default::default()).unwrap();
        let ast = ast[0].clone();

        let (id, def_id, ty) =
            composer.register_top_level(ast, Some(resolver.clone()), "".into()).unwrap();
        internal_resolver.add_id_def(id, def_id);
        if let Some(ty) = ty {
            internal_resolver.add_id_type(id, ty);
        }
    }

    composer.start_analysis(true).unwrap();

    for (i, (def, _)) in composer.definition_ast_list.iter().skip(composer.builtin_num).enumerate()
    {
        let def = &*def.read();
        if let TopLevelDef::Function { signature, name, .. } = def {
            let ty_str =
                composer
                    .unifier
                    .internal_stringify(*signature, &mut |id| id.to_string(), &mut |id| id.to_string(), &mut None);
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
    vec![];
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
    vec![];
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
    vec![];
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
    vec![];
    "self1"
)]
#[test_case(
    vec![
        indoc! {"
            class A(Generic[T]):
                a: int32
                b: T
                c: A[int64]
                def __init__(self, t: T):
                    self.a = 3
                    self.b = T
                def fun(self, a: int32, b: T) -> list[virtual[B[bool]]]:
                    pass
                def foo(self, c: C):
                    pass
        "},
        indoc! {"
            class B(Generic[V], A[float]):
                d: C
                def __init__(self):
                    pass
                def fun(self, a: int32, b: T) -> list[virtual[B[bool]]]:
                    # override
                    pass
        "},
        indoc! {"
            class C(B[bool]):
                e: int64
                def __init__(self):
                    pass
        "}
    ],
    vec![];
    "inheritance_override"
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
    vec!["application of type vars to generic class is not currently supported (at unknown: line 4 column 24)"];
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
    vec![
        indoc! {"
            class A:
                pass
        "}
    ],
    vec!["5: Class {\nname: \"A\",\ndef_id: DefinitionId(5),\nancestors: [CustomClassKind { id: DefinitionId(5), params: [] }],\nfields: [],\nmethods: [],\ntype_vars: []\n}"];
    "simple pass in class"
)]
#[test_case(
    vec![indoc! {"
        class A:
            def __init__():
                pass
    "}],
    vec!["__init__ method must have a `self` parameter (at unknown: line 2 column 5)"];
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
    vec!["a class definition can only have at most one base class declaration and one generic declaration (at unknown: line 1 column 24)"];
    "err multiple inheritance"
)]
#[test_case(
    vec![
        indoc! {"
            class A(Generic[T]):
                a: int32
                b: T
                c: A[int64]
                def __init__(self, t: T):
                    self.a = 3
                    self.b = T
                def fun(self, a: int32, b: T) -> list[virtual[B[bool]]]:
                    pass
        "},
        indoc! {"
            class B(Generic[V], A[float]):
                def __init__(self):
                    pass
                def fun(self, a: int32, b: T) -> list[virtual[B[int32]]]:
                    # override
                    pass
        "}
    ],
    vec!["method fun has same name as ancestors' method, but incompatible type"];
    "err_incompatible_inheritance_method"
)]
#[test_case(
    vec![
        indoc! {"
            class A(Generic[T]):
                a: int32
                b: T
                c: A[int64]
                def __init__(self, t: T):
                    self.a = 3
                    self.b = T
                def fun(self, a: int32, b: T) -> list[virtual[B[bool]]]:
                    pass
        "},
        indoc! {"
            class B(Generic[V], A[float]):
                a: int32
                def __init__(self):
                    pass
                def fun(self, a: int32, b: T) -> list[virtual[B[bool]]]:
                    # override
                    pass
        "}
    ],
    vec!["field `a` has already declared in the ancestor classes"];
    "err_incompatible_inheritance_field"
)]
#[test_case(
    vec![
        indoc! {"
            class A:
                def __init__(self):
                    pass
        "},
        indoc! {"
            class A:
                a: int32
                def __init__(self):
                    pass
        "}
    ],
    vec!["duplicate definition of class `A` (at unknown: line 1 column 1)"];
    "class same name"
)]
fn test_analyze(source: Vec<&str>, res: Vec<&str>) {
    let print = false;
    let mut composer: TopLevelComposer = Default::default();

    let internal_resolver = make_internal_resolver_with_tvar(
        vec![
            ("T".into(), vec![]),
            ("V".into(), vec![composer.primitives_ty.bool, composer.primitives_ty.int32]),
            ("G".into(), vec![composer.primitives_ty.bool, composer.primitives_ty.int64]),
        ],
        &mut composer.unifier,
        print,
    );
    let resolver =
        Arc::new(Resolver(internal_resolver.clone())) as Arc<dyn SymbolResolver + Send + Sync>;

    for s in source {
        let ast = parse_program(s, Default::default()).unwrap();
        let ast = ast[0].clone();

        let (id, def_id, ty) = {
            match composer.register_top_level(ast, Some(resolver.clone()), "".into()) {
                Ok(x) => x,
                Err(msg) => {
                    if print {
                        println!("{}", msg);
                    } else {
                        assert_eq!(res[0], msg);
                    }
                    return;
                }
            }
        };
        internal_resolver.add_id_def(id, def_id);
        if let Some(ty) = ty {
            internal_resolver.add_id_type(id, ty);
        }
    }

    if let Err(msg) = composer.start_analysis(false) {
        if print {
            println!("{}", msg);
        } else {
            assert_eq!(res[0], msg);
        }
    } else {
        // skip 5 to skip primitives
        let mut res_vec: Vec<String> = Vec::new();
        for (def, _) in composer.definition_ast_list.iter().skip(composer.builtin_num) {
            let def = &*def.read();
            res_vec.push(format!("{}\n", def.to_string(composer.unifier.borrow_mut())));
        }
        insta::assert_debug_snapshot!(res_vec);
    }
}

#[test_case(
    vec![
        indoc! {"
            def fun(a: int32, b: int32) -> int32:
                return a + b
        "},
        indoc! {"
            def fib(n: int32) -> int32:
                if n <= 2:
                    return 1
                a = fib(n - 1)
                b = fib(n - 2)
                return fib(n - 1)
        "}
    ],
    vec![];
    "simple function"
)]
#[test_case(
    vec![
        indoc! {"
            class A:
                a: int32
                def __init__(self):
                    self.a = 3
                def fun(self) -> int32:
                    b = self.a + 3
                    return b * self.a
                def clone(self) -> A:
                    SELF = self
                    return SELF
                def sum(self) -> int32:
                    if self.a == 0:
                        return self.a
                    else:
                        a = self.a
                        self.a = self.a - 1
                        return a + self.sum()
                def fib(self, a: int32) -> int32:
                    if a <= 2:
                        return 1
                    return self.fib(a - 1) + self.fib(a - 2)
        "},
        indoc! {"
            def fun(a: A) -> int32:
                return a.fun() + 2
        "}
    ],
    vec![];
    "simple class body"
)]
#[test_case(
    vec![
        indoc! {"
            def fun(a: V, c: G, t: T) -> V:
                b = a
                cc = c
                ret = fun(b, cc, t)
                return ret * ret
        "},
        indoc! {"
            def sum_three(l: list[V]) -> V:
                return l[0] + l[1] + l[2]
        "},
        indoc! {"
            def sum_sq_pair(p: tuple[V, V]) -> list[V]:
                a = p[0]
                b = p[1]
                a = a**a
                b = b**b
                return [a, b]
        "}
    ],
    vec![];
    "type var fun"
)]
#[test_case(
    vec![
        indoc! {"
            class A(Generic[G]):
                a: G
                b: bool
                def __init__(self, aa: G):
                    self.a = aa
                    if 2 > 1:
                        self.b = True
                    else:
                        # self.b = False
                        pass
                def fun(self, a: G) -> list[G]:
                    ret = [a, self.a]
                    return ret if self.b else self.fun(self.a)
        "}
    ],
    vec![];
    "type var class"
)]
#[test_case(
    vec![
        indoc! {"
            class A:
                def fun(self):
                    pass
        "},
        indoc!{"
            class B:
                a: int32
                b: bool
                def __init__(self):
                    # self.b = False
                    if 3 > 2:
                        self.a = 3
                        self.b = False
                    else:
                        self.a = 4
                        self.b = True
        "}
    ],
    vec![];
    "no_init_inst_check"
)]
fn test_inference(source: Vec<&str>, res: Vec<&str>) {
    let print = true;
    let mut composer: TopLevelComposer = Default::default();

    let internal_resolver = make_internal_resolver_with_tvar(
        vec![
            ("T".into(), vec![]),
            (
                "V".into(),
                vec![
                    composer.primitives_ty.float,
                    composer.primitives_ty.int32,
                    composer.primitives_ty.int64,
                ],
            ),
            ("G".into(), vec![composer.primitives_ty.bool, composer.primitives_ty.int64]),
        ],
        &mut composer.unifier,
        print,
    );
    let resolver =
        Arc::new(Resolver(internal_resolver.clone())) as Arc<dyn SymbolResolver + Send + Sync>;

    for s in source {
        let ast = parse_program(s, Default::default()).unwrap();
        let ast = ast[0].clone();

        let (id, def_id, ty) = {
            match composer.register_top_level(ast, Some(resolver.clone()), "".into()) {
                Ok(x) => x,
                Err(msg) => {
                    if print {
                        println!("{}", msg);
                    } else {
                        assert_eq!(res[0], msg);
                    }
                    return;
                }
            }
        };
        internal_resolver.add_id_def(id, def_id);
        if let Some(ty) = ty {
            internal_resolver.add_id_type(id, ty);
        }
    }

    if let Err(msg) = composer.start_analysis(true) {
        if print {
            println!("{}", msg);
        } else {
            assert_eq!(res[0], msg);
        }
    } else {
        // skip 5 to skip primitives
        let mut stringify_folder = TypeToStringFolder { unifier: &mut composer.unifier };
        for (_i, (def, _)) in
            composer.definition_ast_list.iter().skip(composer.builtin_num).enumerate()
        {
            let def = &*def.read();

            if let TopLevelDef::Function { instance_to_stmt, name, .. } = def {
                println!(
                    "=========`{}`: number of instances: {}===========",
                    name,
                    instance_to_stmt.len()
                );
                for inst in instance_to_stmt.iter() {
                    let ast = &inst.1.body;
                    for b in ast.iter() {
                        println!("{:?}", stringify_folder.fold_stmt(b.clone()).unwrap());
                        println!("--------------------");
                    }
                    println!("\n");
                }
            }
        }
    }
}

fn make_internal_resolver_with_tvar(
    tvars: Vec<(StrRef, Vec<Type>)>,
    unifier: &mut Unifier,
    print: bool,
) -> Arc<ResolverInternal> {
    let res: Arc<ResolverInternal> = ResolverInternal {
        id_to_def: Default::default(),
        id_to_type: tvars
            .into_iter()
            .map(|(name, range)| {
                (name, {
                    let (ty, id) = unifier.get_fresh_var_with_range(range.as_slice(), None, None);
                    if print {
                        println!("{}: {:?}, tvar{}", name, ty, id);
                    }
                    ty
                })
            })
            .collect::<HashMap<_, _>>()
            .into(),
        class_names: Default::default(),
    }
    .into();
    if print {
        println!();
    }
    res
}

struct TypeToStringFolder<'a> {
    unifier: &'a mut Unifier,
}

impl<'a> Fold<Option<Type>> for TypeToStringFolder<'a> {
    type TargetU = String;
    type Error = String;
    fn map_user(&mut self, user: Option<Type>) -> Result<Self::TargetU, Self::Error> {
        Ok(if let Some(ty) = user {
            self.unifier.internal_stringify(ty, &mut |id| format!("class{}", id.to_string()), &mut |id| {
                format!("tvar{}", id.to_string())
            }, &mut None)
        } else {
            "None".into()
        })
    }
}
