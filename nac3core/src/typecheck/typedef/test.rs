use super::super::magic_methods::with_fields;
use super::*;
use indoc::indoc;
use itertools::Itertools;
use std::collections::HashMap;
use test_case::test_case;

impl Unifier {
    /// Check whether two types are equal.
    fn eq(&mut self, a: Type, b: Type) -> bool {
        if a == b {
            return true;
        }
        let (ty_a, ty_b) = {
            let table = &mut self.unification_table;
            if table.unioned(a, b) {
                return true;
            }
            (table.probe_value(a).clone(), table.probe_value(b).clone())
        };

        match (&*ty_a, &*ty_b) {
            (
                TypeEnum::TVar { fields: None, id: id1, .. },
                TypeEnum::TVar { fields: None, id: id2, .. },
            ) => id1 == id2,
            (
                TypeEnum::TVar { fields: Some(map1), .. },
                TypeEnum::TVar { fields: Some(map2), .. },
            ) => self.map_eq2(map1, map2),
            (TypeEnum::TTuple { ty: ty1 }, TypeEnum::TTuple { ty: ty2 }) => {
                ty1.len() == ty2.len()
                    && ty1.iter().zip(ty2.iter()).all(|(t1, t2)| self.eq(*t1, *t2))
            }
            (TypeEnum::TList { ty: ty1 }, TypeEnum::TList { ty: ty2 })
            | (TypeEnum::TVirtual { ty: ty1 }, TypeEnum::TVirtual { ty: ty2 }) => {
                self.eq(*ty1, *ty2)
            }
            (
                TypeEnum::TObj { obj_id: id1, params: params1, .. },
                TypeEnum::TObj { obj_id: id2, params: params2, .. },
            ) => id1 == id2 && self.map_eq(params1, params2),
            // TCall and TFunc are not yet implemented
            _ => false,
        }
    }

    fn map_eq<K>(&mut self, map1: &Mapping<K>, map2: &Mapping<K>) -> bool
    where
        K: std::hash::Hash + std::cmp::Eq + std::clone::Clone,
    {
        if map1.len() != map2.len() {
            return false;
        }
        for (k, v) in map1.iter() {
            if !map2.get(k).map(|v1| self.eq(*v, *v1)).unwrap_or(false) {
                return false;
            }
        }
        true
    }

    fn map_eq2<K>(&mut self, map1: &Mapping<K, RecordField>, map2: &Mapping<K, RecordField>) -> bool
    where
        K: std::hash::Hash + std::cmp::Eq + std::clone::Clone,
    {
        if map1.len() != map2.len() {
            return false;
        }
        for (k, v) in map1.iter() {
            if !map2.get(k).map(|v1| self.eq(v.ty, v1.ty)).unwrap_or(false) {
                return false;
            }
        }
        true
    }
}

struct TestEnvironment {
    pub unifier: Unifier,
    pub type_mapping: HashMap<String, Type>,
}

impl TestEnvironment {
    fn new() -> TestEnvironment {
        let mut unifier = Unifier::new();
        let mut type_mapping = HashMap::new();

        type_mapping.insert(
            "int".into(),
            unifier.add_ty(TypeEnum::TObj {
                obj_id: DefinitionId(0),
                fields: HashMap::new(),
                params: HashMap::new(),
            }),
        );
        type_mapping.insert(
            "float".into(),
            unifier.add_ty(TypeEnum::TObj {
                obj_id: DefinitionId(1),
                fields: HashMap::new(),
                params: HashMap::new(),
            }),
        );
        type_mapping.insert(
            "bool".into(),
            unifier.add_ty(TypeEnum::TObj {
                obj_id: DefinitionId(2),
                fields: HashMap::new(),
                params: HashMap::new(),
            }),
        );
        let (v0, id) = unifier.get_dummy_var();
        type_mapping.insert(
            "Foo".into(),
            unifier.add_ty(TypeEnum::TObj {
                obj_id: DefinitionId(3),
                fields: [("a".into(), (v0, true))].iter().cloned().collect::<HashMap<_, _>>(),
                params: [(id, v0)].iter().cloned().collect::<HashMap<_, _>>(),
            }),
        );

        TestEnvironment { unifier, type_mapping }
    }

    fn parse(&mut self, typ: &str, mapping: &Mapping<String>) -> Type {
        let result = self.internal_parse(typ, mapping);
        assert!(result.1.is_empty());
        result.0
    }

    fn internal_parse<'a, 'b>(
        &'a mut self,
        typ: &'b str,
        mapping: &Mapping<String>,
    ) -> (Type, &'b str) {
        // for testing only, so we can just panic when the input is malformed
        let end = typ.find(|c| ['[', ',', ']', '='].contains(&c)).unwrap_or_else(|| typ.len());
        match &typ[..end] {
            "tuple" => {
                let mut s = &typ[end..];
                assert!(&s[0..1] == "[");
                let mut ty = Vec::new();
                while &s[0..1] != "]" {
                    let result = self.internal_parse(&s[1..], mapping);
                    ty.push(result.0);
                    s = result.1;
                }
                (self.unifier.add_ty(TypeEnum::TTuple { ty }), &s[1..])
            }
            "list" => {
                assert!(&typ[end..end + 1] == "[");
                let (ty, s) = self.internal_parse(&typ[end + 1..], mapping);
                assert!(&s[0..1] == "]");
                (self.unifier.add_ty(TypeEnum::TList { ty }), &s[1..])
            }
            "Record" => {
                let mut s = &typ[end..];
                assert!(&s[0..1] == "[");
                let mut fields = HashMap::new();
                while &s[0..1] != "]" {
                    let eq = s.find('=').unwrap();
                    let key = s[1..eq].into();
                    let result = self.internal_parse(&s[eq + 1..], mapping);
                    fields.insert(key, RecordField::new(result.0, true, None));
                    s = result.1;
                }
                (self.unifier.add_record(fields), &s[1..])
            }
            x => {
                let mut s = &typ[end..];
                let ty = mapping.get(x).cloned().unwrap_or_else(|| {
                    // mapping should be type variables, type_mapping should be concrete types
                    // we should not resolve the type of type variables.
                    let mut ty = *self.type_mapping.get(x).unwrap();
                    let te = self.unifier.get_ty(ty);
                    if let TypeEnum::TObj { params, .. } = &*te.as_ref() {
                        if !params.is_empty() {
                            assert!(&s[0..1] == "[");
                            let mut p = Vec::new();
                            while &s[0..1] != "]" {
                                let result = self.internal_parse(&s[1..], mapping);
                                p.push(result.0);
                                s = result.1;
                            }
                            s = &s[1..];
                            ty = self
                                .unifier
                                .subst(ty, &params.keys().cloned().zip(p.into_iter()).collect())
                                .unwrap_or(ty);
                        }
                    }
                    ty
                });
                (ty, s)
            }
        }
    }

    fn unify(&mut self, typ1: Type, typ2: Type) -> Result<(), String> {
        self.unifier.unify(typ1, typ2).map_err(|e| e.to_display(&self.unifier).to_string())
    }
}

#[test_case(2,
    &[("v1", "v2"), ("v2", "float")],
    &[("v1", "float"), ("v2", "float")]
    ; "simple variable"
)]
#[test_case(2,
    &[("v1", "list[v2]"), ("v1", "list[float]")],
    &[("v1", "list[float]"), ("v2", "float")]
    ; "list element"
)]
#[test_case(3,
    &[
        ("v1", "Record[a=v3,b=v3]"),
        ("v2", "Record[b=float,c=v3]"),
        ("v1", "v2")
    ],
    &[
        ("v1", "Record[a=float,b=float,c=float]"),
        ("v2", "Record[a=float,b=float,c=float]"),
        ("v3", "float")
    ]
    ; "record merge"
)]
#[test_case(3,
    &[
        ("v1", "Record[a=float]"),
        ("v2", "Foo[v3]"),
        ("v1", "v2")
    ],
    &[
        ("v1", "Foo[float]"),
        ("v3", "float")
    ]
    ; "record obj merge"
)]
/// Test cases for valid unifications.
fn test_unify(
    variable_count: u32,
    unify_pairs: &[(&'static str, &'static str)],
    verify_pairs: &[(&'static str, &'static str)],
) {
    let unify_count = unify_pairs.len();
    // test all permutations...
    for perm in unify_pairs.iter().permutations(unify_count) {
        let mut env = TestEnvironment::new();
        let mut mapping = HashMap::new();
        for i in 1..=variable_count {
            let v = env.unifier.get_dummy_var();
            mapping.insert(format!("v{}", i), v.0);
        }
        // unification may have side effect when we do type resolution, so freeze the types
        // before doing unification.
        let mut pairs = Vec::new();
        for (a, b) in perm.iter() {
            let t1 = env.parse(a, &mapping);
            let t2 = env.parse(b, &mapping);
            pairs.push((t1, t2));
        }
        for (t1, t2) in pairs {
            env.unifier.unify(t1, t2).unwrap();
        }
        for (a, b) in verify_pairs.iter() {
            println!("{} = {}", a, b);
            let t1 = env.parse(a, &mapping);
            let t2 = env.parse(b, &mapping);
            println!("a = {}, b = {}", env.unifier.stringify(t1), env.unifier.stringify(t2));
            assert!(env.unifier.eq(t1, t2));
        }
    }
}

#[test_case(2,
    &[
        ("v1", "tuple[int]"),
        ("v2", "list[int]"),
    ],
    (("v1", "v2"), "Incompatible types: list[0] and tuple[0]")
    ; "type mismatch"
)]
#[test_case(2,
    &[
        ("v1", "tuple[int]"),
        ("v2", "tuple[float]"),
    ],
    (("v1", "v2"), "Incompatible types: tuple[0] and tuple[1]")
    ; "tuple parameter mismatch"
)]
#[test_case(2,
    &[
        ("v1", "tuple[int,int]"),
        ("v2", "tuple[int]"),
    ],
    (("v1", "v2"), "Tuple length mismatch: got tuple[0, 0] and tuple[0]")
    ; "tuple length mismatch"
)]
#[test_case(3,
    &[
        ("v1", "Record[a=float,b=int]"),
        ("v2", "Foo[v3]"),
    ],
    (("v1", "v2"), "`3[typevar4]::b` field/method does not exist")
    ; "record obj merge"
)]
/// Test cases for invalid unifications.
fn test_invalid_unification(
    variable_count: u32,
    unify_pairs: &[(&'static str, &'static str)],
    erroneous_pair: ((&'static str, &'static str), &'static str),
) {
    let mut env = TestEnvironment::new();
    let mut mapping = HashMap::new();
    for i in 1..=variable_count {
        let v = env.unifier.get_dummy_var();
        mapping.insert(format!("v{}", i), v.0);
    }
    // unification may have side effect when we do type resolution, so freeze the types
    // before doing unification.
    let mut pairs = Vec::new();
    for (a, b) in unify_pairs.iter() {
        let t1 = env.parse(a, &mapping);
        let t2 = env.parse(b, &mapping);
        pairs.push((t1, t2));
    }
    let (t1, t2) =
        (env.parse(erroneous_pair.0 .0, &mapping), env.parse(erroneous_pair.0 .1, &mapping));
    for (a, b) in pairs {
        env.unifier.unify(a, b).unwrap();
    }
    assert_eq!(env.unify(t1, t2), Err(erroneous_pair.1.to_string()));
}

#[test]
fn test_recursive_subst() {
    let mut env = TestEnvironment::new();
    let int = *env.type_mapping.get("int").unwrap();
    let foo_id = *env.type_mapping.get("Foo").unwrap();
    let foo_ty = env.unifier.get_ty(foo_id);
    let mapping: HashMap<_, _>;
    with_fields(&mut env.unifier, foo_id, |_unifier, fields| {
        fields.insert("rec".into(), (foo_id, true));
    });
    if let TypeEnum::TObj { params, .. } = &*foo_ty {
        mapping = params.iter().map(|(id, _)| (*id, int)).collect();
    } else {
        unreachable!()
    }
    let instantiated = env.unifier.subst(foo_id, &mapping).unwrap();
    let instantiated_ty = env.unifier.get_ty(instantiated);
    if let TypeEnum::TObj { fields, .. } = &*instantiated_ty {
        assert!(env.unifier.unioned(fields.get(&"a".into()).unwrap().0, int));
        assert!(env.unifier.unioned(fields.get(&"rec".into()).unwrap().0, instantiated));
    } else {
        unreachable!()
    }
}

#[test]
fn test_virtual() {
    let mut env = TestEnvironment::new();
    let int = env.parse("int", &HashMap::new());
    let fun = env.unifier.add_ty(TypeEnum::TFunc(FunSignature {
        args: vec![],
        ret: int,
        vars: HashMap::new(),
    }));
    let bar = env.unifier.add_ty(TypeEnum::TObj {
        obj_id: DefinitionId(5),
        fields: [("f".into(), (fun, false)), ("a".into(), (int, false))]
            .iter()
            .cloned()
            .collect::<HashMap<StrRef, _>>(),
        params: HashMap::new(),
    });
    let v0 = env.unifier.get_dummy_var().0;
    let v1 = env.unifier.get_dummy_var().0;

    let a = env.unifier.add_ty(TypeEnum::TVirtual { ty: bar });
    let b = env.unifier.add_ty(TypeEnum::TVirtual { ty: v0 });
    let c = env
        .unifier
        .add_record([("f".into(), RecordField::new(v1, false, None))].iter().cloned().collect());
    env.unifier.unify(a, b).unwrap();
    env.unifier.unify(b, c).unwrap();
    assert!(env.unifier.eq(v1, fun));

    let d = env
        .unifier
        .add_record([("a".into(), RecordField::new(v1, true, None))].iter().cloned().collect());
    assert_eq!(env.unify(b, d), Err("`virtual[5]::a` field/method does not exist".to_string()));

    let d = env
        .unifier
        .add_record([("b".into(), RecordField::new(v1, true, None))].iter().cloned().collect());
    assert_eq!(env.unify(b, d), Err("`virtual[5]::b` field/method does not exist".to_string()));
}

#[test]
fn test_typevar_range() {
    let mut env = TestEnvironment::new();
    let int = env.parse("int", &HashMap::new());
    let boolean = env.parse("bool", &HashMap::new());
    let float = env.parse("float", &HashMap::new());
    let int_list = env.parse("list[int]", &HashMap::new());
    let float_list = env.parse("list[float]", &HashMap::new());

    // unification between v and int
    // where v in (int, bool)
    let v = env.unifier.get_fresh_var_with_range(&[int, boolean], None, None).0;
    env.unifier.unify(int, v).unwrap();

    // unification between v and list[int]
    // where v in (int, bool)
    let v = env.unifier.get_fresh_var_with_range(&[int, boolean], None, None).0;
    assert_eq!(
        env.unify(int_list, v),
        Err("Expected any one of these types: 0, 2, but got list[0]".to_string())
    );

    // unification between v and float
    // where v in (int, bool)
    let v = env.unifier.get_fresh_var_with_range(&[int, boolean], None, None).0;
    assert_eq!(
        env.unify(float, v),
        Err("Expected any one of these types: 0, 2, but got 1".to_string())
    );

    let v1 = env.unifier.get_fresh_var_with_range(&[int, boolean], None, None).0;
    let v1_list = env.unifier.add_ty(TypeEnum::TList { ty: v1 });
    let v = env.unifier.get_fresh_var_with_range(&[int, v1_list], None, None).0;
    // unification between v and int
    // where v in (int, list[v1]), v1 in (int, bool)
    env.unifier.unify(int, v).unwrap();

    let v = env.unifier.get_fresh_var_with_range(&[int, v1_list], None, None).0;
    // unification between v and list[int]
    // where v in (int, list[v1]), v1 in (int, bool)
    env.unifier.unify(int_list, v).unwrap();

    let v = env.unifier.get_fresh_var_with_range(&[int, v1_list], None, None).0;
    // unification between v and list[float]
    // where v in (int, list[v1]), v1 in (int, bool)
    assert_eq!(
        env.unify(float_list, v),
        Err("Expected any one of these types: 0, list[typevar5], but got list[1]\n\nNotes:\n    typevar5 ??? {0, 2}".to_string())
    );

    let a = env.unifier.get_fresh_var_with_range(&[int, float], None, None).0;
    let b = env.unifier.get_fresh_var_with_range(&[boolean, float], None, None).0;
    env.unifier.unify(a, b).unwrap();
    env.unifier.unify(a, float).unwrap();

    let a = env.unifier.get_fresh_var_with_range(&[int, float], None, None).0;
    let b = env.unifier.get_fresh_var_with_range(&[boolean, float], None, None).0;
    env.unifier.unify(a, b).unwrap();
    assert_eq!(env.unify(a, int), Err("Expected any one of these types: 1, but got 0".into()));

    let a = env.unifier.get_fresh_var_with_range(&[int, float], None, None).0;
    let b = env.unifier.get_fresh_var_with_range(&[boolean, float], None, None).0;
    let a_list = env.unifier.add_ty(TypeEnum::TList { ty: a });
    let a_list = env.unifier.get_fresh_var_with_range(&[a_list], None, None).0;
    let b_list = env.unifier.add_ty(TypeEnum::TList { ty: b });
    let b_list = env.unifier.get_fresh_var_with_range(&[b_list], None, None).0;
    env.unifier.unify(a_list, b_list).unwrap();
    let float_list = env.unifier.add_ty(TypeEnum::TList { ty: float });
    env.unifier.unify(a_list, float_list).unwrap();
    // previous unifications should not affect a and b
    env.unifier.unify(a, int).unwrap();

    let a = env.unifier.get_fresh_var_with_range(&[int, float], None, None).0;
    let b = env.unifier.get_fresh_var_with_range(&[boolean, float], None, None).0;
    let a_list = env.unifier.add_ty(TypeEnum::TList { ty: a });
    let b_list = env.unifier.add_ty(TypeEnum::TList { ty: b });
    env.unifier.unify(a_list, b_list).unwrap();
    let int_list = env.unifier.add_ty(TypeEnum::TList { ty: int });
    assert_eq!(
        env.unify(a_list, int_list),
        Err("Incompatible types: list[typevar22] and list[0]\
            \n\nNotes:\n    typevar22 ??? {1}".into())
    );

    let a = env.unifier.get_fresh_var_with_range(&[int, float], None, None).0;
    let b = env.unifier.get_dummy_var().0;
    let a_list = env.unifier.add_ty(TypeEnum::TList { ty: a });
    let a_list = env.unifier.get_fresh_var_with_range(&[a_list], None, None).0;
    let b_list = env.unifier.add_ty(TypeEnum::TList { ty: b });
    env.unifier.unify(a_list, b_list).unwrap();
    assert_eq!(
        env.unify(b, boolean),
        Err("Expected any one of these types: 0, 1, but got 2".into())
    );
}

#[test]
fn test_rigid_var() {
    let mut env = TestEnvironment::new();
    let a = env.unifier.get_fresh_rigid_var(None, None).0;
    let b = env.unifier.get_fresh_rigid_var(None, None).0;
    let x = env.unifier.get_dummy_var().0;
    let list_a = env.unifier.add_ty(TypeEnum::TList { ty: a });
    let list_x = env.unifier.add_ty(TypeEnum::TList { ty: x });
    let int = env.parse("int", &HashMap::new());
    let list_int = env.parse("list[int]", &HashMap::new());

    assert_eq!(env.unify(a, b), Err("Incompatible types: typevar3 and typevar2".to_string()));
    env.unifier.unify(list_a, list_x).unwrap();
    assert_eq!(env.unify(list_x, list_int), Err("Incompatible types: list[typevar2] and list[0]".to_string()));

    env.unifier.replace_rigid_var(a, int);
    env.unifier.unify(list_x, list_int).unwrap();
}

#[test]
fn test_instantiation() {
    let mut env = TestEnvironment::new();
    let int = env.parse("int", &HashMap::new());
    let boolean = env.parse("bool", &HashMap::new());
    let float = env.parse("float", &HashMap::new());
    let list_int = env.parse("list[int]", &HashMap::new());

    let obj_map: HashMap<_, _> =
        [(0usize, "int"), (1, "float"), (2, "bool")].iter().cloned().collect();

    let v = env.unifier.get_fresh_var_with_range(&[int, boolean], None, None).0;
    let list_v = env.unifier.add_ty(TypeEnum::TList { ty: v });
    let v1 = env.unifier.get_fresh_var_with_range(&[list_v, int], None, None).0;
    let v2 = env.unifier.get_fresh_var_with_range(&[list_int, float], None, None).0;
    let t = env.unifier.get_dummy_var().0;
    let tuple = env.unifier.add_ty(TypeEnum::TTuple { ty: vec![v, v1, v2] });
    let v3 = env.unifier.get_fresh_var_with_range(&[tuple, t], None, None).0;
    // t = TypeVar('t')
    // v = TypeVar('v', int, bool)
    // v1 = TypeVar('v1', 'list[v]', int)
    // v2 = TypeVar('v2', 'list[int]', float)
    // v3 = TypeVar('v3', tuple[v, v1, v2], t)
    // what values can v3 take?

    let types = env.unifier.get_instantiations(v3).unwrap();
    let expected_types = indoc! {"
        tuple[bool, int, float]
        tuple[bool, int, list[int]]
        tuple[bool, list[bool], float]
        tuple[bool, list[bool], list[int]]
        tuple[bool, list[int], float]
        tuple[bool, list[int], list[int]]
        tuple[int, int, float]
        tuple[int, int, list[int]]
        tuple[int, list[bool], float]
        tuple[int, list[bool], list[int]]
        tuple[int, list[int], float]
        tuple[int, list[int], list[int]]
        v5"
    }
    .split('\n')
    .collect_vec();
    let types = types
        .iter()
        .map(|ty| {
            env.unifier.internal_stringify(
                *ty,
                &mut |i| obj_map.get(&i).unwrap().to_string(),
                &mut |i| format!("v{}", i),
                &mut None,
            )
        })
        .sorted()
        .collect_vec();
    assert_eq!(expected_types, types);
}
