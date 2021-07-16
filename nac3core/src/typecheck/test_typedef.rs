#[cfg(test)]
mod test {
    use super::super::typedef::*;
    use itertools::Itertools;
    use std::collections::HashMap;
    use test_case::test_case;

    struct TestEnvironment {
        pub unifier: Unifier,
        type_mapping: HashMap<String, Type>,
        var_max_id: u32,
    }

    impl TestEnvironment {
        fn new() -> TestEnvironment {
            let unifier = Unifier::new();
            let mut type_mapping = HashMap::new();
            let mut var_max_id = 0;

            type_mapping.insert(
                "int".into(),
                unifier.add_ty(TypeEnum::TObj {
                    obj_id: 0,
                    fields: HashMap::new(),
                    params: HashMap::new(),
                }),
            );
            type_mapping.insert(
                "float".into(),
                unifier.add_ty(TypeEnum::TObj {
                    obj_id: 1,
                    fields: HashMap::new(),
                    params: HashMap::new(),
                }),
            );
            type_mapping.insert(
                "bool".into(),
                unifier.add_ty(TypeEnum::TObj {
                    obj_id: 2,
                    fields: HashMap::new(),
                    params: HashMap::new(),
                }),
            );
            let v0 = unifier.add_ty(TypeEnum::TVar { id: 0 });
            var_max_id += 1;
            type_mapping.insert(
                "Foo".into(),
                unifier.add_ty(TypeEnum::TObj {
                    obj_id: 3,
                    fields: [("a".into(), v0)].iter().cloned().collect(),
                    params: [(0u32, v0)].iter().cloned().collect(),
                }),
            );

            TestEnvironment {
                unifier,
                type_mapping,
                var_max_id,
            }
        }

        fn get_fresh_var(&mut self) -> Type {
            let id = self.var_max_id + 1;
            self.var_max_id += 1;
            self.unifier.add_ty(TypeEnum::TVar { id })
        }

        fn parse(&self, typ: &str, mapping: &Mapping<String>) -> Type {
            let result = self.internal_parse(typ, mapping);
            assert!(result.1.is_empty());
            result.0
        }

        fn internal_parse<'a, 'b>(
            &'a self,
            typ: &'b str,
            mapping: &Mapping<String>,
        ) -> (Type, &'b str) {
            // for testing only, so we can just panic when the input is malformed
            let end = typ
                .find(|c| ['[', ',', ']', '='].contains(&c))
                .unwrap_or_else(|| typ.len());
            match &typ[..end] {
                "Tuple" => {
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
                "List" => {
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
                        let key = s[1..eq].to_string();
                        let result = self.internal_parse(&s[eq + 1..], mapping);
                        fields.insert(key, result.0);
                        s = result.1;
                    }
                    (self.unifier.add_ty(TypeEnum::TRecord { fields }), &s[1..])
                }
                x => {
                    let mut s = &typ[end..];
                    let ty = mapping.get(x).cloned().unwrap_or_else(|| {
                        // mapping should be type variables, type_mapping should be concrete types
                        // we should not resolve the type of type variables.
                        let mut ty = *self.type_mapping.get(x).unwrap();
                        let te = self.unifier.get_ty(ty);
                        if let TypeEnum::TObj { params, .. } = &*te.as_ref().borrow() {
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
    }

    #[test_case(2,
        &[("v1", "v2"), ("v2", "float")],
        &[("v1", "float"), ("v2", "float")]
        ; "simple variable"
    )]
    #[test_case(2,
        &[("v1", "List[v2]"), ("v1", "List[float]")],
        &[("v1", "List[float]"), ("v2", "float")]
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
                let v = env.get_fresh_var();
                mapping.insert(format!("v{}", i), v);
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
                let t1 = env.parse(a, &mapping);
                let t2 = env.parse(b, &mapping);
                assert!(env.unifier.eq(t1, t2));
            }
        }
    }

    #[test_case(2,
        &[
            ("v1", "Tuple[int]"),
            ("v2", "List[int]"),
        ],
        (("v1", "v2"), "Cannot unify TTuple with TList")
        ; "type mismatch"
    )]
    #[test_case(2,
        &[
            ("v1", "Tuple[int]"),
            ("v2", "Tuple[float]"),
        ],
        (("v1", "v2"), "Cannot unify objects with ID 0 and 1")
        ; "tuple parameter mismatch"
    )]
    #[test_case(2,
        &[
            ("v1", "Tuple[int,int]"),
            ("v2", "Tuple[int]"),
        ],
        (("v1", "v2"), "Cannot unify tuples with length 1 and 2")
        ; "tuple length mismatch"
    )]
    #[test_case(3,
        &[
            ("v1", "Record[a=float,b=int]"),
            ("v2", "Foo[v3]"),
        ],
        (("v1", "v2"), "No such attribute b")
        ; "record obj merge"
    )]
    #[test_case(2,
        &[
            ("v1", "List[v2]"),
        ],
        (("v1", "v2"), "Recursive type is prohibited.")
        ; "recursive type for lists"
    )]
    /// Test cases for invalid unifications.
    fn test_invalid_unification(
        variable_count: u32,
        unify_pairs: &[(&'static str, &'static str)],
        errornous_pair: ((&'static str, &'static str), &'static str),
    ) {
        let mut env = TestEnvironment::new();
        let mut mapping = HashMap::new();
        for i in 1..=variable_count {
            let v = env.get_fresh_var();
            mapping.insert(format!("v{}", i), v);
        }
        // unification may have side effect when we do type resolution, so freeze the types
        // before doing unification.
        let mut pairs = Vec::new();
        for (a, b) in unify_pairs.iter() {
            let t1 = env.parse(a, &mapping);
            let t2 = env.parse(b, &mapping);
            pairs.push((t1, t2));
        }
        let (t1, t2) = (
            env.parse(errornous_pair.0 .0, &mapping),
            env.parse(errornous_pair.0 .1, &mapping),
        );
        for (a, b) in pairs {
            env.unifier.unify(a, b).unwrap();
        }
        assert_eq!(env.unifier.unify(t1, t2), Err(errornous_pair.1.to_string()));
    }
}
