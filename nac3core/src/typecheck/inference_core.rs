use super::context::InferenceContext;
use super::typedef::{TypeEnum::*, *};
use std::collections::HashMap;

fn find_subst(
    ctx: &InferenceContext,
    valuation: &Option<(VariableId, Type)>,
    sub: &mut HashMap<VariableId, Type>,
    mut a: Type,
    mut b: Type,
) -> Result<(), String> {
    // TODO: fix error messages later
    if let TypeVariable(id) = a.as_ref() {
        if let Some((assumption_id, t)) = valuation {
            if assumption_id == id {
                a = t.clone();
            }
        }
    }

    let mut substituted = false;
    if let TypeVariable(id) = b.as_ref() {
        if let Some(c) = sub.get(&id) {
            b = c.clone();
            substituted = true;
        }
    }

    match (a.as_ref(), b.as_ref()) {
        (BotType, _) => Ok(()),
        (TypeVariable(id_a), TypeVariable(id_b)) => {
            if substituted {
                return if id_a == id_b {
                    Ok(())
                } else {
                    Err("different variables".to_string())
                };
            }
            let v_a = ctx.get_variable_def(*id_a);
            let v_b = ctx.get_variable_def(*id_b);
            if !v_b.bound.is_empty() {
                if v_a.bound.is_empty() {
                    return Err("unbounded a".to_string());
                } else {
                    let diff: Vec<_> = v_a
                        .bound
                        .iter()
                        .filter(|x| !v_b.bound.contains(x))
                        .collect();
                    if !diff.is_empty() {
                        return Err("different domain".to_string());
                    }
                }
            }
            sub.insert(*id_b, a.clone());
            Ok(())
        }
        (TypeVariable(id_a), _) => {
            let v_a = ctx.get_variable_def(*id_a);
            if v_a.bound.len() == 1 && v_a.bound[0].as_ref() == b.as_ref() {
                Ok(())
            } else {
                Err("different domain".to_string())
            }
        }
        (_, TypeVariable(id_b)) => {
            let v_b = ctx.get_variable_def(*id_b);
            if v_b.bound.is_empty() || v_b.bound.contains(&a) {
                sub.insert(*id_b, a.clone());
                Ok(())
            } else {
                Err("different domain".to_string())
            }
        }
        (_, VirtualClassType(id_b)) => {
            let mut parents;
            match a.as_ref() {
                ClassType(id_a) => {
                    parents = [*id_a].to_vec();
                }
                VirtualClassType(id_a) => {
                    parents = [*id_a].to_vec();
                }
                _ => {
                    return Err("cannot substitute non-class type into virtual class".to_string());
                }
            };
            while !parents.is_empty() {
                if *id_b == parents[0] {
                    return Ok(());
                }
                let c = ctx.get_class_def(parents.remove(0));
                parents.extend_from_slice(&c.parents);
            }
            Err("not subtype".to_string())
        }
        (ParametricType(id_a, param_a), ParametricType(id_b, param_b)) => {
            if id_a != id_b || param_a.len() != param_b.len() {
                Err("different parametric types".to_string())
            } else {
                for (x, y) in param_a.iter().zip(param_b.iter()) {
                    find_subst(ctx, valuation, sub, x.clone(), y.clone())?;
                }
                Ok(())
            }
        }
        (_, _) => {
            if a == b {
                Ok(())
            } else {
                Err("not equal".to_string())
            }
        }
    }
}

fn resolve_call_rec(
    ctx: &InferenceContext,
    valuation: &Option<(VariableId, Type)>,
    obj: Option<Type>,
    func: &str,
    args: &[Type],
) -> Result<Option<Type>, String> {
    let mut subst = obj
        .as_ref()
        .map(|v| v.get_subst(ctx))
        .unwrap_or_else(HashMap::new);

    let fun = match &obj {
        Some(obj) => {
            let base = match obj.as_ref() {
                PrimitiveType(id) => &ctx.get_primitive_def(*id),
                ClassType(id) | VirtualClassType(id) => &ctx.get_class_def(*id).base,
                ParametricType(id, _) => &ctx.get_parametric_def(*id).base,
                _ => return Err("not supported".to_string()),
            };
            base.methods.get(func)
        }
        None => ctx.get_fn_def(func),
    }
    .ok_or_else(|| "no such function".to_string())?;

    if args.len() != fun.args.len() {
        return Err("incorrect parameter number".to_string());
    }
    for (a, b) in args.iter().zip(fun.args.iter()) {
        find_subst(ctx, valuation, &mut subst, a.clone(), b.clone())?;
    }
    let result = fun.result.as_ref().map(|v| v.subst(&subst));
    Ok(result.map(|result| {
        if let SelfType = result {
            obj.unwrap()
        } else {
            result.into()
        }
    }))
}

pub fn resolve_call(
    ctx: &InferenceContext,
    obj: Option<Type>,
    func: &str,
    args: &[Type],
) -> Result<Option<Type>, String> {
    resolve_call_rec(ctx, &None, obj, func, args)
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::super::context::GlobalContext;
    use super::super::primitives::*;
    use std::rc::Rc;

    fn get_inference_context(ctx: GlobalContext) -> InferenceContext {
        InferenceContext::new(ctx, Box::new(|_| Err("unbounded identifier".into())))
    }

    #[test]
    fn test_simple_generic() {
        let mut ctx = basic_ctx();
        let v1 = ctx.add_variable(VarDef {
            name: "V1",
            bound: vec![ctx.get_primitive(INT32_TYPE), ctx.get_primitive(FLOAT_TYPE)],
        });
        let v1 = ctx.get_variable(v1);
        let v2 = ctx.add_variable(VarDef {
            name: "V2",
            bound: vec![
                ctx.get_primitive(BOOL_TYPE),
                ctx.get_primitive(INT32_TYPE),
                ctx.get_primitive(FLOAT_TYPE),
            ],
        });
        let v2 = ctx.get_variable(v2);
        let ctx = get_inference_context(ctx);

        assert_eq!(
            resolve_call(&ctx, None, "int32", &[ctx.get_primitive(FLOAT_TYPE)]),
            Ok(Some(ctx.get_primitive(INT32_TYPE)))
        );

        assert_eq!(
            resolve_call(&ctx, None, "int32", &[ctx.get_primitive(INT32_TYPE)],),
            Ok(Some(ctx.get_primitive(INT32_TYPE)))
        );

        assert_eq!(
            resolve_call(&ctx, None, "float", &[ctx.get_primitive(INT32_TYPE)]),
            Ok(Some(ctx.get_primitive(FLOAT_TYPE)))
        );

        assert_eq!(
            resolve_call(&ctx, None, "float", &[ctx.get_primitive(BOOL_TYPE)]),
            Err("different domain".to_string())
        );

        assert_eq!(
            resolve_call(&ctx, None, "float", &[]),
            Err("incorrect parameter number".to_string())
        );

        assert_eq!(
            resolve_call(&ctx, None, "float", &[v1]),
            Ok(Some(ctx.get_primitive(FLOAT_TYPE)))
        );

        assert_eq!(
            resolve_call(&ctx, None, "float", &[v2]),
            Err("different domain".to_string())
        );
    }

    #[test]
    fn test_methods() {
        let mut ctx = basic_ctx();

        let v0 = ctx.add_variable(VarDef {
            name: "V0",
            bound: vec![],
        });
        let v0 = ctx.get_variable(v0);

        let int32 = ctx.get_primitive(INT32_TYPE);
        let int64 = ctx.get_primitive(INT64_TYPE);
        let ctx = get_inference_context(ctx);

        // simple cases
        assert_eq!(
            resolve_call(&ctx, Some(int32.clone()), "__add__", &[int32.clone()]),
            Ok(Some(int32.clone()))
        );

        assert_ne!(
            resolve_call(&ctx, Some(int32.clone()), "__add__", &[int32.clone()]),
            Ok(Some(int64.clone()))
        );

        assert_eq!(
            resolve_call(&ctx, Some(int32), "__add__", &[int64]),
            Err("not equal".to_string())
        );

        // with type variables
        assert_eq!(
            resolve_call(&ctx, Some(v0.clone()), "__add__", &[v0.clone()]),
            Err("not supported".into())
        );
    }

    #[test]
    fn test_multi_generic() {
        let mut ctx = basic_ctx();
        let v0 = ctx.add_variable(VarDef {
            name: "V0",
            bound: vec![],
        });
        let v0 = ctx.get_variable(v0);
        let v1 = ctx.add_variable(VarDef {
            name: "V1",
            bound: vec![],
        });
        let v1 = ctx.get_variable(v1);
        let v2 = ctx.add_variable(VarDef {
            name: "V2",
            bound: vec![],
        });
        let v2 = ctx.get_variable(v2);
        let v3 = ctx.add_variable(VarDef {
            name: "V3",
            bound: vec![],
        });
        let v3 = ctx.get_variable(v3);

        ctx.add_fn(
            "foo",
            FnDef {
                args: vec![v0.clone(), v0.clone(), v1.clone()],
                result: Some(v0.clone()),
            },
        );

        ctx.add_fn(
            "foo1",
            FnDef {
                args: vec![ParametricType(TUPLE_TYPE, vec![v0.clone(), v0.clone(), v1]).into()],
                result: Some(v0),
            },
        );
        let ctx = get_inference_context(ctx);

        assert_eq!(
            resolve_call(&ctx, None, "foo", &[v2.clone(), v2.clone(), v2.clone()]),
            Ok(Some(v2.clone()))
        );
        assert_eq!(
            resolve_call(&ctx, None, "foo", &[v2.clone(), v2.clone(), v3.clone()]),
            Ok(Some(v2.clone()))
        );
        assert_eq!(
            resolve_call(&ctx, None, "foo", &[v2.clone(), v3.clone(), v3.clone()]),
            Err("different variables".to_string())
        );

        assert_eq!(
            resolve_call(
                &ctx,
                None,
                "foo1",
                &[ParametricType(TUPLE_TYPE, vec![v2.clone(), v2.clone(), v2.clone()]).into()]
            ),
            Ok(Some(v2.clone()))
        );
        assert_eq!(
            resolve_call(
                &ctx,
                None,
                "foo1",
                &[ParametricType(TUPLE_TYPE, vec![v2.clone(), v2.clone(), v3.clone()]).into()]
            ),
            Ok(Some(v2.clone()))
        );
        assert_eq!(
            resolve_call(
                &ctx,
                None,
                "foo1",
                &[ParametricType(TUPLE_TYPE, vec![v2, v3.clone(), v3]).into()]
            ),
            Err("different variables".to_string())
        );
    }

    #[test]
    fn test_class_generics() {
        let mut ctx = basic_ctx();

        let list = ctx.get_parametric_def_mut(LIST_TYPE);
        let t = Rc::new(TypeVariable(list.params[0]));
        list.base.methods.insert(
            "head",
            FnDef {
                args: vec![],
                result: Some(t.clone()),
            },
        );
        list.base.methods.insert(
            "append",
            FnDef {
                args: vec![t],
                result: None,
            },
        );

        let v0 = ctx.add_variable(VarDef {
            name: "V0",
            bound: vec![],
        });
        let v0 = ctx.get_variable(v0);
        let v1 = ctx.add_variable(VarDef {
            name: "V1",
            bound: vec![],
        });
        let v1 = ctx.get_variable(v1);
        let ctx = get_inference_context(ctx);

        assert_eq!(
            resolve_call(
                &ctx,
                Some(ParametricType(LIST_TYPE, vec![v0.clone()]).into()),
                "head",
                &[]
            ),
            Ok(Some(v0.clone()))
        );
        assert_eq!(
            resolve_call(
                &ctx,
                Some(ParametricType(LIST_TYPE, vec![v0.clone()]).into()),
                "append",
                &[v0.clone()]
            ),
            Ok(None)
        );
        assert_eq!(
            resolve_call(
                &ctx,
                Some(ParametricType(LIST_TYPE, vec![v0]).into()),
                "append",
                &[v1]
            ),
            Err("different variables".to_string())
        );
    }

    #[test]
    fn test_virtual_class() {
        let mut ctx = basic_ctx();

        let foo = ctx.add_class(ClassDef {
            base: TypeDef {
                name: "Foo",
                methods: HashMap::new(),
                fields: HashMap::new(),
            },
            parents: vec![],
        });

        let foo1 = ctx.add_class(ClassDef {
            base: TypeDef {
                name: "Foo1",
                methods: HashMap::new(),
                fields: HashMap::new(),
            },
            parents: vec![foo],
        });

        let foo2 = ctx.add_class(ClassDef {
            base: TypeDef {
                name: "Foo2",
                methods: HashMap::new(),
                fields: HashMap::new(),
            },
            parents: vec![foo1],
        });

        let bar = ctx.add_class(ClassDef {
            base: TypeDef {
                name: "bar",
                methods: HashMap::new(),
                fields: HashMap::new(),
            },
            parents: vec![],
        });

        ctx.add_fn(
            "foo",
            FnDef {
                args: vec![VirtualClassType(foo).into()],
                result: None,
            },
        );
        ctx.add_fn(
            "foo1",
            FnDef {
                args: vec![VirtualClassType(foo1).into()],
                result: None,
            },
        );
        let ctx = get_inference_context(ctx);

        assert_eq!(
            resolve_call(&ctx, None, "foo", &[ClassType(foo).into()]),
            Ok(None)
        );

        assert_eq!(
            resolve_call(&ctx, None, "foo", &[ClassType(foo1).into()]),
            Ok(None)
        );

        assert_eq!(
            resolve_call(&ctx, None, "foo", &[ClassType(foo2).into()]),
            Ok(None)
        );

        assert_eq!(
            resolve_call(&ctx, None, "foo", &[ClassType(bar).into()]),
            Err("not subtype".to_string())
        );

        assert_eq!(
            resolve_call(&ctx, None, "foo1", &[ClassType(foo1).into()]),
            Ok(None)
        );

        assert_eq!(
            resolve_call(&ctx, None, "foo1", &[ClassType(foo2).into()]),
            Ok(None)
        );

        assert_eq!(
            resolve_call(&ctx, None, "foo1", &[ClassType(foo).into()]),
            Err("not subtype".to_string())
        );

        // virtual class substitution
        assert_eq!(
            resolve_call(&ctx, None, "foo", &[VirtualClassType(foo).into()]),
            Ok(None)
        );
        assert_eq!(
            resolve_call(&ctx, None, "foo", &[VirtualClassType(foo1).into()]),
            Ok(None)
        );
        assert_eq!(
            resolve_call(&ctx, None, "foo", &[VirtualClassType(foo2).into()]),
            Ok(None)
        );
        assert_eq!(
            resolve_call(&ctx, None, "foo", &[VirtualClassType(bar).into()]),
            Err("not subtype".to_string())
        );
    }
}
