use crate::context::InferenceContext;
use crate::typedef::{TypeEnum::*, *};
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
                TypeVariable(id) => {
                    let v = ctx.get_variable_def(*id);
                    if v.bound.is_empty() {
                        return Err("unbounded type var".to_string());
                    }
                    let results: Result<Vec<_>, String> = v
                        .bound
                        .iter()
                        .map(|ins| {
                            resolve_call_rec(
                                ctx,
                                &Some((*id, ins.clone())),
                                Some(ins.clone()),
                                func,
                                args.clone(),
                            )
                        })
                        .collect();
                    let results = results?;
                    if results.iter().all(|v| v == &results[0]) {
                        return Ok(results[0].clone());
                    }
                    let mut results = results.iter().zip(v.bound.iter()).map(|(r, ins)| {
                        r.as_ref()
                            .map(|v| v.inv_subst(&[(ins.clone(), obj.clone())]))
                    });
                    let first = results.next().unwrap();
                    if results.all(|v| v == first) {
                        return Ok(first);
                    } else {
                        return Err("divergent type after substitution".to_string());
                    }
                }
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

