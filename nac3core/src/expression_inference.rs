use crate::context::InferenceContext;
use crate::inference_core::resolve_call;
use crate::magic_methods::*;
use crate::primitives::*;
use crate::typedef::{Type, TypeEnum::*};
use rustpython_parser::ast::{
    Comparison, Comprehension, ComprehensionKind, Expression, ExpressionType, Operator,
    UnaryOperator,
};
use std::convert::TryInto;

type ParserResult = Result<Option<Type>, String>;

pub fn infer_expr<'b: 'a, 'a>(
    ctx: &mut InferenceContext<'a>,
    expr: &'b Expression,
) -> ParserResult {
    match &expr.node {
        ExpressionType::Number { value } => infer_constant(ctx, value),
        ExpressionType::Identifier { name } => infer_identifier(ctx, name),
        ExpressionType::List { elements } => infer_list(ctx, elements),
        ExpressionType::Tuple { elements } => infer_tuple(ctx, elements),
        ExpressionType::Attribute { value, name } => infer_attribute(ctx, value, name),
        ExpressionType::BoolOp { values, .. } => infer_bool_ops(ctx, values),
        ExpressionType::Binop { a, b, op } => infer_bin_ops(ctx, op, a, b),
        ExpressionType::Unop { op, a } => infer_unary_ops(ctx, op, a),
        ExpressionType::Compare { vals, ops } => infer_compare(ctx, vals, ops),
        ExpressionType::Call {
            args,
            function,
            keywords,
        } => {
            if !keywords.is_empty() {
                Err("keyword is not supported".into())
            } else {
                infer_call(ctx, &args, &function)
            }
        }
        ExpressionType::Subscript { a, b } => infer_subscript(ctx, a, b),
        ExpressionType::IfExpression { test, body, orelse } => {
            infer_if_expr(ctx, &test, &body, orelse)
        }
        ExpressionType::Comprehension { kind, generators } => match kind.as_ref() {
            ComprehensionKind::List { element } => {
                if generators.len() == 1 {
                    infer_list_comprehension(ctx, element, &generators[0])
                } else {
                    Err("only 1 generator statement is supported".into())
                }
            }
            _ => Err("only list comprehension is supported".into()),
        },
        ExpressionType::True | ExpressionType::False => Ok(Some(ctx.get_primitive(BOOL_TYPE))),
        _ => Err("not supported".into()),
    }
}

fn infer_constant(
    ctx: &mut InferenceContext,
    value: &rustpython_parser::ast::Number,
) -> ParserResult {
    use rustpython_parser::ast::Number;
    match value {
        Number::Integer { value } => {
            let int32: Result<i32, _> = value.try_into();
            if int32.is_ok() {
                Ok(Some(ctx.get_primitive(INT32_TYPE)))
            } else {
                let int64: Result<i64, _> = value.try_into();
                if int64.is_ok() {
                    Ok(Some(ctx.get_primitive(INT64_TYPE)))
                } else {
                    Err("integer out of range".into())
                }
            }
        }
        Number::Float { .. } => Ok(Some(ctx.get_primitive(FLOAT_TYPE))),
        _ => Err("not supported".into()),
    }
}

fn infer_identifier(ctx: &mut InferenceContext, name: &str) -> ParserResult {
    Ok(Some(ctx.resolve(name)?))
}

fn infer_list<'b: 'a, 'a>(
    ctx: &mut InferenceContext<'a>,
    elements: &'b [Expression],
) -> ParserResult {
    if elements.is_empty() {
        return Ok(Some(ParametricType(LIST_TYPE, vec![BotType.into()]).into()));
    }

    let mut types = elements.iter().map(|v| infer_expr(ctx, v));

    let head = types.next().unwrap()?;
    if head.is_none() {
        return Err("list elements must have some type".into());
    }
    for v in types {
        if v? != head {
            return Err("inhomogeneous list is not allowed".into());
        }
    }
    Ok(Some(ParametricType(LIST_TYPE, vec![head.unwrap()]).into()))
}

fn infer_tuple<'b: 'a, 'a>(
    ctx: &mut InferenceContext<'a>,
    elements: &'b [Expression],
) -> ParserResult {
    let types: Result<Option<Vec<_>>, String> =
        elements.iter().map(|v| infer_expr(ctx, v)).collect();
    if let Some(t) = types? {
        Ok(Some(ParametricType(TUPLE_TYPE, t).into()))
    } else {
        Err("tuple elements must have some type".into())
    }
}

fn infer_attribute<'a>(
    ctx: &mut InferenceContext<'a>,
    value: &'a Expression,
    name: &str,
) -> ParserResult {
    let value = infer_expr(ctx, value)?.ok_or_else(|| "no value".to_string())?;
    if let TypeVariable(id) = value.as_ref() {
        let v = ctx.get_variable_def(*id);
        if v.bound.is_empty() {
            return Err("no fields on unbounded type variable".into());
        }
        let ty = v.bound[0].get_base(ctx).and_then(|v| v.fields.get(name));
        if ty.is_none() {
            return Err("unknown field".into());
        }
        for x in v.bound[1..].iter() {
            let ty1 = x.get_base(ctx).and_then(|v| v.fields.get(name));
            if ty1 != ty {
                return Err("unknown field (type mismatch between variants)".into());
            }
        }
        return Ok(Some(ty.unwrap().clone()));
    }

    match value.get_base(ctx) {
        Some(b) => match b.fields.get(name) {
            Some(t) => Ok(Some(t.clone())),
            None => Err("no such field".into()),
        },
        None => Err("this object has no fields".into()),
    }
}

fn infer_bool_ops<'a>(ctx: &mut InferenceContext<'a>, values: &'a [Expression]) -> ParserResult {
    assert_eq!(values.len(), 2);
    let left = infer_expr(ctx, &values[0])?.ok_or_else(|| "no value".to_string())?;
    let right = infer_expr(ctx, &values[1])?.ok_or_else(|| "no value".to_string())?;

    let b = ctx.get_primitive(BOOL_TYPE);
    if left == b && right == b {
        Ok(Some(b))
    } else {
        Err("bool operands must be bool".into())
    }
}

fn infer_bin_ops<'b: 'a, 'a>(
    ctx: &mut InferenceContext<'a>,
    op: &Operator,
    left: &'b Expression,
    right: &'b Expression,
) -> ParserResult {
    let left = infer_expr(ctx, left)?.ok_or_else(|| "no value".to_string())?;
    let right = infer_expr(ctx, right)?.ok_or_else(|| "no value".to_string())?;
    let fun = binop_name(op);
    resolve_call(ctx, Some(left), fun, &[right])
}

fn infer_unary_ops<'b: 'a, 'a>(
    ctx: &mut InferenceContext<'a>,
    op: &UnaryOperator,
    obj: &'b Expression,
) -> ParserResult {
    let ty = infer_expr(ctx, obj)?.ok_or_else(|| "no value".to_string())?;
    if let UnaryOperator::Not = op {
        if ty == ctx.get_primitive(BOOL_TYPE) {
            Ok(Some(ty))
        } else {
            Err("logical not must be applied to bool".into())
        }
    } else {
        resolve_call(ctx, Some(ty), unaryop_name(op), &[])
    }
}

fn infer_compare<'b: 'a, 'a>(
    ctx: &mut InferenceContext<'a>,
    vals: &'b [Expression],
    ops: &'b [Comparison],
) -> ParserResult {
    let types: Result<Option<Vec<_>>, _> = vals.iter().map(|v| infer_expr(ctx, v)).collect();
    let types = types?;
    if types.is_none() {
        return Err("comparison operands must have type".into());
    }
    let types = types.unwrap();
    let boolean = ctx.get_primitive(BOOL_TYPE);
    let left = &types[..types.len() - 1];
    let right = &types[1..];

    for ((a, b), op) in left.iter().zip(right.iter()).zip(ops.iter()) {
        let fun = comparison_name(op).ok_or_else(|| "unsupported comparison".to_string())?;
        let ty = resolve_call(ctx, Some(a.clone()), fun, &[b.clone()])?;
        if ty.is_none() || ty.unwrap() != boolean {
            return Err("comparison result must be boolean".into());
        }
    }
    Ok(Some(boolean))
}

fn infer_call<'b: 'a, 'a>(
    ctx: &mut InferenceContext<'a>,
    args: &'b [Expression],
    function: &'b Expression,
) -> ParserResult {
    let types: Result<Option<Vec<_>>, _> = args.iter().map(|v| infer_expr(ctx, v)).collect();
    let types = types?;
    if types.is_none() {
        return Err("function params must have type".into());
    }

    let (obj, fun) = match &function.node {
        ExpressionType::Identifier { name } => (None, name),
        ExpressionType::Attribute { value, name } => (
            Some(infer_expr(ctx, &value)?.ok_or_else(|| "no value".to_string())?),
            name,
        ),
        _ => return Err("not supported".into()),
    };
    resolve_call(ctx, obj, fun.as_str(), &types.unwrap())
}

fn infer_subscript<'b: 'a, 'a>(
    ctx: &mut InferenceContext<'a>,
    a: &'b Expression,
    b: &'b Expression,
) -> ParserResult {
    let a = infer_expr(ctx, a)?.ok_or_else(|| "no value".to_string())?;
    let t = if let ParametricType(LIST_TYPE, ls) = a.as_ref() {
        ls[0].clone()
    } else {
        return Err("subscript is not supported for types other than list".into());
    };

    match &b.node {
        ExpressionType::Slice { elements } => {
            let int32 = ctx.get_primitive(INT32_TYPE);
            let types: Result<Option<Vec<_>>, _> = elements
                .iter()
                .map(|v| {
                    if let ExpressionType::None = v.node {
                        Ok(Some(int32.clone()))
                    } else {
                        infer_expr(ctx, v)
                    }
                })
                .collect();
            let types = types?.ok_or_else(|| "slice must have type".to_string())?;
            if types.iter().all(|v| v == &int32) {
                Ok(Some(a))
            } else {
                Err("slice must be int32 type".into())
            }
        }
        _ => {
            let b = infer_expr(ctx, b)?.ok_or_else(|| "no value".to_string())?;
            if b == ctx.get_primitive(INT32_TYPE) {
                Ok(Some(t))
            } else {
                Err("index must be either slice or int32".into())
            }
        }
    }
}

fn infer_if_expr<'b: 'a, 'a>(
    ctx: &mut InferenceContext<'a>,
    test: &'b Expression,
    body: &'b Expression,
    orelse: &'b Expression,
) -> ParserResult {
    let test = infer_expr(ctx, test)?.ok_or_else(|| "no value".to_string())?;
    if test != ctx.get_primitive(BOOL_TYPE) {
        return Err("test should be bool".into());
    }

    let body = infer_expr(ctx, body)?;
    let orelse = infer_expr(ctx, orelse)?;
    if body.as_ref() == orelse.as_ref() {
        Ok(body)
    } else {
        Err("divergent type".into())
    }
}

fn infer_simple_binding<'a: 'b, 'b>(
    ctx: &mut InferenceContext<'b>,
    name: &'a Expression,
    ty: Type,
) -> Result<(), String> {
    match &name.node {
        ExpressionType::Identifier { name } => {
            if name == "_" {
                Ok(())
            } else if ctx.defined(name.as_str()) {
                Err("duplicated naming".into())
            } else {
                ctx.assign(name.as_str(), ty)?;
                Ok(())
            }
        }
        ExpressionType::Tuple { elements } => {
            if let ParametricType(TUPLE_TYPE, ls) = ty.as_ref() {
                if elements.len() == ls.len() {
                    for (a, b) in elements.iter().zip(ls.iter()) {
                        infer_simple_binding(ctx, a, b.clone())?;
                    }
                    Ok(())
                } else {
                    Err("different length".into())
                }
            } else {
                Err("not supported".into())
            }
        }
        _ => Err("not supported".into()),
    }
}

fn infer_list_comprehension<'b: 'a, 'a>(
    ctx: &mut InferenceContext<'a>,
    element: &'b Expression,
    comprehension: &'b Comprehension,
) -> ParserResult {
    if comprehension.is_async {
        return Err("async is not supported".into());
    }

    let iter = infer_expr(ctx, &comprehension.iter)?.ok_or_else(|| "no value".to_string())?;
    if let ParametricType(LIST_TYPE, ls) = iter.as_ref() {
        ctx.with_scope(|ctx| {
            infer_simple_binding(ctx, &comprehension.target, ls[0].clone())?;

            let boolean = ctx.get_primitive(BOOL_TYPE);
            for test in comprehension.ifs.iter() {
                let result =
                    infer_expr(ctx, test)?.ok_or_else(|| "no value in test".to_string())?;
                if result != boolean {
                    return Err("test must be bool".into());
                }
            }
            let result = infer_expr(ctx, element)?.ok_or_else(|| "no value")?;
            Ok(Some(ParametricType(LIST_TYPE, vec![result]).into()))
        })
        .1
    } else {
        Err("iteration is supported for list only".into())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::context::*;
    use crate::typedef::*;
    use rustpython_parser::parser::parse_expression;
    use std::collections::HashMap;
    use std::rc::Rc;

    fn get_inference_context(ctx: TopLevelContext) -> InferenceContext {
        InferenceContext::new(ctx, Box::new(|_| Err("unbounded identifier".into())))
    }

    #[test]
    fn test_constants() {
        let ctx = basic_ctx();
        let mut ctx = get_inference_context(ctx);

        let ast = parse_expression("123").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result.unwrap().unwrap(), ctx.get_primitive(INT32_TYPE));

        let ast = parse_expression("2147483647").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result.unwrap().unwrap(), ctx.get_primitive(INT32_TYPE));

        let ast = parse_expression("2147483648").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result.unwrap().unwrap(), ctx.get_primitive(INT64_TYPE));

        let ast = parse_expression("9223372036854775807").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result.unwrap().unwrap(), ctx.get_primitive(INT64_TYPE));

        let ast = parse_expression("9223372036854775808").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result, Err("integer out of range".into()));

        let ast = parse_expression("123.456").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result.unwrap().unwrap(), ctx.get_primitive(FLOAT_TYPE));

        let ast = parse_expression("True").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result.unwrap().unwrap(), ctx.get_primitive(BOOL_TYPE));

        let ast = parse_expression("False").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result.unwrap().unwrap(), ctx.get_primitive(BOOL_TYPE));
    }

    #[test]
    fn test_identifier() {
        let ctx = basic_ctx();
        let mut ctx = get_inference_context(ctx);
        ctx.assign("abc", ctx.get_primitive(INT32_TYPE)).unwrap();

        let ast = parse_expression("abc").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result.unwrap().unwrap(), ctx.get_primitive(INT32_TYPE));

        let ast = parse_expression("ab").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result, Err("unbounded identifier".into()));
    }

    #[test]
    fn test_list() {
        let mut ctx = basic_ctx();
        ctx.add_fn(
            "foo",
            FnDef {
                args: vec![],
                result: None,
            },
        );
        let mut ctx = get_inference_context(ctx);
        ctx.assign("abc", ctx.get_primitive(INT32_TYPE)).unwrap();
        // def is reserved...
        ctx.assign("efg", ctx.get_primitive(INT32_TYPE)).unwrap();
        ctx.assign("xyz", ctx.get_primitive(FLOAT_TYPE)).unwrap();

        let ast = parse_expression("[]").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(
            result.unwrap().unwrap(),
            ParametricType(LIST_TYPE, vec![BotType.into()]).into()
        );

        let ast = parse_expression("[abc]").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(
            result.unwrap().unwrap(),
            ParametricType(LIST_TYPE, vec![ctx.get_primitive(INT32_TYPE)]).into()
        );

        let ast = parse_expression("[abc, efg]").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(
            result.unwrap().unwrap(),
            ParametricType(LIST_TYPE, vec![ctx.get_primitive(INT32_TYPE)]).into()
        );

        let ast = parse_expression("[abc, efg, xyz]").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result, Err("inhomogeneous list is not allowed".into()));

        let ast = parse_expression("[foo()]").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result, Err("list elements must have some type".into()));
    }

    #[test]
    fn test_tuple() {
        let mut ctx = basic_ctx();
        ctx.add_fn(
            "foo",
            FnDef {
                args: vec![],
                result: None,
            },
        );
        let mut ctx = get_inference_context(ctx);
        ctx.assign("abc", ctx.get_primitive(INT32_TYPE)).unwrap();
        ctx.assign("efg", ctx.get_primitive(FLOAT_TYPE)).unwrap();

        let ast = parse_expression("(abc, efg)").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(
            result.unwrap().unwrap(),
            ParametricType(
                TUPLE_TYPE,
                vec![ctx.get_primitive(INT32_TYPE), ctx.get_primitive(FLOAT_TYPE)]
            )
            .into()
        );

        let ast = parse_expression("(abc, efg, foo())").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result, Err("tuple elements must have some type".into()));
    }

    #[test]
    fn test_attribute() {
        let mut ctx = basic_ctx();
        ctx.add_fn(
            "none",
            FnDef {
                args: vec![],
                result: None,
            },
        );
        let int32 = ctx.get_primitive(INT32_TYPE);
        let float = ctx.get_primitive(FLOAT_TYPE);

        let foo = ctx.add_class(ClassDef {
            base: TypeDef {
                name: "Foo",
                fields: HashMap::new(),
                methods: HashMap::new(),
            },
            parents: vec![],
        });
        let foo_def = ctx.get_class_def_mut(foo);
        foo_def.base.fields.insert("a", int32.clone());
        foo_def.base.fields.insert("b", ClassType(foo).into());
        foo_def.base.fields.insert("c", int32.clone());

        let bar = ctx.add_class(ClassDef {
            base: TypeDef {
                name: "Bar",
                fields: HashMap::new(),
                methods: HashMap::new(),
            },
            parents: vec![],
        });
        let bar_def = ctx.get_class_def_mut(bar);
        bar_def.base.fields.insert("a", int32);
        bar_def.base.fields.insert("b", ClassType(bar).into());
        bar_def.base.fields.insert("c", float);

        let v0 = ctx.add_variable(VarDef {
            name: "v0",
            bound: vec![],
        });

        let v1 = ctx.add_variable(VarDef {
            name: "v1",
            bound: vec![ClassType(foo).into(), ClassType(bar).into()],
        });

        let mut ctx = get_inference_context(ctx);
        ctx.assign("foo", Rc::new(ClassType(foo))).unwrap();
        ctx.assign("bar", Rc::new(ClassType(bar))).unwrap();
        ctx.assign("foobar", Rc::new(VirtualClassType(foo)))
            .unwrap();
        ctx.assign("v0", ctx.get_variable(v0)).unwrap();
        ctx.assign("v1", ctx.get_variable(v1)).unwrap();
        ctx.assign("bot", Rc::new(BotType)).unwrap();

        let ast = parse_expression("foo.a").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result.unwrap().unwrap(), ctx.get_primitive(INT32_TYPE));

        let ast = parse_expression("foo.d").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result, Err("no such field".into()));

        let ast = parse_expression("foobar.a").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result.unwrap().unwrap(), ctx.get_primitive(INT32_TYPE));

        let ast = parse_expression("v0.a").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result, Err("no fields on unbounded type variable".into()));

        let ast = parse_expression("v1.a").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result.unwrap().unwrap(), ctx.get_primitive(INT32_TYPE));

        // shall we support this?
        let ast = parse_expression("v1.b").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(
            result,
            Err("unknown field (type mismatch between variants)".into())
        );
        // assert_eq!(result.unwrap().unwrap(), TypeVariable(v1).into());

        let ast = parse_expression("v1.c").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(
            result,
            Err("unknown field (type mismatch between variants)".into())
        );

        let ast = parse_expression("v1.d").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result, Err("unknown field".into()));

        let ast = parse_expression("none().a").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result, Err("no value".into()));

        let ast = parse_expression("bot.a").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result, Err("this object has no fields".into()));
    }

    #[test]
    fn test_bool_ops() {
        let mut ctx = basic_ctx();
        ctx.add_fn(
            "none",
            FnDef {
                args: vec![],
                result: None,
            },
        );
        let mut ctx = get_inference_context(ctx);

        let ast = parse_expression("True and False").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result.unwrap().unwrap(), ctx.get_primitive(BOOL_TYPE));

        let ast = parse_expression("True and none()").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result, Err("no value".into()));

        let ast = parse_expression("True and 123").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result, Err("bool operands must be bool".into()));
    }

    #[test]
    fn test_bin_ops() {
        let mut ctx = basic_ctx();
        let v0 = ctx.add_variable(VarDef {
            name: "v0",
            bound: vec![ctx.get_primitive(INT32_TYPE), ctx.get_primitive(INT64_TYPE)],
        });
        let mut ctx = get_inference_context(ctx);
        ctx.assign("a", TypeVariable(v0).into()).unwrap();

        let ast = parse_expression("1 + 2 + 3").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result.unwrap().unwrap(), ctx.get_primitive(INT32_TYPE));

        let ast = parse_expression("a + a + a").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result, Err("not supported".into()));
    }

    #[test]
    fn test_unary_ops() {
        let mut ctx = basic_ctx();
        let v0 = ctx.add_variable(VarDef {
            name: "v0",
            bound: vec![ctx.get_primitive(INT32_TYPE), ctx.get_primitive(INT64_TYPE)],
        });
        let mut ctx = get_inference_context(ctx);
        ctx.assign("a", TypeVariable(v0).into()).unwrap();

        let ast = parse_expression("-(123)").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result.unwrap().unwrap(), ctx.get_primitive(INT32_TYPE));

        let ast = parse_expression("-a").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result, Err("not supported".into()));

        let ast = parse_expression("not True").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result.unwrap().unwrap(), ctx.get_primitive(BOOL_TYPE));

        let ast = parse_expression("not (1)").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result, Err("logical not must be applied to bool".into()));
    }

    #[test]
    fn test_compare() {
        let mut ctx = basic_ctx();
        let v0 = ctx.add_variable(VarDef {
            name: "v0",
            bound: vec![ctx.get_primitive(INT32_TYPE), ctx.get_primitive(INT64_TYPE)],
        });
        let mut ctx = get_inference_context(ctx);
        ctx.assign("a", TypeVariable(v0).into()).unwrap();

        let ast = parse_expression("a == a == a").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result, Err("not supported".into()));

        let ast = parse_expression("a == a == 1").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result, Err("not supported".into()));

        let ast = parse_expression("True > False").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result, Err("no such function".into()));

        let ast = parse_expression("True in False").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result, Err("unsupported comparison".into()));
    }

    #[test]
    fn test_call() {
        let mut ctx = basic_ctx();
        ctx.add_fn(
            "none",
            FnDef {
                args: vec![],
                result: None,
            },
        );

        let foo = ctx.add_class(ClassDef {
            base: TypeDef {
                name: "Foo",
                fields: HashMap::new(),
                methods: HashMap::new(),
            },
            parents: vec![],
        });
        let foo_def = ctx.get_class_def_mut(foo);
        foo_def.base.methods.insert(
            "a",
            FnDef {
                args: vec![],
                result: Some(Rc::new(ClassType(foo))),
            },
        );

        let bar = ctx.add_class(ClassDef {
            base: TypeDef {
                name: "Bar",
                fields: HashMap::new(),
                methods: HashMap::new(),
            },
            parents: vec![],
        });
        let bar_def = ctx.get_class_def_mut(bar);
        bar_def.base.methods.insert(
            "a",
            FnDef {
                args: vec![],
                result: Some(Rc::new(ClassType(bar))),
            },
        );

        let v0 = ctx.add_variable(VarDef {
            name: "v0",
            bound: vec![],
        });
        let v1 = ctx.add_variable(VarDef {
            name: "v1",
            bound: vec![ClassType(foo).into(), ClassType(bar).into()],
        });
        let v2 = ctx.add_variable(VarDef {
            name: "v2",
            bound: vec![
                ClassType(foo).into(),
                ClassType(bar).into(),
                ctx.get_primitive(INT32_TYPE),
            ],
        });
        let mut ctx = get_inference_context(ctx);
        ctx.assign("foo", Rc::new(ClassType(foo))).unwrap();
        ctx.assign("bar", Rc::new(ClassType(bar))).unwrap();
        ctx.assign("foobar", Rc::new(VirtualClassType(foo)))
            .unwrap();
        ctx.assign("v0", ctx.get_variable(v0)).unwrap();
        ctx.assign("v1", ctx.get_variable(v1)).unwrap();
        ctx.assign("v2", ctx.get_variable(v2)).unwrap();
        ctx.assign("bot", Rc::new(BotType)).unwrap();

        let ast = parse_expression("foo.a()").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result.unwrap().unwrap(), ClassType(foo).into());

        let ast = parse_expression("v1.a()").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result, Err("not supported".into()));

        let ast = parse_expression("foobar.a()").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result.unwrap().unwrap(), ClassType(foo).into());

        let ast = parse_expression("none().a()").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result, Err("no value".into()));

        let ast = parse_expression("bot.a()").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result, Err("not supported".into()));

        let ast = parse_expression("[][0].a()").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result, Err("not supported".into()));
    }

    #[test]
    fn infer_subscript() {
        let mut ctx = basic_ctx();
        ctx.add_fn(
            "none",
            FnDef {
                args: vec![],
                result: None,
            },
        );
        let mut ctx = get_inference_context(ctx);

        let ast = parse_expression("[1, 2, 3][0]").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result.unwrap().unwrap(), ctx.get_primitive(INT32_TYPE));

        let ast = parse_expression("[[1]][0][0]").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result.unwrap().unwrap(), ctx.get_primitive(INT32_TYPE));

        let ast = parse_expression("[1, 2, 3][1:2]").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(
            result.unwrap().unwrap(),
            ParametricType(LIST_TYPE, vec![ctx.get_primitive(INT32_TYPE)]).into()
        );

        let ast = parse_expression("[1, 2, 3][1:2:2]").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(
            result.unwrap().unwrap(),
            ParametricType(LIST_TYPE, vec![ctx.get_primitive(INT32_TYPE)]).into()
        );

        let ast = parse_expression("[1, 2, 3][1:1.2]").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result, Err("slice must be int32 type".into()));

        let ast = parse_expression("[1, 2, 3][1:none()]").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result, Err("slice must have type".into()));

        let ast = parse_expression("[1, 2, 3][1.2]").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result, Err("index must be either slice or int32".into()));

        let ast = parse_expression("[1, 2, 3][none()]").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result, Err("no value".into()));

        let ast = parse_expression("none()[1.2]").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result, Err("no value".into()));

        let ast = parse_expression("123[1]").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(
            result,
            Err("subscript is not supported for types other than list".into())
        );
    }

    #[test]
    fn test_if_expr() {
        let mut ctx = basic_ctx();
        ctx.add_fn(
            "none",
            FnDef {
                args: vec![],
                result: None,
            },
        );
        let mut ctx = get_inference_context(ctx);

        let ast = parse_expression("1 if True else 0").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result.unwrap().unwrap(), ctx.get_primitive(INT32_TYPE));

        let ast = parse_expression("none() if True else none()").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result.unwrap(), None);

        let ast = parse_expression("none() if 1 else none()").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result, Err("test should be bool".into()));

        let ast = parse_expression("1 if True else none()").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result, Err("divergent type".into()));
    }

    #[test]
    fn test_list_comp() {
        let mut ctx = basic_ctx();
        ctx.add_fn(
            "none",
            FnDef {
                args: vec![],
                result: None,
            },
        );
        let int32 = ctx.get_primitive(INT32_TYPE);
        let mut ctx = get_inference_context(ctx);
        ctx.assign("z", int32.clone()).unwrap();

        let ast = parse_expression("[x for x in [(1, 2), (2, 3), (3, 4)]][0]").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(
            result.unwrap().unwrap(),
            ParametricType(TUPLE_TYPE, vec![int32.clone(), int32.clone()]).into()
        );

        let ast = parse_expression("[x for (x, y) in [(1, 2), (2, 3), (3, 4)]][0]").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result.unwrap().unwrap(), int32);

        let ast =
            parse_expression("[x for (x, y) in [(1, 2), (2, 3), (3, 4)] if x > 0][0]").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result.unwrap().unwrap(), int32);

        let ast = parse_expression("[x for (x, y) in [(1, 2), (2, 3), (3, 4)] if x][0]").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result, Err("test must be bool".into()));

        let ast = parse_expression("[y for x in []][0]").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result, Err("unbounded identifier".into()));

        let ast = parse_expression("[none() for x in []][0]").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result, Err("no value".into()));

        let ast = parse_expression("[z for z in []][0]").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(result, Err("duplicated naming".into()));

        let ast = parse_expression("[x for x in [] for y in []]").unwrap();
        let result = infer_expr(&mut ctx, &ast);
        assert_eq!(
            result,
            Err("only 1 generator statement is supported".into())
        );
    }
}
