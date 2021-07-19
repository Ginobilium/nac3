use std::convert::TryInto;

use crate::typecheck::context::InferenceContext;
use crate::typecheck::inference_core;
use crate::typecheck::magic_methods;
use crate::typecheck::typedef::{Type, TypeEnum};
use crate::typecheck::primitives;
use rustpython_parser::ast;
use rustpython_parser::ast::fold::Fold;

impl<'a> ast::fold::Fold<()> for InferenceContext<'a> {
    type TargetU = Option<Type>;
    type Error = String;

    fn map_user(&mut self, _user: ()) -> Result<Self::TargetU, Self::Error> {
        Ok(None)
    }

    fn fold_expr(&mut self, node: ast::Expr<()>) -> Result<ast::Expr<Self::TargetU>, Self::Error> {
        // assert_eq!(node.custom, None);
        
        let expr = match &node.node {
            ast::ExprKind::ListComp { .. } => return self.fold_listcomp(node),
            _ => rustpython_parser::ast::fold::fold_expr(self, node)?
        };

        Ok(ast::Expr {
            // compute type info and store in the custom field
            custom: match &expr.node {
                ast::ExprKind::Constant {value, kind: _} => self.infer_constant(value),
                ast::ExprKind::Name {id, ctx: _} => Ok(Some(self.resolve(id)?)),
                ast::ExprKind::List {elts, ctx: _} => self.infer_list(elts),
                ast::ExprKind::Tuple {elts, ctx: _} => self.infer_tuple(elts),
                ast::ExprKind::Attribute {value, attr, ctx: _} => self.infer_arrtibute(value, attr),
                ast::ExprKind::BoolOp {op: _, values} => self.infer_bool_ops(values),
                ast::ExprKind::BinOp {left, op, right} => self.infer_bin_ops(left, op, right),
                ast::ExprKind::UnaryOp {op, operand} => self.infer_unary_ops(op, operand),
                ast::ExprKind::Compare {left, ops, comparators} => self.infer_compare(left, ops, comparators),
                ast::ExprKind::Call {func, args, keywords} => self.infer_call(func, args, keywords),
                ast::ExprKind::Subscript {value, slice, ctx: _} => self.infer_subscript(value, slice),
                ast::ExprKind::IfExp {test, body, orelse} => self.infer_if_expr(test, body, orelse),
                ast::ExprKind::ListComp {elt: _, generators: _} => panic!("should not earch here, the list comp should be folded before"), // already folded
                ast::ExprKind::Slice { .. } => Ok(None), // special handling for slice, which is supported
                _ => Err("not supported yet".into())
            }?,
            location: expr.location,
            node: expr.node
        })
    }
}

impl<'a> InferenceContext<'a> {
    fn infer_constant(&self, constant: &ast::Constant) -> Result<Option<Type>, String> {
        match constant {
            ast::Constant::Bool(_) => 
                Ok(Some(self.get_primitive(primitives::BOOL_TYPE))),
            
            ast::Constant::Int(val) => {
                let int32: Result<i32, _> = val.try_into();
                let int64: Result<i64, _> = val.try_into();
                
                if int32.is_ok() {
                    Ok(Some(self.get_primitive(primitives::INT32_TYPE)))
                } else if int64.is_ok() {
                    Ok(Some(self.get_primitive(primitives::INT64_TYPE)))
                } else {
                    Err("Integer out of bound".into())
                }
            },
    
            ast::Constant::Float(_) => 
                Ok(Some(self.get_primitive(primitives::FLOAT_TYPE))),

            ast::Constant::Tuple(vals) => {
                let result = vals
                    .iter()
                    .map(|x| self.infer_constant(x))
                    .collect::<Vec<_>>();
                
                if result.iter().all(|x| x.is_ok()) {
                    Ok(Some(TypeEnum::ParametricType(
                        primitives::TUPLE_TYPE, 
                        result
                            .into_iter()
                            .map(|x| x.unwrap().unwrap())
                            .collect::<Vec<_>>(),
                    ).into()))
                } else {
                    Err("Some elements in tuple cannot be typed".into())
                }
            }

            _ => Err("not supported".into())
        }
    }

    fn infer_list(&self, elts: &[ast::Expr<Option<Type>>]) -> Result<Option<Type>, String> {
        if elts.is_empty() {
            Ok(Some(TypeEnum::ParametricType(primitives::LIST_TYPE, vec![TypeEnum::BotType.into()]).into()))
        } else {
            let types = elts
                .iter()
                .map(|x| &x.custom)
                .collect::<Vec<_>>();
            
            if types.iter().all(|x| x.is_some()) {
                let head = types.iter().next().unwrap(); // here unwrap alone should be fine after the previous check
                if types.iter().all(|x| x.eq(head)) {
                    Ok(Some(TypeEnum::ParametricType(primitives::LIST_TYPE, vec![(*head).clone().unwrap()]).into()))
                } else {
                    Err("inhomogeneous list is not allowed".into())
                }
            } else {
                Err("list elements must have some type".into())
            }
        }
    }

    fn infer_tuple(&self, elts: &[ast::Expr<Option<Type>>]) -> Result<Option<Type>, String> {
        let types = elts
            .iter()
            .map(|x| (x.custom).clone())
            .collect::<Vec<_>>();

        if types.iter().all(|x| x.is_some()) {
            Ok(Some(TypeEnum::ParametricType(
                primitives::TUPLE_TYPE, 
                types.into_iter().map(|x| x.unwrap()).collect()).into())) // unwrap alone should be fine after the previous check
        } else {
            Err("tuple elements must have some type".into())
        }
    }

    fn infer_arrtibute(&self, value: &ast::Expr<Option<Type>>, attr: &str) -> Result<Option<Type>, String> {
        let ty = value.custom.clone().ok_or_else(|| "no value".to_string())?;
        if let TypeEnum::TypeVariable(id) = ty.as_ref() {
            let v = self.get_variable_def(*id);
            if v.bound.is_empty() {
                return Err("no fields on unbounded type variable".into());
            }
            let ty = v.bound[0].get_base(&self).and_then(|v| v.fields.get(attr));
            if ty.is_none() {
                return Err("unknown field".into());
            }
            for x in v.bound[1..].iter() {
                let ty1 = x.get_base(&self).and_then(|v| v.fields.get(attr));
                if ty1 != ty {
                    return Err("unknown field (type mismatch between variants)".into());
                }
            }
            return Ok(Some(ty.unwrap().clone()));
        }
        
        match ty.get_base(&self) {
            Some(b) => match b.fields.get(attr) {
                Some(t) => Ok(Some(t.clone())),
                None => Err("no such field".into()),
            },
            None => Err("this object has no fields".into()),
        }
    }

    fn infer_bool_ops(&self, values: &[ast::Expr<Option<Type>>]) -> Result<Option<Type>, String> {
        assert_eq!(values.len(), 2); 
        let left = values[0].custom.clone().ok_or_else(|| "no value".to_string())?;
        let right = values[1].custom.clone().ok_or_else(|| "no value".to_string())?;
        let b = self.get_primitive(primitives::BOOL_TYPE);
        if left == b && right == b {
            Ok(Some(b))
        } else {
            Err("bool operands must be bool".to_string())
        }
    }

    fn infer_bin_ops(&self, left: &ast::Expr<Option<Type>>, op: &ast::Operator, right: &ast::Expr<Option<Type>>) -> Result<Option<Type>, String> {
        inference_core::resolve_call(
            &self, 
            Some(left.custom.clone().ok_or_else(|| "no value".to_string())?), 
            magic_methods::binop_name(op), 
            &[right.custom.clone().ok_or_else(|| "no value".to_string())?])
    }

    fn infer_unary_ops(&self, op: &ast::Unaryop, operand: &ast::Expr<Option<Type>>) -> Result<Option<Type>, String> {
        if let ast::Unaryop::Not = op {
            if operand.custom == Some(self.get_primitive(primitives::BOOL_TYPE)) {
                Ok(Some(self.get_primitive(primitives::BOOL_TYPE)))
            } else {
                Err("logical not must be applied to bool".into())
            }
        } else {
            inference_core::resolve_call(&self, operand.custom.clone(), magic_methods::unaryop_name(op), &[])
        }
    }

    fn infer_compare(&self, left: &ast::Expr<Option<Type>>, ops: &[ast::Cmpop], comparators: &[ast::Expr<Option<Type>>]) -> Result<Option<Type>, String> {
        if left.custom.is_none() || (!comparators.iter().all(|x| x.custom.is_some())) {
            Err("comparison operands must have type".into())
        } else {
            let bool_type = Some(self.get_primitive(primitives::BOOL_TYPE));
            let ty_first = inference_core::resolve_call(
                &self, 
                Some(left.custom.clone().ok_or_else(|| "comparator must be able to be typed".to_string())?), 
                magic_methods::comparison_name(&ops[0]).ok_or_else(|| "unsupported comparison".to_string())?, 
                &[comparators[0].custom.clone().ok_or_else(|| "comparator must be able to be typed".to_string())?])?;
            if ty_first != bool_type {
                return Err("comparison result must be boolean".into());
            }

            for ((a, b), op) 
            in comparators[..(comparators.len() - 1)]
                .iter()
                .zip(comparators[1..].iter())
                .zip(ops[1..].iter()) {
                    let ty = inference_core::resolve_call(
                        &self, 
                        Some(a.custom.clone().ok_or_else(|| "comparator must be able to be typed".to_string())?.clone()), 
                        magic_methods::comparison_name(op).ok_or_else(|| "unsupported comparison".to_string())?,
                        &[b.custom.clone().ok_or_else(|| "comparator must be able to be typed".to_string())?.clone()])?;
                    if ty != bool_type {
                        return Err("comparison result must be boolean".into());
                    }
                }
            Ok(bool_type)
        }
    }

    fn infer_call(&self, func: &ast::Expr<Option<Type>>, args: &[ast::Expr<Option<Type>>], _keywords: &[ast::Keyword<Option<Type>>]) -> Result<Option<Type>, String> {
        if args.iter().all(|x| x.custom.is_some()) {
            match &func.node {
                ast::ExprKind::Name {id, ctx: _} 
                    => inference_core::resolve_call(
                        &self, 
                        None, 
                        id, 
                        &args.iter().map(|x| x.custom.clone().unwrap()).collect::<Vec<_>>()),
                
                ast::ExprKind::Attribute {value, attr, ctx: _}
                    => inference_core::resolve_call(
                        &self, 
                        Some(value.custom.clone().ok_or_else(|| "no value".to_string())?), 
                        &attr, 
                        &args.iter().map(|x| x.custom.clone().unwrap()).collect::<Vec<_>>()),
                
                _ => Err("not supported".into())
            }
        } else {
            Err("function params must have type".into())
        }
    }

    fn infer_subscript(&self, value: &ast::Expr<Option<Type>>, slice: &ast::Expr<Option<Type>>) -> Result<Option<Type>, String> {
        let val_type = value.custom.as_ref().ok_or_else(|| "no value".to_string())?.as_ref();
        if let TypeEnum::ParametricType(primitives::LIST_TYPE, ls) = val_type {
            if let ast::ExprKind::Slice {lower, upper, step} = &slice.node {
                let int32_type = self.get_primitive(primitives::INT32_TYPE);
                let l = lower.as_ref().map_or(
                    Ok(&int32_type),
                    |x| x.custom.as_ref().ok_or_else(|| "lower bound cannot be typped".to_string()))?;
                let u = upper.as_ref().map_or(
                    Ok(&int32_type),
                    |x| x.custom.as_ref().ok_or_else(|| "upper bound cannot be typped".to_string()))?;
                let s = step.as_ref().map_or(
                    Ok(&int32_type),
                    |x| x.custom.as_ref().ok_or_else(|| "step cannot be typped".to_string()))?;
                
                if l == &int32_type && u == &int32_type && s == &int32_type {
                    Ok(value.custom.clone())
                } else {
                    Err("slice must be int32 type".into())
                }
            } else if slice.custom == Some(self.get_primitive(primitives::INT32_TYPE)) {
                Ok(Some(ls[0].clone()))
            } else {
                Err("slice or index must be int32 type".into())
            }
        } else if let TypeEnum::ParametricType(primitives::TUPLE_TYPE, ls) = val_type {
            if let ast::ExprKind::Constant {kind: _, value: ast::Constant::Int(val)} = &slice.node {
                let ind: Result<usize, _> = val.try_into();
                if ind.is_ok() && ind.unwrap() < ls.len() {
                    Ok(Some(ls[ind.unwrap()].clone()))
                } else {
                    Err("tuple constant index out of range".into())
                }
            } else {
                Err("tuple index can only be constant".into())
            }
        } else {
            Err("subscript is not supported for types other than list or tuple".into())
        }
    }

    fn infer_if_expr(&self, test: &ast::Expr<Option<Type>>, body: &ast::Expr<Option<Type>>, orelse: &ast::Expr<Option<Type>>) -> Result<Option<Type>, String> {
        if test.custom != Some(self.get_primitive(primitives::BOOL_TYPE)) {
            Err("test should be bool".into())
        } else if body.custom == orelse.custom {
            Ok(body.custom.clone())
        } else {
            Err("divergent type at if expression".into())
        }
    }

    fn _infer_list_comprehesion(&self, elt: &ast::Expr<Option<Type>>, generators: &[ast::Comprehension<Option<Type>>]) -> Result<Option<Type>, String> {
        if generators[0]
            .ifs
            .iter()
            .all(|x| x.custom == Some(self.get_primitive(primitives::BOOL_TYPE))) {
                Ok(Some(TypeEnum::ParametricType(
                    primitives::LIST_TYPE, 
                    vec![elt.custom.clone().ok_or_else(|| "elements should have value".to_string())?]).into()))
            } else {
                Err("test must be bool".into())
            }
    }

    // some pre-folds need special handling
    fn fold_listcomp(&mut self, expr: ast::Expr<()>) -> Result<ast::Expr<Option<Type>>, String> {
        if let ast::Expr {
            location, 
            custom: _, 
            node: ast::ExprKind::ListComp {
                elt, 
                mut generators}} = expr {
            // if is list comprehension, need special pre-fold
            if generators.len() != 1 {
                return Err("only 1 generator statement is supported".into());
            }
            let gen = generators.remove(0);
            if gen.is_async {
                return Err("async is not supported".into());
            }

            let ast::Comprehension {iter, 
                target, 
                ifs, 
                is_async} = gen;
            let iter_folded = Box::new(self.fold_expr(*iter)?);

            if let TypeEnum::ParametricType(
                primitives::LIST_TYPE, 
                ls) = iter_folded
                    .custom
                    .as_ref()
                    .ok_or_else(|| "no value".to_string())?
                    .as_ref()
                    .clone() {
                self.with_scope(|ctx| -> Result<ast::Expr<Option<Type>>, String> {
                    ctx.infer_simple_binding(&target, ls[0].clone())?;
                    let elt_folded = Box::new(ctx.fold_expr(*elt)?);
                    let target_folded = Box::new(ctx.fold_expr(*target)?);
                    let ifs_folded = ifs
                        .into_iter()
                        .map(|x| ctx.fold_expr(x))
                        .collect::<Result<Vec<ast::Expr<Option<Type>>>, _>>()?;
                    
                    if ifs_folded
                        .iter()
                        .all(|x| x.custom == Some(ctx.get_primitive(primitives::BOOL_TYPE))) {
                        Ok(ast::Expr {
                            location,
                            custom: Some(TypeEnum::ParametricType(
                                primitives::LIST_TYPE, 
                                vec![elt_folded
                                    .custom
                                    .clone()
                                    .ok_or_else(|| "elements cannot be typped".to_string())?]).into()),
                            node: ast::ExprKind::ListComp {
                                elt: elt_folded,
                                generators: vec![ast::Comprehension {
                                    target: target_folded,
                                    ifs: ifs_folded,
                                    iter: iter_folded,
                                    is_async
                                }]
                            }
                        })
                    } else {
                        Err("test must be bool".into())
                    }
                }).1
            } else {
                Err("iteration is supported for list only".into())
            }
        } else {
            panic!("this function is for list comprehensions only!");
        }
    }

    fn infer_simple_binding<T>(&mut self, name: &ast::Expr<T>, ty: Type) -> Result<(), String> {
        match &name.node {
            ast::ExprKind::Name {id, ctx: _} => {
                if id == "_" {
                    Ok(())
                } else if self.defined(id) {
                    Err("duplicated naming".into())
                } else {
                    self.assign(id.clone(), ty, name.location)?;
                    Ok(())
                }
            }
    
            ast::ExprKind::Tuple {elts, ctx: _} => {
                if let TypeEnum::ParametricType(primitives::TUPLE_TYPE, ls) = ty.as_ref() {
                    if elts.len() == ls.len() {
                        for (a, b) in elts.iter().zip(ls.iter()) {
                            self.infer_simple_binding(a, b.clone())?;
                        }
                        Ok(())
                    } else {
                        Err("different length".into())
                    }
                } else {
                    Err("not supported".into())
                }
            }
            _ => Err("not supported".into())
        }
    }
}

pub struct ExpressionInferencer<'a> {
    pub ctx: InferenceContext<'a>
}
impl<'a> ExpressionInferencer<'a> {
    pub fn fold_expr(&mut self, expr: ast::Expr) -> Result<ast::Expr<Option<Type>>, String> {
        self.ctx.fold_expr(expr)
    }
}

pub mod test {

    use crate::typecheck::{symbol_resolver::SymbolResolver, symbol_resolver::*, location::*};
    use rustpython_parser::ast::Expr;
    use super::*;
    
    pub fn new_ctx<'a>() -> ExpressionInferencer<'a> {
        struct S;
        impl SymbolResolver for S {
            fn get_symbol_location(&self, _str: &str) -> Option<Location> { None }
            fn get_symbol_type(&self, _str: &str) -> Option<SymbolType> { None }
            fn get_symbol_value(&self, _str: &str) -> Option<SymbolValue> { None }
        }

        ExpressionInferencer {ctx: InferenceContext::new(primitives::basic_ctx(), Box::new(S{}), FileID(3))}
    }

    #[test]
    fn test_i32() {
        let mut inferencer = new_ctx();
        let ast: Expr = Expr {
            location: ast::Location::new(0, 0),
            custom: (),
            node: ast::ExprKind::Constant {
                value: ast::Constant::Int(123.into()),
                kind: None
            }
        };

        let new_ast = inferencer.fold_expr(ast);
        assert_eq!(
            new_ast,
            Ok(ast::Expr {
                location: ast::Location::new(0, 0),
                custom: Some(inferencer.ctx.get_primitive(primitives::INT32_TYPE)),
                node: ast::ExprKind::Constant {
                    value: ast::Constant::Int(123.into()),
                    kind: None
                }
            })
        );
    }

    #[test]
    fn test_i64() {
        let mut inferencer = new_ctx();

        let location = ast::Location::new(0, 0);
        let num: i64 = 99999999999;

        let ast: Expr = Expr {
            location,
            custom: (),
            node: ast::ExprKind::Constant {
                value: ast::Constant::Int(num.into()),
                kind: None,
            }
        };
        
        let new_ast = inferencer.fold_expr(ast).unwrap();

        assert_eq!(
            new_ast, 
            Expr {
                location,
                custom: Some(inferencer.ctx.get_primitive(primitives::INT64_TYPE)),
                node: ast::ExprKind::Constant {
                    value: ast::Constant::Int(num.into()),
                    kind: None,
                }
            }
        );
    }

    #[test]
    fn test_tuple() {
        let mut inferencer = new_ctx();
        let i32_t = inferencer.ctx.get_primitive(primitives::INT32_TYPE);
        let float_t = inferencer.ctx.get_primitive(primitives::FLOAT_TYPE);
        let ast = rustpython_parser::parser::parse_expression("(123, 123.123, 999999999)").unwrap();
        let loc = ast.location;
        let folded = inferencer.fold_expr(ast).unwrap();
        assert_eq!(
            folded,
            ast::Expr {
                location: loc,
                custom: Some(TypeEnum::ParametricType(primitives::TUPLE_TYPE, vec![i32_t.clone(), float_t.clone(), i32_t.clone()]).into()),
                node: ast::ExprKind::Tuple {
                    ctx: ast::ExprContext::Load, 
                    elts: vec![
                        ast::Expr {
                            location: ast::Location::new(1, 2),
                            custom: Some(i32_t.clone()),
                            node: ast::ExprKind::Constant {
                                value: ast::Constant::Int(123.into()),
                                kind: None
                            }
                        },
                        ast::Expr {
                            location: ast::Location::new(1, 7),
                            custom: Some(float_t),
                            node: ast::ExprKind::Constant {
                                value: ast::Constant::Float(123.123),
                                kind: None
                            }
                        },
                        ast::Expr {
                            location: ast::Location::new(1, 16),
                            custom: Some(i32_t),
                            node: ast::ExprKind::Constant {
                                value: ast::Constant::Int(999999999.into()),
                                kind: None
                            }
                        },
                    ]
                }
            }
        );

    }

    #[test]
    fn test_list() {
        let mut inferencer = new_ctx();
        let location = ast::Location::new(0, 0);

        let ast: Expr = Expr {
            location,
            custom: (),
            node: ast::ExprKind::List {
                ctx: ast::ExprContext::Load,
                elts: vec![
                    Expr {
                        location,
                        custom: (),
                        node: ast::ExprKind::Constant {
                            value: ast::Constant::Int(1.into()),
                            kind: None,
                        },
                    },

                    Expr {
                        location,
                        custom: (),
                        node: ast::ExprKind::Constant {
                            value: ast::Constant::Int(2.into()),
                            kind: None,
                        },
                    },
                ],
            }
        };

        let new_ast = inferencer.fold_expr(ast).unwrap();
        assert_eq!(
            new_ast,
            Expr {
                location,
                custom: Some(TypeEnum::ParametricType(primitives::LIST_TYPE, vec![inferencer.ctx.get_primitive(primitives::INT32_TYPE)]).into()),
                node: ast::ExprKind::List {
                    ctx: ast::ExprContext::Load,
                    elts: vec![
                        Expr {
                            location,
                            custom: Some(inferencer.ctx.get_primitive(primitives::INT32_TYPE)),
                            node: ast::ExprKind::Constant {
                                value: ast::Constant::Int(1.into()),
                                kind: None,
                            },
                        },

                        Expr {
                            location,
                            custom: Some(inferencer.ctx.get_primitive(primitives::INT32_TYPE)),
                            // custom: None,
                            node: ast::ExprKind::Constant {
                                value: ast::Constant::Int(2.into()),
                                kind: None,
                            },
                        },
                    ],
                }
            }
        );
    }

    #[test]
    fn test_mix() {
        let mut inf = new_ctx();
        let ast1 = rustpython_parser::parser::parse_expression("False == [True or True, False][0]").unwrap();
        let ast2 = rustpython_parser::parser::parse_expression("False == [True or True, False][0]").unwrap();
        let ast3 = rustpython_parser::parser::parse_expression("1 < 2 < 3").unwrap();
        let ast4 = rustpython_parser::parser::parse_expression("1 + [12312, 1231][0]").unwrap();
        let ast5 = rustpython_parser::parser::parse_expression("not True").unwrap();
        let ast6 = rustpython_parser::parser::parse_expression("[[1]][0][0]").unwrap();
        let ast7 = rustpython_parser::parser::parse_expression("[[1]][0]").unwrap();
        let ast8 = rustpython_parser::parser::parse_expression("[[(1, 2), (2, 3), (3, 4)], [(2, 4), (4, 6)]][0]").unwrap();
        let ast9 = rustpython_parser::parser::parse_expression("[1, 2, 3, 4, 5][1: 2]").unwrap();
        let ast10 = rustpython_parser::parser::parse_expression("4 if False and True else 8").unwrap();
        let ast11 = rustpython_parser::parser::parse_expression("(1, 2, 3, 4)[1]").unwrap();
        let ast12 = rustpython_parser::parser::parse_expression("(1, True, 3, False)[1]").unwrap();
        
        let folded = inf.fold_expr(ast1).unwrap();
        let folded_2 = inf.fold_expr(ast2).unwrap();
        let folded_3 = inf.fold_expr(ast3).unwrap();
        let folded_4 = inf.fold_expr(ast4).unwrap();
        let folded_5 = inf.fold_expr(ast5).unwrap();
        let folded_6 = inf.fold_expr(ast6).unwrap();
        let folded_7 = inf.fold_expr(ast7).unwrap();
        let folded_8 = inf.fold_expr(ast8).unwrap();
        let folded_9 = inf.fold_expr(ast9).unwrap();
        let folded_10 = inf.fold_expr(ast10).unwrap();
        let folded_11 = inf.fold_expr(ast11).unwrap();
        let folded_12 = inf.fold_expr(ast12).unwrap();
        
        println!("{:?}", folded.custom);
        println!("{:?}", folded_2.custom);
        println!("{:?}", folded_3.custom);
        println!("{:?}", folded_4.custom);
        println!("{:?}", folded_5.custom);
        println!("{:?}", folded_6.custom);
        println!("{:?}", folded_7.custom);
        println!("{:?}", folded_8.custom);
        println!("{:?}", folded_9.custom);
        println!("{:?}", folded_10.custom);
        println!("{:?}", folded_11.custom);
        println!("{:?}", folded_12.custom);
    }
}