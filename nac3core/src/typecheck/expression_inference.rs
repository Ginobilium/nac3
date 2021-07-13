use std::convert::TryInto;
use std::fs::create_dir_all;

use crate::typecheck::context::InferenceContext;
use crate::typecheck::inference_core;
use crate::typecheck::magic_methods;
use crate::typecheck::typedef::{Type, TypeEnum};
use crate::typecheck::primitives;
use rustpython_parser::ast;
use rustpython_parser::ast::fold::Fold;

use super::inference_core::resolve_call;

pub struct ExpressionTypeInferencer<'a> {
    pub ctx: InferenceContext<'a>
}

impl<'a> ExpressionTypeInferencer<'a> { // NOTE: add location here in the function parameter for better error message?
    
    fn infer_constant_val(&self, constant: &ast::Constant) -> Result<Option<Type>, String> {
        match constant {
            ast::Constant::Bool(_) => 
                Ok(Some(self.ctx.get_primitive(primitives::BOOL_TYPE))),
            
            ast::Constant::Int(val) => {
                let int32: Result<i32, _> = val.try_into();
                let int64: Result<i64, _> = val.try_into();
                
                if int32.is_ok() {
                    Ok(Some(self.ctx.get_primitive(primitives::INT32_TYPE)))
                } else if int64.is_ok() {
                    Ok(Some(self.ctx.get_primitive(primitives::INT64_TYPE)))
                } else {
                    Err("Integer out of bound".into())
                }
            },
    
            ast::Constant::Float(_) => 
                Ok(Some(self.ctx.get_primitive(primitives::FLOAT_TYPE))),

            ast::Constant::Tuple(vals) => {
                let result = vals
                    .into_iter()
                    .map(|x| self.infer_constant_val(x))
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


    fn infer_list_val(&self, elts: &Vec<ast::Expr<Option<Type>>>) -> Result<Option<Type>, String> {
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

    fn infer_tuple_val(&self, elts: &Vec<ast::Expr<Option<Type>>>) -> Result<Option<Type>, String> {
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

    fn infer_arrtibute(&self, value: &Box<ast::Expr<Option<Type>>>, attr: &str) -> Result<Option<Type>, String> {
        let ty = value.custom.clone().ok_or_else(|| "no value".to_string())?;
        if let TypeEnum::TypeVariable(id) = ty.as_ref() {
            let v = self.ctx.get_variable_def(*id);
            if v.bound.is_empty() {
                return Err("no fields on unbounded type variable".into());
            }
            let ty = v.bound[0].get_base(&self.ctx).and_then(|v| v.fields.get(attr));
            if ty.is_none() {
                return Err("unknown field".into());
            }
            for x in v.bound[1..].iter() {
                let ty1 = x.get_base(&self.ctx).and_then(|v| v.fields.get(attr));
                if ty1 != ty {
                    return Err("unknown field (type mismatch between variants)".into());
                }
            }
            return Ok(Some(ty.unwrap().clone()));
        }
        
        match ty.get_base(&self.ctx) {
            Some(b) => match b.fields.get(attr) {
                Some(t) => Ok(Some(t.clone())),
                None => Err("no such field".into()),
            },
            None => Err("this object has no fields".into()),
        }
    }

    fn infer_bool_ops(&self, values: &Vec<ast::Expr<Option<Type>>>) -> Result<Option<Type>, String> {
        assert_eq!(values.len(), 2); 
        let left = values[0].custom.clone().ok_or_else(|| "no value".to_string())?;
        let right = values[1].custom.clone().ok_or_else(|| "no value".to_string())?;
        let b = self.ctx.get_primitive(primitives::BOOL_TYPE);
        if left == b && right == b {
            Ok(Some(b))
        } else {
            Err("bool operands must be bool".to_string())
        }
    }

    fn _infer_bin_ops(&self, _left: &Box<ast::Expr<Option<Type>>>, _op: &ast::Operator, _right: &Box<ast::Expr<Option<Type>>>) -> Result<Option<Type>, String> {
        Err("no need this function".into())
    }

    fn infer_unary_ops(&self, op: &ast::Unaryop, operand: &Box<ast::Expr<Option<Type>>>) -> Result<Option<Type>, String> {
        if let ast::Unaryop::Not = op {
            if (**operand).custom == Some(self.ctx.get_primitive(primitives::BOOL_TYPE)) {
                Ok(Some(self.ctx.get_primitive(primitives::BOOL_TYPE)))
            } else {
                Err("logical not must be applied to bool".into())
            }
        } else {
            inference_core::resolve_call(&self.ctx, (**operand).custom.clone(), magic_methods::unaryop_name(op), &[])
        }
    }

    fn infer_compare(&self, left: &Box<ast::Expr<Option<Type>>>, ops: &Vec<ast::Cmpop>, comparators: &Vec<ast::Expr<Option<Type>>>) -> Result<Option<Type>, String> {
        assert!(comparators.len() > 0);
        if left.custom.is_none() || (!comparators.iter().all(|x| x.custom.is_some())) {
            Err("comparison operands must have type".into())
        } else {
            let bool_type = Some(self.ctx.get_primitive(primitives::BOOL_TYPE));
            let ty_first = resolve_call(
                &self.ctx, 
                Some(left.custom.clone().ok_or_else(|| "comparator must be able to be typed".to_string())?.clone()), 
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
                    let ty = resolve_call(
                        &self.ctx, 
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

    fn infer_call(&self, func: &Box<ast::Expr<Option<Type>>>, args: &Vec<ast::Expr<Option<Type>>>, _keywords: &Vec<ast::Keyword<Option<Type>>>) -> Result<Option<Type>, String> {
        if args.iter().all(|x| x.custom.is_some()) {
            match &func.node {
                ast::ExprKind::Name {id, ctx: _} 
                    => resolve_call(
                        &self.ctx, 
                        None, 
                        id, 
                        &args.iter().map(|x| x.custom.clone().unwrap()).collect::<Vec<_>>()),
                
                ast::ExprKind::Attribute {value, attr, ctx: _}
                    => resolve_call(
                        &self.ctx, 
                        Some(value.custom.clone().ok_or_else(|| "no value".to_string())?), 
                        &attr, 
                        &args.iter().map(|x| x.custom.clone().unwrap()).collect::<Vec<_>>()),
                
                _ => Err("not supported".into())
            }
        } else {
            Err("function params must have type".into())
        }
    }

    fn infer_subscript(&self, value: &Box<ast::Expr<Option<Type>>>, slice: &Box<ast::Expr<Option<Type>>>) -> Result<Option<Type>, String> {
        // let tt = value.custom.ok_or_else(|| "no value".to_string())?.as_ref();

        let t = if let TypeEnum::ParametricType(primitives::LIST_TYPE, ls) = value.custom.as_ref().ok_or_else(|| "no value".to_string())?.as_ref() {
            ls[0].clone()
        } else {
            return Err("subscript is not supported for types other than list".into());
        };

        if let ast::ExprKind::Slice {lower, upper, step} = &slice.node {
            let int32_type = self.ctx.get_primitive(primitives::INT32_TYPE);
            let l = lower.as_ref().map_or(
                Ok(&int32_type),
                |x| x.custom.as_ref().ok_or("lower bound cannot be typped".to_string()))?;
            let u = upper.as_ref().map_or(
                Ok(&int32_type),
                |x| x.custom.as_ref().ok_or("upper bound cannot be typped".to_string()))?;
            let s = step.as_ref().map_or(
                Ok(&int32_type),
                |x| x.custom.as_ref().ok_or("step cannot be typped".to_string()))?;
            
            if l == &int32_type && u == &int32_type && s == &int32_type {
                Ok(value.custom.clone())
            } else {
                Err("slice must be int32 type".into())
            }
        } else if slice.custom == Some(self.ctx.get_primitive(primitives::INT32_TYPE)) {
            Ok(Some(t))
        } else {
            Err("slice or index must be int32 type".into())
        }
    }

    fn infer_if_expr(&self, test: &Box<ast::Expr<Option<Type>>>, body: &Box<ast::Expr<Option<Type>>>, orelse: &Box<ast::Expr<Option<Type>>>) -> Result<Option<Type>, String> {
        if test.custom != Some(self.ctx.get_primitive(primitives::BOOL_TYPE)) {
            Err("test should be bool".into())
        } else {
            if body.custom == orelse.custom {
                Ok(body.custom.clone())
            } else {
                Err("divergent type at if expression".into())
            }
        }
    }

    fn infer_list_comprehesion(&mut self, elt: &Box<ast::Expr<Option<Type>>>, generators: &Vec<ast::Comprehension<Option<Type>>>) -> Result<Option<Type>, String> {
        if generators[0]
            .ifs
            .iter()
            .all(|x| x.custom == Some(self.ctx.get_primitive(primitives::BOOL_TYPE))) {
                Ok(Some(TypeEnum::ParametricType(
                    primitives::LIST_TYPE, 
                    vec![elt.custom.clone().ok_or_else(|| "elements should have value".to_string())?]).into()))
            } else {
                Err("test must be bool".into())
            }       
    }

    fn fold_comprehension_first(&mut self, node: ast::Comprehension<Option<Type>>) -> Result<ast::Comprehension<Option<Type>>, String> {
        Ok(ast::Comprehension {
            target: node.target,
            iter: Box::new(self.fold_expr(*node.iter)?),
            ifs: node.ifs,
            is_async: node.is_async
        })
    }

    fn fold_comprehension_second(&mut self, node: ast::Comprehension<Option<Type>>) -> Result<ast::Comprehension<Option<Type>>, String> {
        Ok(ast::Comprehension {
            target: Box::new(self.fold_expr(*node.target)?),
            iter: node.iter,
            ifs: node
                .ifs
                .into_iter()
                .map(|x| self.fold_expr(x))
                .collect::<Result<Vec<_>, _>>()?,
            is_async: node.is_async
        })
    }

    fn infer_simple_binding(&mut self, name: &ast::Expr<Option<Type>>, ty: Type) -> Result<(), String> {
        match &name.node {
            ast::ExprKind::Name {id, ctx: _} => {
                if id == "_" {
                    Ok(())
                } else if self.ctx.defined(id) {
                    Err("duplicated naming".into())
                } else {
                    self.ctx.assign(id.clone(), ty, name.location)?;
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


impl<'a> ast::fold::Fold<Option<Type>> for ExpressionTypeInferencer<'a> {
    type TargetU = Option<Type>;
    type Error = String;

    fn map_user(&mut self, user: Option<Type>) -> Result<Self::TargetU, Self::Error> {
        Ok(user)
    }
    
    fn fold_expr(&mut self, node: ast::Expr<Option<Type>>) -> Result<ast::Expr<Self::TargetU>, Self::Error> {
        assert_eq!(node.custom, None); // NOTE: should pass
        let mut expr = node;
        
        if let ast::Expr {location, custom, node: ast::ExprKind::ListComp {elt, generators } } = expr { 
            // is list comprehension, only fold generators which does not include unknown identifiers introduced by list comprehension
            if generators.len() != 1 {
                return Err("only 1 generator statement is supported".into())
            }
            let generators_first_folded = generators
                .into_iter()
                .map(|x| self.fold_comprehension_first(x)).collect::<Result<Vec<_>, _>>()?;

            let gen = &generators_first_folded[0];
            let iter_type = gen.iter.custom.as_ref().ok_or("no value".to_string())?.as_ref();

            if let TypeEnum::ParametricType(primitives::LIST_TYPE, ls) = iter_type {
                self.ctx.stack.level += 1; // FIXME: how to use with_scope??

                self.infer_simple_binding(&gen.target, ls[0].clone())?;
                expr = ast::Expr {
                    location,
                    custom,
                    node: ast::ExprKind::ListComp {
                        elt: Box::new(self.fold_expr(*elt)?),
                        generators: generators_first_folded
                            .into_iter()
                            .map(|x| self.fold_comprehension_second(x))
                            .collect::<Result<Vec<_>, _>>()?
                    }
                };

                self.ctx.stack.level -= 1;
                while !self.ctx.stack.sym_def.is_empty() {
                    let (_, level) = self.ctx.stack.sym_def.last().unwrap();
                    if *level > self.ctx.stack.level {
                        let (name, _) = self.ctx.stack.sym_def.pop().unwrap();
                        let (t, b, l) = self.ctx.sym_table.get_mut(&name).unwrap();
                        // set it to be unreadable
                        *b = false;
                    } else {
                        break;
                    }
                }

            } else {
                return Err("iteration is supported for list only".into());
            }
        } else {
            // if not listcomp which requires special handling, skip current level, make sure child nodes have their type
            expr = ast::fold::fold_expr(self, expr)?;
        }

        match &expr.node {
            ast::ExprKind::Constant {value, kind: _} => 
            Ok(ast::Expr {
                    location: expr.location, 
                    custom: self.infer_constant_val(value)?, 
                    node: expr.node
                }),
                
            ast::ExprKind::Name {id, ctx: _} =>
            Ok(ast::Expr {
                location: expr.location, 
                custom: Some(self.ctx.resolve(id)?),
                    node: expr.node
                }), 
                
            ast::ExprKind::List {elts, ctx: _} => {
                Ok(ast::Expr {
                    location: expr.location,
                    custom: self.infer_list_val(elts)?,
                    node: expr.node
                })
            }

            ast::ExprKind::Tuple {elts, ctx: _} => 
                Ok(ast::Expr {
                    location: expr.location,
                    custom: self.infer_tuple_val(elts)?,
                    node: expr.node
                }),

            ast::ExprKind::Attribute {value, attr, ctx: _} => 
                Ok(ast::Expr {
                    location: expr.location,
                    custom: self.infer_arrtibute(value, attr)?,
                    node: expr.node
                }),

            ast::ExprKind::BoolOp {op: _, values} => 
                Ok(ast::Expr {
                    location: expr.location,
                    custom: self.infer_bool_ops(values)?,
                    node: expr.node
                }),

            ast::ExprKind::BinOp {left, op, right} => 
                Ok(ast::Expr {
                    location: expr.location,
                    custom: inference_core::resolve_call(
                        &self.ctx, 
                        Some(left.custom.clone().ok_or_else(|| "no value".to_string())?), 
                        magic_methods::binop_name(op), 
                        &[right.custom.clone().ok_or_else(|| "no value".to_string())?])?,
                    node: expr.node
                }),

            ast::ExprKind::UnaryOp {op, operand} => 
                Ok(ast::Expr {
                    location: expr.location,
                    custom: self.infer_unary_ops(op, operand)?,
                    node: expr.node
                }),

            ast::ExprKind::Compare {left, ops, comparators} => 
                Ok(ast::Expr {
                    location: expr.location,
                    custom: self.infer_compare(left, ops, comparators)?,
                    node: expr.node
                }),

            ast::ExprKind::Call {func, args, keywords} => 
                Ok(ast::Expr {
                    location: expr.location,
                    custom: self.infer_call(func, args, keywords)?,
                    node: expr.node
                }),

            /* // REVIEW: add a new primitive type for slice and do type check of bounds here?
            ast::ExprKind::Slice {lower, upper, step } => 
                Ok(ast::Expr {
                    location: expr.location,
                    custom: self.infer_slice(lower, upper, step)?, 
                    node: expr.node
                }), */

            ast::ExprKind::Subscript {value, slice, ctx: _} => 
                Ok(ast::Expr {
                    location: expr.location,
                    custom: self.infer_subscript(value, slice)?,
                    node: expr.node
                }),

            ast::ExprKind::IfExp {test, body, orelse} => 
                Ok(ast::Expr {
                    location: expr.location,
                    custom: self.infer_if_expr(test, body, orelse)?,
                    node: expr.node
                }),

            ast::ExprKind::ListComp {elt, generators} => {

                Ok(ast::Expr {
                    location: expr.location,
                    custom: self.infer_list_comprehesion(elt, generators)?,
                    node: expr.node
                })
            }

            _ => { // not supported
                Err("not supported yet".into())
            }
        }
    }
}

pub mod test {

    use crate::typecheck::{symbol_resolver::SymbolResolver, typedef::*, symbol_resolver::*, location::*};
    use rustpython_parser::ast::{self, Expr, fold::Fold};
    use super::*;
    
    pub fn new_ctx<'a>() -> ExpressionTypeInferencer<'a>{
        struct S;

        impl SymbolResolver for S {
            fn get_symbol_location(&self, _str: &str) -> Option<Location> {
                None
            }
        
            fn get_symbol_type(&self, _str: &str) -> Option<SymbolType> {
                None
            }
        
            fn get_symbol_value(&self, _str: &str) -> Option<SymbolValue> {
                None
            }
        }

        ExpressionTypeInferencer {
            ctx: InferenceContext::new(primitives::basic_ctx(), Box::new(S{}), FileID(3)),
        }
    }


    #[test]
    fn test_i64() {
        let mut inferencer = new_ctx();

        let location = ast::Location::new(0, 0);
        let num: i64 = 99999999999;

        let ast: Expr<Option<Type>> = Expr {
            location: location,
            custom: None,
            node: ast::ExprKind::Constant {
                value: ast::Constant::Int(num.into()),
                kind: None,
            }
        };
        
        let new_ast = inferencer.fold_expr(ast).unwrap();

        assert_eq!(
            new_ast, 
            Expr {
                location: location,
                custom: Some(inferencer.ctx.get_primitive(primitives::INT64_TYPE)),
                node: ast::ExprKind::Constant {
                    value: ast::Constant::Int(num.into()),
                    kind: None,
                }
            }
        );
    }

    #[test]
    fn test_list() {
        let mut inferencer = new_ctx();
        let location = ast::Location::new(0, 0);

        let ast: Expr<Option<Type>> = Expr {
            location,
            custom: None,
            node: ast::ExprKind::List {
                ctx: ast::ExprContext::Load,
                elts: vec![
                    Expr {
                        location,
                        custom: None,
                        node: ast::ExprKind::Constant {
                            value: ast::Constant::Int(1.into()),
                            kind: None,
                        },
                    },

                    Expr {
                        location,
                        custom: None,
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
                custom: Some(TypeEnum::ParametricType(primitives::LIST_TYPE, vec![inferencer.ctx.get_primitive(primitives::INT32_TYPE).into()]).into()),
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

}
