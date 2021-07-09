use std::convert::TryInto;

use crate::typecheck::context::InferenceContext;
use crate::typecheck::typedef::{Type, TypeEnum};
use crate::typecheck::primitives;
use rustpython_parser::ast;

use super::magic_methods;

pub struct ExpressionTypeInferencer<'a> {
    pub ctx: InferenceContext<'a>  //FIXME: may need to remove this pub
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
}

// REVIEW: field custom: from () to Option<Type> or just Option<Type>?
impl<'a> ast::fold::Fold<Option<Type>> for ExpressionTypeInferencer<'a> {
    type TargetU = Option<Type>;
    type Error = String;

    fn map_user(&mut self, user: Option<Type>) -> Result<Self::TargetU, Self::Error> {
        Ok(user)
    }

    fn fold_expr(&mut self, expr: ast::Expr<Option<Type>>) -> Result<ast::Expr<Self::TargetU>, Self::Error> {
        let ast::Expr {location, custom, node} = expr;
        match node {
            ast::ExprKind::Constant {value, kind} => 
                Ok(ast::Expr {
                    location, 
                    custom: self.infer_constant_val(&value)?, 
                    node: ast::ExprKind::Constant {value, kind}
                }),
            
            ast::ExprKind::Name {id, ctx} =>
                Ok(ast::Expr {
                    location, 
                    custom: Some(self.ctx.resolve(&*id)?),  // REVIEW: the conversion from String to &str is not sure
                    node: ast::ExprKind::Name {id, ctx}
                }), 

            ast::ExprKind::List {elts, ctx} => {
                /* let folded = ast::fold::fold_expr(
                    self, 
                    ast::Expr {location, custom, node: ast::ExprKind::List {elts, ctx}})?;

                if let ast::Expr {location: _, custom: _, node: ast::ExprKind::List {elts, ctx}} = folded {
                    Ok(ast::Expr {
                        location, 
                        custom: self.infer_list_val(&elts)?,
                        node: ast::ExprKind::List {elts, ctx}
                    })
                } else {
                    Err("something wrong here".into())
                } */
                
                let elts = elts
                    .into_iter()
                    .map(|x| self.fold_expr(x))
                    .collect::<Result<Vec<ast::Expr<Option<Type>>>, _>>()?; // elements inside the vector should now have type info
                
                Ok(ast::Expr {
                    location, 
                    custom: self.infer_list_val(&elts)?,
                    node: ast::ExprKind::List {elts, ctx}
                })
            }

            ast::ExprKind::Tuple {elts, ctx} => {
                // let folded_tup_expr = ast::fold::fold_expr(self, ast::Expr {location, custom, node})?;
                let elts= elts
                    .into_iter()
                    .map(|x| self.fold_expr(x))
                    .collect::<Result<Vec<ast::Expr<Option<Type>>>, _>>()?; // elements inside the vector should now have type info
                
                Ok(ast::Expr {
                    location,
                    custom: self.infer_tuple_val(&elts)?,
                    node: ast::ExprKind::Tuple {elts, ctx}
                })
            }

            ast::ExprKind::Attribute {value, attr, ctx} => {
                let folded_val = self.fold_expr(*value)?;

                match folded_val.custom {
                    Some(ref ty) => {
                        if let TypeEnum::TypeVariable(_) = ty.as_ref() {
                            Err("no fields for type variable".into())
                        } else {
                            ty
                                .clone()
                                .get_base(&self.ctx)
                                .and_then(|b| b.fields.get(&*attr).clone())
                                .map_or_else(
                                    || Err("no such field".into()), 
                                    |v| Ok(ast::Expr {
                                        location,
                                        custom: Some(v.clone()),
                                        node: ast::ExprKind::Attribute {value: Box::new(folded_val), attr, ctx}
                                    }))                            
                        }
                    },
                    None => Err("no value".into())
                }
            }

            ast::ExprKind::BoolOp {op, values} => {
                assert_eq!(values.len(), 2); // NOTE: should panic
                let folded = values
                    .into_iter()
                    .map(|x| self.fold_expr(x))
                    .collect::<Result<Vec<ast::Expr<Option<Type>>>, _>>()?;
                
                if (&folded)
                    .iter()
                    .all(|x| x.custom == Some(self.ctx.get_primitive(primitives::BOOL_TYPE))) {
                    Ok(ast::Expr {
                        location,
                        node: ast::ExprKind::BoolOp {op, values: folded},
                        custom: Some(self.ctx.get_primitive(primitives::BOOL_TYPE))
                    })
                } else {
                    Err("bool operands must be bool".into())
                }
            }

            ast::ExprKind::BinOp {op, left, right} => {
                let folded_left = self.fold_expr(*left)?;
                let folded_right = self.fold_expr(*right)?;
                let fun = magic_methods::binop_name(&op);
                let left_type = folded_left.custom.clone().ok_or_else(|| "no value".to_string())?;
                let right_type = folded_right.custom.clone().ok_or_else(|| "no value".to_string())?;

                let result = crate::typecheck::inference_core::resolve_call(
                    &self.ctx, 
                    Some(left_type), 
                    fun, 
                    &[right_type])?;
                
                Ok(ast::Expr {
                    location,
                    custom: result,
                    node: ast::ExprKind::BinOp {op, left: Box::new(folded_left), right: Box::new(folded_right)}
                })
            }
            
            ast::ExprKind::UnaryOp {op, operand} => {
                let folded = self.fold_expr(*operand)?;
                let ty = folded.custom.clone().ok_or_else(|| "no value".to_string())?;
                if let ast::Unaryop::Not = op {
                    if ty == self.ctx.get_primitive(primitives::BOOL_TYPE) {
                        Ok(ast::Expr {
                            location,
                            node: ast::ExprKind::UnaryOp {op, operand: Box::new(folded)},
                            custom: Some(self.ctx.get_primitive(primitives::BOOL_TYPE))
                        })
                    } else {
                        Err("logical not must be applied to bool".into())
                    }
                } else {
                    Ok(ast::Expr {
                        location,
                        custom: crate::typecheck::inference_core::resolve_call(
                            &self.ctx, 
                            Some(ty), 
                            magic_methods::unaryop_name(&op), 
                            &[])?,
                        node: ast::ExprKind::UnaryOp {op, operand: Box::new(folded)},
                    })
                }

            }
            
            ast::ExprKind::Compare {left, ops, comparators} => {
                Err("not sure".into()) // FIXME: what is the `left` field here?
            }

            ast::ExprKind::Call {func, args, keywords} => {
                if !keywords.is_empty() {
                    Err("keyword is not supported yet".into())
                } else {
                    let folded_args = args
                        .into_iter()
                        .map(|x| self.fold_expr(x))
                        .collect::<Result<Vec<ast::Expr<Option<Type>>>, _>>()?;

                    if !folded_args.iter().all(|x| x.custom.is_some()) {
                        Err("function params must have type".into())
                    } else {
                        match &func.node {
                            ast::ExprKind::Name {id, ctx} => {
                                Ok(ast::Expr {
                                    location,
                                    custom: crate::typecheck::inference_core::resolve_call(
                                        &self.ctx, 
                                        None, 
                                        id,
                                        &folded_args
                                                .iter()
                                                .map(|x| (x.custom.clone().unwrap()))
                                                .collect::<Vec<_>>())?,
                                    node: ast::ExprKind::Call {func, args: folded_args, keywords}
                                })
                            }

                            ast::ExprKind::Attribute {value, attr, ctx} => {
                                // Err("sdf".into())
                                let folded_value = self.fold_expr(**value)?;
                                Ok(ast::Expr {
                                    location,
                                    node: ast::ExprKind::Call {func, args: folded_args, keywords},
                                    custom: crate::typecheck::inference_core::resolve_call(
                                        &self.ctx, 
                                        folded_value.custom, 
                                        attr,
                                        &folded_args
                                                .iter()
                                                .map(|x| (x.custom.clone().unwrap()))
                                                .collect::<Vec<_>>())?
                                })
                            }


                            _ => Err("not supported".into())
                        }
                        // Err("sdf".into())
                    }

                }
            }


            _ => 
                Ok(ast::Expr {location, custom, node})
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
