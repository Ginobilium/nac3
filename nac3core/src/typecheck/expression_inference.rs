use std::convert::TryInto;

use crate::typecheck::context::InferenceContext;
use crate::typecheck::typedef::{Type, TypeEnum};
use crate::typecheck::primitives;
use rustpython_parser::ast;

pub struct ExpressionTypeInferencer<'a> {
    ctx: InferenceContext<'a>  
}

impl<'a> ExpressionTypeInferencer<'a>{ // NOTE: add location here in the function parameter for better error message?
    
    fn infer_constant_val(&mut self, constant: &ast::Constant) -> Result<Option<Type>, String> {
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


    fn infer_list_val(&mut self, elts: &Vec<ast::Expr<Option<Type>>>) -> Result<Option<Type>, String> {
        if elts.is_empty() {
            Ok(Some(TypeEnum::ParametricType(primitives::LIST_TYPE, vec![TypeEnum::BotType.into()]).into()))
        } else {
            let types = elts
                .into_iter()
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
}

// REVIEW: field custom: from None to Option<Type> or just Option<Type> ?
impl<'a> ast::fold::Fold<Option<Type>> for ExpressionTypeInferencer<'a> {
    type TargetU = Option<Type>;
    type Error = String;

    fn map_user(&mut self, user: Option<Type>) -> Result<Self::TargetU, Self::Error> {
        Ok(user)
    }

    fn fold_expr(&mut self, expr: ast::Expr<Option<Type>>) -> Result<ast::Expr<Self::TargetU>, Self::Error> {
        let ast::Expr {location, custom: cus, node} = expr;
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
                let elts= elts
                    .into_iter()
                    .map(|x| self.fold_expr(x))
                    .collect::<Result<Vec<ast::Expr<Option<Type>>>, _>>()?; // elements inside the vector should now have type info
                
                Ok(ast::Expr {
                    location, 
                    custom: self.infer_list_val(&elts)?,
                    node: ast::ExprKind::List {elts, ctx}
                })
            }
            
            _ => 
                Ok(ast::Expr {location, custom: cus, node: node})
        }
    }
}

mod test {

    use crate::typecheck::{symbol_resolver::SymbolResolver, typedef::*, symbol_resolver::*, location::*};
    use rustpython_parser::ast::{self, Expr, fold::Fold};
    use super::*;
    
    fn new_ctx<'a>() -> ExpressionTypeInferencer<'a>{
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
