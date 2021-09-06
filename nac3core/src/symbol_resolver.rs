use std::collections::HashMap;
use std::{cell::RefCell, sync::Arc};

use crate::toplevel::{DefinitionId, TopLevelDef};
use crate::typecheck::{
    type_inferencer::PrimitiveStore,
    typedef::{Type, Unifier},
};
use crate::{location::Location, typecheck::typedef::TypeEnum};
use itertools::{chain, izip};
use parking_lot::RwLock;
use rustpython_parser::ast::Expr;

#[derive(Clone, PartialEq)]
pub enum SymbolValue {
    I32(i32),
    I64(i64),
    Double(f64),
    Bool(bool),
    Tuple(Vec<SymbolValue>),
    // we should think about how to implement bytes later...
    // Bytes(&'a [u8]),
}

pub trait SymbolResolver {
    // get type of type variable identifier or top-level function type
    fn get_symbol_type(
        &self,
        unifier: &mut Unifier,
        primitives: &PrimitiveStore,
        str: &str,
    ) -> Option<Type>;
    // get the top-level definition of identifiers
    fn get_identifier_def(&self, str: &str) -> Option<DefinitionId>;
    fn get_symbol_value(&self, str: &str) -> Option<SymbolValue>;
    fn get_symbol_location(&self, str: &str) -> Option<Location>;
    fn add_id_def(&mut self, id: String, def_id: DefinitionId);
    // handle function call etc.
}

// convert type annotation into type
pub fn parse_type_annotation<T>(
    resolver: &dyn SymbolResolver,
    top_level_defs: &[Arc<RwLock<TopLevelDef>>],
    unifier: &mut Unifier,
    primitives: &PrimitiveStore,
    expr: &Expr<T>,
) -> Result<Type, String> {
    use rustpython_parser::ast::ExprKind::*;
    match &expr.node {
        Name { id, .. } => match id.as_str() {
            "int32" => Ok(primitives.int32),
            "int64" => Ok(primitives.int64),
            "float" => Ok(primitives.float),
            "bool" => Ok(primitives.bool),
            "None" => Ok(primitives.none),
            x => {
                let obj_id = resolver.get_identifier_def(x);
                if let Some(obj_id) = obj_id {
                    let def = top_level_defs[obj_id.0].read();
                    if let TopLevelDef::Class { fields, methods, type_vars, .. } = &*def {
                        if !type_vars.is_empty() {
                            return Err(format!(
                                "Unexpected number of type parameters: expected {} but got 0",
                                type_vars.len()
                            ));
                        }
                        let fields = RefCell::new(
                            chain(
                                fields.iter().map(|(k, v)| (k.clone(), *v)),
                                methods.iter().map(|(k, v, _)| (k.clone(), *v)),
                            )
                            .collect(),
                        );
                        Ok(unifier.add_ty(TypeEnum::TObj {
                            obj_id,
                            fields,
                            params: Default::default(),
                        }))
                    } else {
                        Err("Cannot use function name as type".into())
                    }
                } else {
                    // it could be a type variable
                    let ty = resolver
                        .get_symbol_type(unifier, primitives, x)
                        .ok_or_else(|| "unknown type variable name".to_owned())?;
                    if let TypeEnum::TVar { .. } = &*unifier.get_ty(ty) {
                        Ok(ty)
                    } else {
                        Err(format!("Unknown type annotation {}", x))
                    }
                }
            }
        },
        Subscript { value, slice, .. } => {
            if let Name { id, .. } = &value.node {
                match id.as_str() {
                    "virtual" => {
                        let ty = parse_type_annotation(
                            resolver,
                            top_level_defs,
                            unifier,
                            primitives,
                            slice,
                        )?;
                        Ok(unifier.add_ty(TypeEnum::TVirtual { ty }))
                    }
                    "list" => {
                        let ty = parse_type_annotation(
                            resolver,
                            top_level_defs,
                            unifier,
                            primitives,
                            slice,
                        )?;
                        Ok(unifier.add_ty(TypeEnum::TList { ty }))
                    }
                    "tuple" => {
                        if let Tuple { elts, .. } = &slice.node {
                            let ty = elts
                                .iter()
                                .map(|elt| {
                                    parse_type_annotation(
                                        resolver,
                                        top_level_defs,
                                        unifier,
                                        primitives,
                                        elt,
                                    )
                                })
                                .collect::<Result<Vec<_>, _>>()?;
                            Ok(unifier.add_ty(TypeEnum::TTuple { ty }))
                        } else {
                            Err("Expected multiple elements for tuple".into())
                        }
                    }
                    _ => {
                        let types = if let Tuple { elts, .. } = &slice.node {
                            elts.iter()
                                .map(|v| {
                                    parse_type_annotation(
                                        resolver,
                                        top_level_defs,
                                        unifier,
                                        primitives,
                                        v,
                                    )
                                })
                                .collect::<Result<Vec<_>, _>>()?
                        } else {
                            vec![parse_type_annotation(
                                resolver,
                                top_level_defs,
                                unifier,
                                primitives,
                                slice,
                            )?]
                        };

                        let obj_id = resolver
                            .get_identifier_def(id)
                            .ok_or_else(|| format!("Unknown type annotation {}", id))?;
                        let def = top_level_defs[obj_id.0].read();
                        if let TopLevelDef::Class { fields, methods, type_vars, .. } = &*def {
                            if types.len() != type_vars.len() {
                                return Err(format!(
                                    "Unexpected number of type parameters: expected {} but got {}",
                                    type_vars.len(),
                                    types.len()
                                ));
                            }
                            let mut subst = HashMap::new();
                            for (var, ty) in izip!(type_vars.iter(), types.iter()) {
                                let id = if let TypeEnum::TVar { id, .. } = &*unifier.get_ty(*var) {
                                    *id
                                } else {
                                    unreachable!()
                                };
                                subst.insert(id, *ty);
                            }
                            let mut fields = fields
                                .iter()
                                .map(|(attr, ty)| {
                                    let ty = unifier.subst(*ty, &subst).unwrap_or(*ty);
                                    (attr.clone(), ty)
                                })
                                .collect::<HashMap<_, _>>();
                            fields.extend(methods.iter().map(|(attr, ty, _)| {
                                let ty = unifier.subst(*ty, &subst).unwrap_or(*ty);
                                (attr.clone(), ty)
                            }));
                            Ok(unifier.add_ty(TypeEnum::TObj {
                                obj_id,
                                fields: fields.into(),
                                params: subst.into(),
                            }))
                        } else {
                            Err("Cannot use function name as type".into())
                        }
                    }
                }
            } else {
                Err("unsupported type expression".into())
            }
        }
        _ => Err("unsupported type expression".into()),
    }
}

impl dyn SymbolResolver + Send + Sync {
    pub fn parse_type_annotation<T>(
        &self,
        top_level_defs: &[Arc<RwLock<TopLevelDef>>],
        unifier: &mut Unifier,
        primitives: &PrimitiveStore,
        expr: &Expr<T>,
    ) -> Result<Type, String> {
        parse_type_annotation(self, top_level_defs, unifier, primitives, expr)
    }
}
