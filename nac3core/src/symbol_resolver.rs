use std::collections::HashMap;
use std::fmt::Debug;
use std::{cell::RefCell, sync::Arc};

use crate::toplevel::{DefinitionId, TopLevelDef};
use crate::typecheck::{
    type_inferencer::PrimitiveStore,
    typedef::{Type, Unifier},
};
use crate::{location::Location, typecheck::typedef::TypeEnum};
use itertools::{chain, izip};
use parking_lot::RwLock;
use rustpython_parser::ast::{Expr, StrRef};

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
        str: StrRef,
    ) -> Option<Type>;
    // get the top-level definition of identifiers
    fn get_identifier_def(&self, str: StrRef) -> Option<DefinitionId>;
    fn get_symbol_value(&self, str: StrRef) -> Option<SymbolValue>;
    fn get_symbol_location(&self, str: StrRef) -> Option<Location>;
    // handle function call etc.
}

thread_local! {
    static IDENTIFIER_ID: [StrRef; 8] = [
        "int32".into(),
        "int64".into(),
        "float".into(),
        "bool".into(),
        "None".into(),
        "virtual".into(),
        "list".into(),
        "tuple".into()
    ];
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
    let ids = IDENTIFIER_ID.with(|ids| {
        *ids
    });
    let int32_id = ids[0];
    let int64_id = ids[1];
    let float_id = ids[2];
    let bool_id = ids[3];
    let none_id = ids[4];
    let virtual_id = ids[5];
    let list_id = ids[6];
    let tuple_id = ids[7];

    match &expr.node {
        Name { id, .. } => {
            if *id == int32_id {
                Ok(primitives.int32)
            } else if *id == int64_id {
                Ok(primitives.int64)
            } else if *id == float_id {
                Ok(primitives.float)
            } else if *id == bool_id {
                Ok(primitives.bool)
            } else if *id == none_id {
                Ok(primitives.none)
            } else {
                let obj_id = resolver.get_identifier_def(*id);
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
                                fields.iter().map(|(k, v)| (*k, *v)),
                                methods.iter().map(|(k, v, _)| (*k, *v)),
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
                        .get_symbol_type(unifier, primitives, *id)
                        .ok_or_else(|| "unknown type variable name".to_owned())?;
                    if let TypeEnum::TVar { .. } = &*unifier.get_ty(ty) {
                        Ok(ty)
                    } else {
                        Err(format!("Unknown type annotation {}", id))
                    }
                }
            }
        },
        Subscript { value, slice, .. } => {
            if let Name { id, .. } = &value.node {
                if *id == virtual_id {
                    let ty = parse_type_annotation(
                        resolver,
                        top_level_defs,
                        unifier,
                        primitives,
                        slice,
                    )?;
                    Ok(unifier.add_ty(TypeEnum::TVirtual { ty }))
                } else if *id == list_id {
                    let ty = parse_type_annotation(
                        resolver,
                        top_level_defs,
                        unifier,
                        primitives,
                        slice,
                    )?;
                    Ok(unifier.add_ty(TypeEnum::TList { ty }))
                } else if *id == tuple_id {
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
                } else {
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
                        .get_identifier_def(*id)
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
                                (*attr, ty)
                            })
                            .collect::<HashMap<_, _>>();
                        fields.extend(methods.iter().map(|(attr, ty, _)| {
                            let ty = unifier.subst(*ty, &subst).unwrap_or(*ty);
                            (*attr, ty)
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

impl Debug for dyn SymbolResolver + Send + Sync {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "")
    }
}
