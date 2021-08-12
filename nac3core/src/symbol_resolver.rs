use std::cell::RefCell;
use std::collections::HashMap;

use crate::top_level::{DefinitionId, TopLevelContext, TopLevelDef};
use crate::typecheck::{
    type_inferencer::PrimitiveStore,
    typedef::{Type, Unifier},
};
use crate::{location::Location, typecheck::typedef::TypeEnum};
use itertools::{chain, izip};
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
    // handle function call etc.
}

impl dyn SymbolResolver {
    // convert type annotation into type
    pub fn parse_type_annotation<T>(
        &self,
        top_level: &TopLevelContext,
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
                    let obj_id = self.get_identifier_def(x);
                    if let Some(obj_id) = obj_id {
                        let defs = top_level.definitions.read();
                        let def = defs[obj_id.0].read();
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
                        let ty = self
                            .get_symbol_type(unifier, primitives, x)
                            .ok_or_else(|| "Cannot use function name as type".to_owned())?;
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
                    if id == "virtual" {
                        let ty =
                            self.parse_type_annotation(top_level, unifier, primitives, slice)?;
                        Ok(unifier.add_ty(TypeEnum::TVirtual { ty }))
                    } else {
                        let types = if let Tuple { elts, .. } = &slice.node {
                            elts.iter()
                                .map(|v| {
                                    self.parse_type_annotation(top_level, unifier, primitives, v)
                                })
                                .collect::<Result<Vec<_>, _>>()?
                        } else {
                            vec![self.parse_type_annotation(top_level, unifier, primitives, slice)?]
                        };

                        let obj_id = self
                            .get_identifier_def(id)
                            .ok_or_else(|| format!("Unknown type annotation {}", id))?;
                        let defs = top_level.definitions.read();
                        let def = defs[obj_id.0].read();
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
                            Ok(unifier.add_ty(TypeEnum::TObj { obj_id, fields: fields.into(), params: subst.into() }))
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
}
