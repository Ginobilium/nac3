use std::collections::HashMap;
use std::fmt::Debug;
use std::{cell::RefCell, sync::Arc};

use crate::typecheck::{
    type_inferencer::PrimitiveStore,
    typedef::{Type, Unifier},
};
use crate::{
    codegen::CodeGenContext,
    toplevel::{DefinitionId, TopLevelDef},
};
use crate::{location::Location, typecheck::typedef::TypeEnum};
use inkwell::values::{BasicValueEnum, FloatValue, IntValue, PointerValue};
use itertools::{chain, izip};
use nac3parser::ast::{Expr, StrRef};
use parking_lot::RwLock;

#[derive(Clone, PartialEq, Debug)]
pub enum SymbolValue {
    I32(i32),
    I64(i64),
    Double(f64),
    Bool(bool),
    Tuple(Vec<SymbolValue>),
}

pub trait StaticValue {
    fn get_unique_identifier(&self) -> u64;

    fn to_basic_value_enum<'ctx, 'a>(
        &self,
        ctx: &mut CodeGenContext<'ctx, 'a>,
    ) -> BasicValueEnum<'ctx>;

    fn get_field<'ctx, 'a>(
        &self,
        name: StrRef,
        ctx: &mut CodeGenContext<'ctx, 'a>,
    ) -> Option<ValueEnum<'ctx>>;
}

#[derive(Clone)]
pub enum ValueEnum<'ctx> {
    Static(Arc<dyn StaticValue + Send + Sync>),
    Dynamic(BasicValueEnum<'ctx>),
}

impl<'ctx> From<BasicValueEnum<'ctx>> for ValueEnum<'ctx> {
    fn from(v: BasicValueEnum<'ctx>) -> Self {
        ValueEnum::Dynamic(v)
    }
}

impl<'ctx> From<PointerValue<'ctx>> for ValueEnum<'ctx> {
    fn from(v: PointerValue<'ctx>) -> Self {
        ValueEnum::Dynamic(v.into())
    }
}

impl<'ctx> From<IntValue<'ctx>> for ValueEnum<'ctx> {
    fn from(v: IntValue<'ctx>) -> Self {
        ValueEnum::Dynamic(v.into())
    }
}

impl<'ctx> From<FloatValue<'ctx>> for ValueEnum<'ctx> {
    fn from(v: FloatValue<'ctx>) -> Self {
        ValueEnum::Dynamic(v.into())
    }
}

impl<'ctx> ValueEnum<'ctx> {
    pub fn to_basic_value_enum<'a>(
        self,
        ctx: &mut CodeGenContext<'ctx, 'a>,
    ) -> BasicValueEnum<'ctx> {
        match self {
            ValueEnum::Static(v) => v.to_basic_value_enum(ctx),
            ValueEnum::Dynamic(v) => v,
        }
    }
}

pub trait SymbolResolver {
    // get type of type variable identifier or top-level function type
    fn get_symbol_type(
        &self,
        unifier: &mut Unifier,
        top_level_defs: &[Arc<RwLock<TopLevelDef>>],
        primitives: &PrimitiveStore,
        str: StrRef,
    ) -> Option<Type>;

    // get the top-level definition of identifiers
    fn get_identifier_def(&self, str: StrRef) -> Option<DefinitionId>;

    fn get_symbol_value<'ctx, 'a>(
        &self,
        str: StrRef,
        ctx: &mut CodeGenContext<'ctx, 'a>,
    ) -> Option<ValueEnum<'ctx>>;

    fn get_symbol_location(&self, str: StrRef) -> Option<Location>;
    fn get_default_param_value(&self, expr: &nac3parser::ast::Expr) -> Option<SymbolValue>;
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
    use nac3parser::ast::ExprKind::*;
    let ids = IDENTIFIER_ID.with(|ids| *ids);
    let int32_id = ids[0];
    let int64_id = ids[1];
    let float_id = ids[2];
    let bool_id = ids[3];
    let none_id = ids[4];
    let virtual_id = ids[5];
    let list_id = ids[6];
    let tuple_id = ids[7];

    let name_handling = |id: &StrRef, unifier: &mut Unifier| {
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
                            fields.iter().map(|(k, v, m)| (*k, (*v, *m))),
                            methods.iter().map(|(k, v, _)| (*k, (*v, false))),
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
                    .get_symbol_type(unifier, top_level_defs, primitives, *id)
                    .ok_or_else(|| "unknown type variable name".to_owned())?;
                if let TypeEnum::TVar { .. } = &*unifier.get_ty(ty) {
                    Ok(ty)
                } else {
                    Err(format!("Unknown type annotation {}", id))
                }
            }
        }
    };

    let subscript_name_handle = |id: &StrRef, slice: &Expr<T>, unifier: &mut Unifier| {
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
                    .map(|(attr, ty, is_mutable)| {
                        let ty = unifier.subst(*ty, &subst).unwrap_or(*ty);
                        (*attr, (ty, *is_mutable))
                    })
                    .collect::<HashMap<_, _>>();
                fields.extend(methods.iter().map(|(attr, ty, _)| {
                    let ty = unifier.subst(*ty, &subst).unwrap_or(*ty);
                    (*attr, (ty, false))
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
    };

    match &expr.node {
        Name { id, .. } => name_handling(id, unifier),
        Subscript { value, slice, .. } => {
            if let Name { id, .. } = &value.node {
                subscript_name_handle(id, slice, unifier)
            } else {
                Err(format!("unsupported type expression at {}", expr.location))
            }
        }
        _ => Err(format!("unsupported type expression at {}", expr.location)),
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
