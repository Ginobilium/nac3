use std::fmt::Debug;
use std::sync::Arc;
use std::{collections::HashMap, fmt::Display};

use crate::typecheck::typedef::TypeEnum;
use crate::{
    codegen::CodeGenContext,
    toplevel::{DefinitionId, TopLevelDef},
};
use crate::{
    codegen::CodeGenerator,
    typecheck::{
        type_inferencer::PrimitiveStore,
        typedef::{Type, Unifier},
    },
};
use inkwell::values::{BasicValueEnum, FloatValue, IntValue, PointerValue, StructValue};
use itertools::{chain, izip};
use nac3parser::ast::{Expr, Location, StrRef};
use parking_lot::RwLock;

#[derive(Clone, PartialEq, Debug)]
pub enum SymbolValue {
    I32(i32),
    I64(i64),
    U32(u32),
    U64(u64),
    Str(String),
    Double(f64),
    Bool(bool),
    Tuple(Vec<SymbolValue>),
    OptionSome(Box<SymbolValue>),
    OptionNone,
}

impl Display for SymbolValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SymbolValue::I32(i) => write!(f, "{}", i),
            SymbolValue::I64(i) => write!(f, "int64({})", i),
            SymbolValue::U32(i) => write!(f, "uint32({})", i),
            SymbolValue::U64(i) => write!(f, "uint64({})", i),
            SymbolValue::Str(s) => write!(f, "\"{}\"", s),
            SymbolValue::Double(d) => write!(f, "{}", d),
            SymbolValue::Bool(b) => {
                if *b {
                    write!(f, "True")
                } else {
                    write!(f, "False")
                }
            }
            SymbolValue::Tuple(t) => {
                write!(f, "({})", t.iter().map(|v| format!("{}", v)).collect::<Vec<_>>().join(", "))
            }
            SymbolValue::OptionSome(v) => write!(f, "Some({})", v),
            SymbolValue::OptionNone => write!(f, "none"),
        }
    }
}

pub trait StaticValue {
    fn get_unique_identifier(&self) -> u64;

    fn get_const_obj<'ctx, 'a>(
        &self,
        ctx: &mut CodeGenContext<'ctx, 'a>,
        generator: &mut dyn CodeGenerator,
    ) -> BasicValueEnum<'ctx>;

    fn to_basic_value_enum<'ctx, 'a>(
        &self,
        ctx: &mut CodeGenContext<'ctx, 'a>,
        generator: &mut dyn CodeGenerator,
        expected_ty: Type,
    ) -> Result<BasicValueEnum<'ctx>, String>;

    fn get_field<'ctx, 'a>(
        &self,
        name: StrRef,
        ctx: &mut CodeGenContext<'ctx, 'a>,
    ) -> Option<ValueEnum<'ctx>>;

    fn get_tuple_element<'ctx>(&self, index: u32) -> Option<ValueEnum<'ctx>>;
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

impl<'ctx> From<StructValue<'ctx>> for ValueEnum<'ctx> {
    fn from(v: StructValue<'ctx>) -> Self {
        ValueEnum::Dynamic(v.into())
    }
}

impl<'ctx> ValueEnum<'ctx> {
    pub fn to_basic_value_enum<'a>(
        self,
        ctx: &mut CodeGenContext<'ctx, 'a>,
        generator: &mut dyn CodeGenerator,
        expected_ty: Type,
    ) -> Result<BasicValueEnum<'ctx>, String> {
        match self {
            ValueEnum::Static(v) => v.to_basic_value_enum(ctx, generator, expected_ty),
            ValueEnum::Dynamic(v) => Ok(v),
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
    ) -> Result<Type, String>;

    // get the top-level definition of identifiers
    fn get_identifier_def(&self, str: StrRef) -> Result<DefinitionId, String>;

    fn get_symbol_value<'ctx, 'a>(
        &self,
        str: StrRef,
        ctx: &mut CodeGenContext<'ctx, 'a>,
    ) -> Option<ValueEnum<'ctx>>;

    fn get_default_param_value(&self, expr: &nac3parser::ast::Expr) -> Option<SymbolValue>;
    fn get_string_id(&self, s: &str) -> i32;
    fn get_exception_id(&self, tyid: usize) -> usize;

    fn handle_deferred_eval(
        &self,
        _unifier: &mut Unifier,
        _top_level_defs: &[Arc<RwLock<TopLevelDef>>],
        _primitives: &PrimitiveStore
    ) -> Result<(), String> {
        Ok(())
    }
}

thread_local! {
    static IDENTIFIER_ID: [StrRef; 11] = [
        "int32".into(),
        "int64".into(),
        "float".into(),
        "bool".into(),
        "virtual".into(),
        "list".into(),
        "tuple".into(),
        "str".into(),
        "Exception".into(),
        "uint32".into(),
        "uint64".into(),
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
    let virtual_id = ids[4];
    let list_id = ids[5];
    let tuple_id = ids[6];
    let str_id = ids[7];
    let exn_id = ids[8];
    let uint32_id = ids[9];
    let uint64_id = ids[10];

    let name_handling = |id: &StrRef, loc: Location, unifier: &mut Unifier| {
        if *id == int32_id {
            Ok(primitives.int32)
        } else if *id == int64_id {
            Ok(primitives.int64)
        } else if *id == uint32_id {
            Ok(primitives.uint32)
        } else if *id == uint64_id {
            Ok(primitives.uint64)
        } else if *id == float_id {
            Ok(primitives.float)
        } else if *id == bool_id {
            Ok(primitives.bool)
        } else if *id == str_id {
            Ok(primitives.str)
        } else if *id == exn_id {
            Ok(primitives.exception)
        } else {
            let obj_id = resolver.get_identifier_def(*id);
            match obj_id {
                Ok(obj_id) => {
                    let def = top_level_defs[obj_id.0].read();
                    if let TopLevelDef::Class { fields, methods, type_vars, .. } = &*def {
                        if !type_vars.is_empty() {
                            return Err(format!(
                                "Unexpected number of type parameters: expected {} but got 0",
                                type_vars.len()
                            ));
                        }
                        let fields = chain(
                            fields.iter().map(|(k, v, m)| (*k, (*v, *m))),
                            methods.iter().map(|(k, v, _)| (*k, (*v, false))),
                        )
                        .collect();
                        Ok(unifier.add_ty(TypeEnum::TObj {
                            obj_id,
                            fields,
                            params: Default::default(),
                        }))
                    } else {
                        Err(format!("Cannot use function name as type at {}", loc))
                    }
                }
                Err(_) => {
                    let ty = resolver
                        .get_symbol_type(unifier, top_level_defs, primitives, *id)
                        .map_err(|e| format!("Unknown type annotation at {}: {}", loc, e))?;
                    if let TypeEnum::TVar { .. } = &*unifier.get_ty(ty) {
                        Ok(ty)
                    } else {
                        Err(format!("Unknown type annotation {} at {}", id, loc))
                    }
                }
            }
        }
    };

    let subscript_name_handle = |id: &StrRef, slice: &Expr<T>, unifier: &mut Unifier| {
        if *id == virtual_id {
            let ty = parse_type_annotation(resolver, top_level_defs, unifier, primitives, slice)?;
            Ok(unifier.add_ty(TypeEnum::TVirtual { ty }))
        } else if *id == list_id {
            let ty = parse_type_annotation(resolver, top_level_defs, unifier, primitives, slice)?;
            Ok(unifier.add_ty(TypeEnum::TList { ty }))
        } else if *id == tuple_id {
            if let Tuple { elts, .. } = &slice.node {
                let ty = elts
                    .iter()
                    .map(|elt| {
                        parse_type_annotation(resolver, top_level_defs, unifier, primitives, elt)
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
                        parse_type_annotation(resolver, top_level_defs, unifier, primitives, v)
                    })
                    .collect::<Result<Vec<_>, _>>()?
            } else {
                vec![parse_type_annotation(resolver, top_level_defs, unifier, primitives, slice)?]
            };

            let obj_id = resolver.get_identifier_def(*id)?;
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
                Ok(unifier.add_ty(TypeEnum::TObj { obj_id, fields, params: subst }))
            } else {
                Err("Cannot use function name as type".into())
            }
        }
    };

    match &expr.node {
        Name { id, .. } => name_handling(id, expr.location, unifier),
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

    pub fn get_type_name(
        &self,
        top_level_defs: &[Arc<RwLock<TopLevelDef>>],
        unifier: &mut Unifier,
        ty: Type,
    ) -> String {
        unifier.internal_stringify(
            ty,
            &mut |id| {
                if let TopLevelDef::Class { name, .. } = &*top_level_defs[id].read() {
                    name.to_string()
                } else {
                    unreachable!("expected class definition")
                }
            },
            &mut |id| format!("var{}", id),
            &mut None,
        )
    }
}

impl Debug for dyn SymbolResolver + Send + Sync {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "")
    }
}
