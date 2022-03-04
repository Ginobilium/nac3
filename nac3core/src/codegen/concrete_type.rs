use crate::{
    symbol_resolver::SymbolValue,
    toplevel::DefinitionId,
    typecheck::{
        type_inferencer::PrimitiveStore,
        typedef::{FunSignature, FuncArg, Type, TypeEnum, Unifier},
    },
};

use nac3parser::ast::StrRef;
use std::collections::HashMap;

pub struct ConcreteTypeStore {
    store: Vec<ConcreteTypeEnum>,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct ConcreteType(usize);

#[derive(Clone, Debug)]
pub struct ConcreteFuncArg {
    pub name: StrRef,
    pub ty: ConcreteType,
    pub default_value: Option<SymbolValue>,
}

#[derive(Clone, Debug)]
pub enum Primitive {
    Int32,
    Int64,
    UInt32,
    UInt64,
    Float,
    Bool,
    None,
    Range,
    Str,
    Exception,
}

#[derive(Debug)]
pub enum ConcreteTypeEnum {
    TPrimitive(Primitive),
    TTuple {
        ty: Vec<ConcreteType>,
    },
    TList {
        ty: ConcreteType,
    },
    TObj {
        obj_id: DefinitionId,
        fields: HashMap<StrRef, (ConcreteType, bool)>,
        params: HashMap<u32, ConcreteType>,
    },
    TVirtual {
        ty: ConcreteType,
    },
    TFunc {
        args: Vec<ConcreteFuncArg>,
        ret: ConcreteType,
        vars: HashMap<u32, ConcreteType>,
    },
}

impl ConcreteTypeStore {
    pub fn new() -> ConcreteTypeStore {
        ConcreteTypeStore {
            store: vec![
                ConcreteTypeEnum::TPrimitive(Primitive::Int32),
                ConcreteTypeEnum::TPrimitive(Primitive::Int64),
                ConcreteTypeEnum::TPrimitive(Primitive::Float),
                ConcreteTypeEnum::TPrimitive(Primitive::Bool),
                ConcreteTypeEnum::TPrimitive(Primitive::None),
                ConcreteTypeEnum::TPrimitive(Primitive::Range),
                ConcreteTypeEnum::TPrimitive(Primitive::Str),
                ConcreteTypeEnum::TPrimitive(Primitive::Exception),
                ConcreteTypeEnum::TPrimitive(Primitive::UInt32),
                ConcreteTypeEnum::TPrimitive(Primitive::UInt64),
            ],
        }
    }

    pub fn get(&self, cty: ConcreteType) -> &ConcreteTypeEnum {
        &self.store[cty.0]
    }

    pub fn from_signature(
        &mut self,
        unifier: &mut Unifier,
        primitives: &PrimitiveStore,
        signature: &FunSignature,
        cache: &mut HashMap<Type, Option<ConcreteType>>,
    ) -> ConcreteTypeEnum {
        ConcreteTypeEnum::TFunc {
            args: signature
                .args
                .iter()
                .map(|arg| ConcreteFuncArg {
                    name: arg.name,
                    ty: self.from_unifier_type(unifier, primitives, arg.ty, cache),
                    default_value: arg.default_value.clone(),
                })
                .collect(),
            ret: self.from_unifier_type(unifier, primitives, signature.ret, cache),
            vars: signature
                .vars
                .iter()
                .map(|(id, ty)| (*id, self.from_unifier_type(unifier, primitives, *ty, cache)))
                .collect(),
        }
    }

    pub fn from_unifier_type(
        &mut self,
        unifier: &mut Unifier,
        primitives: &PrimitiveStore,
        ty: Type,
        cache: &mut HashMap<Type, Option<ConcreteType>>,
    ) -> ConcreteType {
        let ty = unifier.get_representative(ty);
        if unifier.unioned(ty, primitives.int32) {
            ConcreteType(0)
        } else if unifier.unioned(ty, primitives.int64) {
            ConcreteType(1)
        } else if unifier.unioned(ty, primitives.float) {
            ConcreteType(2)
        } else if unifier.unioned(ty, primitives.bool) {
            ConcreteType(3)
        } else if unifier.unioned(ty, primitives.none) {
            ConcreteType(4)
        } else if unifier.unioned(ty, primitives.range) {
            ConcreteType(5)
        } else if unifier.unioned(ty, primitives.str) {
            ConcreteType(6)
        } else if unifier.unioned(ty, primitives.exception) {
            ConcreteType(7)
        } else if unifier.unioned(ty, primitives.uint32) {
            ConcreteType(8)
        } else if unifier.unioned(ty, primitives.uint64) {
            ConcreteType(9)
        } else if let Some(cty) = cache.get(&ty) {
            if let Some(cty) = cty {
                *cty
            } else {
                let index = self.store.len();
                // placeholder
                self.store.push(ConcreteTypeEnum::TPrimitive(Primitive::Int32));
                let result = ConcreteType(index);
                cache.insert(ty, Some(result));
                result
            }
        } else {
            cache.insert(ty, None);
            let ty_enum = unifier.get_ty(ty);
            let result = match &*ty_enum {
                TypeEnum::TTuple { ty } => ConcreteTypeEnum::TTuple {
                    ty: ty
                        .iter()
                        .map(|t| self.from_unifier_type(unifier, primitives, *t, cache))
                        .collect(),
                },
                TypeEnum::TList { ty } => ConcreteTypeEnum::TList {
                    ty: self.from_unifier_type(unifier, primitives, *ty, cache),
                },
                TypeEnum::TObj { obj_id, fields, params } => ConcreteTypeEnum::TObj {
                    obj_id: *obj_id,
                    fields: fields
                        .iter()
                        .filter_map(|(name, ty)| {
                            // here we should not have type vars, but some partial instantiated
                            // class methods can still have uninstantiated type vars, so
                            // filter out all the methods, as this will not affect codegen
                            if let TypeEnum::TFunc(..) = &*unifier.get_ty(ty.0) {
                                None
                            } else {
                                Some((
                                    *name,
                                    (
                                        self.from_unifier_type(unifier, primitives, ty.0, cache),
                                        ty.1,
                                    ),
                                ))
                            }
                        })
                        .collect(),
                    params: params
                        .iter()
                        .map(|(id, ty)| {
                            (*id, self.from_unifier_type(unifier, primitives, *ty, cache))
                        })
                        .collect(),
                },
                TypeEnum::TVirtual { ty } => ConcreteTypeEnum::TVirtual {
                    ty: self.from_unifier_type(unifier, primitives, *ty, cache),
                },
                TypeEnum::TFunc(signature) => {
                    self.from_signature(unifier, primitives, &*signature, cache)
                }
                _ => unreachable!(),
            };
            let index = if let Some(ConcreteType(index)) = cache.get(&ty).unwrap() {
                self.store[*index] = result;
                *index
            } else {
                self.store.push(result);
                self.store.len() - 1
            };
            cache.insert(ty, Some(ConcreteType(index)));
            ConcreteType(index)
        }
    }

    pub fn to_unifier_type(
        &self,
        unifier: &mut Unifier,
        primitives: &PrimitiveStore,
        cty: ConcreteType,
        cache: &mut HashMap<ConcreteType, Option<Type>>,
    ) -> Type {
        if let Some(ty) = cache.get_mut(&cty) {
            return if let Some(ty) = ty {
                *ty
            } else {
                *ty = Some(unifier.get_dummy_var().0);
                ty.unwrap()
            };
        }
        cache.insert(cty, None);
        let result = match &self.store[cty.0] {
            ConcreteTypeEnum::TPrimitive(primitive) => {
                let ty = match primitive {
                    Primitive::Int32 => primitives.int32,
                    Primitive::Int64 => primitives.int64,
                    Primitive::UInt32 => primitives.uint32,
                    Primitive::UInt64 => primitives.uint64,
                    Primitive::Float => primitives.float,
                    Primitive::Bool => primitives.bool,
                    Primitive::None => primitives.none,
                    Primitive::Range => primitives.range,
                    Primitive::Str => primitives.str,
                    Primitive::Exception => primitives.exception,
                };
                *cache.get_mut(&cty).unwrap() = Some(ty);
                return ty;
            }
            ConcreteTypeEnum::TTuple { ty } => TypeEnum::TTuple {
                ty: ty
                    .iter()
                    .map(|cty| self.to_unifier_type(unifier, primitives, *cty, cache))
                    .collect(),
            },
            ConcreteTypeEnum::TList { ty } => {
                TypeEnum::TList { ty: self.to_unifier_type(unifier, primitives, *ty, cache) }
            }
            ConcreteTypeEnum::TVirtual { ty } => {
                TypeEnum::TVirtual { ty: self.to_unifier_type(unifier, primitives, *ty, cache) }
            }
            ConcreteTypeEnum::TObj { obj_id, fields, params } => TypeEnum::TObj {
                obj_id: *obj_id,
                fields: fields
                    .iter()
                    .map(|(name, cty)| {
                        (*name, (self.to_unifier_type(unifier, primitives, cty.0, cache), cty.1))
                    })
                    .collect::<HashMap<_, _>>(),
                params: params
                    .iter()
                    .map(|(id, cty)| (*id, self.to_unifier_type(unifier, primitives, *cty, cache)))
                    .collect::<HashMap<_, _>>(),
            },
            ConcreteTypeEnum::TFunc { args, ret, vars } => TypeEnum::TFunc(FunSignature {
                args: args
                    .iter()
                    .map(|arg| FuncArg {
                        name: arg.name,
                        ty: self.to_unifier_type(unifier, primitives, arg.ty, cache),
                        default_value: arg.default_value.clone(),
                    })
                    .collect(),
                ret: self.to_unifier_type(unifier, primitives, *ret, cache),
                vars: vars
                    .iter()
                    .map(|(id, cty)| (*id, self.to_unifier_type(unifier, primitives, *cty, cache)))
                    .collect::<HashMap<_, _>>(),
            }),
        };
        let result = unifier.add_ty(result);
        if let Some(ty) = cache.get(&cty).unwrap() {
            unifier.unify(*ty, result).unwrap();
        }
        cache.insert(cty, Some(result));
        result
    }

    pub fn add_cty(&mut self, cty: ConcreteTypeEnum) -> ConcreteType {
        self.store.push(cty);
        ConcreteType(self.store.len() - 1)
    }
}

impl Default for ConcreteTypeStore {
    fn default() -> Self {
        Self::new()
    }
}
