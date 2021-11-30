use std::{
    borrow::BorrowMut,
    collections::{HashMap, HashSet},
    fmt::Debug,
    iter::FromIterator,
    ops::{Deref, DerefMut},
    sync::Arc,
};

use super::codegen::CodeGenContext;
use super::typecheck::type_inferencer::PrimitiveStore;
use super::typecheck::typedef::{FunSignature, FuncArg, SharedUnifier, Type, TypeEnum, Unifier};
use crate::{
    symbol_resolver::SymbolResolver,
    typecheck::{type_inferencer::CodeLocation, typedef::CallId},
};
use itertools::{izip, Itertools};
use parking_lot::RwLock;
use nac3parser::ast::{self, Stmt, StrRef};
use inkwell::values::BasicValueEnum;

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash, Debug)]
pub struct DefinitionId(pub usize);

pub mod composer;
pub mod helper;
mod type_annotation;
pub mod builtins;
use composer::*;
use type_annotation::*;
#[cfg(test)]
mod test;

type GenCallCallback = Box<
    dyn for<'ctx, 'a> Fn(
            &mut CodeGenContext<'ctx, 'a>,
            Option<(Type, BasicValueEnum)>,
            (&FunSignature, DefinitionId),
            Vec<(Option<StrRef>, BasicValueEnum<'ctx>)>,
        ) -> Option<BasicValueEnum<'ctx>>
        + Send
        + Sync,
>;

pub struct GenCall {
    fp: GenCallCallback,
}

impl GenCall {
    pub fn new(fp: GenCallCallback) -> GenCall {
        GenCall { fp }
    }

    pub fn run<'ctx, 'a>(
        &self,
        ctx: &mut CodeGenContext<'ctx, 'a>,
        obj: Option<(Type, BasicValueEnum<'ctx>)>,
        fun: (&FunSignature, DefinitionId),
        args: Vec<(Option<StrRef>, BasicValueEnum<'ctx>)>,
    ) -> Option<BasicValueEnum<'ctx>> {
        (self.fp)(ctx, obj, fun, args)
    }
}

impl Debug for GenCall {
    fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct FunInstance {
    pub body: Arc<Vec<Stmt<Option<Type>>>>,
    pub calls: Arc<HashMap<CodeLocation, CallId>>,
    pub subst: HashMap<u32, Type>,
    pub unifier_id: usize,
}

#[derive(Debug, Clone)]
pub enum TopLevelDef {
    Class {
        // name for error messages and symbols
        name: StrRef,
        // object ID used for TypeEnum
        object_id: DefinitionId,
        /// type variables bounded to the class.
        type_vars: Vec<Type>,
        // class fields
        // name, type, is mutable
        fields: Vec<(StrRef, Type, bool)>,
        // class methods, pointing to the corresponding function definition.
        methods: Vec<(StrRef, Type, DefinitionId)>,
        // ancestor classes, including itself.
        ancestors: Vec<TypeAnnotation>,
        // symbol resolver of the module defined the class, none if it is built-in type
        resolver: Option<Arc<dyn SymbolResolver + Send + Sync>>,
        // constructor type
        constructor: Option<Type>,
    },
    Function {
        // prefix for symbol, should be unique globally
        name: String,
        // simple name, the same as in method/function definition
        simple_name: StrRef,
        // function signature.
        signature: Type,
        // instantiated type variable IDs
        var_id: Vec<u32>,
        /// Function instance to symbol mapping
        /// Key: string representation of type variable values, sorted by variable ID in ascending
        /// order, including type variables associated with the class.
        /// Value: function symbol name.
        instance_to_symbol: HashMap<String, String>,
        /// Function instances to annotated AST mapping
        /// Key: string representation of type variable values, sorted by variable ID in ascending
        /// order, including type variables associated with the class. Excluding rigid type
        /// variables.
        /// rigid type variables that would be substituted when the function is instantiated.
        instance_to_stmt: HashMap<String, FunInstance>,
        // symbol resolver of the module defined the class
        resolver: Option<Arc<dyn SymbolResolver + Send + Sync>>,
        // custom codegen callback
        codegen_callback: Option<Arc<GenCall>>
    },
}

pub struct TopLevelContext {
    pub definitions: Arc<RwLock<Vec<Arc<RwLock<TopLevelDef>>>>>,
    pub unifiers: Arc<RwLock<Vec<(SharedUnifier, PrimitiveStore)>>>,
    pub personality_symbol: Option<String>,
}
