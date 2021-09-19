use std::{
    borrow::BorrowMut,
    collections::{HashMap, HashSet},
    fmt::Debug,
    iter::FromIterator,
    ops::{Deref, DerefMut},
    sync::Arc,
};

use super::typecheck::type_inferencer::PrimitiveStore;
use super::typecheck::typedef::{FunSignature, FuncArg, SharedUnifier, Type, TypeEnum, Unifier};
use crate::{
    symbol_resolver::SymbolResolver,
    typecheck::{type_inferencer::CodeLocation, typedef::CallId},
};
use itertools::{izip, Itertools};
use parking_lot::RwLock;
use rustpython_parser::ast::{self, Stmt};

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash, Debug)]
pub struct DefinitionId(pub usize);

pub mod composer;
mod helper;
mod type_annotation;
use composer::*;
use type_annotation::*;
#[cfg(test)]
mod test;

#[derive(Clone, Debug)]
pub struct FunInstance {
    pub body: Vec<Stmt<Option<Type>>>,
    pub calls: HashMap<CodeLocation, CallId>,
    pub subst: HashMap<u32, Type>,
    pub unifier_id: usize,
}

#[derive(Debug, Clone)]
pub enum TopLevelDef {
    Class {
        // name for error messages and symbols
        name: String,
        // object ID used for TypeEnum
        object_id: DefinitionId,
        /// type variables bounded to the class.
        type_vars: Vec<Type>,
        // class fields
        fields: Vec<(String, Type)>,
        // class methods, pointing to the corresponding function definition.
        methods: Vec<(String, Type, DefinitionId)>,
        // ancestor classes, including itself.
        ancestors: Vec<TypeAnnotation>,
        // symbol resolver of the module defined the class, none if it is built-in type
        resolver: Option<Arc<Box<dyn SymbolResolver + Send + Sync>>>,
        // constructor type
        constructor: Option<Type>,
    },
    Function {
        // prefix for symbol, should be unique globally, and not ending with numbers
        name: String,
        // simple name, the same as in method/function definition
        simple_name: String,
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
        resolver: Option<Arc<Box<dyn SymbolResolver + Send + Sync>>>,
    },
}

pub struct TopLevelContext {
    pub definitions: Arc<RwLock<Vec<Arc<RwLock<TopLevelDef>>>>>,
    pub unifiers: Arc<RwLock<Vec<(SharedUnifier, PrimitiveStore)>>>,
}
