use std::{collections::HashMap, sync::Arc};

use super::typecheck::type_inferencer::PrimitiveStore;
use super::typecheck::typedef::{SharedUnifier, Type, Unifier};
use crate::symbol_resolver::SymbolResolver;
use inkwell::{
    basic_block::BasicBlock, builder::Builder, context::Context, module::Module,
    types::BasicTypeEnum, values::PointerValue,
};
use parking_lot::RwLock;
use rustpython_parser::ast::Stmt;

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct DefinitionId(pub usize);

pub enum TopLevelDef {
    Class {
        // object ID used for TypeEnum
        object_id: DefinitionId,
        // type variables bounded to the class.
        type_vars: Vec<Type>,
        // class fields
        fields: Vec<(String, Type)>,
        // class methods, pointing to the corresponding function definition.
        methods: Vec<(String, Type, DefinitionId)>,
        // ancestor classes, including itself.
        ancestors: Vec<DefinitionId>,
    },
    Function {
        // prefix for symbol, should be unique globally, and not ending with numbers
        name: String,
        // function signature.
        signature: Type,
        /// Function instance to symbol mapping
        /// Key: string representation of type variable values, sorted by variable ID in ascending
        /// order, including type variables associated with the class.
        /// Value: function symbol name.
        instance_to_symbol: HashMap<String, String>,
        /// Function instances to annotated AST mapping
        /// Key: string representation of type variable values, sorted by variable ID in ascending
        /// order, including type variables associated with the class. Excluding rigid type
        /// variables.
        /// Value: AST annotated with types together with a unification table index. Could contain
        /// rigid type variables that would be substituted when the function is instantiated.
        instance_to_stmt: HashMap<String, (Stmt<Option<Type>>, usize)>,
    },
}

pub struct CodeGenTask {
    pub subst: HashMap<usize, Type>,
    pub symbol_name: String,
    pub body: Stmt<Option<Type>>,
    pub unifier: SharedUnifier,
}

pub struct TopLevelContext {
    pub definitions: Arc<RwLock<Vec<RwLock<TopLevelDef>>>>,
    pub unifiers: Arc<RwLock<Vec<SharedUnifier>>>,
}

pub struct CodeGenContext<'ctx> {
    pub ctx: &'ctx Context,
    pub builder: Builder<'ctx>,
    pub module: Module<'ctx>,
    pub top_level: &'ctx TopLevelContext,
    pub unifier: Unifier,
    pub resolver: Box<dyn SymbolResolver>,
    pub var_assignment: HashMap<String, PointerValue<'ctx>>,
    pub type_cache: HashMap<Type, BasicTypeEnum<'ctx>>,
    pub primitives: PrimitiveStore,
    // stores the alloca for variables
    pub init_bb: BasicBlock<'ctx>,
    // where continue and break should go to respectively
    // the first one is the test_bb, and the second one is bb after the loop
    pub loop_bb: Option<(BasicBlock<'ctx>, BasicBlock<'ctx>)>,
}
