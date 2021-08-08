use std::default;
use std::{collections::HashMap, sync::Arc};

use super::typecheck::type_inferencer::PrimitiveStore;
use super::typecheck::typedef::{SharedUnifier, Type, Unifier, TypeEnum};
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

pub struct TopLevelManager<'a> {
    pub def_index: usize,
    pub ctx: TopLevelContext,
    pub resolver: &'a mut Box<dyn SymbolResolver>,
    pub primitives: (PrimitiveStore, Unifier)
}


use rustpython_parser::ast;
impl<'a> TopLevelManager<'a> {
    pub fn make_primitives() -> (PrimitiveStore, Unifier) {
        let mut unifier = Unifier::new();
        let int32 = unifier.add_ty(TypeEnum::TObj {
            obj_id: DefinitionId(0), // NOTE: what should it be?
            fields: HashMap::new().into(),
            params: HashMap::new(),
        });
        let int64 = unifier.add_ty(TypeEnum::TObj {
            obj_id: DefinitionId(1), // NOTE: what should it be?
            fields: HashMap::new().into(),
            params: HashMap::new(),
        });
        let float = unifier.add_ty(TypeEnum::TObj {
            obj_id: DefinitionId(2), // NOTE: what should it be?
            fields: HashMap::new().into(),
            params: HashMap::new(),
        });
        let bool = unifier.add_ty(TypeEnum::TObj {
            obj_id: DefinitionId(3), // NOTE: what should it be?
            fields: HashMap::new().into(),
            params: HashMap::new(),
        });
        let none = unifier.add_ty(TypeEnum::TObj {
            obj_id: DefinitionId(4), // NOTE: what should it be?
            fields: HashMap::new().into(),
            params: HashMap::new(),
        });
        let primitives = PrimitiveStore { int32, int64, float, bool, none };
        crate::typecheck::magic_methods::set_primitives_magic_methods(&primitives, &mut unifier);
        (primitives, unifier)
    }

    pub fn new(resolver: &'a mut Box<dyn SymbolResolver>) -> Self {
        TopLevelManager {
            def_index: 1,
            ctx: TopLevelContext {
                definitions: Default::default(),
                unifiers: Default::default()
            },
            resolver,
            primitives: Self::make_primitives()
        }
    }

    pub fn register_top_level(&mut self, ast: &ast::Stmt<()>) -> Result<DefinitionId, String>{
        match &ast.node {
            ast::StmtKind::ClassDef {
                name, 
                bases,
                keywords,
                body,
                decorator_list
            } => {
                // ancestors and type_vars are found using the `bases` field
                let mut class_ancestors: Vec<DefinitionId> = Default::default();
                let mut class_type_vars: Vec<Type> = Default::default();
                for base in bases {
                    match &base.node {
                        ast::ExprKind::Subscript {value, slice, ..} => {
                            match &value.node {
                                ast::ExprKind::Name {id, ..} if id == "Generic" => {
                                    match &slice.node {
                                        ast::ExprKind::Tuple {elts, ..} => {
                                            for e in elts {
                                                class_type_vars.push(
                                                    self.resolver.
                                                        parse_type_name(e)
                                                        .ok_or_else(|| "unkown base class type".to_string())?
                                                ); // FIXME: is it correct to use this?
                                            }
                                        },
                                        _ => class_type_vars.push(
                                            self.resolver
                                                .parse_type_name(slice)
                                                .ok_or_else(|| "unkown base class type".to_string())?
                                        ) // FIXME: is it correct to use this?
                                    }
                                },
                                _ => return Err("only subscription on keyword Generic is allowed".into())
                            }
                        },

                        ast::ExprKind::Name {id, ..} => {
                            class_ancestors.push(self.resolver.get_function_def(id)) // FIXME: is it correct to use this?
                        }

                        _ => return Err("unsupported expression in the bases list".into())
                    }
                }

                // fields and methods are determined using the `body` field
                let class_fields: Vec<(String, Type)> = Default::default();
                let class_methods: Vec<(String, Type, DefinitionId)> = Default::default();
                for stmt in body {
                    match &stmt.node {
                        ast::StmtKind::FunctionDef {name, .. } if name != "__init__" => {
                            let result = self.register_top_level(stmt)?;
                            unimplemented!()
                        },

                        _ => unimplemented!()
                    }
                }

                let defs = self.ctx.definitions.write();
                let index = defs.len();

                unimplemented!()
            },
            
            ast::StmtKind::FunctionDef {name, ..} => {
                unimplemented!()
            }

            _ => Err("only expect function definition and class definition".into())
        }
    }
}