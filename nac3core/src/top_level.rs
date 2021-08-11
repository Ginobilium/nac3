use std::{collections::HashMap, sync::Arc};

use super::typecheck::type_inferencer::PrimitiveStore;
use super::typecheck::typedef::{SharedUnifier, Type, TypeEnum, Unifier};
use crate::symbol_resolver::SymbolResolver;
use inkwell::context::Context;
use parking_lot::{Mutex, RwLock};
use rustpython_parser::ast::{self, Stmt};

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
    Initializer {
        class_id: DefinitionId,
    },
}

pub struct TopLevelContext {
    pub definitions: Arc<RwLock<Vec<RwLock<TopLevelDef>>>>,
    pub unifiers: Arc<RwLock<Vec<(SharedUnifier, PrimitiveStore)>>>,
    pub conetexts: Arc<RwLock<Vec<Mutex<Context>>>>,
}

pub struct TopLevelDefInfo<'a> {
    // like adding some info on top of the TopLevelDef for later parsing the class bases, method,
    // and function sigatures
    def: TopLevelDef,                         // the definition entry
    ty: Type,                                 // the entry in the top_level unifier
    ast: Option<ast::Stmt<()>>,               // the ast submitted by applications
    resolver: Option<&'a dyn SymbolResolver>, // the resolver
}

pub struct TopLevelComposer<'a> {
    pub definition_list: Vec<TopLevelDefInfo<'a>>,
    pub primitives: PrimitiveStore,
    pub unifier: Unifier,
}

impl<'a> TopLevelComposer<'a> {
    pub fn make_primitives() -> (PrimitiveStore, Unifier) {
        let mut unifier = Unifier::new();
        let int32 = unifier.add_ty(TypeEnum::TObj {
            obj_id: DefinitionId(0),
            fields: HashMap::new().into(),
            params: HashMap::new(),
        });
        let int64 = unifier.add_ty(TypeEnum::TObj {
            obj_id: DefinitionId(1),
            fields: HashMap::new().into(),
            params: HashMap::new(),
        });
        let float = unifier.add_ty(TypeEnum::TObj {
            obj_id: DefinitionId(2),
            fields: HashMap::new().into(),
            params: HashMap::new(),
        });
        let bool = unifier.add_ty(TypeEnum::TObj {
            obj_id: DefinitionId(3),
            fields: HashMap::new().into(),
            params: HashMap::new(),
        });
        let none = unifier.add_ty(TypeEnum::TObj {
            obj_id: DefinitionId(4),
            fields: HashMap::new().into(),
            params: HashMap::new(),
        });
        let primitives = PrimitiveStore { int32, int64, float, bool, none };
        crate::typecheck::magic_methods::set_primitives_magic_methods(&primitives, &mut unifier);
        (primitives, unifier)
    }

    pub fn new() -> Self {
        let primitives = Self::make_primitives();
        let definition_list: Vec<TopLevelDefInfo<'a>> = vec![
            TopLevelDefInfo {
                def: Self::make_top_level_class_def(0),
                ast: None,
                resolver: None,
                ty: primitives.0.int32,
            },
            TopLevelDefInfo {
                def: Self::make_top_level_class_def(1),
                ast: None,
                resolver: None,
                ty: primitives.0.int64,
            },
            TopLevelDefInfo {
                def: Self::make_top_level_class_def(2),
                ast: None,
                resolver: None,
                ty: primitives.0.float,
            },
            TopLevelDefInfo {
                def: Self::make_top_level_class_def(3),
                ast: None,
                resolver: None,
                ty: primitives.0.bool,
            },
            TopLevelDefInfo {
                def: Self::make_top_level_class_def(4),
                ast: None,
                resolver: None,
                ty: primitives.0.none,
            },
        ]; // the entries for primitive types
        TopLevelComposer { definition_list, primitives: primitives.0, unifier: primitives.1 }
    }

    pub fn make_top_level_class_def(index: usize) -> TopLevelDef {
        TopLevelDef::Class {
            object_id: DefinitionId(index),
            type_vars: Default::default(),
            fields: Default::default(),
            methods: Default::default(),
            ancestors: Default::default(),
        }
    }
    pub fn make_top_level_function_def(name: String, ty: Type) -> TopLevelDef {
        TopLevelDef::Function {
            name,
            signature: ty,
            instance_to_symbol: Default::default(),
            instance_to_stmt: Default::default(),
        }
    }

    // like to make and return a "primitive" symbol resolver? so that the symbol resolver can later
    // figure out primitive type definitions when passed a primitive type name
    pub fn get_primitives_definition(&self) -> Vec<(String, DefinitionId, Type)> {
        vec![
            ("int32".into(), DefinitionId(0), self.primitives.int32),
            ("int64".into(), DefinitionId(0), self.primitives.int32),
            ("float".into(), DefinitionId(0), self.primitives.int32),
            ("bool".into(), DefinitionId(0), self.primitives.int32),
            ("none".into(), DefinitionId(0), self.primitives.int32),
        ]
    }

    pub fn register_top_level(
        &mut self,
        ast: ast::Stmt<()>,
        resolver: &'a dyn SymbolResolver,
    ) -> Result<Vec<(String, DefinitionId, Type)>, String> {
        match &ast.node {
            ast::StmtKind::ClassDef { name, body, .. } => {
                let class_name = name.to_string();
                let def_id = self.definition_list.len();
                // add the class to the unifier
                let ty = self.unifier.add_ty(TypeEnum::TObj {
                    obj_id: DefinitionId(def_id),
                    fields: Default::default(),
                    params: Default::default(),
                });
                // add to the definition list
                self.definition_list.push(TopLevelDefInfo {
                    def: Self::make_top_level_class_def(def_id),
                    resolver: Some(resolver),
                    ast: Some(ast),
                    ty,
                });

                // TODO: parse class def body and register class methods into the def list?
                // FIXME: module's symbol resolver would not know the name of the class methods,
                // thus cannot return their definition_id? so we have to manage it ourselves?  or
                // do we return the class method list of (method_name, def_id, type) to application
                // to be used to build symbol resolver? <- current implementation

                Ok(vec![(class_name, DefinitionId(def_id), ty)]) // FIXME: need to add class method def
            }

            ast::StmtKind::FunctionDef { name, .. } => {
                let fun_name = name.to_string();
                let def_id = self.definition_list.len();
                // add to the unifier
                let ty =
                    self.unifier.add_ty(TypeEnum::TFunc(crate::typecheck::typedef::FunSignature {
                        args: Default::default(),
                        ret: self.primitives.none, // NOTE: this needs to be changed later
                        vars: Default::default(),
                    }));
                // add to the definition list
                self.definition_list.push(TopLevelDefInfo {
                    def: Self::make_top_level_function_def(
                        name.into(),
                        self.primitives.none, // NOTE: this needs to be changed later
                    ),
                    resolver: Some(resolver),
                    ast: Some(ast),
                    ty,
                });

                Ok(vec![(fun_name, DefinitionId(def_id), ty)])
            }

            _ => Err("only registrations of top level classes/functions are supprted".into()),
        }
    }

    /// this should be called after all top level classes are registered, and will actually fill in those fields of the previous dummy one
    pub fn analyze_top_level(&mut self) -> Result<(), String> {
        for mut d in &mut self.definition_list {
            if let (Some(ast), Some(resolver)) = (&d.ast, d.resolver) {
                match &ast.node {
                    ast::StmtKind::ClassDef {
                        name,
                        bases,
                        body,
                        ..
                    } => {
                        // ancestors and typevars associate with the class are analyzed by looking
                        // into the `bases` ast node
                        for b in bases {
                            match &b.node {
                                // base class, name directly available inside the module, can use
                                // this module's symbol resolver
                                ast::ExprKind::Name {id, ..} => {
                                    let def_id = resolver.get_identifier_def(id);
                                    unimplemented!()
                                },
                                // things can be like `class A(BaseModule.Base)`, here we have to
                                // get the symbol resolver of the module `BaseModule`?
                                ast::ExprKind::Attribute {value, attr, ..} => {
                                    // need to change symbol resolver in order to get the symbol
                                    // resolver of the imported module
                                    unimplemented!()
                                },
                                // typevars bounded to the class, things like
                                // `class A(Generic[T, V])`
                                ast::ExprKind::Subscript {value, slice, ..} => {
                                    if let ast::ExprKind::Name {id, ..} = &value.node {
                                        if id == "Generic" {
                                            // TODO: get typevars
                                            unimplemented!()
                                        } else {
                                            return Err("unknown type var".into())
                                        }
                                    }
                                },
                                _ => return Err("not supported".into())
                            }
                        }

                        // class method and field are analyzed by looking into the class body ast node
                        for stmt in body {
                            unimplemented!()
                        }
                    },

                    ast::StmtKind::FunctionDef {
                        name,
                        args,
                        body,
                        returns,
                        ..
                    } => {
                        unimplemented!()
                    }

                    _ => return Err("only expect function and class definitions to be submitted here to be analyzed".into())
                }
            }
        }
        Ok(())
    }
}
