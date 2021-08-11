use std::borrow::Borrow;
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
        // symbol resolver of the module defined the class, none if it is built-in type
        resolver: Option<Arc<Mutex<dyn SymbolResolver + Send>>>,
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
        // symbol resolver of the module defined the class
        resolver: Option<Arc<Mutex<dyn SymbolResolver + Send>>>,
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

pub fn name_mangling(mut class_name: String, method_name: &str) -> String {
    // need to further extend to more name mangling like instantiations of typevar
    class_name.push_str(method_name);
    class_name
}

pub struct TopLevelDefInfo {
    // like adding some info on top of the TopLevelDef for later parsing the class bases, method,
    // and function sigatures
    def: TopLevelDef, // the definition entry
    ty: Type,         // the entry in the top_level unifier
    ast: Option<ast::Stmt<()>>, // the ast submitted by applications, primitives and class methods will have None value here
                                //  resolver: Option<&'a dyn SymbolResolver> // the resolver
}

pub struct TopLevelComposer {
    pub definition_list: Vec<TopLevelDefInfo>,
    pub primitives: PrimitiveStore,
    pub unifier: Unifier,
}

impl TopLevelComposer {
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
        let definition_list: Vec<TopLevelDefInfo> = vec![
            TopLevelDefInfo {
                def: Self::make_top_level_class_def(0, None),
                ast: None,
                ty: primitives.0.int32,
            },
            TopLevelDefInfo {
                def: Self::make_top_level_class_def(1, None),
                ast: None,
                ty: primitives.0.int64,
            },
            TopLevelDefInfo {
                def: Self::make_top_level_class_def(2, None),
                ast: None,
                ty: primitives.0.float,
            },
            TopLevelDefInfo {
                def: Self::make_top_level_class_def(3, None),
                ast: None,
                ty: primitives.0.bool,
            },
            TopLevelDefInfo {
                def: Self::make_top_level_class_def(4, None),
                ast: None,
                ty: primitives.0.none,
            },
        ]; // the entries for primitive types
        TopLevelComposer { definition_list, primitives: primitives.0, unifier: primitives.1 }
    }

    /// already include the definition_id of itself inside the ancestors vector
    pub fn make_top_level_class_def(
        index: usize,
        resolver: Option<Arc<Mutex<dyn SymbolResolver + Send>>>,
    ) -> TopLevelDef {
        TopLevelDef::Class {
            object_id: DefinitionId(index),
            type_vars: Default::default(),
            fields: Default::default(),
            methods: Default::default(),
            ancestors: vec![DefinitionId(index)],
            resolver,
        }
    }

    pub fn make_top_level_function_def(
        name: String,
        ty: Type,
        resolver: Option<Arc<Mutex<dyn SymbolResolver + Send>>>,
    ) -> TopLevelDef {
        TopLevelDef::Function {
            name,
            signature: ty,
            instance_to_symbol: Default::default(),
            instance_to_stmt: Default::default(),
            resolver,
        }
    }

    // like to make and return a "primitive" symbol resolver? so that the symbol resolver
    // can later figure out primitive type definitions when passed a primitive type name
    pub fn get_primitives_definition(&self) -> Vec<(String, DefinitionId, Type)> {
        vec![
            ("int32".into(), DefinitionId(0), self.primitives.int32),
            ("int64".into(), DefinitionId(1), self.primitives.int64),
            ("float".into(), DefinitionId(2), self.primitives.float),
            ("bool".into(), DefinitionId(3), self.primitives.bool),
            ("none".into(), DefinitionId(4), self.primitives.none),
        ]
    }

    pub fn register_top_level(
        &mut self,
        ast: ast::Stmt<()>,
        resolver: Option<Arc<Mutex<dyn SymbolResolver + Send>>>,
    ) -> Result<Vec<(String, DefinitionId, Type)>, String> {
        match &ast.node {
            ast::StmtKind::ClassDef { name, body, .. } => {
                let class_name = name.to_string();
                let class_def_id = self.definition_list.len();

                // add the class to the unifier
                let ty = self.unifier.add_ty(TypeEnum::TObj {
                    obj_id: DefinitionId(class_def_id),
                    fields: Default::default(),
                    params: Default::default(),
                });

                let mut ret_vector: Vec<(String, DefinitionId, Type)> =
                    vec![(class_name.clone(), DefinitionId(class_def_id), ty)];
                // parse class def body and register class methods into the def list
                // NOTE: module's symbol resolver would not know the name of the class methods,
                // thus cannot return their definition_id? so we have to manage it ourselves?
                // or do we return the class method list of (method_name, def_id, type) to
                // application to be used to build symbol resolver? <- current implementation
                // FIXME: better do not return and let symbol resolver to manage the mangled name
                for b in body {
                    if let ast::StmtKind::FunctionDef { name, .. } = &b.node {
                        let fun_name = name_mangling(class_name.clone(), name);
                        let def_id = self.definition_list.len();
                        // add to unifier
                        let ty = self.unifier.add_ty(TypeEnum::TFunc(
                            crate::typecheck::typedef::FunSignature {
                                args: Default::default(),
                                ret: self.primitives.none,
                                vars: Default::default(),
                            },
                        ));
                        // add to the definition list
                        self.definition_list.push(TopLevelDefInfo {
                            def: Self::make_top_level_function_def(fun_name.clone(), ty, None), // FIXME:
                            ty,
                            ast: None, // since it is inside the class def body statments
                        });
                        ret_vector.push((fun_name, DefinitionId(def_id), ty));

                        // if it is the contructor, special handling is needed. In the above
                        // handling, we still add __init__ function to the class method
                        if name == "__init__" {
                            self.definition_list.push(TopLevelDefInfo {
                                def: TopLevelDef::Initializer {
                                    class_id: DefinitionId(class_def_id),
                                },
                                ty: self.primitives.none, // arbitary picked one
                                ast: None, // it is inside the class def body statments
                            })
                            // FIXME: should we return this to the symbol resolver?, should be yes
                        }
                    } else {
                    } // else do nothing
                }
                // add to the definition list
                self.definition_list.push(TopLevelDefInfo {
                    def: Self::make_top_level_class_def(class_def_id, resolver),
                    ast: Some(ast),
                    ty,
                });

                Ok(ret_vector)
            }

            ast::StmtKind::FunctionDef { name, .. } => {
                let fun_name = name.to_string();
                let def_id = self.definition_list.len();
                // add to the unifier
                let ty =
                    self.unifier.add_ty(TypeEnum::TFunc(crate::typecheck::typedef::FunSignature {
                        args: Default::default(),
                        ret: self.primitives.none,
                        vars: Default::default(),
                    }));
                // add to the definition list
                self.definition_list.push(TopLevelDefInfo {
                    def: Self::make_top_level_function_def(
                        name.into(),
                        self.primitives.none,
                        resolver,
                    ),
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
            if let Some(ast) = &d.ast {
                match &ast.node {
                    ast::StmtKind::ClassDef {
                        bases,
                        body,
                        ..
                    } => {
                        // get the mutable reference of the entry in the definition list, get the `TopLevelDef`
                        let (_,
                            ancestors,
                            fields,
                            methods,
                            type_vars,
                            // resolver,
                        ) = if let TopLevelDef::Class {
                            object_id,
                            ancestors,
                            fields,
                            methods,
                            type_vars,
                            resolver
                        } = &mut d.def {
                            (object_id, ancestors, fields, methods, type_vars) // FIXME: this unwrap is not safe
                        } else { unreachable!() };

                        // try to get mutable reference of the entry in the unification table, get the `TypeEnum`
                        let (params,
                            fields
                        ) = if let TypeEnum::TObj {
                            params, // FIXME: this params is immutable, even if this is mutable, what should the key be, get the original typevar's var_id?
                            fields,
                            ..
                        } = self.unifier.get_ty(d.ty).borrow() {
                            (params, fields)
                        } else { unreachable!() };

                        // ancestors and typevars associate with the class are analyzed by looking
                        // into the `bases` ast node
                        for b in bases {
                            match &b.node {
                                // typevars bounded to the class, things like `class A(Generic[T, V, ImportedModule.T])`
                                // should update the TopLevelDef::Class.typevars and the TypeEnum::TObj.params
                                ast::ExprKind::Subscript {value, slice, ..} if {
                                    if let ast::ExprKind::Name {id, ..} = &value.node {
                                        id == "Generic"
                                    } else { false }
                                } => {
                                    match &slice.node {
                                        // `class Foo(Generic[T, V, P, ImportedModule.T]):`
                                        ast::ExprKind::Tuple {elts, ..} => {
                                            for e in elts {
                                                // TODO: I'd better parse the node to get the Type of the type vars(can have things like: A.B.C.typevar?)
                                                match &e.node {
                                                    ast::ExprKind::Name {id, ..} => {
                                                        // the def_list
                                                        // type_vars.push(resolver.get_symbol_type(id).ok_or_else(|| "unknown type variable".to_string())?); FIXME:

                                                        // the TypeEnum of the class
                                                        // FIXME: the `params` destructed above is not mutable, even if this is mutable, what should the key be?
                                                        unimplemented!()
                                                    },

                                                    _ => unimplemented!()
                                                }
                                            }
                                        },

                                        // `class Foo(Generic[T]):`
                                        ast::ExprKind::Name {id, ..} => {
                                            // the def_list
                                            // type_vars.push(resolver.get_symbol_type(id).ok_or_else(|| "unknown type variable".to_string())?); FIXME:

                                            // the TypeEnum of the class
                                            // FIXME: the `params` destructed above is not mutable, even if this is mutable, what should the key be?
                                            unimplemented!()
                                        },

                                        // `class Foo(Generic[ImportedModule.T])`
                                        ast::ExprKind::Attribute {value, attr, ..} => {
                                            // TODO:
                                            unimplemented!()
                                        },

                                        _ => return Err("not supported".into()) // NOTE: it is really all the supported cases?
                                    };
                                },

                                // base class, name directly available inside the
                                // module, can use this module's symbol resolver
                                ast::ExprKind::Name {id, ..} => {
                                    // let def_id = resolver.get_identifier_def(id); FIXME:
                                    // the definition list
                                    // ancestors.push(def_id);
                                },

                                // base class, things can be like `class A(BaseModule.Base)`, here we have to get the
                                // symbol resolver of the module `BaseModule`?
                                ast::ExprKind::Attribute {value, attr, ..} => {
                                    if let ast::ExprKind::Name {id, ..} = &value.node {
                                        // if let Some(base_module_resolver) = resolver.get_module_resolver(id) {
                                        //     let def_id = base_module_resolver.get_identifier_def(attr);
                                        //     // the definition list
                                        //     ancestors.push(def_id);
                                        // } else { return Err("unkown imported module".into()) } FIXME:
                                    } else { return Err("unkown imported module".into()) }
                                },

                                // `class Foo(ImportedModule.A[int, bool])`, A is a class with associated type variables
                                ast::ExprKind::Subscript {value, slice, ..} => {
                                    unimplemented!()
                                },
                                _ => return Err("not supported".into())
                            }
                        }

                        // class method and field are analyzed by
                        // looking into the class body ast node
                        for stmt in body {
                            if let ast::StmtKind::FunctionDef {
                                    name,
                                    args,
                                    body,
                                    returns,
                                    ..
                            } = &stmt.node {

                            } else {  }
                            // do nothing. we do not care about things like this?
                            // class A:
                            //     a = 3
                            //     b = [2, 3]


                        }
                    },

                    // top level function definition
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

