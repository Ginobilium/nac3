use std::borrow::{Borrow, BorrowMut};
use std::collections::HashSet;
use std::{collections::HashMap, sync::Arc};

use super::typecheck::type_inferencer::PrimitiveStore;
use super::typecheck::typedef::{SharedUnifier, Type, TypeEnum, Unifier};
use crate::symbol_resolver::SymbolResolver;
use crate::typecheck::typedef::{FunSignature, FuncArg};
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
        resolver: Option<Arc<Mutex<dyn SymbolResolver + Send + Sync>>>,
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
        resolver: Option<Arc<Mutex<dyn SymbolResolver + Send + Sync>>>,
    },
    Initializer {
        class_id: DefinitionId,
    },
}

pub struct TopLevelContext {
    pub definitions: Arc<RwLock<Vec<RwLock<TopLevelDef>>>>,
    pub unifiers: Arc<RwLock<Vec<(SharedUnifier, PrimitiveStore)>>>,
}

pub struct TopLevelComposer {
    // list of top level definitions, same as top level context
    pub definition_list: Arc<RwLock<Vec<RwLock<TopLevelDef>>>>,
    // list of top level Type, the index is same as the field `definition_list`
    pub ty_list: RwLock<Vec<Type>>,
    // list of top level ast, the index is same as the field `definition_list` and `ty_list`
    pub ast_list: RwLock<Vec<Option<ast::Stmt<()>>>>,
    // start as a primitive unifier, will add more top_level defs inside
    pub unifier: RwLock<Unifier>,
    // primitive store
    pub primitives: PrimitiveStore,
    // mangled class method name to def_id
    pub class_method_to_def_id: RwLock<HashMap<String, DefinitionId>>,
}

impl TopLevelComposer {
    pub fn to_top_level_context(&self) -> TopLevelContext {
        TopLevelContext {
            definitions: self.definition_list.clone(),
            // FIXME: all the big unifier or?
            unifiers: Default::default(),
        }
    }

    fn name_mangling(mut class_name: String, method_name: &str) -> String {
        class_name.push_str(method_name);
        class_name
    }

    pub fn make_primitives() -> (PrimitiveStore, Unifier) {
        let mut unifier = Unifier::new();
        let int32 = unifier.add_ty(TypeEnum::TObj {
            obj_id: DefinitionId(0),
            fields: HashMap::new().into(),
            params: HashMap::new().into(),
        });
        let int64 = unifier.add_ty(TypeEnum::TObj {
            obj_id: DefinitionId(1),
            fields: HashMap::new().into(),
            params: HashMap::new().into(),
        });
        let float = unifier.add_ty(TypeEnum::TObj {
            obj_id: DefinitionId(2),
            fields: HashMap::new().into(),
            params: HashMap::new().into(),
        });
        let bool = unifier.add_ty(TypeEnum::TObj {
            obj_id: DefinitionId(3),
            fields: HashMap::new().into(),
            params: HashMap::new().into(),
        });
        let none = unifier.add_ty(TypeEnum::TObj {
            obj_id: DefinitionId(4),
            fields: HashMap::new().into(),
            params: HashMap::new().into(),
        });
        let primitives = PrimitiveStore { int32, int64, float, bool, none };
        crate::typecheck::magic_methods::set_primitives_magic_methods(&primitives, &mut unifier);
        (primitives, unifier)
    }

    /// return a composer and things to make a "primitive" symbol resolver, so that the symbol
    /// resolver can later figure out primitive type definitions when passed a primitive type name
    pub fn new() -> (Vec<(String, DefinitionId, Type)>, Self) {
        let primitives = Self::make_primitives();

        let top_level_def_list = vec![
            RwLock::new(Self::make_top_level_class_def(0, None)),
            RwLock::new(Self::make_top_level_class_def(1, None)),
            RwLock::new(Self::make_top_level_class_def(2, None)),
            RwLock::new(Self::make_top_level_class_def(3, None)),
            RwLock::new(Self::make_top_level_class_def(4, None)),
        ];

        let ast_list: Vec<Option<ast::Stmt<()>>> = vec![None, None, None, None, None];

        let ty_list: Vec<Type> = vec![
            primitives.0.int32,
            primitives.0.int64,
            primitives.0.float,
            primitives.0.bool,
            primitives.0.none,
        ];

        let composer = TopLevelComposer {
            definition_list: RwLock::new(top_level_def_list).into(),
            ty_list: RwLock::new(ty_list),
            ast_list: RwLock::new(ast_list),
            primitives: primitives.0,
            unifier: primitives.1.into(),
            class_method_to_def_id: Default::default(),
        };
        (
            vec![
                ("int32".into(), DefinitionId(0), composer.primitives.int32),
                ("int64".into(), DefinitionId(1), composer.primitives.int64),
                ("float".into(), DefinitionId(2), composer.primitives.float),
                ("bool".into(), DefinitionId(3), composer.primitives.bool),
                ("none".into(), DefinitionId(4), composer.primitives.none),
            ],
            composer,
        )
    }

    /// already include the definition_id of itself inside the ancestors vector
    pub fn make_top_level_class_def(
        index: usize,
        resolver: Option<Arc<Mutex<dyn SymbolResolver + Send + Sync>>>,
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
        resolver: Option<Arc<Mutex<dyn SymbolResolver + Send + Sync>>>,
    ) -> TopLevelDef {
        TopLevelDef::Function {
            name,
            signature: ty,
            instance_to_symbol: Default::default(),
            instance_to_stmt: Default::default(),
            resolver,
        }
    }

    pub fn register_top_level(
        &mut self,
        ast: ast::Stmt<()>,
        resolver: Option<Arc<Mutex<dyn SymbolResolver + Send + Sync>>>,
    ) -> Result<(String, DefinitionId, Type), String> {
        // get write access to the lists
        let (mut def_list, mut ty_list, mut ast_list) =
            (self.definition_list.write(), self.ty_list.write(), self.ast_list.write());

        // will be deleted after tested
        assert_eq!(ty_list.len(), def_list.len());
        assert_eq!(def_list.len(), ast_list.len());

        match &ast.node {
            ast::StmtKind::ClassDef { name, body, .. } => {
                let class_name = name.to_string();
                let class_def_id = def_list.len();

                // add the class to the unifier
                let ty = self.unifier.write().add_ty(TypeEnum::TObj {
                    obj_id: DefinitionId(class_def_id),
                    fields: Default::default(),
                    params: Default::default(),
                });

                // add the class to the definition lists
                def_list
                    .push(Self::make_top_level_class_def(class_def_id, resolver.clone()).into());
                ty_list.push(ty);
                // since later when registering class method, ast will still be used,
                // here push None temporarly, later will push the ast
                ast_list.push(None);

                // parse class def body and register class methods into the def list.
                // module's symbol resolver would not know the name of the class methods,
                // thus cannot return their definition_id? so we have to manage it ourselves
                // by using the field `class_method_to_def_id`
                for b in body {
                    if let ast::StmtKind::FunctionDef { name, .. } = &b.node {
                        let fun_name = Self::name_mangling(class_name.clone(), name);
                        let def_id = def_list.len();

                        // add to unifier
                        let ty = self.unifier.write().add_ty(TypeEnum::TFunc(FunSignature {
                            args: Default::default(),
                            ret: self.primitives.none,
                            vars: Default::default(),
                        }));

                        // add to the definition list
                        def_list.push(
                            Self::make_top_level_function_def(
                                fun_name.clone(),
                                ty,
                                resolver.clone(),
                            )
                            .into(),
                        );
                        ty_list.push(ty);
                        // the ast of class method is in the class, push None in to the list here
                        ast_list.push(None);

                        // class method, do not let the symbol manager manage it, use our own map
                        self.class_method_to_def_id.write().insert(fun_name, DefinitionId(def_id));

                        // if it is the contructor, special handling is needed. In the above
                        // handling, we still add __init__ function to the class method
                        if name == "__init__" {
                            // NOTE: how can this later be fetched?
                            def_list.push(
                                TopLevelDef::Initializer { class_id: DefinitionId(class_def_id) }
                                    .into(),
                            );
                            // arbitarily push one to make sure the index is correct
                            ty_list.push(self.primitives.none);
                            ast_list.push(None);
                        }
                    }
                }

                // move the ast to the entry of the class in the ast_list
                ast_list[class_def_id] = Some(ast);

                // return
                Ok((class_name, DefinitionId(class_def_id), ty))
            }

            ast::StmtKind::FunctionDef { name, .. } => {
                let fun_name = name.to_string();

                // add to the unifier
                let ty = self.unifier.write().add_ty(TypeEnum::TFunc(FunSignature {
                    args: Default::default(),
                    ret: self.primitives.none,
                    vars: Default::default(),
                }));

                // add to the definition list
                def_list.push(
                    Self::make_top_level_function_def(name.into(), self.primitives.none, resolver)
                        .into(),
                );
                ty_list.push(ty);
                ast_list.push(Some(ast));

                // return
                Ok((fun_name, DefinitionId(def_list.len() - 1), ty))
            }

            _ => Err("only registrations of top level classes/functions are supprted".into()),
        }
    }

    pub fn analyze_top_level_class_type_var(&mut self) -> Result<(), String> {
        let mut def_list = self.definition_list.write();
        let ty_list = self.ty_list.read();
        let ast_list = self.ast_list.read();
        let mut unifier = self.unifier.write();

        for (def, ty, ast) in def_list
            .iter_mut()
            .zip(ty_list.iter())
            .zip(ast_list.iter())
            .map(|((x, y), z)| (x, y, z))
            .collect::<Vec<(&mut RwLock<TopLevelDef>, &Type, &Option<ast::Stmt<()>>)>>()
        {
            unimplemented!()
        }
        unimplemented!()
    }

    /// this should be called after all top level classes are registered, and
    /// will actually fill in those fields of the previous dummy one
    pub fn analyze_top_level(&mut self) -> Result<(), String> {
        let mut def_list = self.definition_list.write();
        let ty_list = self.ty_list.read();
        let ast_list = self.ast_list.read();
        let mut unifier = self.unifier.write();

        for (def, ty, ast) in def_list
            .iter_mut()
            .zip(ty_list.iter())
            .zip(ast_list.iter())
            .map(|((x, y), z)| (x, y, z))
            .collect::<Vec<(&mut RwLock<TopLevelDef>, &Type, &Option<ast::Stmt<()>>)>>()
        {
            // only analyze those entries with ast, and class_method(whose ast in class def)
            match ast {
                Some(ast::Located{node: ast::StmtKind::ClassDef {
                    bases,
                    body,
                    name: class_name,
                    ..
                }, .. }) => {
                    // get the mutable reference of the entry in the
                    // definition list, get the `TopLevelDef`
                    let (
                        def_ancestors,
                        def_fields,
                        def_methods,
                        def_type_vars,
                        resolver,
                    ) = if let TopLevelDef::Class {
                        object_id: _,
                        ancestors,
                        fields,
                        methods,
                        type_vars,
                        resolver: Some(resolver)
                    } = def.get_mut() {
                        (ancestors, fields, methods, type_vars, resolver.lock())
                    } else { unreachable!() };

                    // try to get mutable reference of the entry in the
                    // unification table, get the `TypeEnum`
                    let type_enum = unifier.get_ty(*ty);
                    let (
                        enum_params,
                        enum_fields
                    ) = if let TypeEnum::TObj {
                        params,
                        fields,
                        ..
                    } = type_enum.borrow() {
                        (params, fields)
                    } else { unreachable!() };

                    // ancestors and typevars associate with the class are analyzed by looking
                    // into the `bases` ast node
                    // `Generic` should only occur once, use this flag
                    let mut generic_occured = false;
                    // TODO: haven't check this yet
                    let mut occured_type_var: HashSet<Type> = Default::default();
                    // TODO: haven't check this yet
                    let mut occured_base: HashSet<DefinitionId> = Default::default();
                    for b in bases {
                        match &b.node {
                            // analyze typevars bounded to the class,
                            // only support things like `class A(Generic[T, V])`,
                            // things like `class A(Generic[T, V, ImportedModule.T])` is not supported
                            // i.e. only simple names are allowed in the subscript
                            // should update the TopLevelDef::Class.typevars and the TypeEnum::TObj.params
                            ast::ExprKind::Subscript {value, slice, ..} if {
                                // can only be `Generic[...]` and this can only appear once
                                if let ast::ExprKind::Name { id, .. } = &value.node {
                                    if id == "Generic" {
                                        if !generic_occured {
                                            generic_occured = true;
                                            true
                                        } else {
                                            return Err("Only single Generic[...] or Protocol[...] can be in bases".into())
                                        }
                                    } else { false }
                                } else { false }
                            } => {
                                match &slice.node {
                                    // `class Foo(Generic[T, V, P]):` multiple element inside the subscript
                                    ast::ExprKind::Tuple {elts, ..} => {
                                        let tys = elts
                                            .iter()
                                            // here parse_type_annotation should be fine,
                                            // since we only expect type vars, which is not relevant
                                            // to the top-level parsing
                                            .map(|x| resolver.parse_type_annotation(
                                                &self.to_top_level_context(),
                                                unifier.borrow_mut(),
                                                &self.primitives,
                                                x))
                                            .collect::<Result<Vec<_>, _>>()?;

                                        let ty_var_ids = tys
                                            .iter()
                                            .map(|t| {
                                                let tmp = unifier.get_ty(*t);
                                                // make sure it is type var
                                                if let TypeEnum::TVar {id, ..} = tmp.as_ref() {
                                                    Ok(*id)
                                                } else {
                                                    Err("Expect type variabls here".to_string())
                                                }
                                            })
                                            .collect::<Result<Vec<_>, _>>()?;

                                        // write to TypeEnum
                                        for (id, ty) in ty_var_ids.iter().zip(tys.iter()) {
                                            enum_params.borrow_mut().insert(*id, *ty);
                                        }

                                        // write to TopLevelDef
                                        for ty in tys{
                                            def_type_vars.push(ty)
                                        }
                                    },

                                    // `class Foo(Generic[T]):`, only single element
                                    _ => {
                                        let ty = resolver.parse_type_annotation(
                                            &self.to_top_level_context(),
                                            unifier.borrow_mut(),
                                            &self.primitives,
                                            &slice
                                        )?;

                                        let ty_var_id = if let TypeEnum::TVar { id, .. } = unifier
                                            .get_ty(ty)
                                            .as_ref() { *id } else {
                                                return Err("Expect type variabls here".to_string())
                                            };

                                        // write to TypeEnum
                                        enum_params.borrow_mut().insert(ty_var_id, ty);

                                        // write to TopLevelDef
                                        def_type_vars.push(ty);
                                    },
                                };
                            }

                            // analyze base classes, which is possible in
                            // other cases, we parse for the base class
                            // FIXME: calling parse_type_annotation here might cause some problem
                            // when the base class is parametrized `BaseClass[int, bool]`, since the
                            // analysis of type var of some class is not done yet.
                            // we can first only look at the name, and later check the
                            // parameter when others are done
                            // Or
                            // first get all the class' type var analyzed, and then
                            // analyze the base class
                            _ => {
                                let ty = resolver.parse_type_annotation(
                                    &self.to_top_level_context(),
                                    unifier.borrow_mut(),
                                    &self.primitives,
                                    b
                                )?;

                                let obj_def_id = if let TypeEnum::TObj { obj_id, .. } = unifier
                                    .get_ty(ty)
                                    .as_ref() {
                                        *obj_id
                                    } else {
                                        return Err("Expect concrete classes/types here".into())
                                    };

                                // write to TopLevelDef
                                def_ancestors.push(obj_def_id);
                            }
                        }
                    }

                    // class method and field are analyzed by
                    // looking into the class body ast node
                    // NOTE: should consider parents' method and fields(check re-def and add),
                    // but we do it later we go over these again after we finish analyze the
                    // fields/methods as declared in the ast
                    // method with same name should not occur twice, so use this
                    let defined_method: HashSet<String> = Default::default();
                    for stmt in body {
                        if let ast::StmtKind::FunctionDef {
                            name: func_name,
                            args,
                            body,
                            returns,
                            ..
                        } = &stmt.node {
                            // build type enum, need FunSignature {args, vars, ret}
                            // args. Now only args with no default TODO: other kinds of args
                            let func_args = args.args
                                .iter()
                                .map(|x| -> Result<FuncArg, String> {
                                    Ok(FuncArg {
                                    name: x.node.arg.clone(),
                                    ty: resolver.parse_type_annotation(
                                        &self.to_top_level_context(),
                                        unifier.borrow_mut(),
                                        &self.primitives,
                                        x
                                            .node
                                            .annotation
                                            .as_ref()
                                            .ok_or_else(|| "type annotations required for function parameters".to_string())?
                                        )?,
                                    default_value: None
                                    })
                                })
                                .collect::<Result<Vec<FuncArg>, _>>()?;
                            // vars. find TypeVars used in the argument type annotation
                            let func_vars = func_args
                                .iter()
                                .filter_map(|FuncArg { ty, .. } | {
                                    if let TypeEnum::TVar { id, .. } = unifier.get_ty(*ty).as_ref() {
                                        Some((*id, *ty))
                                    } else { None }
                                })
                                .collect::<HashMap<u32, Type>>();
                            // return type
                            let func_ret = resolver
                                .parse_type_annotation(
                                    &self.to_top_level_context(),
                                    unifier.borrow_mut(),
                                    &self.primitives,
                                    returns
                                        .as_ref()
                                        .ok_or_else(|| "return type annotations required here".to_string())?
                                        .as_ref(),
                                )?;
                            // build the TypeEnum
                            let func_type_sig = FunSignature {
                                args: func_args,
                                vars: func_vars,
                                ret: func_ret
                            };

                            // write to the TypeEnum and Def_list (by replacing the ty with the new Type created above)
                            let func_name_mangled = Self::name_mangling(class_name.clone(), func_name);
                            let def_id = self.class_method_to_def_id.read()[&func_name_mangled];
                            unimplemented!();


                            if func_name == "__init__" {
                                // special for constructor, need to look into the fields
                                // TODO: look into the function body and see
                            }
                        } else {
                            // do nothing. we do not care about things like this?
                            // class A:
                            //     a = 3
                            //     b = [2, 3]
                        }
                    }
                },

                // top level function definition
                Some(ast::Located{node: ast::StmtKind::FunctionDef {
                    name,
                    args,
                    body,
                    returns,
                    ..
                }, .. }) => {
                    // TODO:
                    unimplemented!()
                }

                // only expect class def and function def ast
                _ => return Err("only expect function and class definitions to be submitted here to be analyzed".into())
            }
        }
        Ok(())
    }
}
