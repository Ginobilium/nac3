use std::borrow::BorrowMut;
use std::ops::{Deref, DerefMut};
use std::{collections::HashMap, collections::HashSet, sync::Arc};

use super::typecheck::type_inferencer::PrimitiveStore;
use super::typecheck::typedef::{SharedUnifier, Type, TypeEnum, Unifier};
use crate::typecheck::typedef::{FunSignature, FuncArg};
use crate::{symbol_resolver::SymbolResolver, typecheck::typedef::Mapping};
use itertools::Itertools;
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

impl TopLevelDef {
    fn get_function_type(&self) -> Result<Type, String> {
        if let Self::Function { signature, .. } = self {
            Ok(*signature)
        } else {
            Err("only expect function def here".into())
        }
    }
}

pub struct TopLevelContext {
    pub definitions: Arc<RwLock<Vec<Arc<RwLock<TopLevelDef>>>>>,
    pub unifiers: Arc<RwLock<Vec<(SharedUnifier, PrimitiveStore)>>>,
}

pub struct TopLevelComposer {
    // list of top level definitions, same as top level context
    pub definition_ast_list: Arc<RwLock<Vec<(Arc<RwLock<TopLevelDef>>, Option<ast::Stmt<()>>)>>>,
    // start as a primitive unifier, will add more top_level defs inside
    pub unifier: Unifier,
    // primitive store
    pub primitives: PrimitiveStore,
    // mangled class method name to def_id
    pub class_method_to_def_id: HashMap<String, DefinitionId>,
    // record the def id of the classes whoses fields and methods are to be analyzed
    pub to_be_analyzed_class: Vec<DefinitionId>,
}

impl TopLevelComposer {
    pub fn to_top_level_context(&self) -> TopLevelContext {
        let def_list =
            self.definition_ast_list.read().iter().map(|(x, _)| x.clone()).collect::<Vec<_>>();
        TopLevelContext {
            definitions: RwLock::new(def_list).into(),
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
            Arc::new(RwLock::new(Self::make_top_level_class_def(0, None))),
            Arc::new(RwLock::new(Self::make_top_level_class_def(1, None))),
            Arc::new(RwLock::new(Self::make_top_level_class_def(2, None))),
            Arc::new(RwLock::new(Self::make_top_level_class_def(3, None))),
            Arc::new(RwLock::new(Self::make_top_level_class_def(4, None))),
        ];

        let ast_list: Vec<Option<ast::Stmt<()>>> = vec![None, None, None, None, None];

        let composer = TopLevelComposer {
            definition_ast_list: RwLock::new(
                top_level_def_list.into_iter().zip(ast_list).collect_vec(),
            )
            .into(),
            primitives: primitives.0,
            unifier: primitives.1,
            class_method_to_def_id: Default::default(),
            to_be_analyzed_class: Default::default(),
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
    /// when first regitering, the type_vars, fields, methods, ancestors are invalid
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

    /// when first registering, the type is a invalid value
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

    /// step 0, register, just remeber the names of top level classes/function
    pub fn register_top_level(
        &mut self,
        ast: ast::Stmt<()>,
        resolver: Option<Arc<Mutex<dyn SymbolResolver + Send + Sync>>>,
    ) -> Result<(String, DefinitionId), String> {
        let mut def_list = self.definition_ast_list.write();
        match &ast.node {
            ast::StmtKind::ClassDef { name, body, .. } => {
                let class_name = name.to_string();
                let class_def_id = def_list.len();

                // add the class to the definition lists
                // since later when registering class method, ast will still be used,
                // here push None temporarly, later will move the ast inside
                let mut class_def_ast = (
                    Arc::new(RwLock::new(Self::make_top_level_class_def(
                        class_def_id,
                        resolver.clone(),
                    ))),
                    None,
                );

                // parse class def body and register class methods into the def list.
                // module's symbol resolver would not know the name of the class methods,
                // thus cannot return their definition_id
                let mut class_method_name_def_ids: Vec<(
                    String,
                    Arc<RwLock<TopLevelDef>>,
                    DefinitionId,
                )> = Vec::new();
                let mut class_method_index_offset = 0;
                for b in body {
                    if let ast::StmtKind::FunctionDef { name: method_name, .. } = &b.node {
                        let method_name = Self::name_mangling(class_name.clone(), method_name);
                        let method_def_id = def_list.len() + {
                            class_method_index_offset += 1;
                            class_method_index_offset
                        };

                        // dummy method define here
                        // the ast of class method is in the class, push None in to the list here
                        class_method_name_def_ids.push((
                            method_name.clone(),
                            RwLock::new(Self::make_top_level_function_def(
                                method_name.clone(),
                                self.primitives.none,
                                resolver.clone(),
                            ))
                            .into(),
                            DefinitionId(method_def_id),
                        ));
                    }
                }
                // move the ast to the entry of the class in the ast_list
                class_def_ast.1 = Some(ast);

                // now class_def_ast and class_method_def_ast_ids are ok, put them into actual def list in correct order
                def_list.push(class_def_ast);
                for (name, def, id) in class_method_name_def_ids {
                    def_list.push((def, None));
                    self.class_method_to_def_id.insert(name, id);
                }

                // put the constructor into the def_list
                def_list.push((
                    RwLock::new(TopLevelDef::Initializer { class_id: DefinitionId(class_def_id) })
                        .into(),
                    None,
                ));

                // class, put its def_id into the to be analyzed set
                self.to_be_analyzed_class.push(DefinitionId(class_def_id));

                Ok((class_name, DefinitionId(class_def_id)))
            }

            ast::StmtKind::FunctionDef { name, .. } => {
                let fun_name = name.to_string();

                // add to the definition list
                def_list.push((
                    RwLock::new(Self::make_top_level_function_def(
                        name.into(),
                        self.primitives.none,
                        resolver,
                    ))
                    .into(),
                    Some(ast),
                ));

                // return
                Ok((fun_name, DefinitionId(def_list.len() - 1)))
            }

            _ => Err("only registrations of top level classes/functions are supprted".into()),
        }
    }

    /// step 1, analyze the type vars associated with top level class
    fn analyze_top_level_class_type_var(&mut self) -> Result<(), String> {
        let mut def_list = self.definition_ast_list.write();
        let converted_top_level = &self.to_top_level_context();
        let primitives = &self.primitives;
        let unifier = &mut self.unifier;

        for (class_def, class_ast) in def_list.iter_mut() {
            // only deal with class def here
            let mut class_def = class_def.write();
            let (class_bases_ast, class_def_type_vars, class_resolver) = {
                if let TopLevelDef::Class { type_vars, resolver, .. } = class_def.deref_mut() {
                    if let Some(ast::Located {
                        node: ast::StmtKind::ClassDef { bases, .. }, ..
                    }) = class_ast
                    {
                        (bases, type_vars, resolver)
                    } else {
                        unreachable!("must be both class")
                    }
                } else {
                    continue;
                }
            };
            let class_resolver = class_resolver.as_ref().unwrap().lock();

            let mut is_generic = false;
            for b in class_bases_ast {
                match &b.node {
                    // analyze typevars bounded to the class,
                    // only support things like `class A(Generic[T, V])`,
                    // things like `class A(Generic[T, V, ImportedModule.T])` is not supported
                    // i.e. only simple names are allowed in the subscript
                    // should update the TopLevelDef::Class.typevars and the TypeEnum::TObj.params
                    ast::ExprKind::Subscript { value, slice, .. } if matches!(&value.node, ast::ExprKind::Name { id, .. } if id == "Generic") =>
                    {
                        if !is_generic {
                            is_generic = true;
                        } else {
                            return Err("Only single Generic[...] can be in bases".into());
                        }

                        // if `class A(Generic[T, V, G])`
                        if let ast::ExprKind::Tuple { elts, .. } = &slice.node {
                            // parse the type vars
                            let type_vars = elts
                                .iter()
                                .map(|e| {
                                    class_resolver.parse_type_annotation(
                                        converted_top_level,
                                        unifier.borrow_mut(),
                                        primitives,
                                        e,
                                    )
                                })
                                .collect::<Result<Vec<_>, _>>()?;

                            // check if all are unique type vars
                            let mut occured_type_var_id: HashSet<u32> = HashSet::new();
                            let all_unique_type_var = type_vars.iter().all(|x| {
                                let ty = unifier.get_ty(*x);
                                if let TypeEnum::TVar { id, .. } = ty.as_ref() {
                                    occured_type_var_id.insert(*id)
                                } else {
                                    false
                                }
                            });

                            if !all_unique_type_var {
                                return Err("expect unique type variables".into());
                            }

                            // add to TopLevelDef
                            class_def_type_vars.extend(type_vars);

                        // `class A(Generic[T])`
                        } else {
                            let ty = class_resolver.parse_type_annotation(
                                converted_top_level,
                                unifier.borrow_mut(),
                                primitives,
                                &slice,
                            )?;
                            // check if it is type var
                            let is_type_var =
                                matches!(unifier.get_ty(ty).as_ref(), &TypeEnum::TVar { .. });
                            if !is_type_var {
                                return Err("expect type variable here".into());
                            }

                            // add to TopLevelDef
                            class_def_type_vars.push(ty);
                        }
                    }

                    // if others, do nothing in this function
                    _ => continue,
                }
            }
        }
        Ok(())
    }

    /// step 2, base classes. Need to separate step1 and step2 for this reason:
    /// `class B(Generic[T, V]);
    /// class A(B[int, bool])`
    /// if the type var associated with class `B` has not been handled properly,
    /// the parse of type annotation of `B[int, bool]` will fail
    fn analyze_top_level_class_bases(&mut self) -> Result<(), String> {
        let mut def_list = self.definition_ast_list.write();
        let converted_top_level = &self.to_top_level_context();
        let primitives = &self.primitives;
        let unifier = &mut self.unifier;

        for (class_def, class_ast) in def_list.iter_mut() {
            let mut class_def = class_def.write();
            let (class_bases, class_ancestors, class_resolver) = {
                if let TopLevelDef::Class { ancestors, resolver, .. } = class_def.deref_mut() {
                    if let Some(ast::Located {
                        node: ast::StmtKind::ClassDef { bases, .. }, ..
                    }) = class_ast
                    {
                        (bases, ancestors, resolver)
                    } else {
                        unreachable!("must be both class")
                    }
                } else {
                    continue;
                }
            };
            let class_resolver = class_resolver.as_ref().unwrap().lock();
            for b in class_bases {
                // type vars have already been handled, so skip on `Generic[...]`
                if let ast::ExprKind::Subscript { value, .. } = &b.node {
                    if let ast::ExprKind::Name { id, .. } = &value.node {
                        if id == "Generic" {
                            continue;
                        }
                    }
                }
                // get the def id of the base class
                let base_ty = class_resolver.parse_type_annotation(
                    converted_top_level,
                    unifier.borrow_mut(),
                    primitives,
                    b,
                )?;
                let base_id =
                    if let TypeEnum::TObj { obj_id, .. } = unifier.get_ty(base_ty).as_ref() {
                        *obj_id
                    } else {
                        return Err("expect concrete class/type to be base class".into());
                    };

                // write to the class ancestors, make sure the uniqueness
                if !class_ancestors.contains(&base_id) {
                    class_ancestors.push(base_id);
                } else {
                    return Err("cannot specify the same base class twice".into());
                }
            }
        }
        Ok(())
    }

    /// step 3, class fields and methods
    // FIXME: need analyze base classes here
    // FIXME: how to deal with self type
    // FIXME: how to prevent cycles
    fn analyze_top_level_class_fields_methods(&mut self) -> Result<(), String> {
        let mut def_ast_list = self.definition_ast_list.write();
        let converted_top_level = &self.to_top_level_context();
        let primitives = &self.primitives;
        let to_be_analyzed_class = &mut self.to_be_analyzed_class;
        let unifier = &mut self.unifier;

        'class: loop {
            if to_be_analyzed_class.is_empty() {
                break;
            }

            let class_ind = to_be_analyzed_class.remove(0).0;
            let (class_name, class_body, class_resolver) = {
                let (class_def, class_ast) = &mut def_ast_list[class_ind];
                if let Some(ast::Located {
                    node: ast::StmtKind::ClassDef { name, body, .. }, ..
                }) = class_ast.as_ref()
                {
                    if let TopLevelDef::Class { resolver, .. } = class_def.write().deref() {
                        (name, body, resolver.as_ref().unwrap().clone())
                    } else {
                        unreachable!()
                    }
                } else {
                    unreachable!("should be class def ast")
                }
            };

            // need these vectors to check re-defining methods, class fields
            // and store the parsed result in case some method cannot be typed for now
            let mut class_methods_parsing_result: Vec<(String, Type, DefinitionId)> = vec![];
            let mut class_fields_parsing_result: Vec<(String, Type)> = vec![];
            for b in class_body {
                if let ast::StmtKind::FunctionDef {
                    args: method_args_ast,
                    body: method_body_ast,
                    name: method_name,
                    returns: method_returns_ast,
                    ..
                } = &b.node
                {
                    let arg_name_tys: Vec<(String, Type)> = {
                        let mut result = vec![];
                        for a in &method_args_ast.args {
                            if a.node.arg != "self" {
                                let annotation = a
                                    .node
                                    .annotation
                                    .as_ref()
                                    .ok_or_else(|| {
                                        "type annotation for function parameter is needed"
                                            .to_string()
                                    })?
                                    .as_ref();

                                let ty = class_resolver.as_ref().lock().parse_type_annotation(
                                    converted_top_level,
                                    unifier.borrow_mut(),
                                    primitives,
                                    annotation,
                                )?;
                                if !Self::check_ty_analyzed(ty, unifier, to_be_analyzed_class) {
                                    to_be_analyzed_class.push(DefinitionId(class_ind));
                                    continue 'class;
                                }
                                result.push((a.node.arg.to_string(), ty));
                            } else {
                                // TODO: handle self, how
                                unimplemented!()
                            }
                        }
                        result
                    };

                    let method_type_var = arg_name_tys
                        .iter()
                        .filter_map(|(_, ty)| {
                            let ty_enum = unifier.get_ty(*ty);
                            if let TypeEnum::TVar { id, .. } = ty_enum.as_ref() {
                                Some((*id, *ty))
                            } else {
                                None
                            }
                        })
                        .collect::<Mapping<u32>>();

                    let ret_ty = {
                        if method_name != "__init__" {
                            let ty = method_returns_ast
                                .as_ref()
                                .map(|x| {
                                    class_resolver.as_ref().lock().parse_type_annotation(
                                        converted_top_level,
                                        unifier.borrow_mut(),
                                        primitives,
                                        x.as_ref(),
                                    )
                                })
                                .ok_or_else(|| "return type annotation error".to_string())??;
                            if !Self::check_ty_analyzed(ty, unifier, to_be_analyzed_class) {
                                to_be_analyzed_class.push(DefinitionId(class_ind));
                                continue 'class;
                            } else {
                                ty
                            }
                        } else {
                            // TODO: __init__ function, self type, how
                            unimplemented!()
                        }
                    };

                    // handle fields
                    let class_field_name_tys: Option<Vec<(String, Type)>> = if method_name
                        == "__init__"
                    {
                        let mut result: Vec<(String, Type)> = vec![];
                        for body in method_body_ast {
                            match &body.node {
                                ast::StmtKind::AnnAssign { target, annotation, .. }
                                    if {
                                        if let ast::ExprKind::Attribute { value, .. } = &target.node
                                        {
                                            matches!(
                                                &value.node,
                                                ast::ExprKind::Name { id, .. } if id == "self")
                                        } else {
                                            false
                                        }
                                    } =>
                                {
                                    let field_ty =
                                        class_resolver.as_ref().lock().parse_type_annotation(
                                            converted_top_level,
                                            unifier.borrow_mut(),
                                            primitives,
                                            annotation.as_ref(),
                                        )?;
                                    if !Self::check_ty_analyzed(
                                        field_ty,
                                        unifier,
                                        to_be_analyzed_class,
                                    ) {
                                        to_be_analyzed_class.push(DefinitionId(class_ind));
                                        continue 'class;
                                    } else {
                                        result.push((
                                            if let ast::ExprKind::Attribute { attr, .. } =
                                                &target.node
                                            {
                                                attr.to_string()
                                            } else {
                                                unreachable!()
                                            },
                                            field_ty,
                                        ))
                                    }
                                }

                                // exclude those without type annotation
                                ast::StmtKind::Assign { targets, .. }
                                    if {
                                        if let ast::ExprKind::Attribute { value, .. } =
                                            &targets[0].node
                                        {
                                            matches!(
                                                &value.node,
                                                ast::ExprKind::Name {id, ..} if id == "self")
                                        } else {
                                            false
                                        }
                                    } =>
                                {
                                    return Err("class fields type annotation needed".into())
                                }

                                // do nothing
                                _ => {}
                            }
                        }
                        Some(result)
                    } else {
                        None
                    };

                    // current method all type ok, put the current method into the list
                    if class_methods_parsing_result.iter().any(|(name, _, _)| name == method_name) {
                        return Err("duplicate method definition".into());
                    } else {
                        class_methods_parsing_result.push((
                            method_name.clone(),
                            unifier.add_ty(TypeEnum::TFunc(
                                FunSignature {
                                    ret: ret_ty,
                                    args: arg_name_tys
                                        .into_iter()
                                        .map(|(name, ty)| FuncArg { name, ty, default_value: None })
                                        .collect_vec(),
                                    vars: method_type_var,
                                }
                                .into(),
                            )),
                            *self
                                .class_method_to_def_id
                                .get(&Self::name_mangling(class_name.clone(), method_name))
                                .unwrap(),
                        ))
                    }

                    // put the fiedlds inside
                    if let Some(class_field_name_tys) = class_field_name_tys {
                        assert!(class_fields_parsing_result.is_empty());
                        class_fields_parsing_result.extend(class_field_name_tys);
                    }
                } else {
                    // what should we do with `class A: a = 3`?
                    // do nothing, continue the for loop to iterate class ast
                    continue;
                }
            }

            // now it should be confirmed that every
            // methods and fields of the class can be correctly typed, put the results
            // into the actual class def method and fields field
            let (class_def, _) = &def_ast_list[class_ind];
            let mut class_def = class_def.write();
            if let TopLevelDef::Class { fields, methods, .. } = class_def.deref_mut() {
                for (ref n, ref t) in class_fields_parsing_result {
                    fields.push((n.clone(), *t));
                }
                for (n, t, id) in &class_methods_parsing_result {
                    methods.push((n.clone(), *t, *id));
                }
            } else {
                unreachable!()
            }

            // change the signature field of the class methods
            for (_, ty, id) in &class_methods_parsing_result {
                let (method_def, _) = &def_ast_list[id.0];
                let mut method_def = method_def.write();
                if let TopLevelDef::Function { signature, .. } = method_def.deref_mut() {
                    *signature = *ty;
                }
            }
        }
        Ok(())
    }

    fn analyze_top_level_function(&mut self) -> Result<(), String> {
        unimplemented!()
    }

    fn analyze_top_level_field_instantiation(&mut self) -> Result<(), String> {
        unimplemented!()
    }

    fn check_ty_analyzed(ty: Type, unifier: &mut Unifier, to_be_analyzed: &[DefinitionId]) -> bool {
        let type_enum = unifier.get_ty(ty);
        match type_enum.as_ref() {
            TypeEnum::TObj { obj_id, .. } => !to_be_analyzed.contains(obj_id),
            TypeEnum::TVirtual { ty } => {
                if let TypeEnum::TObj { obj_id, .. } = unifier.get_ty(*ty).as_ref() {
                    !to_be_analyzed.contains(obj_id)
                } else {
                    unreachable!()
                }
            }
            TypeEnum::TVar { .. } => true,
            _ => unreachable!(),
        }
    }
}
