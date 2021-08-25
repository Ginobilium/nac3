use std::{collections::{HashMap, HashSet}, sync::Arc, ops::{Deref, DerefMut}, borrow::BorrowMut};

use super::typecheck::type_inferencer::PrimitiveStore;
use super::typecheck::typedef::{FunSignature, FuncArg, SharedUnifier, Type, TypeEnum, Unifier};
use crate::{
    symbol_resolver::SymbolResolver,
    typecheck::{type_inferencer::CodeLocation, typedef::CallId},
};
use itertools::{izip, Itertools};
use parking_lot::RwLock;
use rustpython_parser::ast::{self, Stmt};

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct DefinitionId(pub usize);

mod type_annotation;
use type_annotation::*;

pub struct FunInstance {
    pub body: Vec<Stmt<Option<Type>>>,
    pub calls: HashMap<CodeLocation, CallId>,
    pub subst: HashMap<u32, Type>,
    pub unifier_id: usize,
}

pub enum TopLevelDef {
    Class {
        // name for error messages and symbols
        name: String,
        // object ID used for TypeEnum
        object_id: DefinitionId,
        // type variables bounded to the class.
        // the first field in the tuple is the var_id of the
        // original typevar defined in the top level and returned
        // by the symbol resolver
        type_vars: Vec<(u32, Type)>,
        // class fields
        fields: Vec<(String, Type)>,
        // class methods, pointing to the corresponding function definition.
        methods: Vec<(String, Type, DefinitionId)>,
        // ancestor classes, including itself.
        ancestors: Vec<TypeAnnotation>,
        // symbol resolver of the module defined the class, none if it is built-in type
        resolver: Option<Arc<Box<dyn SymbolResolver + Send + Sync>>>,
    },
    Function {
        // prefix for symbol, should be unique globally, and not ending with numbers
        name: String,
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
    Initializer {
        class_id: DefinitionId,
    },
}

pub struct TopLevelContext {
    pub definitions: Arc<RwLock<Vec<Arc<RwLock<TopLevelDef>>>>>,
    pub unifiers: Arc<RwLock<Vec<(SharedUnifier, PrimitiveStore)>>>,
}

pub struct TopLevelComposer {
    // list of top level definitions, same as top level context
    pub definition_ast_list: Vec<(Arc<RwLock<TopLevelDef>>, Option<ast::Stmt<()>>)>,
    // start as a primitive unifier, will add more top_level defs inside
    pub unifier: Unifier,
    // primitive store
    pub primitives_ty: PrimitiveStore,
    // keyword list to prevent same custom def class name
    pub keyword_list: Vec<String>,
}

impl Default for TopLevelComposer {
    fn default() -> Self {
        Self::new()
    }
}

impl TopLevelComposer {
    pub fn make_top_level_context(self) -> TopLevelContext {
        TopLevelContext {
            definitions: RwLock::new(
                self.definition_ast_list.into_iter().map(|(x, ..)| x).collect::<Vec<_>>(),
            )
            .into(),
            // FIXME: all the big unifier or?
            unifiers: Default::default(),
        }
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
    // TODO: add list and tuples?
    pub fn new() -> Self {
        let primitives = Self::make_primitives();

        let top_level_def_list = vec![
            Arc::new(RwLock::new(Self::make_top_level_class_def(0, None, "int32"))),
            Arc::new(RwLock::new(Self::make_top_level_class_def(1, None, "int64"))),
            Arc::new(RwLock::new(Self::make_top_level_class_def(2, None, "float"))),
            Arc::new(RwLock::new(Self::make_top_level_class_def(3, None, "bool"))),
            Arc::new(RwLock::new(Self::make_top_level_class_def(4, None, "none"))),
        ];

        let ast_list: Vec<Option<ast::Stmt<()>>> = vec![None, None, None, None, None];

        TopLevelComposer {
            definition_ast_list: izip!(top_level_def_list, ast_list).collect_vec(),
            primitives_ty: primitives.0,
            unifier: primitives.1,
            // class_method_to_def_id: Default::default(),
            // to_be_analyzed_class: Default::default(),
            keyword_list: vec![
                "Generic".into(),
                "virtual".into(),
                "list".into(),
                "tuple".into(),
                "int32".into(),
                "int64".into(),
                "float".into(),
                "bool".into(),
                "none".into(),
                "None".into(),
            ],
        }
    }

    /// already include the definition_id of itself inside the ancestors vector
    /// when first regitering, the type_vars, fields, methods, ancestors are invalid
    pub fn make_top_level_class_def(
        index: usize,
        resolver: Option<Arc<Box<dyn SymbolResolver + Send + Sync>>>,
        name: &str,
    ) -> TopLevelDef {
        TopLevelDef::Class {
            name: name.to_string(),
            object_id: DefinitionId(index),
            type_vars: Default::default(),
            fields: Default::default(),
            methods: Default::default(),
            ancestors: Default::default(),
            resolver,
        }
    }

    /// when first registering, the type is a invalid value
    pub fn make_top_level_function_def(
        name: String,
        ty: Type,
        resolver: Option<Arc<Box<dyn SymbolResolver + Send + Sync>>>,
    ) -> TopLevelDef {
        TopLevelDef::Function {
            name,
            signature: ty,
            var_id: Default::default(),
            instance_to_symbol: Default::default(),
            instance_to_stmt: Default::default(),
            resolver,
        }
    }

    fn make_class_method_name(mut class_name: String, method_name: &str) -> String {
        class_name.push_str(method_name);
        class_name
    }

    fn extract_def_list(&self) -> Vec<Arc<RwLock<TopLevelDef>>> {
        self.definition_ast_list.iter().map(|(def, ..)| def.clone()).collect_vec()
    }

    /// step 0, register, just remeber the names of top level classes/function
    /// and check duplicate class/method/function definition
    pub fn register_top_level(
        &mut self,
        ast: ast::Stmt<()>,
        resolver: Option<Arc<Box<dyn SymbolResolver + Send + Sync>>>,
    ) -> Result<(String, DefinitionId), String> {
        let mut defined_class_name: HashSet<String> = HashSet::new();
        let mut defined_class_method_name: HashSet<String> = HashSet::new();
        let mut defined_function_name: HashSet<String> = HashSet::new();
        match &ast.node {
            ast::StmtKind::ClassDef { name, body, .. } => {
                if self.keyword_list.contains(name) {
                    return Err("cannot use keyword as a class name".into());
                }
                if !defined_class_name.insert(name.clone()) {
                    return Err("duplicate definition of class".into());
                }

                let class_name = name.to_string();
                let class_def_id = self.definition_ast_list.len();

                // since later when registering class method, ast will still be used,
                // here push None temporarly, later will move the ast inside
                let mut class_def_ast = (
                    Arc::new(RwLock::new(Self::make_top_level_class_def(
                        class_def_id,
                        resolver.clone(),
                        name.as_str(),
                    ))),
                    None,
                );

                // parse class def body and register class methods into the def list.
                // module's symbol resolver would not know the name of the class methods,
                // thus cannot return their definition_id
                let mut class_method_name_def_ids: Vec<(
                    // the simple method name without class name
                    String,
                    // in this top level def, method name is prefixed with the class name
                    Arc<RwLock<TopLevelDef>>,
                    DefinitionId,
                    Type,
                )> = Vec::new();
                // we do not push anything to the def list, so we keep track of the index
                // and then push in the correct order after the for loop
                let mut class_method_index_offset = 0;
                for b in body {
                    if let ast::StmtKind::FunctionDef { name: method_name, .. } = &b.node {
                        let global_class_method_name =
                            Self::make_class_method_name(class_name.clone(), method_name);
                        if !defined_class_method_name.insert(global_class_method_name.clone()) {
                            return Err("duplicate class method definition".into());
                        }
                        let method_def_id = self.definition_ast_list.len() + {
                            // plus 1 here since we already have the class def
                            class_method_index_offset += 1;
                            class_method_index_offset
                        };

                        // dummy method define here
                        let dummy_method_type = self.unifier.get_fresh_var();
                        class_method_name_def_ids.push((
                            method_name.clone(),
                            RwLock::new(Self::make_top_level_function_def(
                                global_class_method_name,
                                // later unify with parsed type
                                dummy_method_type.0,
                                resolver.clone(),
                            ))
                            .into(),
                            DefinitionId(method_def_id),
                            dummy_method_type.0,
                        ));
                    } else {
                        // do nothing
                        continue;
                    }
                }

                // move the ast to the entry of the class in the ast_list
                class_def_ast.1 = Some(ast);
                // get the methods into the class_def
                for (name, _, id, ty) in &class_method_name_def_ids {
                    let mut class_def = class_def_ast.0.write();
                    if let TopLevelDef::Class { methods, .. } = class_def.deref_mut() {
                        methods.push((name.clone(), *ty, *id))
                    } else {
                        unreachable!()
                    }
                }
                // now class_def_ast and class_method_def_ast_ids are ok, put them into actual def list in correct order
                self.definition_ast_list.push(class_def_ast);
                for (_, def, ..) in class_method_name_def_ids {
                    self.definition_ast_list.push((def, None));
                }

                // put the constructor into the def_list
                self.definition_ast_list.push((
                    RwLock::new(TopLevelDef::Initializer { class_id: DefinitionId(class_def_id) })
                        .into(),
                    None,
                ));

                Ok((class_name, DefinitionId(class_def_id)))
            }

            ast::StmtKind::FunctionDef { name, .. } => {
                let fun_name = name.to_string();
                if !defined_function_name.insert(name.to_string()) {
                    return Err("duplicate top level function define".into());
                }

                // add to the definition list
                self.definition_ast_list.push((
                    RwLock::new(Self::make_top_level_function_def(
                        name.into(),
                        // unify with correct type later
                        self.unifier.get_fresh_var().0,
                        resolver,
                    ))
                    .into(),
                    Some(ast),
                ));

                // return
                Ok((fun_name, DefinitionId(self.definition_ast_list.len() - 1)))
            }

            _ => Err("only registrations of top level classes/functions are supprted".into()),
        }
    }

    /// step 1, analyze the type vars associated with top level class
    /// note that we make a duplicate of the type var returned by symbol resolver
    /// since one top level type var may be used at multiple places
    fn analyze_top_level_class_type_var(&mut self) -> Result<(), String> {
        let def_list = &self.definition_ast_list;
        let temp_def_list = self.extract_def_list();
        let unifier = self.unifier.borrow_mut();
        let primitives_store = &self.primitives_ty;

        for (class_def, class_ast) in def_list {
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
            let class_resolver = class_resolver.as_ref().unwrap();
            let class_resolver = class_resolver.deref();

            let mut is_generic = false;
            for b in class_bases_ast {
                match &b.node {
                    // analyze typevars bounded to the class,
                    // only support things like `class A(Generic[T, V])`,
                    // things like `class A(Generic[T, V, ImportedModule.T])` is not supported
                    // i.e. only simple names are allowed in the subscript
                    // should update the TopLevelDef::Class.typevars and the TypeEnum::TObj.params
                    ast::ExprKind::Subscript { value, slice, .. }
                        if {
                            matches!(
                                &value.node,
                                ast::ExprKind::Name { id, .. } if id == "Generic"
                            )
                        } =>
                    {
                        if !is_generic {
                            is_generic = true;
                        } else {
                            return Err("Only single Generic[...] can be in bases".into());
                        }

                        let mut type_var_list: Vec<&ast::Expr<()>> = vec![];
                        // if `class A(Generic[T, V, G])`
                        if let ast::ExprKind::Tuple { elts, .. } = &slice.node {
                            type_var_list.extend(elts.iter());
                        // `class A(Generic[T])`
                        } else {
                            type_var_list.push(slice.deref());
                        }

                        // parse the type vars
                        let type_vars = type_var_list
                            .into_iter()
                            .map(|e| {
                                class_resolver.parse_type_annotation(
                                    &temp_def_list,
                                    unifier,
                                    primitives_store,
                                    e,
                                )
                            })
                            .collect::<Result<Vec<_>, _>>()?;

                        // check if all are unique type vars
                        let all_unique_type_var = {
                            let mut occured_type_var_id: HashSet<u32> = HashSet::new();
                            type_vars.iter().all(|x| {
                                let ty = unifier.get_ty(*x);
                                if let TypeEnum::TVar { id, .. } = ty.as_ref() {
                                    occured_type_var_id.insert(*id)
                                } else {
                                    false
                                }
                            })
                        };
                        if !all_unique_type_var {
                            return Err("expect unique type variables".into());
                        }

                        // NOTE: create a copy of all type vars for the type vars associated with class
                        let type_vars = type_vars
                            .into_iter()
                            .map(|x| {
                                // must be type var here after previous check
                                let dup = duplicate_type_var(unifier, x);
                                (dup.2, dup.0)
                            })
                            .collect_vec();

                        // add to TopLevelDef
                        class_def_type_vars.extend(type_vars);
                    }

                    // if others, do nothing in this function
                    _ => continue,
                }
            }
        }
        Ok(())
    }

    /// step 2, base classes.
    /// now that the type vars of all classes are done, handle base classes and
    /// put Self class into the ancestors list. We only allow single inheritance
    fn analyze_top_level_class_bases(&mut self) -> Result<(), String> {
        let temp_def_list = self.extract_def_list();
        for (class_def, class_ast) in self.definition_ast_list.iter_mut() {
            let mut class_def = class_def.write();
            let (class_bases, class_ancestors, class_resolver, class_id) = {
                if let TopLevelDef::Class { ancestors, resolver, object_id, .. } = class_def.deref_mut() {
                    if let Some(ast::Located {
                        node: ast::StmtKind::ClassDef { bases, .. }, ..
                    }) = class_ast
                    {
                        (bases, ancestors, resolver, *object_id)
                    } else {
                        unreachable!("must be both class")
                    }
                } else {
                    continue;
                }
            };
            let class_resolver = class_resolver.as_ref().unwrap();
            let class_resolver = class_resolver.deref();

            // only allow single inheritance
            let mut has_base = false;
            for b in class_bases {
                // type vars have already been handled, so skip on `Generic[...]`
                if matches!(
                    &b.node,
                    ast::ExprKind::Subscript { value, .. }
                        if matches!(
                            &value.node,
                            ast::ExprKind::Name { id, .. } if id == "Generic"
                        )
                ) {
                    continue;
                }

                if has_base {
                    return Err("a class def can only have at most one base class \
                    declaration and one generic declaration"
                        .into());
                }
                has_base = true;

                let base_ty = parse_ast_to_type_annotation_kinds(
                    class_resolver,
                    &temp_def_list,
                    self.unifier.borrow_mut(),
                    &self.primitives_ty,
                    b,
                )?;

                if let TypeAnnotation::CustomClassKind { id, .. } = &base_ty {
                    // check to prevent cyclic base class
                    let all_base = Self::get_all_base(*id, &temp_def_list);
                    if all_base.contains(&class_id) {
                        return Err("cyclic base detected".into());
                    }
                    class_ancestors.push(base_ty);
                } else {
                    return Err(
                        "class base declaration can only be concretized custom class".into()
                    );
                }
            }

            // push self to the ancestors
            class_ancestors.push(
                make_self_type_annotation(&temp_def_list, class_id, self.unifier.borrow_mut())?
            )
        }
        Ok(())
    }

    /// step 3, class fields and methods
    fn analyze_top_level_class_fields_methods(&mut self) -> Result<(), String> {
        let temp_def_list = self.extract_def_list();
        let unifier = self.unifier.borrow_mut();
        let primitives = &self.primitives_ty;
        let def_ast_list = &self.definition_ast_list;

        let mut type_var_to_concrete_def: HashMap<Type, TypeAnnotation> = HashMap::new();
        for (class_def, class_ast) in def_ast_list {
            Self::analyze_single_class(
                class_def.clone(),
                &class_ast.as_ref().unwrap().node,
                &temp_def_list,
                unifier,
                primitives,
                &mut type_var_to_concrete_def,
            )?
        }

        // base class methods add and check
        // TODO:

        // unification of previously assigned typevar
        for (ty, def) in type_var_to_concrete_def {
            let target_ty =
                get_type_from_type_annotation_kinds(&temp_def_list, unifier, primitives, &def)?;
            unifier.unify(ty, target_ty)?;
        }

        Ok(())
    }

    /// step 4, after class methods are done
    fn analyze_top_level_function(&mut self) -> Result<(), String> {
        let def_list = &self.definition_ast_list;
        let temp_def_list = self.extract_def_list();
        let unifier = self.unifier.borrow_mut();
        let primitives_store = &self.primitives_ty;

        for (function_def, function_ast) in def_list {
            let function_def = function_def.read();
            let function_def = function_def.deref();
            let function_ast = if let Some(function_ast) = function_ast {
                function_ast
            } else {
                continue;
            };
            if let TopLevelDef::Function { signature: dummy_ty, resolver, .. } = function_def {
                if let ast::StmtKind::FunctionDef { args, returns, .. } = &function_ast.node {
                    let resolver = resolver.as_ref();
                    let resolver = resolver.unwrap();
                    let resolver = resolver.deref();
                    let function_resolver = resolver.deref();

                    // occured type vars should not be handled separately
                    // TODO: type vars occured as applications of generic classes is not handled
                    let mut occured_type_var: HashMap<u32, Type> = HashMap::new();
                    let mut function_var_map: HashMap<u32, Type> = HashMap::new();
                    let arg_types = {
                        // make sure no duplicate parameter
                        let mut defined_paramter_name: HashSet<String> = HashSet::new();
                        let have_unique_fuction_parameter_name = args
                            .args
                            .iter()
                            .all(|x| defined_paramter_name.insert(x.node.arg.clone()));
                        if !have_unique_fuction_parameter_name {
                            return Err("top level function have duplicate parameter name".into());
                        }

                        args.args
                            .iter()
                            .map(|x| -> Result<FuncArg, String> {
                                let annotation = x
                                    .node
                                    .annotation
                                    .as_ref()
                                    .ok_or_else(|| {
                                        "function parameter type annotation needed".to_string()
                                    })?
                                    .as_ref();

                                let mut ty = function_resolver.parse_type_annotation(
                                    temp_def_list.as_slice(),
                                    unifier,
                                    primitives_store,
                                    annotation,
                                )?;
                                if let TypeEnum::TVar { id, .. } =
                                    unifier.get_ty(ty).as_ref()
                                {
                                    if let Some(occured_ty) = occured_type_var.get(id) {
                                        // if already occured
                                        ty = *occured_ty;
                                    } else {
                                        // if not, create a duplicate
                                        let ty_copy = duplicate_type_var(unifier, ty);
                                        ty = ty_copy.0;
                                        occured_type_var.insert(*id, ty);
                                        function_var_map.insert(ty_copy.1, ty_copy.0);
                                    }
                                }

                                Ok(FuncArg {
                                    name: x.node.arg.clone(),
                                    ty,
                                    default_value: Default::default(),
                                })
                            })
                            .collect::<Result<Vec<_>, _>>()?
                    };

                    let return_ty = {
                        let return_annotation = returns
                            .as_ref()
                            .ok_or_else(|| "function return type needed".to_string())?
                            .as_ref();
                        function_resolver.parse_type_annotation(
                            temp_def_list.as_slice(),
                            unifier,
                            primitives_store,
                            return_annotation,
                        )?
                    };

                    let function_ty = unifier.add_ty(TypeEnum::TFunc(
                        FunSignature { args: arg_types, ret: return_ty, vars: function_var_map }
                            .into(),
                    ));
                    unifier.unify(*dummy_ty, function_ty)?;
                } else {
                    unreachable!("must be both function");
                }
            } else {
                continue;
            }
        }
        Ok(())
    }

    /// step 5, field instantiation?
    fn analyze_top_level_field_instantiation(&mut self) -> Result<(), String> {
        // TODO:
        unimplemented!()
    }

    fn analyze_single_class(
        class_def: Arc<RwLock<TopLevelDef>>,
        class_ast: &ast::StmtKind<()>,
        temp_def_list: &[Arc<RwLock<TopLevelDef>>],
        unifier: &mut Unifier,
        primitives: &PrimitiveStore,
        type_var_to_concrete_def: &mut HashMap<Type, TypeAnnotation>,
    ) -> Result<(), String> {
        let mut class_def = class_def.write();
        let (
            class_id,
            _class_name,
            _class_bases_ast,
            class_body_ast,
            _class_ancestor_def,
            class_fields_def,
            class_methods_def,
            class_type_vars_def,
            class_resolver,
        ) = if let TopLevelDef::Class {
            object_id,
            ancestors,
            fields,
            methods,
            resolver,
            type_vars,
            ..
        } = class_def.deref_mut()
        {
            if let ast::StmtKind::ClassDef { name, bases, body, .. } = &class_ast {
                (
                    object_id,
                    name.clone(),
                    bases,
                    body,
                    ancestors,
                    fields,
                    methods,
                    type_vars,
                    resolver,
                )
            } else {
                unreachable!("here must be class def ast");
            }
        } else {
            unreachable!("here must be class def ast");
        };
        let class_resolver = class_resolver.as_ref().unwrap();
        let class_resolver = class_resolver;

        for b in class_body_ast {
            if let ast::StmtKind::FunctionDef { args, returns, name, body, .. } = &b.node {
                let (method_dummy_ty, ..) =
                    Self::get_class_method_def_info(class_methods_def, name)?;

                // handle var map, to create a new copy of type var
                // while tracking the type var associated with class
                // TODO: type vars occured as applications of generic classes is not handled
                let mut method_var_map: HashMap<u32, Type> = HashMap::new();
                let arg_type: Vec<FuncArg> = {
                    // check method parameters cannot have same name
                    let mut defined_paramter_name: HashSet<String> = HashSet::new();
                    let have_unique_fuction_parameter_name =
                        args.args.iter().all(|x| defined_paramter_name.insert(x.node.arg.clone()));
                    if !have_unique_fuction_parameter_name {
                        return Err("class method have duplicate parameter name".into());
                    }

                    let mut result = Vec::new();
                    for x in &args.args {
                        let name = x.node.arg.clone();
                        if name != "self" {
                            let type_ann = {
                                let annotation_expr = x
                                    .node
                                    .annotation
                                    .as_ref()
                                    .ok_or_else(|| "type annotation needed".to_string())?
                                    .as_ref();
                                parse_ast_to_type_annotation_kinds(
                                    class_resolver,
                                    temp_def_list,
                                    unifier,
                                    primitives,
                                    annotation_expr,
                                )?
                            };
                            // handle to differentiate type vars that are
                            // asscosiated with the class and that are not
                            // TODO: type vars occured as applications of generic classes is not handled
                            if let TypeAnnotation::TypeVarKind(id, ty) = &type_ann {
                                let associated = class_type_vars_def
                                    .iter()
                                    .filter(|(class_type_var_id, _)| *class_type_var_id == *id)
                                    .collect_vec();
                                match associated.len() {
                                    // 0, do nothing, this is not a type var
                                    // associated with the method's class
                                    // TODO: but this type var can occur multiple times in this
                                    // method's param list, still need to keep track of type vars
                                    // associated with this function
                                    0 => {}
                                    // is type var associated with class, do the unification here
                                    1 => {
                                        unifier.unify(*ty, associated[0].1)?;
                                    }
                                    _ => {
                                        unreachable!("there should not be duplicate type var");
                                    }
                                }

                                // just insert the id and ty of self
                                // since the function is not instantiated yet
                                if let TypeEnum::TVar { id, .. } = unifier.get_ty(*ty).as_ref() {
                                    method_var_map.insert(*id, *ty);
                                } else {
                                    unreachable!("must be type var")
                                }
                            }
                            let dummy_func_arg = FuncArg {
                                name,
                                ty: unifier.get_fresh_var().0,
                                // TODO: symbol default value?
                                default_value: None,
                            };
                            // push the dummy type and the type annotation
                            // into the list for later unification
                            type_var_to_concrete_def.insert(dummy_func_arg.ty, type_ann.clone());
                            result.push(dummy_func_arg)
                        } else {
                            // if the parameter name is self
                            // python does not seem to enforce the name
                            // representing the self class object to be
                            // `self`, but we do it here

                            let dummy_func_arg = FuncArg {
                                name: "self".into(),
                                ty: unifier.get_fresh_var().0,
                                default_value: None,
                            };
                            type_var_to_concrete_def
                                .insert(
                                    dummy_func_arg.ty,
                                    make_self_type_annotation(temp_def_list, *class_id, unifier)?
                                );
                            result.push(dummy_func_arg);
                        }
                    }
                    result
                };
                let ret_type = {
                    if name != "__init__" {
                        let result = returns
                            .as_ref()
                            .ok_or_else(|| "method return type annotation needed".to_string())?
                            .as_ref();
                        let annotation = parse_ast_to_type_annotation_kinds(
                            class_resolver,
                            temp_def_list,
                            unifier,
                            primitives,
                            result,
                        )?;
                        let dummy_return_type = unifier.get_fresh_var().0;
                        type_var_to_concrete_def.insert(dummy_return_type, annotation.clone());
                        dummy_return_type
                    } else {
                        // if is the "__init__" function, the return type is self
                        let dummy_return_type = unifier.get_fresh_var().0;
                        type_var_to_concrete_def
                            .insert(
                                dummy_return_type,
                                make_self_type_annotation(temp_def_list, *class_id, unifier)?
                            );
                        dummy_return_type
                    }
                };

                let method_type = unifier.add_ty(TypeEnum::TFunc(
                    FunSignature { args: arg_type, ret: ret_type, vars: method_var_map }.into(),
                ));
                // unify now since function type is not in type annotation define
                // which is fine since type within method_type will be subst later
                unifier.unify(method_dummy_ty, method_type)?;

                // class fields
                if name == "__init__" {
                    for b in body {
                        let mut defined_fields: HashSet<String> = HashSet::new();
                        // TODO: check the type of value, field instantiation check?
                        if let ast::StmtKind::AnnAssign { annotation, target, value: _, .. } =
                            &b.node
                        {
                            if let ast::ExprKind::Attribute { value, attr, .. } = &target.node {
                                if matches!(&value.node, ast::ExprKind::Name { id, .. } if id == "self")
                                {
                                    if defined_fields.insert(attr.to_string()) {
                                        let dummy_field_type = unifier.get_fresh_var().0;
                                        class_fields_def.push((attr.to_string(), dummy_field_type));
                                        let annotation = parse_ast_to_type_annotation_kinds(
                                            class_resolver,
                                            &temp_def_list,
                                            unifier,
                                            primitives,
                                            annotation.as_ref(),
                                        )?;
                                        type_var_to_concrete_def
                                            .insert(dummy_field_type, annotation);
                                    } else {
                                        return Err("same class fields defined twice".into());
                                    }
                                }
                            }
                        }
                    }
                }
            } else {
                continue;
            }
        }
        Ok(())
    }

    fn get_class_method_def_info(
        class_methods_def: &[(String, Type, DefinitionId)],
        method_name: &str,
    ) -> Result<(Type, DefinitionId), String> {
        for (name, ty, def_id) in class_methods_def {
            if name == method_name {
                return Ok((*ty, *def_id));
            }
        }
        Err(format!("no method {} in the current class", method_name))
    }

    /// get all base class def id of a class, including it self
    fn get_all_base(
        child: DefinitionId,
        temp_def_list: &[Arc<RwLock<TopLevelDef>>]
    ) -> Vec<DefinitionId> {
        let mut result: Vec<DefinitionId> = Vec::new();
        let child_def = temp_def_list.get(child.0).unwrap();
        let child_def = child_def.read();
        let child_def = child_def.deref();

        if let TopLevelDef::Class { ancestors, .. } = child_def {
            for a in ancestors {
                if let TypeAnnotation::CustomClassKind { id, .. } = a {
                    if *id != child {
                        result.extend(Self::get_all_base(*id, temp_def_list));
                    }
                } else {
                    unreachable!("must be class type annotation type")
                }
            }
        } else {
            unreachable!("this function should only be called with class def id as parameter")
        }

        result.push(child);
        result
    }
}
