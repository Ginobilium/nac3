use std::{borrow::{Borrow, BorrowMut}, collections::{HashMap, HashSet}, iter::FromIterator, ops::{Deref, DerefMut}, sync::Arc};

use super::typecheck::type_inferencer::PrimitiveStore;
use super::typecheck::typedef::{FunSignature, FuncArg, SharedUnifier, Type, TypeEnum, Unifier};
use crate::{
    symbol_resolver::SymbolResolver,
    typecheck::{type_inferencer::CodeLocation, typedef::CallId},
};
use itertools::{izip, Itertools};
use parking_lot::{Mutex, RwLock};
use rustpython_parser::ast::{self, Stmt};

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub struct DefinitionId(pub usize);

mod type_annotation;
use type_annotation::*;
mod helper;
#[cfg(test)]
mod test;

#[derive(Clone)]
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
        /// type variables bounded to the class.
        type_vars: Vec<Type>,
        // class fields
        fields: Vec<(String, Type)>,
        // class methods, pointing to the corresponding function definition.
        methods: Vec<(String, Type, DefinitionId)>,
        // ancestor classes, including itself.
        ancestors: Vec<TypeAnnotation>,
        // symbol resolver of the module defined the class, none if it is built-in type
        resolver: Option<Arc<Mutex<Box<dyn SymbolResolver + Send + Sync>>>>,
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
        resolver: Option<Arc<Mutex<Box<dyn SymbolResolver + Send + Sync>>>>,
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
    // keyword list to prevent same user-defined name
    pub keyword_list: HashSet<String>,
    // to prevent duplicate definition
    pub defined_class_name: HashSet<String>,
    pub defined_class_method_name: HashSet<String>,
    pub defined_function_name: HashSet<String>,
}

impl Default for TopLevelComposer {
    fn default() -> Self {
        Self::new()
    }
}

impl TopLevelComposer {
    /// return a composer and things to make a "primitive" symbol resolver, so that the symbol
    /// resolver can later figure out primitive type definitions when passed a primitive type name
    pub fn new() -> Self {
        let primitives = Self::make_primitives();

        TopLevelComposer {
            definition_ast_list: Default::default(),
            primitives_ty: primitives.0,
            unifier: primitives.1,
            // class_method_to_def_id: Default::default(),
            // to_be_analyzed_class: Default::default(),
            keyword_list: HashSet::from_iter(vec![
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
            ]),
            defined_class_method_name: Default::default(),
            defined_class_name: Default::default(),
            defined_function_name: Default::default(),
        }
    }

    pub fn make_top_level_context(self) -> TopLevelContext {
        TopLevelContext {
            definitions: RwLock::new(
                self.definition_ast_list.into_iter().map(|(x, ..)| x).collect_vec(),
            )
            .into(),
            // FIXME: all the big unifier or?
            unifiers: Default::default(),
        }
    }

    fn extract_def_list(&self) -> Vec<Arc<RwLock<TopLevelDef>>> {
        self.definition_ast_list.iter().map(|(def, ..)| def.clone()).collect_vec()
    }

    /// register, just remeber the names of top level classes/function
    /// and check duplicate class/method/function definition
    pub fn register_top_level(
        &mut self,
        ast: ast::Stmt<()>,
        resolver: Option<Arc<Mutex<Box<dyn SymbolResolver + Send + Sync>>>>,
    ) -> Result<(String, DefinitionId), String> {
        let defined_class_name = &mut self.defined_class_name;
        let defined_class_method_name = &mut self.defined_class_method_name;
        let defined_function_name = &mut self.defined_function_name;
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
                        name,
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
                let mut has_init = false;
                for b in body {
                    if let ast::StmtKind::FunctionDef { name: method_name, .. } = &b.node {
                        if self.keyword_list.contains(name) {
                            return Err("cannot use keyword as a method name".into());
                        }
                        let global_class_method_name =
                            Self::make_class_method_name(class_name.clone(), method_name);
                        if !defined_class_method_name.insert(global_class_method_name.clone()) {
                            return Err("duplicate class method definition".into());
                        }
                        if method_name == "__init__" {
                            has_init = true;
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
                if !has_init {
                    return Err("class def must have __init__ method defined".into());
                }

                // move the ast to the entry of the class in the ast_list
                class_def_ast.1 = Some(ast);
                // get the methods into the top level class_def
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
                if self.keyword_list.contains(name) {
                    return Err("cannot use keyword as a top level function name".into());
                }
                let fun_name = name.to_string();
                if !defined_function_name.insert(name.to_string()) {
                    return Err("duplicate top level function define".into());
                }

                // add to the definition list
                self.definition_ast_list.push((
                    RwLock::new(Self::make_top_level_function_def(
                        name.into(),
                        // dummy here, unify with correct type later
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

    pub fn start_analysis(&mut self) -> Result<(), String> {
        self.analyze_top_level_class_type_var()?;
        self.analyze_top_level_class_bases()?;
        self.analyze_top_level_class_fields_methods()?;
        self.analyze_top_level_function()?;
        Ok(())
    }

    /// step 1, analyze the type vars associated with top level class
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

                        let type_var_list: Vec<&ast::Expr<()>>;
                        // if `class A(Generic[T, V, G])`
                        if let ast::ExprKind::Tuple { elts, .. } = &slice.node {
                            type_var_list = elts.iter().collect_vec();
                        // `class A(Generic[T])`
                        } else {
                            type_var_list = vec![slice.deref()];
                        }

                        // parse the type vars
                        let type_vars = type_var_list
                            .into_iter()
                            .map(|e| {
                                class_resolver.lock().parse_type_annotation(
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
        let unifier = self.unifier.borrow_mut();

        // first, only push direct parent into the list
        for (class_def, class_ast) in self.definition_ast_list.iter_mut() {
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
            let class_resolver = class_resolver.as_ref().unwrap();
            let class_resolver = class_resolver.deref();

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

                // the function parse_ast_to make sure that no type var occured in
                // bast_ty if it is a CustomClassKind
                let base_ty = parse_ast_to_type_annotation_kinds(
                    class_resolver,
                    &temp_def_list,
                    unifier,
                    &self.primitives_ty,
                    b,
                )?;

                if let TypeAnnotation::CustomClassKind { .. } = &base_ty {
                    class_ancestors.push(base_ty);
                } else {
                    return Err("class base declaration can only be custom class".into());
                }
            }
        }

        // second, get all ancestors
        let mut ancestors_store: HashMap<DefinitionId, Vec<TypeAnnotation>> = Default::default();
        for (class_def, _) in self.definition_ast_list.iter_mut() {
            let mut class_def = class_def.write();
            let (class_ancestors, class_id) = {
                if let TopLevelDef::Class { ancestors, object_id, .. } = class_def.deref_mut() {
                    (ancestors, *object_id)
                } else {
                    continue;
                }
            };
            ancestors_store.insert(
                class_id,
                // if class has direct parents, get all ancestors of its parents. Else just empty
                if class_ancestors.is_empty() {
                    vec![]
                } else {
                    Self::get_all_ancestors_helper(&class_ancestors[0], temp_def_list.as_slice())?
                },
            );
        }

        // insert the ancestors to the def list
        for (class_def, _) in self.definition_ast_list.iter_mut() {
            let mut class_def = class_def.write();
            let (class_ancestors, class_id) = {
                if let TopLevelDef::Class { ancestors, object_id, .. } = class_def.deref_mut() {
                    (ancestors, *object_id)
                } else {
                    continue;
                }
            };

            let ans = ancestors_store.get_mut(&class_id).unwrap();
            class_ancestors.append(ans);

            // insert self type annotation to the front of the vector to maintain the order
            class_ancestors
                .insert(0, make_self_type_annotation(temp_def_list.as_slice(), class_id)?);
        }

        Ok(())
    }

    /// step 3, class fields and methods
    fn analyze_top_level_class_fields_methods(&mut self) -> Result<(), String> {
        let temp_def_list = self.extract_def_list();
        let primitives = &self.primitives_ty;
        let def_ast_list = &self.definition_ast_list;
        let unifier = self.unifier.borrow_mut();

        let mut type_var_to_concrete_def: HashMap<Type, TypeAnnotation> = HashMap::new();

        for (class_def, class_ast) in def_ast_list {
            if matches!(&*class_def.read(), TopLevelDef::Class { .. }) {
                Self::analyze_single_class_methods_fields(
                    class_def.clone(),
                    &class_ast.as_ref().unwrap().node,
                    &temp_def_list,
                    unifier,
                    primitives,
                    &mut type_var_to_concrete_def,
                    &self.keyword_list,
                )?
            }
        }

        // handle the inheritanced methods and fields
        let mut current_ancestor_depth: usize = 2;
        loop {
            let mut finished = true;

            for (class_def, _) in def_ast_list {
                let mut class_def = class_def.write();
                if let TopLevelDef::Class { ancestors, .. } = class_def.deref() {
                    // if the length of the ancestor is equal to the current depth
                    // it means that all the ancestors of the class is handled
                    if ancestors.len() == current_ancestor_depth {
                        finished = false;
                        Self::analyze_single_class_ancestors(
                            class_def.deref_mut(),
                            &temp_def_list,
                            unifier,
                            primitives,
                            &mut type_var_to_concrete_def,
                        )?;
                    }
                }
            }

            if finished {
                break;
            } else {
                current_ancestor_depth += 1;
            }

            if current_ancestor_depth > def_ast_list.len() + 1 {
                unreachable!("cannot be longer than the whole top level def list")
            }
        }

        // unification of previously assigned typevar
        for (ty, def) in type_var_to_concrete_def {
            let target_ty =
                get_type_from_type_annotation_kinds(&temp_def_list, unifier, primitives, &def)?;
            unifier.unify(ty, target_ty)?;
        }

        Ok(())
    }

    /// step 4, after class methods are done, top level functions have nothing unknown
    fn analyze_top_level_function(&mut self) -> Result<(), String> {
        let def_list = &self.definition_ast_list;
        let keyword_list = &self.keyword_list;
        let temp_def_list = self.extract_def_list();
        let unifier = self.unifier.borrow_mut();
        let primitives_store = &self.primitives_ty;

        for (function_def, function_ast) in def_list {
            let function_def = function_def.read();
            let function_def = function_def.deref();
            let function_ast = if let Some(function_ast) = function_ast {
                function_ast
            } else {
                // no ast, class method, continue
                continue;
            };

            if let TopLevelDef::Function { signature: dummy_ty, resolver, .. } = function_def {
                if let ast::StmtKind::FunctionDef { args, returns, .. } = &function_ast.node {
                    let resolver = resolver.as_ref();
                    let resolver = resolver.unwrap();
                    let resolver = resolver.deref();

                    let mut function_var_map: HashMap<u32, Type> = HashMap::new();
                    let arg_types = {
                        // make sure no duplicate parameter
                        let mut defined_paramter_name: HashSet<String> = HashSet::new();
                        let have_unique_fuction_parameter_name = args.args.iter().all(|x| {
                            defined_paramter_name.insert(x.node.arg.clone())
                                && !keyword_list.contains(&x.node.arg)
                                && "self" != x.node.arg
                        });
                        if !have_unique_fuction_parameter_name {
                            return Err("top level function must have unique parameter names \
                            and names thould not be the same as the keywords"
                                .into());
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

                                let type_annotation = parse_ast_to_type_annotation_kinds(
                                    resolver,
                                    temp_def_list.as_slice(),
                                    unifier,
                                    primitives_store,
                                    annotation,
                                )?;
                                
                                let type_vars_within =
                                    get_type_var_contained_in_type_annotation(&type_annotation)
                                        .into_iter()
                                        .map(|x| -> Result<(u32, Type), String> {
                                            if let TypeAnnotation::TypeVarKind(ty) = x {
                                                Ok((Self::get_var_id(ty, unifier)?, ty))
                                            } else {
                                                unreachable!("must be type var annotation kind")
                                            }
                                        })
                                        .collect::<Result<Vec<_>, _>>()?;
                                for (id, ty) in type_vars_within {
                                    if let Some(prev_ty) = function_var_map.insert(id, ty) {
                                        // if already have the type inserted, make sure they are the same thing
                                        assert_eq!(prev_ty, ty);
                                    }
                                }

                                let ty = get_type_from_type_annotation_kinds(
                                    temp_def_list.as_ref(),
                                    unifier,
                                    primitives_store,
                                    &type_annotation,
                                )?;

                                Ok(FuncArg {
                                    name: x.node.arg.clone(),
                                    ty,
                                    default_value: Default::default(),
                                })
                            })
                            .collect::<Result<Vec<_>, _>>()?
                    };

                    let return_ty = {
                        if let Some(returns) = returns {
                            let return_ty_annotation = {
                                let return_annotation = returns.as_ref();
                                parse_ast_to_type_annotation_kinds(
                                    resolver,
                                    &temp_def_list,
                                    unifier,
                                    primitives_store,
                                    return_annotation,
                                )?
                            };

                            let type_vars_within =
                                get_type_var_contained_in_type_annotation(&return_ty_annotation)
                                    .into_iter()
                                    .map(|x| -> Result<(u32, Type), String> {
                                        if let TypeAnnotation::TypeVarKind(ty) = x {
                                            Ok((Self::get_var_id(ty, unifier)?, ty))
                                        } else {
                                            unreachable!("must be type var here")
                                        }
                                    })
                                    .collect::<Result<Vec<_>, _>>()?;
                            for (id, ty) in type_vars_within {
                                if let Some(prev_ty) = function_var_map.insert(id, ty) {
                                    // if already have the type inserted, make sure they are the same thing
                                    assert_eq!(prev_ty, ty);
                                }
                            }

                            get_type_from_type_annotation_kinds(
                                &temp_def_list,
                                unifier,
                                primitives_store,
                                &return_ty_annotation,
                            )?
                        } else {
                            primitives_store.none
                        }
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
                // not top level function def, skip
                continue;
            }
        }
        Ok(())
    }

    fn analyze_single_class_methods_fields(
        class_def: Arc<RwLock<TopLevelDef>>,
        class_ast: &ast::StmtKind<()>,
        temp_def_list: &[Arc<RwLock<TopLevelDef>>],
        unifier: &mut Unifier,
        primitives: &PrimitiveStore,
        type_var_to_concrete_def: &mut HashMap<Type, TypeAnnotation>,
        keyword_list: &HashSet<String>,
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
                    *object_id,
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
        let class_resolver = class_resolver.as_ref();

        for b in class_body_ast {
            if let ast::StmtKind::FunctionDef { args, returns, name, body, .. } = &b.node {
                let (method_dummy_ty, ..) =
                    Self::get_class_method_def_info(class_methods_def, name)?;

                // the method var map can surely include the class's generic parameters
                let mut method_var_map: HashMap<u32, Type> = class_type_vars_def
                    .iter()
                    .map(|ty| {
                        if let TypeEnum::TVar { id, .. } = unifier.get_ty(*ty).as_ref() {
                            (*id, *ty)
                        } else {
                            unreachable!("must be type var here")
                        }
                    })
                    .collect();

                let arg_types: Vec<FuncArg> = {
                    // check method parameters cannot have same name
                    let mut defined_paramter_name: HashSet<String> = HashSet::new();
                    let have_unique_fuction_parameter_name = args.args.iter().all(|x| {
                        defined_paramter_name.insert(x.node.arg.clone())
                            && !keyword_list.contains(&x.node.arg)
                    });
                    if !have_unique_fuction_parameter_name {
                        return Err("class method must have unique parameter names \
                        and names thould not be the same as the keywords"
                            .into());
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

                            // find type vars within this method parameter type annotation
                            let type_vars_within =
                                get_type_var_contained_in_type_annotation(&type_ann);
                            // handle the class type var and the method type var
                            for type_var_within in type_vars_within {
                                if let TypeAnnotation::TypeVarKind(ty) = type_var_within {
                                    let id = Self::get_var_id(ty, unifier)?;
                                    if let Some(prev_ty) = method_var_map.insert(id, ty) {
                                        // if already in the list, make sure they are the same?
                                        assert_eq!(prev_ty, ty);
                                    }
                                } else {
                                    unreachable!("must be type var annotation");
                                }
                            }

                            // finish handling type vars
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
                            // `self`??, but we do it here
                            let dummy_func_arg = FuncArg {
                                name: "self".into(),
                                ty: unifier.get_fresh_var().0,
                                default_value: None,
                            };
                            type_var_to_concrete_def.insert(
                                dummy_func_arg.ty,
                                make_self_type_annotation(temp_def_list, class_id)?,
                            );
                            result.push(dummy_func_arg);
                        }
                    }
                    result
                };

                let ret_type = {
                    if name != "__init__" {
                        if let Some(result) = returns {
                            let result = result.as_ref();
                            let annotation = parse_ast_to_type_annotation_kinds(
                                class_resolver,
                                temp_def_list,
                                unifier,
                                primitives,
                                result,
                            )?;

                            // find type vars within this return type annotation
                            let type_vars_within =
                                get_type_var_contained_in_type_annotation(&annotation);
                            // handle the class type var and the method type var
                            for type_var_within in type_vars_within {
                                if let TypeAnnotation::TypeVarKind(ty) = type_var_within {
                                    let id = Self::get_var_id(ty, unifier)?;
                                    if let Some(prev_ty) = method_var_map.insert(id, ty) {
                                        // if already in the list, make sure they are the same?
                                        assert_eq!(prev_ty, ty);
                                    }
                                } else {
                                    unreachable!("must be type var annotation");
                                }
                            }

                            let dummy_return_type = unifier.get_fresh_var().0;
                            type_var_to_concrete_def.insert(dummy_return_type, annotation.clone());
                            dummy_return_type
                        } else {
                            // if do not have return annotation, return none
                            // for uniform handling, still use type annoatation
                            let dummy_return_type = unifier.get_fresh_var().0;
                            type_var_to_concrete_def.insert(
                                dummy_return_type,
                                TypeAnnotation::PrimitiveKind(primitives.none),
                            );
                            dummy_return_type
                        }
                    } else {
                        // if is the "__init__" function, the return type is self
                        let dummy_return_type = unifier.get_fresh_var().0;
                        type_var_to_concrete_def.insert(
                            dummy_return_type,
                            make_self_type_annotation(temp_def_list, class_id)?,
                        );
                        dummy_return_type
                    }
                };

                let method_type = unifier.add_ty(TypeEnum::TFunc(
                    FunSignature { args: arg_types, ret: ret_type, vars: method_var_map }.into(),
                ));
                // NOTE: unify now since function type is not in type annotation define
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

                                        // find type vars within this return type annotation
                                        let type_vars_within =
                                            get_type_var_contained_in_type_annotation(&annotation);
                                        // handle the class type var and the method type var
                                        for type_var_within in type_vars_within {
                                            if let TypeAnnotation::TypeVarKind(t) = type_var_within
                                            {
                                                if !class_type_vars_def.contains(&t) {
                                                    return Err("class fields can only use type \
                                                    vars declared as class generic type vars"
                                                        .into());
                                                }
                                            } else {
                                                unreachable!("must be type var annotation");
                                            }
                                        }

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

    fn analyze_single_class_ancestors(
        class_def: &mut TopLevelDef,
        temp_def_list: &[Arc<RwLock<TopLevelDef>>],
        unifier: &mut Unifier,
        _primitives: &PrimitiveStore,
        type_var_to_concrete_def: &mut HashMap<Type, TypeAnnotation>,
    ) -> Result<(), String> {
        let (
            _class_id,
            class_ancestor_def,
            class_fields_def,
            class_methods_def,
            _class_type_vars_def,
            _class_resolver,
        ) = if let TopLevelDef::Class {
            object_id,
            ancestors,
            fields,
            methods,
            resolver,
            type_vars,
            ..
        } = class_def
        {
            (*object_id, ancestors, fields, methods, type_vars, resolver)
        } else {
            unreachable!("here must be class def ast");
        };

        // since when this function is called, the ancestors of the direct parent
        // are supposed to be already handled, so we only need to deal with the direct parent
        let base = class_ancestor_def.get(1).unwrap();
        if let TypeAnnotation::CustomClassKind { id, params: _ } = base {
            let base = temp_def_list.get(id.0).unwrap();
            let base = base.read();
            if let TopLevelDef::Class { methods, fields, .. } = &*base {
                // handle methods override
                // since we need to maintain the order, create a new list
                let mut new_child_methods: Vec<(String, Type, DefinitionId)> = Vec::new();
                let mut is_override: HashSet<String> = HashSet::new();
                for (anc_method_name, anc_method_ty, anc_method_def_id) in methods {
                    // find if there is a method with same name in the child class
                    let mut to_be_added =
                        (anc_method_name.to_string(), *anc_method_ty, *anc_method_def_id);
                    for (class_method_name, class_method_ty, class_method_defid) in
                        class_methods_def.iter()
                    {
                        if class_method_name == anc_method_name {
                            // ignore and handle self
                            let ok = class_method_name == "__init__"
                                && Self::check_overload_function_type(
                                    *class_method_ty,
                                    *anc_method_ty,
                                    unifier,
                                    type_var_to_concrete_def,
                                );
                            if !ok {
                                return Err("method has same name as ancestors' method, but incompatible type".into());
                            }
                            // mark it as added
                            is_override.insert(class_method_name.to_string());
                            to_be_added = (
                                class_method_name.to_string(),
                                *class_method_ty,
                                *class_method_defid,
                            );
                            break;
                        }
                    }
                    new_child_methods.push(to_be_added);
                }
                // add those that are not overriding method to the new_child_methods
                for (class_method_name, class_method_ty, class_method_defid) in
                    class_methods_def.iter()
                {
                    if !is_override.contains(class_method_name) {
                        new_child_methods.push((
                            class_method_name.to_string(),
                            *class_method_ty,
                            *class_method_defid,
                        ));
                    }
                }
                // use the new_child_methods to replace all the elements in `class_methods_def`
                class_methods_def.drain(..);
                class_methods_def.extend(new_child_methods);

                // handle class fields
                let mut new_child_fields: Vec<(String, Type)> = Vec::new();
                let mut is_override: HashSet<String> = HashSet::new();
                for (anc_field_name, anc_field_ty) in fields {
                    let mut to_be_added = (anc_field_name.to_string(), *anc_field_ty);
                    // find if there is a fields with the same name in the child class
                    for (class_field_name, class_field_ty) in class_fields_def.iter() {
                        if class_field_name == anc_field_name {
                            let ok = Self::check_overload_field_type(
                                *class_field_ty,
                                *anc_field_ty,
                                unifier,
                                type_var_to_concrete_def,
                            );
                            if !ok {
                                return Err("fields has same name as ancestors' field, but incompatible type".into());
                            }
                            // mark it as added
                            is_override.insert(class_field_name.to_string());
                            to_be_added = (class_field_name.to_string(), *class_field_ty);
                            break;
                        }
                    }
                    new_child_fields.push(to_be_added);
                }
                for (class_field_name, class_field_ty) in class_fields_def.iter() {
                    if !is_override.contains(class_field_name) {
                        new_child_fields.push((class_field_name.to_string(), *class_field_ty));
                    }
                }
                class_fields_def.drain(..);
                class_fields_def.extend(new_child_fields);
            } else {
                unreachable!("must be top level class def")
            }
        } else {
            unreachable!("must be class type annotation")
        }

        Ok(())
    }
}
