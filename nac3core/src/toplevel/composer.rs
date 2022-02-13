use std::cell::RefCell;

use nac3parser::ast::fold::Fold;

use crate::{
    typecheck::type_inferencer::{FunctionData, Inferencer},
    codegen::{expr::get_subst_key, stmt::exn_constructor},
    symbol_resolver::SymbolValue,
};

use super::*;

pub struct ComposerConfig {
    pub kernel_ann: Option<&'static str>,
    pub kernel_invariant_ann: &'static str,
}

impl Default for ComposerConfig {
    fn default() -> Self {
        ComposerConfig {
            kernel_ann: None,
            kernel_invariant_ann: "Invariant"
        }
    }
}

type DefAst = (Arc<RwLock<TopLevelDef>>, Option<ast::Stmt<()>>);
pub struct TopLevelComposer {
    // list of top level definitions, same as top level context
    pub definition_ast_list: Vec<DefAst>,
    // start as a primitive unifier, will add more top_level defs inside
    pub unifier: Unifier,
    // primitive store
    pub primitives_ty: PrimitiveStore,
    // keyword list to prevent same user-defined name
    pub keyword_list: HashSet<StrRef>,
    // to prevent duplicate definition
    pub defined_names: HashSet<String>,
    // get the class def id of a class method
    pub method_class: HashMap<DefinitionId, DefinitionId>,
    // number of built-in function and classes in the definition list, later skip
    pub builtin_num: usize,
    pub core_config: ComposerConfig,
}

impl Default for TopLevelComposer {
    fn default() -> Self {
        Self::new(vec![], Default::default()).0
    }
}

impl TopLevelComposer {
    /// return a composer and things to make a "primitive" symbol resolver, so that the symbol
    /// resolver can later figure out primitive type definitions when passed a primitive type name
    pub fn new(
        builtins: Vec<(StrRef, FunSignature, Arc<GenCall>)>,
        core_config: ComposerConfig
    ) -> (Self, HashMap<StrRef, DefinitionId>, HashMap<StrRef, Type>) {
        let mut primitives = Self::make_primitives();
        let (mut definition_ast_list, builtin_name_list) = builtins::get_builtins(&mut primitives);
        let primitives_ty = primitives.0;
        let mut unifier = primitives.1;
        let mut keyword_list: HashSet<StrRef> = HashSet::from_iter(vec![
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
            "range".into(),
            "str".into(),
            "self".into(),
            "Kernel".into(),
            "KernelInvariant".into(),
        ]);
        let defined_names: HashSet<String> = Default::default();
        let method_class: HashMap<DefinitionId, DefinitionId> = Default::default();

        let mut builtin_id: HashMap<StrRef, DefinitionId> = Default::default();
        let mut builtin_ty: HashMap<StrRef, Type> = Default::default();

        for (id, name) in builtin_name_list.iter().rev().enumerate() {
            let name = (**name).into();
            let id = definition_ast_list.len() - id - 1;
            let def = definition_ast_list[id].0.read();
            if let TopLevelDef::Function { simple_name, signature, .. } = &*def {
                assert!(name == *simple_name);
                builtin_ty.insert(name, *signature);
                builtin_id.insert(name, DefinitionId(id));
            } else if let TopLevelDef::Class { name, constructor, object_id, type_vars, .. } = &*def {
                assert!(id == object_id.0);
                assert!(type_vars.is_empty());
                if let Some(constructor) = constructor {
                    builtin_ty.insert(*name, *constructor);
                }
                builtin_id.insert(*name, DefinitionId(id));
            }
        }

        for (name, sig, codegen_callback) in builtins {
            let fun_sig = unifier.add_ty(TypeEnum::TFunc(RefCell::new(sig)));
            builtin_ty.insert(name, fun_sig);
            builtin_id.insert(name, DefinitionId(definition_ast_list.len()));
            definition_ast_list.push((
                Arc::new(RwLock::new(TopLevelDef::Function {
                    name: name.into(),
                    simple_name: name,
                    signature: fun_sig,
                    instance_to_stmt: Default::default(),
                    instance_to_symbol: Default::default(),
                    var_id: Default::default(),
                    resolver: None,
                    codegen_callback: Some(codegen_callback),
                })),
                None,
            ));
            keyword_list.insert(name);
        }

        (
            TopLevelComposer {
                builtin_num: definition_ast_list.len(),
                definition_ast_list,
                primitives_ty,
                unifier,
                keyword_list,
                defined_names,
                method_class,
                core_config,
            },
            builtin_id,
            builtin_ty,
        )
    }

    pub fn make_top_level_context(&self) -> TopLevelContext {
        TopLevelContext {
            definitions: RwLock::new(
                self.definition_ast_list.iter().map(|(x, ..)| x.clone()).collect_vec(),
            )
            .into(),
            // NOTE: only one for now
            unifiers: Arc::new(RwLock::new(vec![(
                self.unifier.get_shared_unifier(),
                self.primitives_ty,
            )])),
            personality_symbol: Some("__artiq_personality".into()),
        }
    }

    pub fn extract_def_list(&self) -> Vec<Arc<RwLock<TopLevelDef>>> {
        self.definition_ast_list.iter().map(|(def, ..)| def.clone()).collect_vec()
    }

    /// register, just remember the names of top level classes/function
    /// and check duplicate class/method/function definition
    pub fn register_top_level(
        &mut self,
        ast: ast::Stmt<()>,
        resolver: Option<Arc<dyn SymbolResolver + Send + Sync>>,
        mod_path: String,
    ) -> Result<(StrRef, DefinitionId, Option<Type>), String> {
        let defined_names = &mut self.defined_names;
        match &ast.node {
            ast::StmtKind::ClassDef { name: class_name, bases, body, .. } => {
                if self.keyword_list.contains(class_name) {
                    return Err(format!(
                        "cannot use keyword `{}` as a class name (at {})",
                        class_name,
                        ast.location
                    ));
                }
                let fully_qualified_class_name = if mod_path.is_empty() {
                    *class_name
                } else {
                    format!("{}.{}", &mod_path, class_name).into()
                };
                if !defined_names.insert(fully_qualified_class_name.into()) {
                    return Err(format!(
                        "duplicate definition of class `{}` (at {})",
                        class_name,
                        ast.location
                    ));
                }

                let class_name = *class_name;
                let class_def_id = self.definition_ast_list.len();

                // since later when registering class method, ast will still be used,
                // here push None temporarily, later will move the ast inside
                let constructor_ty = self.unifier.get_fresh_var().0;
                let mut class_def_ast = (
                    Arc::new(RwLock::new(Self::make_top_level_class_def(
                        class_def_id,
                        resolver.clone(),
                        fully_qualified_class_name,
                        Some(constructor_ty),
                    ))),
                    None,
                );

                // parse class def body and register class methods into the def list.
                // module's symbol resolver would not know the name of the class methods,
                // thus cannot return their definition_id
                type MethodInfo = (
                    // the simple method name without class name
                    StrRef,
                    // in this top level def, method name is prefixed with the class name
                    Arc<RwLock<TopLevelDef>>,
                    DefinitionId,
                    Type,
                    ast::Stmt<()>,
                );
                let mut class_method_name_def_ids: Vec<MethodInfo> = Vec::new();
                // we do not push anything to the def list, so we keep track of the index
                // and then push in the correct order after the for loop
                let mut class_method_index_offset = 0;
                let init_id = "__init__".into();
                let exception_id = "Exception".into();
                // TODO: Fix this hack. We will generate constructor for classes that inherit
                // from Exception class (directly or indirectly), but this code cannot handle
                // subclass of other exception classes.
                let mut contains_constructor = bases
                    .iter().any(|base| matches!(base.node, ast::ExprKind::Name { id, .. } if id == exception_id));
                for b in body {
                    if let ast::StmtKind::FunctionDef { name: method_name, .. } = &b.node {
                        if method_name == &init_id {
                            contains_constructor = true;
                        }
                        if self.keyword_list.contains(method_name) {
                            return Err(format!(
                                "cannot use keyword `{}` as a method name (at {})",
                                method_name,
                                b.location
                            ));
                        }
                        let global_class_method_name = Self::make_class_method_name(
                            fully_qualified_class_name.into(),
                            &method_name.to_string(),
                        );
                        if !defined_names.insert(global_class_method_name.clone()) {
                            return Err(format!(
                                "class method `{}` defined twice (at {})",
                                global_class_method_name,
                                b.location
                            ));
                        }
                        let method_def_id = self.definition_ast_list.len() + {
                            // plus 1 here since we already have the class def
                            class_method_index_offset += 1;
                            class_method_index_offset
                        };

                        // dummy method define here
                        let dummy_method_type = self.unifier.get_fresh_var();
                        class_method_name_def_ids.push((
                            *method_name,
                            RwLock::new(Self::make_top_level_function_def(
                                global_class_method_name,
                                *method_name,
                                // later unify with parsed type
                                dummy_method_type.0,
                                resolver.clone(),
                            ))
                            .into(),
                            DefinitionId(method_def_id),
                            dummy_method_type.0,
                            b.clone(),
                        ));
                    } else {
                        // do nothing
                        continue;
                    }
                }

                // move the ast to the entry of the class in the ast_list
                class_def_ast.1 = Some(ast);
                // get the methods into the top level class_def
                for (name, _, id, ty, ..) in &class_method_name_def_ids {
                    let mut class_def = class_def_ast.0.write();
                    if let TopLevelDef::Class { methods, .. } = class_def.deref_mut() {
                        methods.push((*name, *ty, *id));
                        self.method_class.insert(*id, DefinitionId(class_def_id));
                    } else {
                        unreachable!()
                    }
                }
                // now class_def_ast and class_method_def_ast_ids are ok, put them into actual def list in correct order
                self.definition_ast_list.push(class_def_ast);
                for (_, def, _, _, ast) in class_method_name_def_ids {
                    self.definition_ast_list.push((def, Some(ast)));
                }

                let result_ty = if contains_constructor { Some(constructor_ty) } else { None };
                Ok((class_name, DefinitionId(class_def_id), result_ty))
            }

            ast::StmtKind::FunctionDef { name, .. } => {
                // if self.keyword_list.contains(name) {
                //     return Err("cannot use keyword as a top level function name".into());
                // }
                let global_fun_name = if mod_path.is_empty() {
                    name.to_string()
                } else {
                    format!("{}.{}", mod_path, name)
                };
                if !defined_names.insert(global_fun_name.clone()) {
                    return Err(format!(
                        "top level function `{}` defined twice (at {})",
                        global_fun_name,
                        ast.location
                    ));
                }

                let fun_name = *name;
                let ty_to_be_unified = self.unifier.get_fresh_var().0;
                // add to the definition list
                self.definition_ast_list.push((
                    RwLock::new(Self::make_top_level_function_def(
                        global_fun_name,
                        *name,
                        // dummy here, unify with correct type later
                        ty_to_be_unified,
                        resolver,
                    ))
                    .into(),
                    Some(ast),
                ));

                // return
                Ok((
                    fun_name,
                    DefinitionId(self.definition_ast_list.len() - 1),
                    Some(ty_to_be_unified),
                ))
            }

            _ => Err(format!(
                "registrations of constructs other than top level classes/functions are not supported (at {})",
                ast.location
            )),
        }
    }

    pub fn start_analysis(&mut self, inference: bool) -> Result<(), String> {
        self.analyze_top_level_class_type_var()?;
        self.analyze_top_level_class_bases()?;
        self.analyze_top_level_class_fields_methods()?;
        self.analyze_top_level_function()?;
        if inference {
            self.analyze_function_instance()?;
        }
        Ok(())
    }

    /// step 1, analyze the type vars associated with top level class
    fn analyze_top_level_class_type_var(&mut self) -> Result<(), String> {
        let def_list = &self.definition_ast_list;
        let temp_def_list = self.extract_def_list();
        let unifier = self.unifier.borrow_mut();
        let primitives_store = &self.primitives_ty;

        // skip 5 to skip analyzing the primitives
        for (class_def, class_ast) in def_list.iter().skip(self.builtin_num) {
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
                                ast::ExprKind::Name { id, .. } if id == &"Generic".into()
                            )
                        } =>
                    {
                        if !is_generic {
                            is_generic = true;
                        } else {
                            return Err(format!(
                                "only single Generic[...] is allowed (at {})",
                                b.location
                            ));
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
                            return Err(format!(
                                "duplicate type variable occurs (at {})",
                                slice.location
                            ));
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
        if self.unifier.top_level.is_none() {
            let ctx = Arc::new(self.make_top_level_context());
            self.unifier.top_level = Some(ctx);
        }

        let temp_def_list = self.extract_def_list();
        let unifier = self.unifier.borrow_mut();

        // first, only push direct parent into the list
        for (class_def, class_ast) in self.definition_ast_list.iter_mut().skip(self.builtin_num) {
            let mut class_def = class_def.write();
            let (class_def_id, class_bases, class_ancestors, class_resolver, class_type_vars) = {
                if let TopLevelDef::Class { ancestors, resolver, object_id, type_vars, .. } =
                    class_def.deref_mut()
                {
                    if let Some(ast::Located {
                        node: ast::StmtKind::ClassDef { bases, .. }, ..
                    }) = class_ast
                    {
                        (object_id, bases, ancestors, resolver, type_vars)
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
                            ast::ExprKind::Name { id, .. } if id == &"Generic".into()
                        )
                ) {
                    continue;
                }

                if has_base {
                    return Err(format!(
                        "a class definition can only have at most one base class \
                        declaration and one generic declaration (at {})",
                        b.location
                    ));
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
                    vec![(*class_def_id, class_type_vars.clone())].into_iter().collect(),
                )?;

                if let TypeAnnotation::CustomClass { .. } = &base_ty {
                    class_ancestors.push(base_ty);
                } else {
                    return Err(format!(
                        "class base declaration can only be custom class (at {})",
                        b.location,
                    ));
                }
            }
        }

        // second, get all ancestors
        let mut ancestors_store: HashMap<DefinitionId, Vec<TypeAnnotation>> = Default::default();
        for (class_def, _) in self.definition_ast_list.iter().skip(self.builtin_num) {
            let class_def = class_def.read();
            let (class_ancestors, class_id) = {
                if let TopLevelDef::Class { ancestors, object_id, .. } = class_def.deref() {
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
        for (class_def, class_ast) in self.definition_ast_list.iter_mut().skip(self.builtin_num) {
            let mut class_def = class_def.write();
            let (class_ancestors, class_id, class_type_vars) = {
                if let TopLevelDef::Class { ancestors, object_id, type_vars, .. } =
                    class_def.deref_mut()
                {
                    (ancestors, *object_id, type_vars)
                } else {
                    continue;
                }
            };

            let ans = ancestors_store.get_mut(&class_id).unwrap();
            class_ancestors.append(ans);

            // insert self type annotation to the front of the vector to maintain the order
            class_ancestors
                .insert(0, make_self_type_annotation(class_type_vars.as_slice(), class_id));

            // special case classes that inherit from Exception
            if class_ancestors.iter().any(|ann| matches!(ann, TypeAnnotation::CustomClass { id, .. } if id.0 == 7)) {
                // if inherited from Exception, the body should be a pass
                if let ast::StmtKind::ClassDef { body, .. } = &class_ast.as_ref().unwrap().node {
                    for stmt in body.iter() {
                        if matches!(stmt.node, ast::StmtKind::FunctionDef { .. } | ast::StmtKind::AnnAssign { .. }) {
                            return Err("Classes inherited from exception should have no custom fields/methods".into());
                        }
                    }
                } else {
                    unreachable!()
                }
            }
        }

        // deal with ancestor of Exception object
        if let TopLevelDef::Class { name, ancestors, object_id, .. } = &mut *self.definition_ast_list[7].0.write() {
            assert_eq!(*name, "Exception".into());
            ancestors.push(make_self_type_annotation(&[], *object_id));
        } else {
            unreachable!();
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

        for (class_def, class_ast) in def_ast_list.iter().skip(self.builtin_num) {
            if matches!(&*class_def.read(), TopLevelDef::Class { .. }) {
                Self::analyze_single_class_methods_fields(
                    class_def.clone(),
                    &class_ast.as_ref().unwrap().node,
                    &temp_def_list,
                    unifier,
                    primitives,
                    &mut type_var_to_concrete_def,
                    (&self.keyword_list, &self.core_config)
                )?
            }
        }

        // handle the inheritanced methods and fields
        let mut current_ancestor_depth: usize = 2;
        loop {
            let mut finished = true;

            for (class_def, _) in def_ast_list.iter().skip(self.builtin_num) {
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

        // skip 5 to skip analyzing the primitives
        for (function_def, function_ast) in def_list.iter().skip(self.builtin_num) {
            let mut function_def = function_def.write();
            let function_def = function_def.deref_mut();
            let function_ast = if let Some(x) = function_ast.as_ref() {
                x
            } else {
                // if let TopLevelDef::Function { name, .. } = ``
                continue;
            };

            if let TopLevelDef::Function { signature: dummy_ty, resolver, var_id, .. } =
                function_def
            {
                if matches!(unifier.get_ty(*dummy_ty).as_ref(), TypeEnum::TFunc(_)) {
                    // already have a function type, is class method, skip
                    continue;
                }
                if let ast::StmtKind::FunctionDef { args, returns, .. } = &function_ast.node {
                    let resolver = resolver.as_ref();
                    let resolver = resolver.unwrap();
                    let resolver = resolver.deref();

                    let mut function_var_map: HashMap<u32, Type> = HashMap::new();
                    let arg_types = {
                        // make sure no duplicate parameter
                        let mut defined_paramter_name: HashSet<_> = HashSet::new();
                        for x in args.args.iter() {
                            if !defined_paramter_name.insert(x.node.arg) || keyword_list.contains(&x.node.arg) {
                                return Err(format!(
                                    "top level function must have unique parameter names \
                                    and names should not be the same as the keywords (at {})",
                                    x.location
                                ));
                            }
                        }

                        let arg_with_default: Vec<(&ast::Located<ast::ArgData<()>>, Option<&ast::Expr>)> = args
                            .args
                            .iter()
                            .rev()
                            .zip(args
                                .defaults
                                .iter()
                                .rev()
                                .map(|x| -> Option<&ast::Expr> { Some(x) })
                                .chain(std::iter::repeat(None))
                            ).collect_vec();

                        arg_with_default
                            .iter()
                            .rev()
                            .map(|(x, default)| -> Result<FuncArg, String> {
                                let annotation = x
                                    .node
                                    .annotation
                                    .as_ref()
                                    .ok_or_else(|| {
                                        format!(
                                            "function parameter `{}` at {} need type annotation",
                                            x.node.arg, x.location
                                        )
                                    })?
                                    .as_ref();

                                let type_annotation = parse_ast_to_type_annotation_kinds(
                                    resolver,
                                    temp_def_list.as_slice(),
                                    unifier,
                                    primitives_store,
                                    annotation,
                                    // NOTE: since only class need this, for function
                                    // it should be fine to be empty map
                                    HashMap::new(),
                                )?;

                                let type_vars_within =
                                    get_type_var_contained_in_type_annotation(&type_annotation)
                                        .into_iter()
                                        .map(|x| -> Result<(u32, Type), String> {
                                            if let TypeAnnotation::TypeVar(ty) = x {
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
                                    name: x.node.arg,
                                    ty,
                                    default_value: match default {
                                        None => None,
                                        Some(default) => Some({
                                            let v = Self::parse_parameter_default_value(default, resolver)?;
                                            Self::check_default_param_type(
                                                &v,
                                                &type_annotation,
                                                primitives_store,
                                                unifier
                                            ).map_err(|err| format!("{} (at {})", err, x.location))?;
                                            v
                                        })
                                    }
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
                                    // NOTE: since only class need this, for function
                                    // it should be fine to be empty map
                                    HashMap::new(),
                                )?
                            };

                            let type_vars_within =
                                get_type_var_contained_in_type_annotation(&return_ty_annotation)
                                    .into_iter()
                                    .map(|x| -> Result<(u32, Type), String> {
                                        if let TypeAnnotation::TypeVar(ty) = x {
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
                    var_id.extend_from_slice(function_var_map
                        .iter()
                        .filter_map(|(id, ty)| {
                            if matches!(&*unifier.get_ty(*ty), TypeEnum::TVar { range, .. } if range.borrow().is_empty()) {
                                None
                            } else {
                                Some(*id)
                            }
                        })
                        .collect_vec()
                        .as_slice()
                    );
                    let function_ty = unifier.add_ty(TypeEnum::TFunc(
                        FunSignature { args: arg_types, ret: return_ty, vars: function_var_map }
                            .into(),
                    ));
                    unifier
                        .unify(*dummy_ty, function_ty)
                        .map_err(|old| format!("{} (at {})", old, function_ast.location))?;
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
        core_info: (&HashSet<StrRef>, &ComposerConfig),
    ) -> Result<(), String> {
        let (keyword_list, core_config) = core_info;
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
        } = &mut *class_def
        {
            if let ast::StmtKind::ClassDef { name, bases, body, .. } = &class_ast {
                (*object_id, *name, bases, body, ancestors, fields, methods, type_vars, resolver)
            } else {
                unreachable!("here must be class def ast");
            }
        } else {
            unreachable!("here must be toplevel class def");
        };
        let class_resolver = class_resolver.as_ref().unwrap();
        let class_resolver = class_resolver.as_ref();

        let mut defined_fields: HashSet<_> = HashSet::new();
        for b in class_body_ast {
            match &b.node {
                ast::StmtKind::FunctionDef { args, returns, name, .. } => {
                    let (method_dummy_ty, method_id) =
                        Self::get_class_method_def_info(class_methods_def, *name)?;

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
                        let mut defined_paramter_name: HashSet<_> = HashSet::new();
                        let zelf: StrRef = "self".into();
                        for x in args.args.iter() {
                            if !defined_paramter_name.insert(x.node.arg)
                                || (keyword_list.contains(&x.node.arg) && x.node.arg != zelf) {
                                return Err(format!(
                                    "top level function must have unique parameter names \
                                    and names should not be the same as the keywords (at {})",
                                    x.location
                                ))
                            }
                        }

                        if name == &"__init__".into() && !defined_paramter_name.contains(&zelf) {
                            return Err(format!("__init__ method must have a `self` parameter (at {})", b.location));
                        }
                        if !defined_paramter_name.contains(&zelf) {
                            return Err(format!("class method must have a `self` parameter (at {})", b.location));
                        }

                        let mut result = Vec::new();

                        let arg_with_default: Vec<(&ast::Located<ast::ArgData<()>>, Option<&ast::Expr>)> = args
                            .args
                            .iter()
                            .rev()
                            .zip(args
                                .defaults
                                .iter()
                                .rev()
                                .map(|x| -> Option<&ast::Expr> { Some(x) })
                                .chain(std::iter::repeat(None))
                            ).collect_vec();

                        for (x, default) in arg_with_default.into_iter().rev() {
                            let name = x.node.arg;
                            if name != zelf {
                                let type_ann = {
                                    let annotation_expr = x
                                        .node
                                        .annotation
                                        .as_ref()
                                        .ok_or_else(|| {
                                            format!(
                                                "type annotation for `{}` at {} needed",
                                                x.node.arg, x.location
                                            )
                                        })?
                                        .as_ref();
                                    parse_ast_to_type_annotation_kinds(
                                        class_resolver,
                                        temp_def_list,
                                        unifier,
                                        primitives,
                                        annotation_expr,
                                        vec![(class_id, class_type_vars_def.clone())]
                                            .into_iter()
                                            .collect(),
                                    )?
                                };
                                // find type vars within this method parameter type annotation
                                let type_vars_within =
                                    get_type_var_contained_in_type_annotation(&type_ann);
                                // handle the class type var and the method type var
                                for type_var_within in type_vars_within {
                                    if let TypeAnnotation::TypeVar(ty) = type_var_within {
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
                                    default_value: match default {
                                        None => None,
                                        Some(default) => {
                                            if name == "self".into() {
                                                return Err(format!("`self` parameter cannot take default value (at {})", x.location));
                                            }
                                            Some({
                                                let v = Self::parse_parameter_default_value(default, class_resolver)?;
                                                Self::check_default_param_type(&v, &type_ann, primitives, unifier)
                                                    .map_err(|err| format!("{} (at {})", err, x.location))?;
                                                v
                                            })
                                        }
                                    }
                                };
                                // push the dummy type and the type annotation
                                // into the list for later unification
                                type_var_to_concrete_def
                                    .insert(dummy_func_arg.ty, type_ann.clone());
                                result.push(dummy_func_arg)
                            }
                        }
                        result
                    };

                    let ret_type = {
                        if let Some(result) = returns {
                            let result = result.as_ref();
                            let annotation = parse_ast_to_type_annotation_kinds(
                                class_resolver,
                                temp_def_list,
                                unifier,
                                primitives,
                                result,
                                vec![(class_id, class_type_vars_def.clone())].into_iter().collect(),
                            )?;
                            // find type vars within this return type annotation
                            let type_vars_within =
                                get_type_var_contained_in_type_annotation(&annotation);
                            // handle the class type var and the method type var
                            for type_var_within in type_vars_within {
                                if let TypeAnnotation::TypeVar(ty) = type_var_within {
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
                                TypeAnnotation::Primitive(primitives.none),
                            );
                            dummy_return_type
                        }
                    };

                    if let TopLevelDef::Function { var_id, .. } =
                        temp_def_list.get(method_id.0).unwrap().write().deref_mut()
                    {
                        var_id.extend_from_slice(method_var_map
                            .iter()
                            .filter_map(|(id, ty)| {
                                if matches!(&*unifier.get_ty(*ty), TypeEnum::TVar { range, .. } if range.borrow().is_empty()) {
                                    None
                                } else {
                                    Some(*id)
                                }
                            })
                            .collect_vec()
                            .as_slice()
                        );
                    } else {
                        unreachable!()
                    }
                    let method_type = unifier.add_ty(TypeEnum::TFunc(
                        FunSignature { args: arg_types, ret: ret_type, vars: method_var_map }
                            .into(),
                    ));

                    // unify now since function type is not in type annotation define
                    // which should be fine since type within method_type will be subst later
                    unifier.unify(method_dummy_ty, method_type)?;
                }
                ast::StmtKind::AnnAssign { target, annotation, value: None, .. } => {
                    if let ast::ExprKind::Name { id: attr, .. } = &target.node {
                        if defined_fields.insert(attr.to_string()) {
                            let dummy_field_type = unifier.get_fresh_var().0;

                            // handle Kernel[T], KernelInvariant[T]
                            let (annotation, mutable) = match &annotation.node {
                                ast::ExprKind::Subscript { value, slice, .. } if matches!(
                                    &value.node,
                                    ast::ExprKind::Name { id, .. } if id == &core_config.kernel_invariant_ann.into()
                                ) => (slice, false),
                                ast::ExprKind::Subscript { value, slice, .. } if matches!(
                                    &value.node,
                                    ast::ExprKind::Name { id, .. } if core_config.kernel_ann.map_or(false, |c| id == &c.into())
                                ) => (slice, true),
                                _ if core_config.kernel_ann.is_none() => (annotation, true),
                                _ => continue // ignore fields annotated otherwise
                            };
                            class_fields_def.push((*attr, dummy_field_type, mutable));

                            let parsed_annotation = parse_ast_to_type_annotation_kinds(
                                class_resolver,
                                temp_def_list,
                                unifier,
                                primitives,
                                annotation.as_ref(),
                                vec![(class_id, class_type_vars_def.clone())].into_iter().collect(),
                            )?;
                            // find type vars within this return type annotation
                            let type_vars_within =
                                get_type_var_contained_in_type_annotation(&parsed_annotation);
                            // handle the class type var and the method type var
                            for type_var_within in type_vars_within {
                                if let TypeAnnotation::TypeVar(t) = type_var_within {
                                    if !class_type_vars_def.contains(&t) {
                                        return Err(format!(
                                            "class fields can only use type \
                                            vars declared as class generic type vars (at {})",
                                            annotation.location
                                        ));
                                    }
                                } else {
                                    unreachable!("must be type var annotation");
                                }
                            }
                            type_var_to_concrete_def.insert(dummy_field_type, parsed_annotation);
                        } else {
                            return Err(format!(
                                "same class fields `{}` defined twice (at {})",
                                attr,
                                target.location
                            ));
                        }
                    } else {
                        return Err(format!(
                            "unsupported statement type in class definition body (at {})",
                            target.location
                        ));
                    }
                }
                ast::StmtKind::Pass { .. } => {}
                ast::StmtKind::Expr { value: _, .. } => {} // typically a docstring; ignoring all expressions matches CPython behavior
                _ => return Err(format!(
                    "unsupported statement in class definition body (at {})",
                    b.location
                )),
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
        if let TypeAnnotation::CustomClass { id, params: _ } = base {
            let base = temp_def_list.get(id.0).unwrap();
            let base = base.read();
            if let TopLevelDef::Class { methods, fields, .. } = &*base {
                // handle methods override
                // since we need to maintain the order, create a new list
                let mut new_child_methods: Vec<(StrRef, Type, DefinitionId)> = Vec::new();
                let mut is_override: HashSet<StrRef> = HashSet::new();
                for (anc_method_name, anc_method_ty, anc_method_def_id) in methods {
                    // find if there is a method with same name in the child class
                    let mut to_be_added = (*anc_method_name, *anc_method_ty, *anc_method_def_id);
                    for (class_method_name, class_method_ty, class_method_defid) in
                        class_methods_def.iter()
                    {
                        if class_method_name == anc_method_name {
                            // ignore and handle self
                            // if is __init__ method, no need to check return type
                            let ok = class_method_name == &"__init__".into()
                                || Self::check_overload_function_type(
                                    *class_method_ty,
                                    *anc_method_ty,
                                    unifier,
                                    type_var_to_concrete_def,
                                );
                            if !ok {
                                return Err(format!(
                                    "method {} has same name as ancestors' method, but incompatible type",
                                    class_method_name
                                ));
                            }
                            // mark it as added
                            is_override.insert(*class_method_name);
                            to_be_added =
                                (*class_method_name, *class_method_ty, *class_method_defid);
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
                            *class_method_name,
                            *class_method_ty,
                            *class_method_defid,
                        ));
                    }
                }
                // use the new_child_methods to replace all the elements in `class_methods_def`
                class_methods_def.drain(..);
                class_methods_def.extend(new_child_methods);

                // handle class fields
                let mut new_child_fields: Vec<(StrRef, Type, bool)> = Vec::new();
                // let mut is_override: HashSet<_> = HashSet::new();
                for (anc_field_name, anc_field_ty, mutable) in fields {
                    let to_be_added = (*anc_field_name, *anc_field_ty, *mutable);
                    // find if there is a fields with the same name in the child class
                    for (class_field_name, ..) in class_fields_def.iter() {
                        if class_field_name == anc_field_name {
                            return Err(format!(
                                "field `{}` has already declared in the ancestor classes",
                                class_field_name
                            ));
                        }
                    }
                    new_child_fields.push(to_be_added);
                }
                for (class_field_name, class_field_ty, mutable) in class_fields_def.iter() {
                    if !is_override.contains(class_field_name) {
                        new_child_fields.push((*class_field_name, *class_field_ty, *mutable));
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

    /// step 5, analyze and call type inferecer to fill the `instance_to_stmt` of topleveldef::function
    fn analyze_function_instance(&mut self) -> Result<(), String> {
        // first get the class contructor type correct for the following type check in function body
        // also do class field instantiation check
        let init_str_id = "__init__".into();
        let mut definition_extension = Vec::new();
        let mut constructors = Vec::new();
        for (i, (def, ast)) in self.definition_ast_list.iter().enumerate().skip(self.builtin_num) {
            let class_def = def.read();
            if let TopLevelDef::Class {
                constructor,
                ancestors,
                methods,
                fields,
                type_vars,
                name: class_name,
                object_id,
                resolver: _,
                ..
            } = &*class_def
            {
                let self_type = get_type_from_type_annotation_kinds(
                    self.extract_def_list().as_slice(),
                    &mut self.unifier,
                    &self.primitives_ty,
                    &make_self_type_annotation(type_vars, *object_id),
                )?;
                if ancestors.iter().any(|ann| matches!(ann, TypeAnnotation::CustomClass { id, .. } if id.0 == 7)) {
                    // create constructor for these classes
                    let string = self.primitives_ty.str;
                    let int64 = self.primitives_ty.int64;
                    let signature = self.unifier.add_ty(TypeEnum::TFunc(RefCell::new(FunSignature {
                        args: vec![
                            FuncArg { name: "msg".into(), ty: string,
                                default_value: Some(SymbolValue::Str("".into()))},
                            FuncArg { name: "param0".into(), ty: int64,
                                default_value: Some(SymbolValue::I64(0))},
                            FuncArg { name: "param1".into(), ty: int64,
                                default_value: Some(SymbolValue::I64(0))},
                            FuncArg { name: "param2".into(), ty: int64,
                                default_value: Some(SymbolValue::I64(0))},
                        ],
                        ret: self_type,
                        vars: Default::default()
                    })));
                    let cons_fun = TopLevelDef::Function {
                        name: format!("{}.{}", class_name, "__init__"),
                        simple_name: init_str_id,
                        signature,
                        var_id: Default::default(),
                        instance_to_symbol: Default::default(),
                        instance_to_stmt: Default::default(),
                        resolver: None,
                        codegen_callback: Some(Arc::new(GenCall::new(Box::new(exn_constructor))))
                    };
                    constructors.push((i, signature, definition_extension.len()));
                    definition_extension.push((Arc::new(RwLock::new(cons_fun)), None));
                    self.unifier
                        .unify(constructor.unwrap(), signature)
                        .map_err(|old| format!("{} (at {})", old, ast.as_ref().unwrap().location))?;
                    continue;
                }
                let mut init_id: Option<DefinitionId> = None;
                // get the class contructor type correct
                let (contor_args, contor_type_vars) = {
                    let mut constructor_args: Vec<FuncArg> = Vec::new();
                    let mut type_vars: HashMap<u32, Type> = HashMap::new();
                    for (name, func_sig, id) in methods {
                        if *name == init_str_id {
                            init_id = Some(*id);
                            if let TypeEnum::TFunc(sig) = self.unifier.get_ty(*func_sig).as_ref() {
                                let FunSignature { args, vars, .. } = &*sig.borrow();
                                constructor_args.extend_from_slice(args);
                                type_vars.extend(vars);
                            } else {
                                unreachable!("must be typeenum::tfunc")
                            }
                        }
                    }
                    (constructor_args, type_vars)
                };
                let contor_type = self.unifier.add_ty(TypeEnum::TFunc(
                    FunSignature { args: contor_args, ret: self_type, vars: contor_type_vars }
                        .into(),
                ));
                self.unifier
                    .unify(constructor.unwrap(), contor_type)
                    .map_err(|old| format!("{} (at {})", old, ast.as_ref().unwrap().location))?;

                // class field instantiation check
                if let (Some(init_id), false) = (init_id, fields.is_empty()) {
                    let init_ast =
                        self.definition_ast_list.get(init_id.0).unwrap().1.as_ref().unwrap();
                    if let ast::StmtKind::FunctionDef { name, body, .. } = &init_ast.node {
                        if *name != init_str_id {
                            unreachable!("must be init function here")
                        }
                        let all_inited = Self::get_all_assigned_field(body.as_slice())?;
                        for (f, _, _) in fields {
                            if !all_inited.contains(f) {
                                return Err(format!(
                                    "fields `{}` of class `{}` not fully initialized in the initializer (at {})",
                                    f,
                                    class_name,
                                    body[0].location,
                                ));
                            }
                        }
                    }
                }
            }
        }
        for (i, signature, id) in constructors.into_iter() {
            if let TopLevelDef::Class { methods, .. } = &mut *self.definition_ast_list[i].0.write() {
                methods.push((init_str_id, signature,
                        DefinitionId(self.definition_ast_list.len() + id)));
            } else {
                unreachable!()
            }
        }
        self.definition_ast_list.extend_from_slice(&definition_extension);

        let ctx = Arc::new(self.make_top_level_context());
        // type inference inside function body
        for (id, (def, ast)) in self.definition_ast_list.iter().enumerate().skip(self.builtin_num)
        {
            if ast.is_none() {
                continue;
            }
            let mut function_def = def.write();
            if let TopLevelDef::Function {
                instance_to_stmt,
                instance_to_symbol,
                name,
                simple_name,
                signature,
                resolver,
                var_id: insted_vars,
                ..
            } = &mut *function_def
            {
                if let TypeEnum::TFunc(func_sig) = self.unifier.get_ty(*signature).as_ref() {
                    let FunSignature { args, ret, vars } = &*func_sig.borrow();
                    // None if is not class method
                    let uninst_self_type = {
                        if let Some(class_id) = self.method_class.get(&DefinitionId(id)) {
                            let class_def = self.definition_ast_list.get(class_id.0).unwrap();
                            let class_def = class_def.0.read();
                            if let TopLevelDef::Class { type_vars, .. } = &*class_def {
                                let ty_ann = make_self_type_annotation(type_vars, *class_id);
                                let self_ty = get_type_from_type_annotation_kinds(
                                    self.extract_def_list().as_slice(),
                                    &mut self.unifier,
                                    &self.primitives_ty,
                                    &ty_ann,
                                )?;
                                Some((self_ty, type_vars.clone()))
                            } else {
                                unreachable!("must be class def")
                            }
                        } else {
                            None
                        }
                    };
                    // carefully handle those with bounds, without bounds and no typevars
                    // if class methods, `vars` also contains all class typevars here
                    let (type_var_subst_comb, no_range_vars) = {
                        let unifier = &mut self.unifier;
                        let mut no_ranges: Vec<Type> = Vec::new();
                        let var_ids = vars.keys().copied().collect_vec();
                        let var_combs = vars
                            .iter()
                            .map(|(_, ty)| {
                                unifier.get_instantiations(*ty).unwrap_or_else(|| {
                                    let rigid = unifier.get_fresh_rigid_var().0;
                                    no_ranges.push(rigid);
                                    vec![rigid]
                                })
                            })
                            .multi_cartesian_product()
                            .collect_vec();
                        let mut result: Vec<HashMap<u32, Type>> = Default::default();
                        for comb in var_combs {
                            result.push(var_ids.clone().into_iter().zip(comb).collect());
                        }
                        // NOTE: if is empty, means no type var, append a empty subst, ok to do this?
                        if result.is_empty() {
                            result.push(HashMap::new())
                        }
                        (result, no_ranges)
                    };

                    for subst in type_var_subst_comb {
                        // for each instance
                        let inst_ret = self.unifier.subst(*ret, &subst).unwrap_or(*ret);
                        let inst_args = {
                            let unifier = &mut self.unifier;
                            args.iter()
                                .map(|a| FuncArg {
                                    name: a.name,
                                    ty: unifier.subst(a.ty, &subst).unwrap_or(a.ty),
                                    default_value: a.default_value.clone(),
                                })
                                .collect_vec()
                        };
                        let self_type = {
                            let unifier = &mut self.unifier;
                            uninst_self_type
                                .clone()
                                .map(|(self_type, type_vars)| {
                                    let subst_for_self = {
                                        let class_ty_var_ids = type_vars
                                            .iter()
                                            .map(|x| {
                                                if let TypeEnum::TVar { id, .. } = &*unifier.get_ty(*x) {
                                                    *id
                                                } else {
                                                    unreachable!("must be type var here");
                                                }
                                            })
                                            .collect::<HashSet<_>>();
                                        subst
                                            .iter()
                                            .filter_map(|(ty_var_id, ty_var_target)| {
                                                if class_ty_var_ids.contains(ty_var_id) {
                                                    Some((*ty_var_id, *ty_var_target))
                                                } else {
                                                    None
                                                }
                                            })
                                            .collect::<HashMap<_, _>>()
                                    };
                                    unifier.subst(self_type, &subst_for_self).unwrap_or(self_type)
                                })
                        };
                        let mut identifiers = {
                            // NOTE: none and function args?
                            let mut result: HashSet<_> = HashSet::new();
                            result.insert("None".into());
                            if self_type.is_some() {
                                result.insert("self".into());
                            }
                            result.extend(inst_args.iter().map(|x| x.name));
                            result
                        };
                        let mut calls: HashMap<CodeLocation, CallId> = HashMap::new();
                        let mut inferencer = Inferencer {
                            top_level: ctx.as_ref(),
                            defined_identifiers: identifiers.clone(),
                            function_data: &mut FunctionData {
                                resolver: resolver.as_ref().unwrap().clone(),
                                return_type: if self
                                    .unifier
                                    .unioned(inst_ret, self.primitives_ty.none)
                                {
                                    None
                                } else {
                                    Some(inst_ret)
                                },
                                // NOTE: allowed type vars
                                bound_variables: no_range_vars.clone(),
                            },
                            unifier: &mut self.unifier,
                            variable_mapping: {
                                // NOTE: none and function args?
                                let mut result: HashMap<StrRef, Type> = HashMap::new();
                                result.insert("None".into(), self.primitives_ty.none);
                                if let Some(self_ty) = self_type {
                                    result.insert("self".into(), self_ty);
                                }
                                result.extend(inst_args.iter().map(|x| (x.name, x.ty)));
                                result
                            },
                            primitives: &self.primitives_ty,
                            virtual_checks: &mut Vec::new(),
                            calls: &mut calls,
                            in_handler: false
                        };

                        let fun_body =
                            if let ast::StmtKind::FunctionDef { body, decorator_list, .. } =
                                ast.clone().unwrap().node
                            {
                                if !decorator_list.is_empty()
                                    && matches!(&decorator_list[0].node,
                                        ast::ExprKind::Name{ id, .. } if id == &"extern".into())
                                {
                                    instance_to_symbol.insert("".into(), simple_name.to_string());
                                    continue;
                                }
                                if !decorator_list.is_empty()
                                    && matches!(&decorator_list[0].node,
                                        ast::ExprKind::Name{ id, .. } if id == &"rpc".into())
                                {
                                    instance_to_symbol.insert("".into(), simple_name.to_string());
                                    continue;
                                }
                                body
                            } else {
                                unreachable!("must be function def ast")
                            }
                            .into_iter()
                            .map(|b| inferencer.fold_stmt(b))
                            .collect::<Result<Vec<_>, _>>()?;

                        let returned =
                            inferencer.check_block(fun_body.as_slice(), &mut identifiers)?;
                        {
                            // check virtuals
                            let defs = ctx.definitions.read();
                            for (subtype, base, loc) in inferencer.virtual_checks.iter() {
                                let base_id = {
                                    let base = inferencer.unifier.get_ty(*base);
                                    if let TypeEnum::TObj { obj_id, .. } = &*base {
                                        *obj_id
                                    } else {
                                        return Err(format!("Base type should be a class (at {})", loc))
                                    }
                                };
                                let subtype_id = {
                                    let ty = inferencer.unifier.get_ty(*subtype);
                                    if let TypeEnum::TObj { obj_id, .. } = &*ty {
                                        *obj_id
                                    } else {
                                        let base_repr = inferencer.unifier.default_stringify(*base);
                                        let subtype_repr = inferencer.unifier.default_stringify(*subtype);
                                        return Err(format!("Expected a subtype of {}, but got {} (at {})", base_repr, subtype_repr, loc))
                                    }
                                };
                                let subtype_entry = defs[subtype_id.0].read();
                                if let TopLevelDef::Class { ancestors, .. } = &*subtype_entry {
                                    let m = ancestors.iter()
                                        .find(|kind| matches!(kind, TypeAnnotation::CustomClass { id, .. } if *id == base_id));
                                    if m.is_none() {
                                        let base_repr = inferencer.unifier.default_stringify(*base);
                                        let subtype_repr = inferencer.unifier.default_stringify(*subtype);
                                        return Err(format!("Expected a subtype of {}, but got {} (at {})", base_repr, subtype_repr, loc))
                                    }
                                } else {
                                    unreachable!();
                                }
                            }
                        }
                        if !self.unifier.unioned(inst_ret, self.primitives_ty.none) && !returned {
                            let def_ast_list = &self.definition_ast_list;
                            let ret_str = self.unifier.stringify(
                                inst_ret,
                                &mut |id| {
                                    if let TopLevelDef::Class { name, .. } =
                                        &*def_ast_list[id].0.read()
                                    {
                                        name.to_string()
                                    } else {
                                        unreachable!("must be class id here")
                                    }
                                },
                                &mut |id| format!("tvar{}", id),
                            );
                            return Err(format!(
                                "expected return type of `{}` in function `{}` (at {})",
                                ret_str,
                                name,
                                ast.as_ref().unwrap().location
                            ));
                        }

                        instance_to_stmt.insert(
                            get_subst_key(
                                &mut self.unifier,
                                self_type,
                                &subst,
                                Some(insted_vars),
                            ),
                            FunInstance {
                                body: Arc::new(fun_body),
                                unifier_id: 0,
                                calls: Arc::new(calls),
                                subst,
                            },
                        );
                    }
                } else {
                    unreachable!("must be typeenum::tfunc")
                }
            } else {
                continue;
            }
        }
        Ok(())
    }
}
