use rustpython_parser::ast::fold::Fold;

use crate::typecheck::type_inferencer::{FunctionData, Inferencer};

use super::*;

type DefAst = (Arc<RwLock<TopLevelDef>>, Option<ast::Stmt<()>>);
pub struct TopLevelComposer {
    // list of top level definitions, same as top level context
    pub definition_ast_list: Vec<DefAst>,
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
    // get the class def id of a class method
    pub method_class: HashMap<DefinitionId, DefinitionId>,
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
            definition_ast_list: {
                let top_level_def_list = vec![
                    Arc::new(RwLock::new(Self::make_top_level_class_def(0, None, "int32"))),
                    Arc::new(RwLock::new(Self::make_top_level_class_def(1, None, "int64"))),
                    Arc::new(RwLock::new(Self::make_top_level_class_def(2, None, "float"))),
                    Arc::new(RwLock::new(Self::make_top_level_class_def(3, None, "bool"))),
                    Arc::new(RwLock::new(Self::make_top_level_class_def(4, None, "none"))),
                ];
                let ast_list: Vec<Option<ast::Stmt<()>>> = vec![None, None, None, None, None];
                izip!(top_level_def_list, ast_list).collect_vec()
            },
            primitives_ty: primitives.0,
            unifier: primitives.1,
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
                "self".into(),
                "Kernel".into(),
                "KernelImmutable".into(),
            ]),
            defined_class_method_name: Default::default(),
            defined_class_name: Default::default(),
            defined_function_name: Default::default(),
            method_class: Default::default(),
        }
    }

    pub fn make_top_level_context(&self) -> TopLevelContext {
        TopLevelContext {
            definitions: RwLock::new(
                self.definition_ast_list.iter().map(|(x, ..)| x.clone()).collect_vec(),
            )
            .into(),
            // FIXME: all the big unifier or?
            unifiers: Arc::new(RwLock::new(vec![(
                self.unifier.get_shared_unifier(),
                self.primitives_ty,
            )])),
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
        resolver: Option<Arc<Box<dyn SymbolResolver + Send + Sync>>>,
        mod_path: String,
    ) -> Result<(String, DefinitionId, Option<Type>), String> {
        let defined_class_name = &mut self.defined_class_name;
        let defined_class_method_name = &mut self.defined_class_method_name;
        let defined_function_name = &mut self.defined_function_name;
        match &ast.node {
            ast::StmtKind::ClassDef { name, body, .. } => {
                if self.keyword_list.contains(name) {
                    return Err("cannot use keyword as a class name".into());
                }
                if !defined_class_name.insert({
                    let mut n = mod_path.clone();
                    n.push_str(name.as_str());
                    n
                }) {
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
                type MethodInfo = (
                    // the simple method name without class name
                    String,
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
                let mut has_init = false;
                for b in body {
                    if let ast::StmtKind::FunctionDef { name: method_name, .. } = &b.node {
                        if self.keyword_list.contains(name) {
                            return Err("cannot use keyword as a method name".into());
                        }
                        let global_class_method_name =
                            Self::make_class_method_name(class_name.clone(), method_name);
                        if !defined_class_method_name.insert({
                            let mut n = mod_path.clone();
                            n.push_str(global_class_method_name.as_str());
                            n
                        }) {
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
                            b.clone(),
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
                for (name, _, id, ty, ..) in &class_method_name_def_ids {
                    let mut class_def = class_def_ast.0.write();
                    if let TopLevelDef::Class { methods, .. } = class_def.deref_mut() {
                        methods.push((name.clone(), *ty, *id));
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

                // put the constructor into the def_list
                self.definition_ast_list.push((
                    RwLock::new(TopLevelDef::Initializer { class_id: DefinitionId(class_def_id) })
                        .into(),
                    None,
                ));

                Ok((class_name, DefinitionId(class_def_id), None))
            }

            ast::StmtKind::FunctionDef { name, .. } => {
                if self.keyword_list.contains(name) {
                    return Err("cannot use keyword as a top level function name".into());
                }
                let fun_name = name.to_string();
                if !defined_function_name.insert({
                    let mut n = mod_path;
                    n.push_str(name.as_str());
                    n
                }) {
                    return Err("duplicate top level function define".into());
                }

                let ty_to_be_unified = self.unifier.get_fresh_var().0;
                // add to the definition list
                self.definition_ast_list.push((
                    RwLock::new(Self::make_top_level_function_def(
                        name.into(),
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

            _ => Err("only registrations of top level classes/functions are supprted".into()),
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
        for (class_def, class_ast) in def_list.iter().skip(5) {
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
        // skip 5 to skip analyzing the primitives
        for (class_def, class_ast) in self.definition_ast_list.iter_mut().skip(5) {
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
                    class_resolver.as_ref(),
                    &temp_def_list,
                    unifier,
                    &self.primitives_ty,
                    b,
                    vec![(*class_def_id, class_type_vars.clone())].into_iter().collect(),
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
        // skip 5 to skip analyzing the primitives
        for (class_def, _) in self.definition_ast_list.iter().skip(5) {
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
        // skip 5 to skip analyzing the primitives
        for (class_def, _) in self.definition_ast_list.iter_mut().skip(5) {
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

        // skip 5 to skip analyzing the primitives
        for (class_def, class_ast) in def_ast_list.iter().skip(5) {
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

        // println!("type_var_to_concrete_def1: {:?}", type_var_to_concrete_def);

        // handle the inheritanced methods and fields
        let mut current_ancestor_depth: usize = 2;
        loop {
            let mut finished = true;

            for (class_def, _) in def_ast_list.iter().skip(5) {
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

        // println!("type_var_to_concrete_def3: {:?}\n", type_var_to_concrete_def);

        // unification of previously assigned typevar
        for (ty, def) in type_var_to_concrete_def {
            // println!(
            //     "{:?}_{} -> {:?}\n",
            //     ty,
            //     unifier.stringify(ty,
            //         &mut |id| format!("class{}", id),
            //         &mut |id| format!("tvar{}", id)
            //     ),
            //     def
            // );
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
        for (function_def, function_ast) in def_list.iter().skip(5) {
            let mut function_def = function_def.write();
            let function_def = function_def.deref_mut();
            let function_ast = if let Some(x) = function_ast.as_ref() {
                x
            } else {
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
                        let mut defined_paramter_name: HashSet<String> = HashSet::new();
                        let have_unique_fuction_parameter_name = args.args.iter().all(|x| {
                            defined_paramter_name.insert(x.node.arg.clone())
                                && !keyword_list.contains(&x.node.arg)
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
                                    resolver.as_ref(),
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
                                    resolver.as_ref(),
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
                    var_id.extend_from_slice(
                        function_var_map.keys().into_iter().copied().collect_vec().as_slice(),
                    );
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
            unreachable!("here must be toplevel class def");
        };
        let class_resolver = class_resolver.as_ref().unwrap();
        let class_resolver = class_resolver.as_ref();

        let mut defined_fields: HashSet<String> = HashSet::new();
        for b in class_body_ast {
            if let ast::StmtKind::FunctionDef { args, returns, name, .. } = &b.node {
                let (method_dummy_ty, method_id) =
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
                            && (!keyword_list.contains(&x.node.arg) || x.node.arg == "self")
                    });
                    if !have_unique_fuction_parameter_name {
                        return Err("class method must have unique parameter names \
                        and names thould not be the same as the keywords"
                            .into());
                    }
                    if name == "__init__" && !defined_paramter_name.contains("self") {
                        return Err("__init__ function must have a `self` parameter".into());
                    }
                    if !defined_paramter_name.contains("self") {
                        return Err("currently does not support static method".into());
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
                                    class_resolver.as_ref(),
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
                                // TODO: default value?
                                default_value: None,
                            };
                            // push the dummy type and the type annotation
                            // into the list for later unification
                            type_var_to_concrete_def.insert(dummy_func_arg.ty, type_ann.clone());
                            result.push(dummy_func_arg)
                        }
                    }
                    result
                };

                let ret_type = {
                    if let Some(result) = returns {
                        let result = result.as_ref();
                        let annotation = parse_ast_to_type_annotation_kinds(
                            class_resolver.as_ref(),
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
                };

                if let TopLevelDef::Function { var_id, .. } =
                    temp_def_list.get(method_id.0).unwrap().write().deref_mut()
                {
                    var_id.extend_from_slice(
                        method_var_map.keys().into_iter().copied().collect_vec().as_slice(),
                    );
                }
                let method_type = unifier.add_ty(TypeEnum::TFunc(
                    FunSignature { args: arg_types, ret: ret_type, vars: method_var_map }.into(),
                ));

                // unify now since function type is not in type annotation define
                // which should be fine since type within method_type will be subst later
                unifier.unify(method_dummy_ty, method_type)?;
            } else if let ast::StmtKind::AnnAssign { target, annotation, value: None, .. } = &b.node
            {
                if let ast::ExprKind::Name { id: attr, .. } = &target.node {
                    if defined_fields.insert(attr.to_string()) {
                        let dummy_field_type = unifier.get_fresh_var().0;
                        class_fields_def.push((attr.to_string(), dummy_field_type));

                        // handle Kernel[T], KernelImmutable[T]
                        let annotation = {
                            match &annotation.as_ref().node {
                                ast::ExprKind::Subscript { value, slice, .. }
                                    if {
                                        matches!(&value.node, ast::ExprKind::Name { id, .. } if id == "Kernel" || id == "KernelImmutable")
                                    } =>
                                {
                                    slice
                                }
                                _ => annotation,
                            }
                        };

                        let annotation = parse_ast_to_type_annotation_kinds(
                            class_resolver.as_ref(),
                            &temp_def_list,
                            unifier,
                            primitives,
                            annotation.as_ref(),
                            vec![(class_id, class_type_vars_def.clone())].into_iter().collect(),
                        )?;
                        // find type vars within this return type annotation
                        let type_vars_within =
                            get_type_var_contained_in_type_annotation(&annotation);
                        // handle the class type var and the method type var
                        for type_var_within in type_vars_within {
                            if let TypeAnnotation::TypeVarKind(t) = type_var_within {
                                if !class_type_vars_def.contains(&t) {
                                    return Err("class fields can only use type \
                                    vars declared as class generic type vars"
                                        .into());
                                }
                            } else {
                                unreachable!("must be type var annotation");
                            }
                        }
                        type_var_to_concrete_def.insert(dummy_field_type, annotation);
                    } else {
                        return Err("same class fields defined twice".into());
                    }
                }
            } else {
                return Err("unsupported statement type in class definition body".into());
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
                            // if is __init__ method, no need to check return type
                            let ok = class_method_name == "__init__"
                                || Self::check_overload_function_type(
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
                // let mut is_override: HashSet<String> = HashSet::new();
                for (anc_field_name, anc_field_ty) in fields {
                    let to_be_added = (anc_field_name.to_string(), *anc_field_ty);
                    // find if there is a fields with the same name in the child class
                    for (class_field_name, ..) in class_fields_def.iter() {
                        if class_field_name == anc_field_name {
                            // let ok = Self::check_overload_field_type(
                            //     *class_field_ty,
                            //     *anc_field_ty,
                            //     unifier,
                            //     type_var_to_concrete_def,
                            // );
                            // if !ok {
                            //     return Err("fields has same name as ancestors' field, but incompatible type".into());
                            // }
                            // // mark it as added
                            // is_override.insert(class_field_name.to_string());
                            // to_be_added = (class_field_name.to_string(), *class_field_ty);
                            // break;
                            return Err(format!(
                                "field `{}` has already declared in the ancestor classes",
                                class_field_name
                            ));
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

    /// step 5, analyze and call type inferecer to fill the `instance_to_stmt` of topleveldef::function
    fn analyze_function_instance(&mut self) -> Result<(), String> {
        for (id, (def, ast)) in self.definition_ast_list.iter().enumerate() {
            let mut function_def = def.write();
            if let TopLevelDef::Function {
                instance_to_stmt,
                name,
                signature,
                var_id,
                resolver,
                ..
            } = &mut *function_def
            {
                if let TypeEnum::TFunc(func_sig) = self.unifier.get_ty(*signature).as_ref() {
                    let FunSignature { args, ret, vars } = &*func_sig.borrow();
                    // None if is not class method
                    let self_type = {
                        if let Some(class_id) = self.method_class.get(&DefinitionId(id)) {
                            let class_def = self.definition_ast_list.get(class_id.0).unwrap();
                            let class_def = class_def.0.read();
                            if let TopLevelDef::Class { type_vars, .. } = &*class_def {
                                let ty_ann = make_self_type_annotation(type_vars, *class_id);
                                Some(get_type_from_type_annotation_kinds(
                                    self.extract_def_list().as_slice(),
                                    &mut self.unifier,
                                    &self.primitives_ty,
                                    &ty_ann,
                                )?)
                            } else {
                                unreachable!("must be class def")
                            }
                        } else {
                            None
                        }
                    };
                    let (type_var_subst_comb, no_range_vars) = {
                        let unifier = &mut self.unifier;
                        let mut no_ranges: Vec<Type> = Vec::new();
                        let var_ids = vars.iter().map(|(id, ty)| {
                            if matches!(unifier.get_ty(*ty).as_ref(), TypeEnum::TVar { range, .. } if range.borrow().is_empty()) {
                                no_ranges.push(*ty);
                            }
                            *id
                        })
                        .collect_vec();
                        let var_combs = vars
                            .iter()
                            .map(|(_, ty)| {
                                unifier.get_instantiations(*ty).unwrap_or_else(|| vec![*ty])
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
                            args
                                .iter()
                                .map(|a| FuncArg {
                                    name: a.name.clone(),
                                    ty: unifier.subst(a.ty, &subst).unwrap_or(a.ty),
                                    default_value: a.default_value.clone(),
                                })
                                .collect_vec()
                        };
                        let self_type = {
                            let unifier = &mut self.unifier;
                            self_type.map(|x| unifier.subst(x, &subst).unwrap_or(x))
                        };

                        let mut identifiers = {
                            // NOTE: none and function args?
                            let mut result: HashSet<String> = HashSet::new();
                            result.insert("None".into());
                            if self_type.is_some() {
                                result.insert("self".into());
                            }
                            result.extend(inst_args.iter().map(|x| x.name.clone()));
                            result
                        };
                        let mut inferencer = Inferencer {
                            top_level: &self.make_top_level_context(),
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
                                let mut result: HashMap<String, Type> = HashMap::new();
                                result.insert("None".into(), self.primitives_ty.none);
                                if let Some(self_ty) = self_type {
                                    result.insert("self".into(), self_ty);
                                }
                                result.extend(inst_args.iter().map(|x| (x.name.clone(), x.ty)));
                                result
                            },
                            primitives: &self.primitives_ty,
                            virtual_checks: &mut Vec::new(),
                            calls: &mut HashMap::new(),
                        };

                        let fun_body = if let ast::StmtKind::FunctionDef { body, .. } =
                            ast.clone().unwrap().node
                        {
                            body
                        } else {
                            unreachable!("must be function def ast")
                        }
                        .into_iter()
                        .map(|b| inferencer.fold_stmt(b))
                        .collect::<Result<Vec<_>, _>>()?;

                        let returned =
                            inferencer.check_block(fun_body.as_slice(), &mut identifiers)?;

                        if !self.unifier.unioned(inst_ret, self.primitives_ty.none) && !returned {
                            let ret_str = self.unifier.stringify(
                                inst_ret,
                                &mut |id| format!("class{}", id),
                                &mut |id| format!("tvar{}", id),
                            );
                            return Err(format!(
                                "expected return type of {} in function `{}`",
                                ret_str, name
                            ));
                        }

                        instance_to_stmt.insert(
                            // NOTE: refer to codegen/expr/get_subst_key function
                            {
                                let unifier = &mut self.unifier;
                                subst
                                    .keys()
                                    .sorted()
                                    .map(|id| {
                                        let ty = subst.get(id).unwrap();
                                        unifier.stringify(*ty, &mut |id| id.to_string(), &mut |id| id.to_string())
                                    }).join(", ")
                            },
                            FunInstance {
                                body: fun_body,
                                unifier_id: 0,
                                calls: HashMap::new(),
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
