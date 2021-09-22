use super::*;

impl TopLevelDef {
    pub fn to_string<F, G>(
        &self,
        unifier: &mut Unifier,
        obj_to_name: &mut F,
        var_to_name: &mut G,
    ) -> String
    where
        F: FnMut(usize) -> String,
        G: FnMut(u32) -> String,
    {
        match self {
            TopLevelDef::Class {
                name, ancestors, fields, methods, object_id, type_vars, ..
            } => {
                let fields_str = fields
                    .iter()
                    .map(|(n, ty)| {
                        (n.to_string(), unifier.stringify(*ty, obj_to_name, var_to_name))
                    })
                    .collect_vec();

                let methods_str = methods
                    .iter()
                    .map(|(n, ty, id)| {
                        (n.to_string(), unifier.stringify(*ty, obj_to_name, var_to_name), *id)
                    })
                    .collect_vec();

                format!(
                    "Class {{\nname: {:?},\ndef_id: {:?},\nancestors: {:?},\nfields: {:?},\nmethods: {:?},\ntype_vars: {:?}\n}}",
                    name,
                    object_id,
                    ancestors,
                    fields_str,
                    methods_str,
                    type_vars,
                )
            }
            TopLevelDef::Function { name, signature, var_id, .. } => format!(
                "Function {{\nname: {:?},\nsig: {:?},\nvar_id: {:?}\n}}",
                name,
                unifier.stringify(*signature, obj_to_name, var_to_name),
                {
                    // preserve the order for debug output and test
                    let mut r = var_id.clone();
                    r.sort_unstable();
                    r
                }
            ),
        }
    }
}

impl TopLevelComposer {
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

    /// already include the definition_id of itself inside the ancestors vector
    /// when first regitering, the type_vars, fields, methods, ancestors are invalid
    pub fn make_top_level_class_def(
        index: usize,
        resolver: Option<Arc<Box<dyn SymbolResolver + Send + Sync>>>,
        name: StrRef,
        constructor: Option<Type>,
    ) -> TopLevelDef {
        TopLevelDef::Class {
            name,
            object_id: DefinitionId(index),
            type_vars: Default::default(),
            fields: Default::default(),
            methods: Default::default(),
            ancestors: Default::default(),
            constructor,
            resolver,
        }
    }

    /// when first registering, the type is a invalid value
    pub fn make_top_level_function_def(
        name: String,
        simple_name: StrRef,
        ty: Type,
        resolver: Option<Arc<Box<dyn SymbolResolver + Send + Sync>>>,
    ) -> TopLevelDef {
        TopLevelDef::Function {
            name,
            simple_name,
            signature: ty,
            var_id: Default::default(),
            instance_to_symbol: Default::default(),
            instance_to_stmt: Default::default(),
            resolver,
        }
    }

    pub fn make_class_method_name(mut class_name: String, method_name: &str) -> String {
        class_name.push('.');
        class_name.push_str(method_name);
        class_name
    }

    pub fn get_class_method_def_info(
        class_methods_def: &[(StrRef, Type, DefinitionId)],
        method_name: StrRef,
    ) -> Result<(Type, DefinitionId), String> {
        for (name, ty, def_id) in class_methods_def {
            if name == &method_name {
                return Ok((*ty, *def_id));
            }
        }
        Err(format!("no method {} in the current class", method_name))
    }

    /// get all base class def id of a class, excluding itself. \
    /// this function should called only after the direct parent is set
    /// and before all the ancestors are set
    /// and when we allow single inheritance \
    /// the order of the returned list is from the child to the deepest ancestor
    pub fn get_all_ancestors_helper(
        child: &TypeAnnotation,
        temp_def_list: &[Arc<RwLock<TopLevelDef>>],
    ) -> Result<Vec<TypeAnnotation>, String> {
        let mut result: Vec<TypeAnnotation> = Vec::new();
        let mut parent = Self::get_parent(child, temp_def_list);
        while let Some(p) = parent {
            parent = Self::get_parent(&p, temp_def_list);
            let p_id = if let TypeAnnotation::CustomClassKind { id, .. } = &p {
                *id
            } else {
                unreachable!("must be class kind annotation")
            };
            // check cycle
            let no_cycle = result.iter().all(|x| {
                if let TypeAnnotation::CustomClassKind { id, .. } = x {
                    id.0 != p_id.0
                } else {
                    unreachable!("must be class kind annotation")
                }
            });
            if no_cycle {
                result.push(p);
            } else {
                return Err("cyclic inheritance detected".into());
            }
        }
        Ok(result)
    }

    /// should only be called when finding all ancestors, so panic when wrong
    fn get_parent(
        child: &TypeAnnotation,
        temp_def_list: &[Arc<RwLock<TopLevelDef>>],
    ) -> Option<TypeAnnotation> {
        let child_id = if let TypeAnnotation::CustomClassKind { id, .. } = child {
            *id
        } else {
            unreachable!("should be class type annotation")
        };
        let child_def = temp_def_list.get(child_id.0).unwrap();
        let child_def = child_def.read();
        if let TopLevelDef::Class { ancestors, .. } = &*child_def {
            if !ancestors.is_empty() {
                Some(ancestors[0].clone())
            } else {
                None
            }
        } else {
            unreachable!("child must be top level class def")
        }
    }

    /// get the var_id of a given TVar type
    pub fn get_var_id(var_ty: Type, unifier: &mut Unifier) -> Result<u32, String> {
        if let TypeEnum::TVar { id, .. } = unifier.get_ty(var_ty).as_ref() {
            Ok(*id)
        } else {
            Err("not type var".to_string())
        }
    }

    pub fn check_overload_function_type(
        this: Type,
        other: Type,
        unifier: &mut Unifier,
        type_var_to_concrete_def: &HashMap<Type, TypeAnnotation>,
    ) -> bool {
        let this = unifier.get_ty(this);
        let this = this.as_ref();
        let other = unifier.get_ty(other);
        let other = other.as_ref();
        if let (TypeEnum::TFunc(this_sig), TypeEnum::TFunc(other_sig)) = (this, other) {
            let (this_sig, other_sig) = (&*this_sig.borrow(), &*other_sig.borrow());
            let (
                FunSignature { args: this_args, ret: this_ret, vars: _this_vars },
                FunSignature { args: other_args, ret: other_ret, vars: _other_vars },
            ) = (this_sig, other_sig);
            // check args
            let args_ok = this_args
                .iter()
                .map(|FuncArg { name, ty, .. }| (name, type_var_to_concrete_def.get(ty).unwrap()))
                .zip(other_args.iter().map(|FuncArg { name, ty, .. }| {
                    (name, type_var_to_concrete_def.get(ty).unwrap())
                }))
                .all(|(this, other)| {
                    if this.0 == &"self".into() && this.0 == other.0 {
                        true
                    } else {
                        this.0 == other.0
                            && check_overload_type_annotation_compatible(this.1, other.1, unifier)
                    }
                });

            // check rets
            let ret_ok = check_overload_type_annotation_compatible(
                type_var_to_concrete_def.get(this_ret).unwrap(),
                type_var_to_concrete_def.get(other_ret).unwrap(),
                unifier,
            );

            // return
            args_ok && ret_ok
        } else {
            unreachable!("this function must be called with function type")
        }
    }

    pub fn check_overload_field_type(
        this: Type,
        other: Type,
        unifier: &mut Unifier,
        type_var_to_concrete_def: &HashMap<Type, TypeAnnotation>,
    ) -> bool {
        check_overload_type_annotation_compatible(
            type_var_to_concrete_def.get(&this).unwrap(),
            type_var_to_concrete_def.get(&other).unwrap(),
            unifier,
        )
    }

    pub fn get_all_assigned_field(stmts: &[ast::Stmt<()>]) -> Result<HashSet<StrRef>, String> {
        let mut result = HashSet::new();
        for s in stmts {
            match &s.node {
                ast::StmtKind::AnnAssign { target, .. }
                    if {
                        if let ast::ExprKind::Attribute { value, .. } = &target.node {
                            if let ast::ExprKind::Name { id, .. } = &value.node {
                                id == &"self".into()
                            } else {
                                false
                            }
                        } else {
                            false
                        }
                    } =>
                {
                    return Err(format!(
                        "redundant type annotation for class fields at {}",
                        s.location
                    ))
                }
                ast::StmtKind::Assign { targets, .. } => {
                    for t in targets {
                        if let ast::ExprKind::Attribute { value, attr, .. } = &t.node {
                            if let ast::ExprKind::Name { id, .. } = &value.node {
                                if id == &"self".into() {
                                    result.insert(attr.clone());
                                }
                            }
                        }
                    }
                }
                // TODO: do not check for For and While?
                ast::StmtKind::For { body, orelse, .. }
                | ast::StmtKind::While { body, orelse, .. } => {
                    result.extend(Self::get_all_assigned_field(body.as_slice())?);
                    result.extend(Self::get_all_assigned_field(orelse.as_slice())?);
                }
                ast::StmtKind::If { body, orelse, .. } => {
                    let inited_for_sure = Self::get_all_assigned_field(body.as_slice())?
                        .intersection(&Self::get_all_assigned_field(orelse.as_slice())?)
                        .cloned()
                        .collect::<HashSet<_>>();
                    result.extend(inited_for_sure);
                }
                ast::StmtKind::Try { body, orelse, finalbody, .. } => {
                    let inited_for_sure = Self::get_all_assigned_field(body.as_slice())?
                        .intersection(&Self::get_all_assigned_field(orelse.as_slice())?)
                        .cloned()
                        .collect::<HashSet<_>>();
                    result.extend(inited_for_sure);
                    result.extend(Self::get_all_assigned_field(finalbody.as_slice())?);
                }
                ast::StmtKind::With { body, .. } => {
                    result.extend(Self::get_all_assigned_field(body.as_slice())?);
                }
                ast::StmtKind::Pass => {}
                ast::StmtKind::Assert { .. } => {}
                ast::StmtKind::Expr { .. } => {}

                _ => {
                    unimplemented!()
                }
            }
        }
        Ok(result)
    }
}
