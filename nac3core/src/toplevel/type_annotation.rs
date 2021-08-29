use super::*;

#[derive(Clone)]
pub enum TypeAnnotation {
    PrimitiveKind(Type),
    // we use type vars kind at params to represent self type
    CustomClassKind {
        id: DefinitionId,
        // params can also be type var
        params: Vec<TypeAnnotation>,
    },
    // can only be CustomClassKind
    VirtualKind(Box<TypeAnnotation>),
    TypeVarKind(Type),
}

pub fn parse_ast_to_type_annotation_kinds<T>(
    resolver: &Box<dyn SymbolResolver + Send + Sync>,
    top_level_defs: &[Arc<RwLock<TopLevelDef>>],
    unifier: &mut Unifier,
    primitives: &PrimitiveStore,
    expr: &ast::Expr<T>,
) -> Result<TypeAnnotation, String> {
    match &expr.node {
        ast::ExprKind::Name { id, .. } => match id.as_str() {
            "int32" => Ok(TypeAnnotation::PrimitiveKind(primitives.int32)),
            "int64" => Ok(TypeAnnotation::PrimitiveKind(primitives.int64)),
            "float" => Ok(TypeAnnotation::PrimitiveKind(primitives.float)),
            "bool" => Ok(TypeAnnotation::PrimitiveKind(primitives.bool)),
            "None" => Ok(TypeAnnotation::PrimitiveKind(primitives.none)),
            x => {
                if let Some(obj_id) = resolver.get_identifier_def(x) {
                    let def = top_level_defs[obj_id.0].read();
                    if let TopLevelDef::Class { type_vars, .. } = &*def {
                        // also check param number here
                        if !type_vars.is_empty() {
                            return Err(format!(
                                "expect {} type variable parameter but got 0",
                                type_vars.len()
                            ));
                        }
                        Ok(TypeAnnotation::CustomClassKind { id: obj_id, params: vec![] })
                    } else {
                        Err("function cannot be used as a type".into())
                    }
                } else if let Some(ty) = resolver.get_symbol_type(unifier, primitives, id) {
                    if let TypeEnum::TVar { .. } = unifier.get_ty(ty).as_ref() {
                        Ok(TypeAnnotation::TypeVarKind(ty))
                    } else {
                        Err("not a type variable identifier".into())
                    }
                } else {
                    Err("name cannot be parsed as a type annotation".into())
                }
            }
        },

        // TODO: subscript or call for virtual?
        ast::ExprKind::Subscript { value, slice, .. }
            if { matches!(&value.node, ast::ExprKind::Name { id, .. } if id == "virtual") } =>
        {
            let def = parse_ast_to_type_annotation_kinds(
                resolver,
                top_level_defs,
                unifier,
                primitives,
                slice.as_ref(),
            )?;
            if !matches!(def, TypeAnnotation::CustomClassKind { .. }) {
                unreachable!("must be concretized custom class kind in the virtual")
            }
            Ok(TypeAnnotation::VirtualKind(def.into()))
        }

        ast::ExprKind::Subscript { value, slice, .. } => {
            if let ast::ExprKind::Name { id, .. } = &value.node {
                if vec!["virtual", "Generic"].contains(&id.as_str()) {
                    return Err("keywords cannot be class name".into());
                }
                let obj_id = resolver
                    .get_identifier_def(id)
                    .ok_or_else(|| "unknown class name".to_string())?;
                let def = top_level_defs[obj_id.0].read();
                if let TopLevelDef::Class { type_vars, .. } = &*def {
                    let param_type_infos = {
                        let params_ast = if let ast::ExprKind::Tuple { elts, .. } = &slice.node {
                            elts.iter().collect_vec()
                        } else {
                            vec![slice.as_ref()]
                        };
                        if type_vars.len() != params_ast.len() {
                            return Err(format!(
                                "expect {} type parameters but got {}",
                                type_vars.len(),
                                params_ast.len()
                            ));
                        }
                        params_ast
                            .into_iter()
                            .map(|x| {
                                parse_ast_to_type_annotation_kinds(
                                    resolver,
                                    top_level_defs,
                                    unifier,
                                    primitives,
                                    x,
                                )
                            })
                            .collect::<Result<Vec<_>, _>>()?
                    };

                    // allow type var in class generic application list
                    Ok(TypeAnnotation::CustomClassKind { id: obj_id, params: param_type_infos })
                } else {
                    Err("function cannot be used as a type".into())
                }
            } else {
                Err("unsupported expression type for class name".into())
            }
        }

        _ => Err("unsupported expression for type annotation".into()),
    }
}

pub fn get_type_from_type_annotation_kinds(
    top_level_defs: &[Arc<RwLock<TopLevelDef>>],
    unifier: &mut Unifier,
    primitives: &PrimitiveStore,
    ann: &TypeAnnotation,
) -> Result<Type, String> {
    match ann {
        TypeAnnotation::CustomClassKind { id, params } => {
            let class_def = top_level_defs[id.0].read();
            if let TopLevelDef::Class { fields, methods, type_vars, .. } = &*class_def {
                if type_vars.len() != params.len() {
                    Err(format!(
                        "unexpected number of type parameters: expected {} but got {}",
                        type_vars.len(),
                        params.len()
                    ))
                } else {
                    let param_ty = params
                        .iter()
                        .map(|x| {
                            get_type_from_type_annotation_kinds(
                                top_level_defs,
                                unifier,
                                primitives,
                                x,
                            )
                        })
                        .collect::<Result<Vec<_>, _>>()?;

                    let subst = type_vars
                        .iter()
                        .map(|x| {
                            if let TypeEnum::TVar { id, .. } = unifier.get_ty(*x).as_ref() {
                                // this is for the class generic application,
                                // we only need the information for the copied type var
                                // associated with the class
                                *id
                            } else {
                                unreachable!()
                            }
                        })
                        .zip(param_ty.into_iter())
                        .collect::<HashMap<u32, Type>>();
                    let mut tobj_fields = methods
                        .iter()
                        .map(|(name, ty, _)| {
                            let subst_ty = unifier.subst(*ty, &subst).unwrap_or(*ty);
                            (name.clone(), subst_ty)
                        })
                        .collect::<HashMap<String, Type>>();
                    tobj_fields.extend(fields.iter().map(|(name, ty)| {
                        let subst_ty = unifier.subst(*ty, &subst).unwrap_or(*ty);
                        (name.clone(), subst_ty)
                    }));
                    Ok(unifier.add_ty(TypeEnum::TObj {
                        obj_id: *id,
                        fields: tobj_fields.into(),
                        params: subst.into(),
                    }))
                }
            } else {
                unreachable!("should be class def here")
            }
        }
        TypeAnnotation::PrimitiveKind(ty) | TypeAnnotation::TypeVarKind(ty) => Ok(*ty),
        TypeAnnotation::VirtualKind(ty) => {
            let ty = get_type_from_type_annotation_kinds(
                top_level_defs,
                unifier,
                primitives,
                ty.as_ref(),
            )?;
            Ok(unifier.add_ty(TypeEnum::TVirtual { ty }))
        }
    }
}

/// given an def id, return a type annotation of self \
/// ```python
/// class A(Generic[T, V]):
///     def fun(self):
/// ```
/// the type of `self` should be similar to `A[T, V]`, where `T`, `V`
/// considered to be type variables associated with the class \
/// \
/// But note that here we do not make a duplication of `T`, `V`, we direclty
/// use them as they are in the TopLevelDef::Class since those in the
/// TopLevelDef::Class.type_vars will be substitute later when seeing applications/instantiations
/// the Type of their fields and methods will also be subst when application/instantiation \
/// \
/// Note this implicit self type is different with seeing `A[T, V]` explicitly outside
/// the class def ast body, where it is a new instantiation of the generic class `A`,
/// but equivalent to seeing `A[T, V]` inside the class def body ast, where although we
/// create copies of `T` and `V`, we will find them out as occured type vars in the analyze_class()
/// and unify them with the class generic `T`, `V`
pub fn make_self_type_annotation(
    top_level_defs: &[Arc<RwLock<TopLevelDef>>],
    def_id: DefinitionId,
) -> Result<TypeAnnotation, String> {
    let obj_def =
        top_level_defs.get(def_id.0).ok_or_else(|| "invalid definition id".to_string())?;
    let obj_def = obj_def.read();
    let obj_def = obj_def.deref();

    if let TopLevelDef::Class { type_vars, .. } = obj_def {
        Ok(TypeAnnotation::CustomClassKind {
            id: def_id,
            params: type_vars.iter().map(|ty| TypeAnnotation::TypeVarKind(*ty)).collect_vec(),
        })
    } else {
        unreachable!("must be top level class def here")
    }
}

/// get all the occurences of type vars contained in a type annotation
/// e.g. `A[int, B[T], V, virtual[C[G]]]` => [T, V, G]
/// this function will not make a duplicate of type var
pub fn get_type_var_contained_in_type_annotation(ann: &TypeAnnotation) -> Vec<TypeAnnotation> {
    let mut result: Vec<TypeAnnotation> = Vec::new();
    match ann {
        TypeAnnotation::TypeVarKind(..) => result.push(ann.clone()),
        TypeAnnotation::VirtualKind(ann) => {
            result.extend(get_type_var_contained_in_type_annotation(ann.as_ref()))
        }
        TypeAnnotation::CustomClassKind { params, .. } => {
            for p in params {
                result.extend(get_type_var_contained_in_type_annotation(p));
            }
        }
        _ => {}
    }
    result
}

/// get the var_id of a given TVar type
pub fn get_var_id(var_ty: Type, unifier: &mut Unifier) -> Result<u32, String> {
    if let TypeEnum::TVar { id, .. } = unifier.get_ty(var_ty).as_ref() {
        Ok(*id)
    } else {
        Err("not type var".to_string())
    }
}
