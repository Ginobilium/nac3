use super::*;

#[derive(Clone)]
pub enum TypeAnnotation {
    PrimitiveKind(Type),
    // we use type vars kind at
    // params to represent self type
    CustomClassKind {
        id: DefinitionId,
        // can not be type var, others are all fine
        // TODO: can also be type var?
        params: Vec<TypeAnnotation>,
    },
    // can only be CustomClassKind
    VirtualKind(Box<TypeAnnotation>),
    // the first u32 refers to the var_id of the
    // TVar returned by the symbol resolver,
    // this is used to handle type vars
    // associated with class/functions
    // since when associating we create a copy of type vars
    TypeVarKind(u32, Type),
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
                    if let TopLevelDef::Class { .. } = &*def {
                        Ok(TypeAnnotation::CustomClassKind {
                            id: obj_id,
                            params: vec![],
                        })
                    } else {
                        Err("function cannot be used as a type".into())
                    }
                } else if let Some(ty) = resolver.get_symbol_type(unifier, primitives, id) {
                    if let TypeEnum::TVar { id, .. } = unifier.get_ty(ty).as_ref()
                    {
                        // NOTE: always create a new one here
                        // and later unify if needed
                        // but record the var_id of the original type var
                        // returned by symbol resolver
                        Ok(TypeAnnotation::TypeVarKind(
                            *id,
                            // TODO: maybe not duplicate will also be fine here?
                            duplicate_type_var(unifier, ty).0
                        ))
                    } else {
                        Err("not a type variable identifier".into())
                    }
                } else {
                    Err("name cannot be parsed as a type annotation".into())
                }
            }
        },

        // TODO: subscript or call?
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
                if let TopLevelDef::Class { .. } = &*def {
                    let param_type_infos = if let ast::ExprKind::Tuple { elts, .. } = &slice.node {
                        elts.iter()
                            .map(|v| {
                                parse_ast_to_type_annotation_kinds(
                                    resolver,
                                    top_level_defs,
                                    unifier,
                                    primitives,
                                    v,
                                )
                            })
                            .collect::<Result<Vec<_>, _>>()?
                    } else {
                        vec![parse_ast_to_type_annotation_kinds(
                            resolver,
                            top_level_defs,
                            unifier,
                            primitives,
                            slice,
                        )?]
                    };
                    // NOTE: allow type var in class generic application list
                    Ok(TypeAnnotation::CustomClassKind {
                        id: obj_id,
                        params: param_type_infos,
                    })
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
                    // FIXME: TODO: cannot directy subst type var here? need to subst types in fields/methods
                    let subst = type_vars
                        .iter()
                        .map(|x| {
                            if let TypeEnum::TVar { id, .. } = unifier.get_ty(x.1).as_ref() {
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
        TypeAnnotation::PrimitiveKind(ty) => Ok(*ty),
        TypeAnnotation::TypeVarKind(_, ty) => Ok(*ty),
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

/// the first return is the duplicated type \
/// the second return is the var_id of the duplicated type \
/// the third return is the var_id of the original type
#[inline]
pub fn duplicate_type_var(
    unifier: &mut Unifier,
    type_var: Type
) -> (Type, u32, u32) {
    let ty = unifier.get_ty(type_var);
    if let TypeEnum::TVar { id, range, .. } = ty.as_ref() {
        let range = range.borrow();
        let range = range.as_slice();
        let dup = unifier.get_fresh_var_with_range(range);
        (dup.0, dup.1, *id)
    } else {
        unreachable!("must be type var here to be duplicated");
    }
}

/// given an def id, return a type annotation of self \
/// ```python
/// class A(Generic[T, V]):
///     def fun(self):
/// ```
/// the type of `self` should be equivalent to `A[T, V]`, where `T`, `V`
/// considered to be type variables associated with the class
pub fn make_self_type_annotation(
    top_level_defs: &[Arc<RwLock<TopLevelDef>>],
    def_id: DefinitionId,
    unifier: &mut Unifier,
) -> Result<TypeAnnotation, String> {
    let obj_def = top_level_defs
        .get(def_id.0)
        .ok_or_else(|| "invalid definition id".to_string())?;
    let obj_def = obj_def.read();
    let obj_def = obj_def.deref();

    if let TopLevelDef::Class { type_vars, .. } = obj_def {
        Ok(TypeAnnotation::CustomClassKind {
            id: def_id,
            params: type_vars
                .iter()
                .map(|(var_id, ty)| TypeAnnotation::TypeVarKind(
                        *var_id,
                        duplicate_type_var(unifier, *ty).0
                ))
                .collect_vec()
        })
    } else {
        unreachable!("must be top level class def here")
    }
}

/// get all the occurences of type vars contained in a type annotation
/// e.g. `A[int, B[T], V]` => [T, V]
pub fn get_type_var_contained_in_type_annotation(ann: &TypeAnnotation) -> Vec<TypeAnnotation> {
    let mut result: Vec<TypeAnnotation> = Vec::new();
    match ann {
        TypeAnnotation::TypeVarKind( .. ) => result.push(ann.clone()),
        TypeAnnotation::VirtualKind(ann) => result.extend(
            get_type_var_contained_in_type_annotation(ann.as_ref())
        ),
        TypeAnnotation::CustomClassKind { params, .. } => {
            for p in params {
                result.extend(get_type_var_contained_in_type_annotation(p));
            }
        },
        _ => { }
    }
    result
}
