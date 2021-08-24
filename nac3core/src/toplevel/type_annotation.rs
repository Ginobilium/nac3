use super::*;
use crate::typecheck::typedef::TypeVarMeta;
#[derive(Clone)]
pub enum TypeAnnotation {
    PrimitiveKind(Type),
    ConcretizedCustomClassKind {
        id: DefinitionId,
        // can not be type var, others are all fine
        // TODO: can also be type var?
        params: Vec<TypeAnnotation>,
    },
    // can only be ConcretizedCustomClassKind
    VirtualKind(Box<TypeAnnotation>),
    // the first u32 refers to the var_id of the
    // TVar returned by the symbol resolver,
    // this is used to handle type vars
    // associated with class/functions
    // since when associating we create a copy of type vars
    TypeVarKind(u32, Type),
    SelfTypeKind(DefinitionId),
}
pub fn parse_ast_to_type_annotation_kinds<T>(
    resolver: &dyn SymbolResolver,
    top_level_defs: &[Arc<RwLock<TopLevelDef>>],
    unifier: &mut Unifier,
    primitives: &PrimitiveStore,
    expr: &ast::Expr<T>,
) -> Result<TypeAnnotation, String> {
    let results = vec![
        parse_ast_to_concrete_primitive_kind(
            resolver,
            top_level_defs,
            unifier,
            primitives,
            expr,
        ),
        parse_ast_to_concretized_custom_class_kind(
            resolver,
            top_level_defs,
            unifier,
            primitives,
            expr,
        ),
        parse_ast_to_type_variable_kind(resolver, top_level_defs, unifier, primitives, expr),
        parse_ast_to_virtual_kind(resolver, top_level_defs, unifier, primitives, expr),
    ];
    let results = results.iter().filter(|x| x.is_ok()).collect_vec();
    if results.len() == 1 {
        results[0].clone()
    } else {
        Err("cannot parsed the type annotation without ambiguity".into())
    }
}
pub fn get_type_from_type_annotation_kinds(
    top_level_defs: &[Arc<RwLock<TopLevelDef>>],
    unifier: &mut Unifier,
    primitives: &PrimitiveStore,
    ann: &TypeAnnotation,
) -> Result<Type, String> {
    match ann {
        TypeAnnotation::ConcretizedCustomClassKind { id, params } => {
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
        TypeAnnotation::SelfTypeKind(obj_id) => {
            let class_def = top_level_defs[obj_id.0].read();
            if let TopLevelDef::Class { fields, methods, type_vars, .. } = &*class_def {
                let subst = type_vars
                    .iter()
                    .map(|x| {
                        if let TypeEnum::TVar { id, .. } = unifier.get_ty(x.1).as_ref() {
                            (*id, x.1)
                        } else {
                            unreachable!()
                        }
                    })
                    .collect::<HashMap<u32, Type>>();
                let mut tobj_fields = methods
                    .iter()
                    .map(|(name, ty, _)| (name.clone(), *ty))
                    .collect::<HashMap<String, Type>>();
                tobj_fields.extend(fields.clone().into_iter());
                Ok(unifier.add_ty(TypeEnum::TObj {
                    obj_id: *obj_id,
                    fields: tobj_fields.into(),
                    params: subst.into(),
                }))
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
fn parse_ast_to_concrete_primitive_kind<T>(
    _resolver: &dyn SymbolResolver,
    _top_level_defs: &[Arc<RwLock<TopLevelDef>>],
    _unifier: &mut Unifier,
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
            _ => Err("not primitive".into()),
        },
        _ => Err("not primitive".into()),
    }
}
pub fn parse_ast_to_concretized_custom_class_kind<T>(
    resolver: &dyn SymbolResolver,
    top_level_defs: &[Arc<RwLock<TopLevelDef>>],
    unifier: &mut Unifier,
    primitives: &PrimitiveStore,
    expr: &ast::Expr<T>,
) -> Result<TypeAnnotation, String> {
    match &expr.node {
        ast::ExprKind::Name { id, .. } => match id.as_str() {
            "int32" | "int64" | "float" | "bool" | "None" => {
                Err("expect custom class instead of primitives here".into())
            }
            x => {
                let obj_id = resolver
                    .get_identifier_def(x)
                    .ok_or_else(|| "unknown class name".to_string())?;
                let def = top_level_defs[obj_id.0].read();
                if let TopLevelDef::Class { .. } = &*def {
                    Ok(TypeAnnotation::ConcretizedCustomClassKind {
                        id: obj_id,
                        params: vec![],
                    })
                } else {
                    Err("function cannot be used as a type".into())
                }
            }
        },
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
                    let param_type_infos =
                        if let ast::ExprKind::Tuple { elts, .. } = &slice.node {
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
                    // TODO: allow type var in class generic application list
                    // if param_type_infos
                    //     .iter()
                    //     .any(|x| matches!(x, TypeAnnotation::TypeVarKind(..)))
                    // {
                    //     return Err(
                    //         "cannot apply type variable to class generic parameters".into()
                    //     );
                    // }
                    Ok(TypeAnnotation::ConcretizedCustomClassKind {
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
        _ => Err("unsupported expression type for concretized class".into()),
    }
}
pub fn parse_ast_to_virtual_kind<T>(
    resolver: &dyn SymbolResolver,
    top_level_defs: &[Arc<RwLock<TopLevelDef>>],
    unifier: &mut Unifier,
    primitives: &PrimitiveStore,
    expr: &ast::Expr<T>,
) -> Result<TypeAnnotation, String> {
    match &expr.node {
        ast::ExprKind::Subscript { value, slice, .. }
            if { matches!(&value.node, ast::ExprKind::Name { id, .. } if id == "virtual") } =>
        {
            let def = parse_ast_to_concretized_custom_class_kind(
                resolver,
                top_level_defs,
                unifier,
                primitives,
                slice.as_ref(),
            )?;
            if !matches!(def, TypeAnnotation::ConcretizedCustomClassKind { .. }) {
                unreachable!("must be concretized custom class kind in the virtual")
            }
            Ok(TypeAnnotation::VirtualKind(def.into()))
        }
        _ => Err("virtual type annotation must be like `virtual[ .. ]`".into()),
    }
}
pub fn parse_ast_to_type_variable_kind<T>(
    resolver: &dyn SymbolResolver,
    _top_level_defs: &[Arc<RwLock<TopLevelDef>>],
    unifier: &mut Unifier,
    primitives: &PrimitiveStore,
    expr: &ast::Expr<T>,
) -> Result<TypeAnnotation, String> {
    if let ast::ExprKind::Name { id, .. } = &expr.node {
        let ty = resolver
            .get_symbol_type(unifier, primitives, id)
            .ok_or_else(|| "unknown type variable name".to_string())?;
        if let TypeEnum::TVar { id, meta: TypeVarMeta::Generic, range } =
            unifier.get_ty(ty).as_ref()
        {
            // NOTE: always create a new one here
            // and later unify if needed
            // but record the var_id of the original type var returned by symbol resolver
            let range = range.borrow();
            let range = range.as_slice();
            Ok(TypeAnnotation::TypeVarKind(*id, unifier.get_fresh_var_with_range(range).0))
        } else {
            Err("not a type variable identifier".into())
        }
    } else {
        Err("unsupported expression for type variable".into())
    }
}