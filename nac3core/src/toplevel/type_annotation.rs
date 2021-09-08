use std::cell::RefCell;

use crate::typecheck::typedef::TypeVarMeta;

use super::*;

#[derive(Clone, Debug)]
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
    ListKind(Box<TypeAnnotation>),
    TupleKind(Vec<TypeAnnotation>),
}

pub fn parse_ast_to_type_annotation_kinds<T>(
    resolver: &(dyn SymbolResolver + Send + Sync),
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

        // virtual
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

        // list
        ast::ExprKind::Subscript { value, slice, .. }
            if { matches!(&value.node, ast::ExprKind::Name { id, .. } if id == "list") } =>
        {
            let def_ann = parse_ast_to_type_annotation_kinds(
                resolver,
                top_level_defs,
                unifier,
                primitives,
                slice.as_ref(),
            )?;
            Ok(TypeAnnotation::ListKind(def_ann.into()))
        }

        // tuple
        ast::ExprKind::Subscript { value, slice, .. }
            if { matches!(&value.node, ast::ExprKind::Name { id, .. } if id == "tuple") } =>
        {
            if let ast::ExprKind::Tuple { elts, .. } = &slice.node {
                let type_annotations = elts
                    .iter()
                    .map(|e| {
                        parse_ast_to_type_annotation_kinds(
                            resolver,
                            top_level_defs,
                            unifier,
                            primitives,
                            e,
                        )
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(TypeAnnotation::TupleKind(type_annotations))
            } else {
                Err("Expect multiple elements for tuple".into())
            }
        }

        // custom class
        ast::ExprKind::Subscript { value, slice, .. } => {
            if let ast::ExprKind::Name { id, .. } = &value.node {
                if vec!["virtual", "Generic", "list", "tuple"].contains(&id.as_str()) {
                    return Err("keywords cannot be class name".into());
                }
                let obj_id = resolver
                    .get_identifier_def(id)
                    .ok_or_else(|| "unknown class name".to_string())?;
                let def = top_level_defs[obj_id.0].read();
                if let TopLevelDef::Class { type_vars, .. } = &*def {
                    // we do not check whether the application of type variables are compatible here
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
                        let result = params_ast
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
                            .collect::<Result<Vec<_>, _>>()?;

                        // make sure the result do not contain any type vars
                        let no_type_var = result
                            .iter()
                            .all(|x| get_type_var_contained_in_type_annotation(x).is_empty());
                        if no_type_var {
                            result
                        } else {
                            return Err("application of type vars to generic class \
                            is not currently supported"
                                .into());
                        }
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

                    let subst = {
                        // NOTE: check for compatible range here
                        let mut result: HashMap<u32, Type> = HashMap::new();
                        for (tvar, p) in type_vars.iter().zip(param_ty) {
                            if let TypeEnum::TVar { id, range, meta: TypeVarMeta::Generic } = unifier.get_ty(*tvar).as_ref() {
                                let ok: bool = {
                                    // NOTE: create a temp type var and unify to check compatibility
                                    let temp = unifier.get_fresh_var_with_range(range.borrow().as_slice());
                                    unifier.unify(temp.0, p).is_ok()
                                };
                                if ok {
                                    result.insert(*id, p);
                                } else {
                                    return Err(format!(
                                        "cannot apply type {} to type variable with id {:?}",
                                        unifier.stringify(
                                            p,
                                            &mut |id| format!("class{}", id),
                                            &mut |id| format!("tvar{}", id)
                                        ),
                                        *id
                                    ))
                                }
                            } else {
                                unreachable!("must be generic type var")
                            }
                        }
                        result
                    };
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

                    println!("tobj_fields: {:?}", tobj_fields);
                    println!(
                        "{:?}: {}\n",
                        tobj_fields.get("__init__").unwrap(),
                        unifier.stringify(
                            *tobj_fields.get("__init__").unwrap(),
                            &mut |id| format!("class{}", id),
                            &mut |id| format!("tvar{}", id)
                        )
                    );

                    Ok(unifier.add_ty(TypeEnum::TObj {
                        obj_id: *id,
                        fields: RefCell::new(tobj_fields),
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
        TypeAnnotation::ListKind(ty) => {
            let ty = get_type_from_type_annotation_kinds(
                top_level_defs,
                unifier,
                primitives,
                ty.as_ref(),
            )?;
            Ok(unifier.add_ty(TypeEnum::TList { ty }))
        }
        TypeAnnotation::TupleKind(tys) => {
            let tys = tys
                .iter()
                .map(|x| {
                    get_type_from_type_annotation_kinds(top_level_defs, unifier, primitives, x)
                })
                .collect::<Result<Vec<_>, _>>()?;
            Ok(unifier.add_ty(TypeEnum::TTuple { ty: tys }))
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
pub fn make_self_type_annotation(type_vars: &[Type], object_id: DefinitionId) -> TypeAnnotation {
    TypeAnnotation::CustomClassKind {
        id: object_id,
        params: type_vars.iter().map(|ty| TypeAnnotation::TypeVarKind(*ty)).collect_vec(),
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
        TypeAnnotation::ListKind(ann) => result.extend(get_type_var_contained_in_type_annotation(ann.as_ref())),
        TypeAnnotation::TupleKind(anns) => {
            for a in anns {
                result.extend(get_type_var_contained_in_type_annotation(a));
            }
        }
        TypeAnnotation::PrimitiveKind( .. ) => {}
    }
    result
}

/// check the type compatibility for overload
pub fn check_overload_type_annotation_compatible(
    this: &TypeAnnotation,
    other: &TypeAnnotation,
    unifier: &mut Unifier,
) -> bool {
    match (this, other) {
        (TypeAnnotation::PrimitiveKind(a), TypeAnnotation::PrimitiveKind(b)) => a == b,
        (TypeAnnotation::TypeVarKind(a), TypeAnnotation::TypeVarKind(b)) => {
            let a = unifier.get_ty(*a);
            let a = a.deref();
            let b = unifier.get_ty(*b);
            let b = b.deref();
            if let (
                TypeEnum::TVar { id: a, meta: TypeVarMeta::Generic, .. },
                TypeEnum::TVar { id: b, meta: TypeVarMeta::Generic, .. },
            ) = (a, b)
            {
                a == b
            } else {
                unreachable!("must be type var")
            }
        }
        (TypeAnnotation::VirtualKind(a), TypeAnnotation::VirtualKind(b))
        | (TypeAnnotation::ListKind(a), TypeAnnotation::ListKind(b)) => {
            check_overload_type_annotation_compatible(a.as_ref(), b.as_ref(), unifier)
        }

        (TypeAnnotation::TupleKind(a), TypeAnnotation::TupleKind(b)) => {
            a.len() == b.len() && {
                a.iter()
                    .zip(b)
                    .all(|(a, b)| check_overload_type_annotation_compatible(a, b, unifier))
            }
        }

        (
            TypeAnnotation::CustomClassKind { id: a, params: a_p },
            TypeAnnotation::CustomClassKind { id: b, params: b_p },
        ) => {
            a.0 == b.0 && {
                a_p.len() == b_p.len() && {
                    a_p.iter()
                        .zip(b_p)
                        .all(|(a, b)| check_overload_type_annotation_compatible(a, b, unifier))
                }
            }
        }

        _ => false,
    }
}
