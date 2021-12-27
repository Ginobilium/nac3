use std::cell::RefCell;

use crate::typecheck::typedef::TypeVarMeta;
use super::*;

#[derive(Clone, Debug)]
pub enum TypeAnnotation {
    Primitive(Type),
    // we use type vars kind at params to represent self type
    CustomClass {
        id: DefinitionId,
        // params can also be type var
        params: Vec<TypeAnnotation>,
    },
    // can only be CustomClassKind
    Virtual(Box<TypeAnnotation>),
    TypeVar(Type),
    List(Box<TypeAnnotation>),
    Tuple(Vec<TypeAnnotation>),
}

impl TypeAnnotation {
    pub fn stringify(&self, unifier: &mut Unifier) -> String {
        use TypeAnnotation::*;
        match self {
            Primitive(ty) | TypeVar(ty) => unifier.default_stringify(*ty),
            CustomClass { id, params } => {
                let class_name = match unifier.top_level {
                    Some(ref top) => if let TopLevelDef::Class { name, .. } = &*top.definitions.read()[id.0].read() {
                        (*name).into()
                    } else {
                        format!("def_{}", id.0)
                    }
                    None => format!("def_{}", id.0)
                };
                format!("{{class: {}, params: {:?}}}", class_name, params.iter().map(|p| p.stringify(unifier)).collect_vec())
            }
            Virtual(ty) | List(ty) => ty.stringify(unifier),
            Tuple(types) => format!("({:?})", types.iter().map(|p| p.stringify(unifier)).collect_vec()),
        }
    }
}

pub fn parse_ast_to_type_annotation_kinds<T>(
    resolver: &(dyn SymbolResolver + Send + Sync),
    top_level_defs: &[Arc<RwLock<TopLevelDef>>],
    unifier: &mut Unifier,
    primitives: &PrimitiveStore,
    expr: &ast::Expr<T>,
    // the key stores the type_var of this topleveldef::class, we only need this field here
    locked: HashMap<DefinitionId, Vec<Type>>,
) -> Result<TypeAnnotation, String> {
    let name_handle = |id: &StrRef, unifier: &mut Unifier, locked: HashMap<DefinitionId, Vec<Type>>| {
        if id == &"int32".into() {
            Ok(TypeAnnotation::Primitive(primitives.int32))
        } else if id == &"int64".into() {
            Ok(TypeAnnotation::Primitive(primitives.int64))
        } else if id == &"float".into() {
            Ok(TypeAnnotation::Primitive(primitives.float))
        } else if id == &"bool".into() {
            Ok(TypeAnnotation::Primitive(primitives.bool))
        } else if id == &"None".into() {
            Ok(TypeAnnotation::Primitive(primitives.none))
        } else if id == &"str".into() {
            Ok(TypeAnnotation::Primitive(primitives.str))
        } else if let Some(obj_id) = resolver.get_identifier_def(*id) {
            let type_vars = {
                let def_read = top_level_defs[obj_id.0].try_read();
                if let Some(def_read) = def_read {
                    if let TopLevelDef::Class { type_vars, .. } = &*def_read {
                        type_vars.clone()
                    } else {
                        return Err(format!(
                            "function cannot be used as a type (at {})",
                            expr.location
                        ));
                    }
                } else {
                    locked.get(&obj_id).unwrap().clone()
                }
            };
            // check param number here
            if !type_vars.is_empty() {
                return Err(format!(
                    "expect {} type variable parameter but got 0 (at {})",
                    type_vars.len(),
                    expr.location,
                ));
            }
            Ok(TypeAnnotation::CustomClass { id: obj_id, params: vec![] })
        } else if let Some(ty) = resolver.get_symbol_type(unifier, top_level_defs, primitives, *id) {
            if let TypeEnum::TVar { .. } = unifier.get_ty(ty).as_ref() {
                Ok(TypeAnnotation::TypeVar(ty))
            } else {
                Err(format!(
                    "`{}` is not a valid type annotation (at {})",
                    id,
                    expr.location
                ))
            }
        } else {
            Err(format!("`{}` is not a valid type annotation (at {})", id, expr.location))
        }
    };

    let class_name_handle =
        |id: &StrRef, slice: &ast::Expr<T>, unifier: &mut Unifier, mut locked: HashMap<DefinitionId, Vec<Type>>| {
        if vec!["virtual".into(), "Generic".into(), "list".into(), "tuple".into()]
                .contains(id)
            {
                return Err(format!("keywords cannot be class name (at {})", expr.location));
            }
        let obj_id = resolver
            .get_identifier_def(*id)
            .ok_or_else(|| "unknown class name".to_string())?;
        let type_vars = {
            let def_read = top_level_defs[obj_id.0].try_read();
            if let Some(def_read) = def_read {
                if let TopLevelDef::Class { type_vars, .. } = &*def_read {
                    type_vars.clone()
                } else {
                    unreachable!("must be class here")
                }
            } else {
                locked.get(&obj_id).unwrap().clone()
            }
        };
        // we do not check whether the application of type variables are compatible here
        let param_type_infos = {
            let params_ast = if let ast::ExprKind::Tuple { elts, .. } = &slice.node {
                elts.iter().collect_vec()
            } else {
                vec![slice]
            };
            if type_vars.len() != params_ast.len() {
                return Err(format!(
                    "expect {} type parameters but got {} (at {})",
                    type_vars.len(),
                    params_ast.len(),
                    params_ast[0].location,
                ));
            }
            let result = params_ast
                .iter()
                .map(|x| {
                    parse_ast_to_type_annotation_kinds(
                        resolver,
                        top_level_defs,
                        unifier,
                        primitives,
                        x,
                        {
                            locked.insert(obj_id, type_vars.clone());
                            locked.clone()
                        },
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
                return Err(format!(
                    "application of type vars to generic class \
                    is not currently supported (at {})",
                    params_ast[0].location
                ));
            }
        };
        Ok(TypeAnnotation::CustomClass { id: obj_id, params: param_type_infos })
    };
    match &expr.node {
        ast::ExprKind::Name { id, .. } => name_handle(id, unifier, locked),
        // virtual
        ast::ExprKind::Subscript { value, slice, .. }
            if {
                matches!(&value.node, ast::ExprKind::Name { id, .. } if id == &"virtual".into())
            } =>
        {
            let def = parse_ast_to_type_annotation_kinds(
                resolver,
                top_level_defs,
                unifier,
                primitives,
                slice.as_ref(),
                locked,
            )?;
            if !matches!(def, TypeAnnotation::CustomClass { .. }) {
                unreachable!("must be concretized custom class kind in the virtual")
            }
            Ok(TypeAnnotation::Virtual(def.into()))
        }

        // list
        ast::ExprKind::Subscript { value, slice, .. }
            if {
                matches!(&value.node, ast::ExprKind::Name { id, .. } if id == &"list".into())
            } =>
        {
            let def_ann = parse_ast_to_type_annotation_kinds(
                resolver,
                top_level_defs,
                unifier,
                primitives,
                slice.as_ref(),
                locked,
            )?;
            Ok(TypeAnnotation::List(def_ann.into()))
        }

        // tuple
        ast::ExprKind::Subscript { value, slice, .. }
            if {
                matches!(&value.node, ast::ExprKind::Name { id, .. } if id == &"tuple".into())
            } =>
        {
            let tup_elts = {
                if let ast::ExprKind::Tuple { elts, .. } = &slice.node {
                    elts.as_slice()
                } else {
                    std::slice::from_ref(slice.as_ref())
                }
            };
            let type_annotations = tup_elts
                .iter()
                .map(|e| {
                    parse_ast_to_type_annotation_kinds(
                        resolver,
                        top_level_defs,
                        unifier,
                        primitives,
                        e,
                        locked.clone(),
                    )
                })
                .collect::<Result<Vec<_>, _>>()?;
            Ok(TypeAnnotation::Tuple(type_annotations))
        }

        // custom class
        ast::ExprKind::Subscript { value, slice, .. } => {
            if let ast::ExprKind::Name { id, .. } = &value.node {
                class_name_handle(id, slice, unifier, locked)
            } else {
                Err(format!("unsupported expression type for class name (at {})", value.location))
            }
        }

        _ => Err(format!("unsupported expression for type annotation (at {})", expr.location)),
    }
}

// no need to have the `locked` parameter, unlike the `parse_ast_to_type_annotation_kinds`, since
// when calling this function, there should be no topleveldef::class being write, and this function
// also only read the toplevedefs
pub fn get_type_from_type_annotation_kinds(
    top_level_defs: &[Arc<RwLock<TopLevelDef>>],
    unifier: &mut Unifier,
    primitives: &PrimitiveStore,
    ann: &TypeAnnotation,
) -> Result<Type, String> {
    match ann {
        TypeAnnotation::CustomClass { id: obj_id, params } => {
            let def_read = top_level_defs[obj_id.0].read();
            let class_def: &TopLevelDef = def_read.deref();
            if let TopLevelDef::Class { fields, methods, type_vars, .. } = class_def {
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
                        // check for compatible range
                        // TODO: if allow type var to be applied(now this disallowed in the parse_to_type_annotation), need more check
                        let mut result: HashMap<u32, Type> = HashMap::new();
                        for (tvar, p) in type_vars.iter().zip(param_ty) {
                            if let TypeEnum::TVar { id, range, meta: TypeVarMeta::Generic } =
                                unifier.get_ty(*tvar).as_ref()
                            {
                                let ok: bool = {
                                    // create a temp type var and unify to check compatibility
                                    p == *tvar || {
                                        let temp =
                                            unifier.get_fresh_var_with_range(range.borrow().as_slice());
                                        unifier.unify(temp.0, p).is_ok()
                                    }
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
                                    ));
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
                            // methods are immutable
                            (*name, (subst_ty, false))
                        })
                        .collect::<HashMap<_, _>>();
                    tobj_fields.extend(fields.iter().map(|(name, ty, mutability)| {
                        let subst_ty = unifier.subst(*ty, &subst).unwrap_or(*ty);
                        (*name, (subst_ty, *mutability))
                    }));

                    Ok(unifier.add_ty(TypeEnum::TObj {
                        obj_id: *obj_id,
                        fields: RefCell::new(tobj_fields),
                        params: subst.into(),
                    }))
                }
            } else {
                unreachable!("should be class def here")
            }
        }
        TypeAnnotation::Primitive(ty) | TypeAnnotation::TypeVar(ty) => Ok(*ty),
        TypeAnnotation::Virtual(ty) => {
            let ty = get_type_from_type_annotation_kinds(
                top_level_defs,
                unifier,
                primitives,
                ty.as_ref(),
            )?;
            Ok(unifier.add_ty(TypeEnum::TVirtual { ty }))
        }
        TypeAnnotation::List(ty) => {
            let ty = get_type_from_type_annotation_kinds(
                top_level_defs,
                unifier,
                primitives,
                ty.as_ref(),
            )?;
            Ok(unifier.add_ty(TypeEnum::TList { ty }))
        }
        TypeAnnotation::Tuple(tys) => {
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
/// the Type of their fields and methods will also be subst when application/instantiation
pub fn make_self_type_annotation(type_vars: &[Type], object_id: DefinitionId) -> TypeAnnotation {
    TypeAnnotation::CustomClass {
        id: object_id,
        params: type_vars.iter().map(|ty| TypeAnnotation::TypeVar(*ty)).collect_vec(),
    }
}

/// get all the occurences of type vars contained in a type annotation
/// e.g. `A[int, B[T], V, virtual[C[G]]]` => [T, V, G]
/// this function will not make a duplicate of type var
pub fn get_type_var_contained_in_type_annotation(ann: &TypeAnnotation) -> Vec<TypeAnnotation> {
    let mut result: Vec<TypeAnnotation> = Vec::new();
    match ann {
        TypeAnnotation::TypeVar(..) => result.push(ann.clone()),
        TypeAnnotation::Virtual(ann) => {
            result.extend(get_type_var_contained_in_type_annotation(ann.as_ref()))
        }
        TypeAnnotation::CustomClass { params, .. } => {
            for p in params {
                result.extend(get_type_var_contained_in_type_annotation(p));
            }
        }
        TypeAnnotation::List(ann) => {
            result.extend(get_type_var_contained_in_type_annotation(ann.as_ref()))
        }
        TypeAnnotation::Tuple(anns) => {
            for a in anns {
                result.extend(get_type_var_contained_in_type_annotation(a));
            }
        }
        TypeAnnotation::Primitive(..) => {}
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
        (TypeAnnotation::Primitive(a), TypeAnnotation::Primitive(b)) => a == b,
        (TypeAnnotation::TypeVar(a), TypeAnnotation::TypeVar(b)) => {
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
        (TypeAnnotation::Virtual(a), TypeAnnotation::Virtual(b))
        | (TypeAnnotation::List(a), TypeAnnotation::List(b)) => {
            check_overload_type_annotation_compatible(a.as_ref(), b.as_ref(), unifier)
        }

        (TypeAnnotation::Tuple(a), TypeAnnotation::Tuple(b)) => {
            a.len() == b.len() && {
                a.iter()
                    .zip(b)
                    .all(|(a, b)| check_overload_type_annotation_compatible(a, b, unifier))
            }
        }

        (
            TypeAnnotation::CustomClass { id: a, params: a_p },
            TypeAnnotation::CustomClass { id: b, params: b_p },
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
