use super::*;
use crate::typecheck::typedef::TypeVarMeta;

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

    pub fn make_class_method_name(mut class_name: String, method_name: &str) -> String {
        class_name.push_str(method_name);
        class_name
    }

    pub fn get_class_method_def_info(
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

    /// get all base class def id of a class, excluding itself. \
    /// this function should called only after the direct parent is set
    /// and before all the ancestors are set
    /// and when we allow single inheritance \
    /// the order of the returned list is from the child to the deepest ancestor
    pub fn get_all_ancestors_helper(
        child: &TypeAnnotation,
        temp_def_list: &[Arc<RwLock<TopLevelDef>>],
    ) -> Vec<TypeAnnotation> {
        let mut result: Vec<TypeAnnotation> = Vec::new();
        let mut parent = Self::get_parent(child, temp_def_list);
        while let Some(p) = parent {
            parent = Self::get_parent(&p, temp_def_list);
            result.push(p);
        }
        result
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

    pub fn check_overload_type_compatible(unifier: &mut Unifier, ty: Type, other: Type) -> bool {
        let ty = unifier.get_ty(ty);
        let ty = ty.as_ref();
        let other = unifier.get_ty(other);
        let other = other.as_ref();

        match (ty, other) {
            (TypeEnum::TList { ty }, TypeEnum::TList { ty: other })
            | (TypeEnum::TVirtual { ty }, TypeEnum::TVirtual { ty: other }) => {
                Self::check_overload_type_compatible(unifier, *ty, *other)
            }

            (TypeEnum::TTuple { ty }, TypeEnum::TTuple { ty: other }) => ty
                .iter()
                .zip(other)
                .all(|(ty, other)| Self::check_overload_type_compatible(unifier, *ty, *other)),

            (
                TypeEnum::TObj { obj_id, params, .. },
                TypeEnum::TObj { obj_id: other_obj_id, params: other_params, .. },
            ) => {
                let params = &*params.borrow();
                let other_params = &*other_params.borrow();
                obj_id.0 == other_obj_id.0
                    && (params.iter().all(|(var_id, ty)| {
                        if let Some(other_ty) = other_params.get(var_id) {
                            Self::check_overload_type_compatible(unifier, *ty, *other_ty)
                        } else {
                            false
                        }
                    }))
            }

            (
                TypeEnum::TVar { id, meta: TypeVarMeta::Generic, .. },
                TypeEnum::TVar { id: other_id, meta: TypeVarMeta::Generic, .. },
            ) => {
                // NOTE: directly compare var_id?
                *id == *other_id
            }

            _ => false,
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
}
