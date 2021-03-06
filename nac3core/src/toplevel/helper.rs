use std::convert::TryInto;

use crate::symbol_resolver::SymbolValue;
use nac3parser::ast::{Constant, Location};

use super::*;

impl TopLevelDef {
    pub fn to_string(&self, unifier: &mut Unifier) -> String {
        match self {
            TopLevelDef::Class { name, ancestors, fields, methods, type_vars, .. } => {
                let fields_str = fields
                    .iter()
                    .map(|(n, ty, _)| (n.to_string(), unifier.stringify(*ty)))
                    .collect_vec();

                let methods_str = methods
                    .iter()
                    .map(|(n, ty, id)| (n.to_string(), unifier.stringify(*ty), *id))
                    .collect_vec();
                format!(
                    "Class {{\nname: {:?},\nancestors: {:?},\nfields: {:?},\nmethods: {:?},\ntype_vars: {:?}\n}}",
                    name,
                    ancestors.iter().map(|ancestor| ancestor.stringify(unifier)).collect_vec(),
                    fields_str.iter().map(|(a, _)| a).collect_vec(),
                    methods_str.iter().map(|(a, b, _)| (a, b)).collect_vec(),
                    type_vars.iter().map(|id| unifier.stringify(*id)).collect_vec(),
                )
            }
            TopLevelDef::Function { name, signature, var_id, .. } => format!(
                "Function {{\nname: {:?},\nsig: {:?},\nvar_id: {:?}\n}}",
                name,
                unifier.stringify(*signature),
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
            fields: HashMap::new(),
            params: HashMap::new(),
        });
        let int64 = unifier.add_ty(TypeEnum::TObj {
            obj_id: DefinitionId(1),
            fields: HashMap::new(),
            params: HashMap::new(),
        });
        let float = unifier.add_ty(TypeEnum::TObj {
            obj_id: DefinitionId(2),
            fields: HashMap::new(),
            params: HashMap::new(),
        });
        let bool = unifier.add_ty(TypeEnum::TObj {
            obj_id: DefinitionId(3),
            fields: HashMap::new(),
            params: HashMap::new(),
        });
        let none = unifier.add_ty(TypeEnum::TObj {
            obj_id: DefinitionId(4),
            fields: HashMap::new(),
            params: HashMap::new(),
        });
        let range = unifier.add_ty(TypeEnum::TObj {
            obj_id: DefinitionId(5),
            fields: HashMap::new(),
            params: HashMap::new(),
        });
        let str = unifier.add_ty(TypeEnum::TObj {
            obj_id: DefinitionId(6),
            fields: HashMap::new(),
            params: HashMap::new(),
        });
        let exception = unifier.add_ty(TypeEnum::TObj {
            obj_id: DefinitionId(7),
            fields: vec![
                ("__name__".into(), (int32, true)),
                ("__file__".into(), (int32, true)),
                ("__line__".into(), (int32, true)),
                ("__col__".into(), (int32, true)),
                ("__func__".into(), (str, true)),
                ("__message__".into(), (str, true)),
                ("__param0__".into(), (int64, true)),
                ("__param1__".into(), (int64, true)),
                ("__param2__".into(), (int64, true)),
            ]
            .into_iter()
            .collect::<HashMap<_, _>>(),
            params: HashMap::new(),
        });
        let uint32 = unifier.add_ty(TypeEnum::TObj {
            obj_id: DefinitionId(8),
            fields: HashMap::new(),
            params: HashMap::new(),
        });
        let uint64 = unifier.add_ty(TypeEnum::TObj {
            obj_id: DefinitionId(9),
            fields: HashMap::new(),
            params: HashMap::new(),
        });

        let option_type_var = unifier.get_fresh_var(Some("option_type_var".into()), None);
        let is_some_type_fun_ty = unifier.add_ty(TypeEnum::TFunc(FunSignature {
            args: vec![],
            ret: bool,
            vars: HashMap::from([(option_type_var.1, option_type_var.0)]),
        }));
        let unwrap_fun_ty = unifier.add_ty(TypeEnum::TFunc(FunSignature {
            args: vec![],
            ret: option_type_var.0,
            vars: HashMap::from([(option_type_var.1, option_type_var.0)]),
        }));
        let option = unifier.add_ty(TypeEnum::TObj {
            obj_id: DefinitionId(10),
            fields: vec![
                ("is_some".into(), (is_some_type_fun_ty, true)),
                ("is_none".into(), (is_some_type_fun_ty, true)),
                ("unwrap".into(), (unwrap_fun_ty, true)),
            ]
            .into_iter()
            .collect::<HashMap<_, _>>(),
            params: HashMap::from([(option_type_var.1, option_type_var.0)]),
        });

        let primitives = PrimitiveStore {
            int32,
            int64,
            float,
            bool,
            none,
            range,
            str,
            exception,
            uint32,
            uint64,
            option,
        };
        crate::typecheck::magic_methods::set_primitives_magic_methods(&primitives, &mut unifier);
        (primitives, unifier)
    }

    /// already include the definition_id of itself inside the ancestors vector
    /// when first registering, the type_vars, fields, methods, ancestors are invalid
    pub fn make_top_level_class_def(
        index: usize,
        resolver: Option<Arc<dyn SymbolResolver + Send + Sync>>,
        name: StrRef,
        constructor: Option<Type>,
        loc: Option<Location>,
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
            loc,
        }
    }

    /// when first registering, the type is a invalid value
    pub fn make_top_level_function_def(
        name: String,
        simple_name: StrRef,
        ty: Type,
        resolver: Option<Arc<dyn SymbolResolver + Send + Sync>>,
        loc: Option<Location>,
    ) -> TopLevelDef {
        TopLevelDef::Function {
            name,
            simple_name,
            signature: ty,
            var_id: Default::default(),
            instance_to_symbol: Default::default(),
            instance_to_stmt: Default::default(),
            resolver,
            codegen_callback: None,
            loc,
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
            let p_id = if let TypeAnnotation::CustomClass { id, .. } = &p {
                *id
            } else {
                unreachable!("must be class kind annotation")
            };
            // check cycle
            let no_cycle = result.iter().all(|x| {
                if let TypeAnnotation::CustomClass { id, .. } = x {
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
        let child_id = if let TypeAnnotation::CustomClass { id, .. } = child {
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
        if let (
            TypeEnum::TFunc(FunSignature { args: this_args, ret: this_ret, .. }),
            TypeEnum::TFunc(FunSignature { args: other_args, ret: other_ret, .. }),
        ) = (this, other)
        {
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
                                    result.insert(*attr);
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
                ast::StmtKind::Pass { .. } => {}
                ast::StmtKind::Assert { .. } => {}
                ast::StmtKind::Expr { .. } => {}

                _ => {
                    unimplemented!()
                }
            }
        }
        Ok(result)
    }

    pub fn parse_parameter_default_value(
        default: &ast::Expr,
        resolver: &(dyn SymbolResolver + Send + Sync),
    ) -> Result<SymbolValue, String> {
        parse_parameter_default_value(default, resolver)
    }

    pub fn check_default_param_type(
        val: &SymbolValue,
        ty: &TypeAnnotation,
        primitive: &PrimitiveStore,
        unifier: &mut Unifier,
    ) -> Result<(), String> {
        fn type_default_param(
            val: &SymbolValue,
            primitive: &PrimitiveStore,
            unifier: &mut Unifier,
        ) -> TypeAnnotation {
            match val {
                SymbolValue::Bool(..) => TypeAnnotation::Primitive(primitive.bool),
                SymbolValue::Double(..) => TypeAnnotation::Primitive(primitive.float),
                SymbolValue::I32(..) => TypeAnnotation::Primitive(primitive.int32),
                SymbolValue::I64(..) => TypeAnnotation::Primitive(primitive.int64),
                SymbolValue::U32(..) => TypeAnnotation::Primitive(primitive.uint32),
                SymbolValue::U64(..) => TypeAnnotation::Primitive(primitive.uint64),
                SymbolValue::Str(..) => TypeAnnotation::Primitive(primitive.str),
                SymbolValue::Tuple(vs) => {
                    let vs_tys = vs
                        .iter()
                        .map(|v| type_default_param(v, primitive, unifier))
                        .collect::<Vec<_>>();
                    TypeAnnotation::Tuple(vs_tys)
                }
                SymbolValue::OptionNone => TypeAnnotation::CustomClass {
                    id: primitive.option.get_obj_id(unifier),
                    params: Default::default(),
                },
                SymbolValue::OptionSome(v) => {
                    let ty = type_default_param(v, primitive, unifier);
                    TypeAnnotation::CustomClass {
                        id: primitive.option.get_obj_id(unifier),
                        params: vec![ty],
                    }
                }
            }
        }

        fn is_compatible(
            found: &TypeAnnotation,
            expect: &TypeAnnotation,
            unifier: &mut Unifier,
            primitive: &PrimitiveStore,
        ) -> bool {
            match (found, expect) {
                (TypeAnnotation::Primitive(f), TypeAnnotation::Primitive(e)) => {
                    unifier.unioned(*f, *e)
                }
                (
                    TypeAnnotation::CustomClass { id: f_id, params: f_param },
                    TypeAnnotation::CustomClass { id: e_id, params: e_param },
                ) => {
                    *f_id == *e_id
                        && *f_id == primitive.option.get_obj_id(unifier)
                        && (f_param.is_empty()
                            || (f_param.len() == 1
                                && e_param.len() == 1
                                && is_compatible(&f_param[0], &e_param[0], unifier, primitive)))
                }
                (TypeAnnotation::Tuple(f), TypeAnnotation::Tuple(e)) => {
                    f.len() == e.len()
                        && f.iter()
                            .zip(e.iter())
                            .all(|(f, e)| is_compatible(f, e, unifier, primitive))
                }
                _ => false,
            }
        }

        let found = type_default_param(val, primitive, unifier);
        if !is_compatible(&found, ty, unifier, primitive) {
            Err(format!(
                "incompatible default parameter type, expect {}, found {}",
                ty.stringify(unifier),
                found.stringify(unifier),
            ))
        } else {
            Ok(())
        }
    }
}

pub fn parse_parameter_default_value(
    default: &ast::Expr,
    resolver: &(dyn SymbolResolver + Send + Sync),
) -> Result<SymbolValue, String> {
    fn handle_constant(val: &Constant, loc: &Location) -> Result<SymbolValue, String> {
        match val {
            Constant::Int(v) => {
                if let Ok(v) = (*v).try_into() {
                    Ok(SymbolValue::I32(v))
                } else {
                    Err(format!("integer value out of range at {}", loc))
                }
            }
            Constant::Float(v) => Ok(SymbolValue::Double(*v)),
            Constant::Bool(v) => Ok(SymbolValue::Bool(*v)),
            Constant::Tuple(tuple) => Ok(SymbolValue::Tuple(
                tuple.iter().map(|x| handle_constant(x, loc)).collect::<Result<Vec<_>, _>>()?,
            )),
            Constant::None => Err(format!(
                "`None` is not supported, use `none` for option type instead ({})",
                loc
            )),
            _ => unimplemented!("this constant is not supported at {}", loc),
        }
    }
    match &default.node {
        ast::ExprKind::Constant { value, .. } => handle_constant(value, &default.location),
        ast::ExprKind::Call { func, args, .. } if args.len() == 1 => {
            match &func.node {
                ast::ExprKind::Name { id, .. } if *id == "int64".into() => match &args[0].node {
                    ast::ExprKind::Constant { value: Constant::Int(v), .. } => {
                        let v: Result<i64, _> = (*v).try_into();
                        match v {
                            Ok(v) => Ok(SymbolValue::I64(v)),
                            _ => Err(format!("default param value out of range at {}", default.location)),
                        }
                    }
                    _ => Err(format!("only allow constant integer here at {}", default.location))
                }
                ast::ExprKind::Name { id, .. } if *id == "uint32".into() => match &args[0].node {
                    ast::ExprKind::Constant { value: Constant::Int(v), .. } => {
                        let v: Result<u32, _> = (*v).try_into();
                        match v {
                            Ok(v) => Ok(SymbolValue::U32(v)),
                            _ => Err(format!("default param value out of range at {}", default.location)),
                        }
                    }
                    _ => Err(format!("only allow constant integer here at {}", default.location))
                }
                ast::ExprKind::Name { id, .. } if *id == "uint64".into() => match &args[0].node {
                    ast::ExprKind::Constant { value: Constant::Int(v), .. } => {
                        let v: Result<u64, _> = (*v).try_into();
                        match v {
                            Ok(v) => Ok(SymbolValue::U64(v)),
                            _ => Err(format!("default param value out of range at {}", default.location)),
                        }
                    }
                    _ => Err(format!("only allow constant integer here at {}", default.location))
                }
                ast::ExprKind::Name { id, .. } if *id == "Some".into() => Ok(
                    SymbolValue::OptionSome(
                        Box::new(parse_parameter_default_value(&args[0], resolver)?)
                    )
                ),
                _ => Err(format!("unsupported default parameter at {}", default.location)),
            }
        }
        ast::ExprKind::Tuple { elts, .. } => Ok(SymbolValue::Tuple(elts
            .iter()
            .map(|x| parse_parameter_default_value(x, resolver))
            .collect::<Result<Vec<_>, _>>()?
        )),
        ast::ExprKind::Name { id, .. } if id == &"none".into() => Ok(SymbolValue::OptionNone),
        ast::ExprKind::Name { id, .. } => {
            resolver.get_default_param_value(default).ok_or_else(
                || format!(
                    "`{}` cannot be used as a default parameter at {} \
                    (not primitive type, option or tuple / not defined?)",
                    id,
                    default.location
                )
            )
        }
        _ => Err(format!(
            "unsupported default parameter (not primitive type, option or tuple) at {}",
            default.location
        ))
    }
}
