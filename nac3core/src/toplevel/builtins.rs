use super::*;
use crate::{
    codegen::{
        expr::destructure_range, irrt::calculate_len_for_slice_range, stmt::exn_constructor,
    },
    symbol_resolver::SymbolValue,
};
use inkwell::{types::BasicType, FloatPredicate, IntPredicate};

type BuiltinInfo = (Vec<(Arc<RwLock<TopLevelDef>>, Option<Stmt>)>, &'static [&'static str]);

pub fn get_exn_constructor(
    name: &str,
    class_id: usize,
    cons_id: usize,
    unifier: &mut Unifier,
    primitives: &PrimitiveStore
)-> (TopLevelDef, TopLevelDef, Type, Type) {
    let int32 = primitives.int32;
    let int64 = primitives.int64;
    let string = primitives.str;
    let exception_fields = vec![
        ("__name__".into(), int32, true),
        ("__file__".into(), string, true),
        ("__line__".into(), int32, true),
        ("__col__".into(), int32, true),
        ("__func__".into(), string, true),
        ("__message__".into(), string, true),
        ("__param0__".into(), int64, true),
        ("__param1__".into(), int64, true),
        ("__param2__".into(), int64, true),
    ];
    let exn_cons_args = vec![
        FuncArg {
            name: "msg".into(),
            ty: string,
            default_value: Some(SymbolValue::Str("".into())),
        },
        FuncArg { name: "param0".into(), ty: int64, default_value: Some(SymbolValue::I64(0)) },
        FuncArg { name: "param1".into(), ty: int64, default_value: Some(SymbolValue::I64(0)) },
        FuncArg { name: "param2".into(), ty: int64, default_value: Some(SymbolValue::I64(0)) },
    ];
    let exn_type = unifier.add_ty(TypeEnum::TObj {
        obj_id: DefinitionId(class_id),
        fields: exception_fields.iter().map(|(a, b, c)| (*a, (*b, *c))).collect(),
        params: Default::default(),
    });
    let signature = unifier.add_ty(TypeEnum::TFunc(FunSignature {
        args: exn_cons_args,
        ret: exn_type,
        vars: Default::default(),
    }));
    let fun_def = TopLevelDef::Function {
        name: format!("{}.__init__", name),
        simple_name: "__init__".into(),
        signature,
        var_id: Default::default(),
        instance_to_symbol: Default::default(),
        instance_to_stmt: Default::default(),
        resolver: None,
        codegen_callback: Some(Arc::new(GenCall::new(Box::new(exn_constructor)))),
        loc: None,
    };
    let class_def = TopLevelDef::Class {
        name: name.into(),
        object_id: DefinitionId(class_id),
        type_vars: Default::default(),
        fields: exception_fields,
        methods: vec![("__init__".into(), signature, DefinitionId(cons_id))],
        ancestors: vec![
            TypeAnnotation::CustomClass { id: DefinitionId(class_id), params: Default::default() },
            TypeAnnotation::CustomClass { id: DefinitionId(7), params: Default::default() },
        ],
        constructor: Some(signature),
        resolver: None,
        loc: None,
    };
    (fun_def, class_def, signature, exn_type)
}

pub fn get_builtins(primitives: &mut (PrimitiveStore, Unifier)) -> BuiltinInfo {
    let int32 = primitives.0.int32;
    let int64 = primitives.0.int64;
    let uint32 = primitives.0.uint32;
    let uint64 = primitives.0.uint64;
    let float = primitives.0.float;
    let boolean = primitives.0.bool;
    let range = primitives.0.range;
    let string = primitives.0.str;
    let num_ty = primitives.1.get_fresh_var_with_range(
        &[int32, int64, float, boolean, uint32, uint64],
        Some("N".into()),
        None,
    );
    let var_map: HashMap<_, _> = vec![(num_ty.1, num_ty.0)].into_iter().collect();
    let exception_fields = vec![
        ("__name__".into(), int32, true),
        ("__file__".into(), string, true),
        ("__line__".into(), int32, true),
        ("__col__".into(), int32, true),
        ("__func__".into(), string, true),
        ("__message__".into(), string, true),
        ("__param0__".into(), int64, true),
        ("__param1__".into(), int64, true),
        ("__param2__".into(), int64, true),
    ];

    // for Option, is_some and is_none share the same type: () -> bool,
    // and they are methods under the same class `Option`
    let (is_some_ty, unwrap_ty, (option_ty_var, option_ty_var_id)) =
        if let TypeEnum::TObj { fields, params, .. } =
            primitives.1.get_ty(primitives.0.option).as_ref()
        {
            (
                *fields.get(&"is_some".into()).unwrap(),
                *fields.get(&"unwrap".into()).unwrap(),
                (*params.iter().next().unwrap().1, *params.iter().next().unwrap().0),
            )
        } else {
            unreachable!()
        };
    let top_level_def_list = vec![
        Arc::new(RwLock::new(TopLevelComposer::make_top_level_class_def(
            0,
            None,
            "int32".into(),
            None,
            None,
        ))),
        Arc::new(RwLock::new(TopLevelComposer::make_top_level_class_def(
            1,
            None,
            "int64".into(),
            None,
            None,
        ))),
        Arc::new(RwLock::new(TopLevelComposer::make_top_level_class_def(
            2,
            None,
            "float".into(),
            None,
            None,
        ))),
        Arc::new(RwLock::new(TopLevelComposer::make_top_level_class_def(
            3,
            None,
            "bool".into(),
            None,
            None,
        ))),
        Arc::new(RwLock::new(TopLevelComposer::make_top_level_class_def(
            4,
            None,
            "none".into(),
            None,
            None,
        ))),
        Arc::new(RwLock::new(TopLevelComposer::make_top_level_class_def(
            5,
            None,
            "range".into(),
            None,
            None,
        ))),
        Arc::new(RwLock::new(TopLevelComposer::make_top_level_class_def(
            6,
            None,
            "str".into(),
            None,
            None,
        ))),
        Arc::new(RwLock::new(TopLevelDef::Class {
            name: "Exception".into(),
            object_id: DefinitionId(7),
            type_vars: Default::default(),
            fields: exception_fields,
            methods: Default::default(),
            ancestors: vec![],
            constructor: None,
            resolver: None,
            loc: None,
        })),
        Arc::new(RwLock::new(TopLevelComposer::make_top_level_class_def(
            8,
            None,
            "uint32".into(),
            None,
            None,
        ))),
        Arc::new(RwLock::new(TopLevelComposer::make_top_level_class_def(
            9,
            None,
            "uint64".into(),
            None,
            None,
        ))),
        Arc::new(RwLock::new({
            TopLevelDef::Class {
                name: "Option".into(),
                object_id: DefinitionId(10),
                type_vars: vec![option_ty_var],
                fields: vec![],
                methods: vec![
                    ("is_some".into(), is_some_ty.0, DefinitionId(11)),
                    ("is_none".into(), is_some_ty.0, DefinitionId(12)),
                    ("unwrap".into(), unwrap_ty.0, DefinitionId(13)),
                ],
                ancestors: vec![TypeAnnotation::CustomClass {
                    id: DefinitionId(10),
                    params: Default::default(),
                }],
                constructor: None,
                resolver: None,
                loc: None,
            }
        })),
        Arc::new(RwLock::new(TopLevelDef::Function {
            name: "Option.is_some".into(),
            simple_name: "is_some".into(),
            signature: is_some_ty.0,
            var_id: vec![option_ty_var_id],
            instance_to_symbol: Default::default(),
            instance_to_stmt: Default::default(),
            resolver: None,
            codegen_callback: Some(Arc::new(GenCall::new(Box::new(
                |ctx, obj, _, _, generator| {
                    let obj_val = obj.unwrap().1.clone().to_basic_value_enum(ctx, generator)?;
                    if let BasicValueEnum::PointerValue(ptr) = obj_val {
                        Ok(Some(ctx.builder.build_is_not_null(ptr, "is_some").into()))
                    } else {
                        unreachable!("option must be ptr")
                    }
                },
            )))),
            loc: None,
        })),
        Arc::new(RwLock::new(TopLevelDef::Function {
            name: "Option.is_none".into(),
            simple_name: "is_none".into(),
            signature: is_some_ty.0,
            var_id: vec![option_ty_var_id],
            instance_to_symbol: Default::default(),
            instance_to_stmt: Default::default(),
            resolver: None,
            codegen_callback: Some(Arc::new(GenCall::new(Box::new(
                |ctx, obj, _, _, generator| {
                    let obj_val = obj.unwrap().1.clone().to_basic_value_enum(ctx, generator)?;
                    if let BasicValueEnum::PointerValue(ptr) = obj_val {
                        Ok(Some(ctx.builder.build_is_null(ptr, "is_none").into()))
                    } else {
                        unreachable!("option must be ptr")
                    }
                },
            )))),
            loc: None,
        })),
        Arc::new(RwLock::new(TopLevelDef::Function {
            name: "Option.unwrap".into(),
            simple_name: "unwrap".into(),
            signature: unwrap_ty.0,
            var_id: vec![option_ty_var_id],
            instance_to_symbol: Default::default(),
            instance_to_stmt: Default::default(),
            resolver: None,
            codegen_callback: Some(Arc::new(GenCall::new(Box::new(
                |ctx, obj, _, _, generator| {
                    unreachable!("handled in gen_expr")
                },
            )))),
            loc: None,
        })),
        Arc::new(RwLock::new(TopLevelDef::Function {
            name: "int32".into(),
            simple_name: "int32".into(),
            signature: primitives.1.add_ty(TypeEnum::TFunc(FunSignature {
                args: vec![FuncArg { name: "n".into(), ty: num_ty.0, default_value: None }],
                ret: int32,
                vars: var_map.clone(),
            })),
            var_id: Default::default(),
            instance_to_symbol: Default::default(),
            instance_to_stmt: Default::default(),
            resolver: None,
            codegen_callback: Some(Arc::new(GenCall::new(Box::new(
                |ctx, _, fun, args, generator| {
                    let int32 = ctx.primitives.int32;
                    let int64 = ctx.primitives.int64;
                    let uint32 = ctx.primitives.uint32;
                    let uint64 = ctx.primitives.uint64;
                    let float = ctx.primitives.float;
                    let boolean = ctx.primitives.bool;
                    let arg_ty = fun.0.args[0].ty;
                    let arg = args[0].1.clone().to_basic_value_enum(ctx, generator)?;
                    Ok(if ctx.unifier.unioned(arg_ty, boolean) {
                        Some(
                            ctx.builder
                                .build_int_z_extend(
                                    arg.into_int_value(),
                                    ctx.ctx.i32_type(),
                                    "zext",
                                )
                                .into(),
                        )
                    } else if ctx.unifier.unioned(arg_ty, int32)
                        || ctx.unifier.unioned(arg_ty, uint32)
                    {
                        Some(arg)
                    } else if ctx.unifier.unioned(arg_ty, int64)
                        || ctx.unifier.unioned(arg_ty, uint64)
                    {
                        Some(
                            ctx.builder
                                .build_int_truncate(
                                    arg.into_int_value(),
                                    ctx.ctx.i32_type(),
                                    "trunc",
                                )
                                .into(),
                        )
                    } else if ctx.unifier.unioned(arg_ty, float) {
                        let val = ctx
                            .builder
                            .build_float_to_signed_int(
                                arg.into_float_value(),
                                ctx.ctx.i32_type(),
                                "fptosi",
                            )
                            .into();
                        Some(val)
                    } else {
                        unreachable!()
                    })
                },
            )))),
            loc: None,
        })),
        Arc::new(RwLock::new(TopLevelDef::Function {
            name: "int64".into(),
            simple_name: "int64".into(),
            signature: primitives.1.add_ty(TypeEnum::TFunc(FunSignature {
                args: vec![FuncArg { name: "n".into(), ty: num_ty.0, default_value: None }],
                ret: int64,
                vars: var_map.clone(),
            })),
            var_id: Default::default(),
            instance_to_symbol: Default::default(),
            instance_to_stmt: Default::default(),
            resolver: None,
            codegen_callback: Some(Arc::new(GenCall::new(Box::new(
                |ctx, _, fun, args, generator| {
                    let int32 = ctx.primitives.int32;
                    let int64 = ctx.primitives.int64;
                    let uint32 = ctx.primitives.uint32;
                    let uint64 = ctx.primitives.uint64;
                    let float = ctx.primitives.float;
                    let boolean = ctx.primitives.bool;
                    let arg_ty = fun.0.args[0].ty;
                    let arg = args[0].1.clone().to_basic_value_enum(ctx, generator)?;
                    Ok(
                        if ctx.unifier.unioned(arg_ty, boolean)
                            || ctx.unifier.unioned(arg_ty, uint32)
                        {
                            Some(
                                ctx.builder
                                    .build_int_z_extend(
                                        arg.into_int_value(),
                                        ctx.ctx.i64_type(),
                                        "zext",
                                    )
                                    .into(),
                            )
                        } else if ctx.unifier.unioned(arg_ty, int32) {
                            Some(
                                ctx.builder
                                    .build_int_s_extend(
                                        arg.into_int_value(),
                                        ctx.ctx.i64_type(),
                                        "sext",
                                    )
                                    .into(),
                            )
                        } else if ctx.unifier.unioned(arg_ty, int64)
                            || ctx.unifier.unioned(arg_ty, uint64)
                        {
                            Some(arg)
                        } else if ctx.unifier.unioned(arg_ty, float) {
                            let val = ctx
                                .builder
                                .build_float_to_signed_int(
                                    arg.into_float_value(),
                                    ctx.ctx.i64_type(),
                                    "fptosi",
                                )
                                .into();
                            Some(val)
                        } else {
                            unreachable!()
                        },
                    )
                },
            )))),
            loc: None,
        })),
        Arc::new(RwLock::new(TopLevelDef::Function {
            name: "uint32".into(),
            simple_name: "uint32".into(),
            signature: primitives.1.add_ty(TypeEnum::TFunc(FunSignature {
                args: vec![FuncArg { name: "n".into(), ty: num_ty.0, default_value: None }],
                ret: uint32,
                vars: var_map.clone(),
            })),
            var_id: Default::default(),
            instance_to_symbol: Default::default(),
            instance_to_stmt: Default::default(),
            resolver: None,
            codegen_callback: Some(Arc::new(GenCall::new(Box::new(
                |ctx, _, fun, args, generator| {
                    let int32 = ctx.primitives.int32;
                    let int64 = ctx.primitives.int64;
                    let uint32 = ctx.primitives.uint32;
                    let uint64 = ctx.primitives.uint64;
                    let float = ctx.primitives.float;
                    let boolean = ctx.primitives.bool;
                    let arg_ty = fun.0.args[0].ty;
                    let arg = args[0].1.clone().to_basic_value_enum(ctx, generator)?;
                    let res = if ctx.unifier.unioned(arg_ty, boolean) {
                        ctx.builder
                            .build_int_z_extend(arg.into_int_value(), ctx.ctx.i64_type(), "zext")
                            .into()
                    } else if ctx.unifier.unioned(arg_ty, int32)
                        || ctx.unifier.unioned(arg_ty, uint32)
                    {
                        arg
                    } else if ctx.unifier.unioned(arg_ty, int64)
                        || ctx.unifier.unioned(arg_ty, uint64)
                    {
                        ctx.builder
                            .build_int_truncate(arg.into_int_value(), ctx.ctx.i32_type(), "trunc")
                            .into()
                    } else if ctx.unifier.unioned(arg_ty, float) {
                        ctx.builder
                            .build_float_to_unsigned_int(
                                arg.into_float_value(),
                                ctx.ctx.i32_type(),
                                "ftoi",
                            )
                            .into()
                    } else {
                        unreachable!();
                    };
                    Ok(Some(res))
                },
            )))),
            loc: None,
        })),
        Arc::new(RwLock::new(TopLevelDef::Function {
            name: "uint64".into(),
            simple_name: "uint64".into(),
            signature: primitives.1.add_ty(TypeEnum::TFunc(FunSignature {
                args: vec![FuncArg { name: "n".into(), ty: num_ty.0, default_value: None }],
                ret: uint64,
                vars: var_map.clone(),
            })),
            var_id: Default::default(),
            instance_to_symbol: Default::default(),
            instance_to_stmt: Default::default(),
            resolver: None,
            codegen_callback: Some(Arc::new(GenCall::new(Box::new(
                |ctx, _, fun, args, generator| {
                    let int32 = ctx.primitives.int32;
                    let int64 = ctx.primitives.int64;
                    let uint32 = ctx.primitives.uint32;
                    let uint64 = ctx.primitives.uint64;
                    let float = ctx.primitives.float;
                    let boolean = ctx.primitives.bool;
                    let arg_ty = fun.0.args[0].ty;
                    let arg = args[0].1.clone().to_basic_value_enum(ctx, generator)?;
                    let res = if ctx.unifier.unioned(arg_ty, int32)
                        || ctx.unifier.unioned(arg_ty, uint32)
                        || ctx.unifier.unioned(arg_ty, boolean)
                    {
                        ctx.builder
                            .build_int_z_extend(arg.into_int_value(), ctx.ctx.i64_type(), "zext")
                            .into()
                    } else if ctx.unifier.unioned(arg_ty, int64)
                        || ctx.unifier.unioned(arg_ty, uint64)
                    {
                        arg
                    } else if ctx.unifier.unioned(arg_ty, float) {
                        ctx.builder
                            .build_float_to_unsigned_int(
                                arg.into_float_value(),
                                ctx.ctx.i64_type(),
                                "ftoi",
                            )
                            .into()
                    } else {
                        unreachable!();
                    };
                    Ok(Some(res))
                },
            )))),
            loc: None,
        })),
        Arc::new(RwLock::new(TopLevelDef::Function {
            name: "float".into(),
            simple_name: "float".into(),
            signature: primitives.1.add_ty(TypeEnum::TFunc(FunSignature {
                args: vec![FuncArg { name: "n".into(), ty: num_ty.0, default_value: None }],
                ret: float,
                vars: var_map.clone(),
            })),
            var_id: Default::default(),
            instance_to_symbol: Default::default(),
            instance_to_stmt: Default::default(),
            resolver: None,
            codegen_callback: Some(Arc::new(GenCall::new(Box::new(
                |ctx, _, fun, args, generator| {
                    let int32 = ctx.primitives.int32;
                    let int64 = ctx.primitives.int64;
                    let boolean = ctx.primitives.bool;
                    let float = ctx.primitives.float;
                    let arg_ty = fun.0.args[0].ty;
                    let arg = args[0].1.clone().to_basic_value_enum(ctx, generator)?;
                    Ok(
                        if ctx.unifier.unioned(arg_ty, boolean)
                            || ctx.unifier.unioned(arg_ty, int32)
                            || ctx.unifier.unioned(arg_ty, int64)
                        {
                            let arg = arg.into_int_value();
                            let val = ctx
                                .builder
                                .build_signed_int_to_float(arg, ctx.ctx.f64_type(), "sitofp")
                                .into();
                            Some(val)
                        } else if ctx.unifier.unioned(arg_ty, float) {
                            Some(arg)
                        } else {
                            unreachable!()
                        },
                    )
                },
            )))),
            loc: None,
        })),
        Arc::new(RwLock::new(TopLevelDef::Function {
            name: "round".into(),
            simple_name: "round".into(),
            signature: primitives.1.add_ty(TypeEnum::TFunc(FunSignature {
                args: vec![FuncArg { name: "n".into(), ty: float, default_value: None }],
                ret: int32,
                vars: Default::default(),
            })),
            var_id: Default::default(),
            instance_to_symbol: Default::default(),
            instance_to_stmt: Default::default(),
            resolver: None,
            codegen_callback: Some(Arc::new(GenCall::new(Box::new(
                |ctx, _, _, args, generator| {
                    let arg = args[0].1.clone().to_basic_value_enum(ctx, generator)?;
                    let round_intrinsic =
                        ctx.module.get_function("llvm.round.f64").unwrap_or_else(|| {
                            let float = ctx.ctx.f64_type();
                            let fn_type = float.fn_type(&[float.into()], false);
                            ctx.module.add_function("llvm.round.f64", fn_type, None)
                        });
                    let val = ctx
                        .builder
                        .build_call(round_intrinsic, &[arg.into()], "round")
                        .try_as_basic_value()
                        .left()
                        .unwrap();
                    Ok(Some(
                        ctx.builder
                            .build_float_to_signed_int(
                                val.into_float_value(),
                                ctx.ctx.i32_type(),
                                "fptosi",
                            )
                            .into(),
                    ))
                },
            )))),
            loc: None,
        })),
        Arc::new(RwLock::new(TopLevelDef::Function {
            name: "round64".into(),
            simple_name: "round64".into(),
            signature: primitives.1.add_ty(TypeEnum::TFunc(FunSignature {
                args: vec![FuncArg { name: "n".into(), ty: float, default_value: None }],
                ret: int64,
                vars: Default::default(),
            })),
            var_id: Default::default(),
            instance_to_symbol: Default::default(),
            instance_to_stmt: Default::default(),
            resolver: None,
            codegen_callback: Some(Arc::new(GenCall::new(Box::new(
                |ctx, _, _, args, generator| {
                    let arg = args[0].1.clone().to_basic_value_enum(ctx, generator)?;
                    let round_intrinsic =
                        ctx.module.get_function("llvm.round.f64").unwrap_or_else(|| {
                            let float = ctx.ctx.f64_type();
                            let fn_type = float.fn_type(&[float.into()], false);
                            ctx.module.add_function("llvm.round.f64", fn_type, None)
                        });
                    let val = ctx
                        .builder
                        .build_call(round_intrinsic, &[arg.into()], "round")
                        .try_as_basic_value()
                        .left()
                        .unwrap();
                    Ok(Some(
                        ctx.builder
                            .build_float_to_signed_int(
                                val.into_float_value(),
                                ctx.ctx.i64_type(),
                                "fptosi",
                            )
                            .into(),
                    ))
                },
            )))),
            loc: None,
        })),
        Arc::new(RwLock::new(TopLevelDef::Function {
            name: "range".into(),
            simple_name: "range".into(),
            signature: primitives.1.add_ty(TypeEnum::TFunc(FunSignature {
                args: vec![
                    FuncArg { name: "start".into(), ty: int32, default_value: None },
                    FuncArg {
                        name: "stop".into(),
                        ty: int32,
                        // placeholder
                        default_value: Some(SymbolValue::I32(0)),
                    },
                    FuncArg {
                        name: "step".into(),
                        ty: int32,
                        default_value: Some(SymbolValue::I32(1)),
                    },
                ],
                ret: range,
                vars: Default::default(),
            })),
            var_id: Default::default(),
            instance_to_symbol: Default::default(),
            instance_to_stmt: Default::default(),
            resolver: None,
            codegen_callback: Some(Arc::new(GenCall::new(Box::new(
                |ctx, _, _, args, generator| {
                    let mut start = None;
                    let mut stop = None;
                    let mut step = None;
                    let int32 = ctx.ctx.i32_type();
                    let zero = int32.const_zero();
                    for (i, arg) in args.iter().enumerate() {
                        if arg.0 == Some("start".into()) {
                            start = Some(arg.1.clone().to_basic_value_enum(ctx, generator)?);
                        } else if arg.0 == Some("stop".into()) {
                            stop = Some(arg.1.clone().to_basic_value_enum(ctx, generator)?);
                        } else if arg.0 == Some("step".into()) {
                            step = Some(arg.1.clone().to_basic_value_enum(ctx, generator)?);
                        } else if i == 0 {
                            start = Some(arg.1.clone().to_basic_value_enum(ctx, generator)?);
                        } else if i == 1 {
                            stop = Some(arg.1.clone().to_basic_value_enum(ctx, generator)?);
                        } else if i == 2 {
                            step = Some(arg.1.clone().to_basic_value_enum(ctx, generator)?);
                        }
                    }
                    // TODO: error when step == 0
                    let step = step.unwrap_or_else(|| int32.const_int(1, false).into());
                    let stop = stop.unwrap_or_else(|| {
                        let v = start.unwrap();
                        start = None;
                        v
                    });
                    let start = start.unwrap_or_else(|| int32.const_zero().into());
                    let ty = int32.array_type(3);
                    let ptr = ctx.builder.build_alloca(ty, "range");
                    unsafe {
                        let a = ctx.builder.build_in_bounds_gep(ptr, &[zero, zero], "start");
                        let b = ctx.builder.build_in_bounds_gep(
                            ptr,
                            &[zero, int32.const_int(1, false)],
                            "end",
                        );
                        let c = ctx.builder.build_in_bounds_gep(
                            ptr,
                            &[zero, int32.const_int(2, false)],
                            "step",
                        );
                        ctx.builder.build_store(a, start);
                        ctx.builder.build_store(b, stop);
                        ctx.builder.build_store(c, step);
                    }
                    Ok(Some(ptr.into()))
                },
            )))),
            loc: None,
        })),
        Arc::new(RwLock::new(TopLevelDef::Function {
            name: "str".into(),
            simple_name: "str".into(),
            signature: primitives.1.add_ty(TypeEnum::TFunc(FunSignature {
                args: vec![FuncArg { name: "s".into(), ty: string, default_value: None }],
                ret: string,
                vars: Default::default(),
            })),
            var_id: Default::default(),
            instance_to_symbol: Default::default(),
            instance_to_stmt: Default::default(),
            resolver: None,
            codegen_callback: Some(Arc::new(GenCall::new(Box::new(
                |ctx, _, _, args, generator| {
                    Ok(Some(args[0].1.clone().to_basic_value_enum(ctx, generator)?))
                },
            )))),
            loc: None,
        })),
        Arc::new(RwLock::new(TopLevelDef::Function {
            name: "bool".into(),
            simple_name: "bool".into(),
            signature: primitives.1.add_ty(TypeEnum::TFunc(FunSignature {
                args: vec![FuncArg { name: "n".into(), ty: num_ty.0, default_value: None }],
                ret: primitives.0.bool,
                vars: var_map.clone(),
            })),
            var_id: Default::default(),
            instance_to_symbol: Default::default(),
            instance_to_stmt: Default::default(),
            resolver: None,
            codegen_callback: Some(Arc::new(GenCall::new(Box::new(
                |ctx, _, fun, args, generator| {
                    let int32 = ctx.primitives.int32;
                    let int64 = ctx.primitives.int64;
                    let float = ctx.primitives.float;
                    let boolean = ctx.primitives.bool;
                    let arg_ty = fun.0.args[0].ty;
                    let arg = args[0].1.clone().to_basic_value_enum(ctx, generator)?;
                    Ok(if ctx.unifier.unioned(arg_ty, boolean) {
                        Some(arg)
                    } else if ctx.unifier.unioned(arg_ty, int32) {
                        Some(
                            ctx.builder
                                .build_int_compare(
                                    IntPredicate::NE,
                                    ctx.ctx.i32_type().const_zero(),
                                    arg.into_int_value(),
                                    "bool",
                                )
                                .into(),
                        )
                    } else if ctx.unifier.unioned(arg_ty, int64) {
                        Some(
                            ctx.builder
                                .build_int_compare(
                                    IntPredicate::NE,
                                    ctx.ctx.i64_type().const_zero(),
                                    arg.into_int_value(),
                                    "bool",
                                )
                                .into(),
                        )
                    } else if ctx.unifier.unioned(arg_ty, float) {
                        let val = ctx
                            .builder
                            .build_float_compare(
                                // UEQ as bool(nan) is True
                                FloatPredicate::UEQ,
                                arg.into_float_value(),
                                ctx.ctx.f64_type().const_zero(),
                                "bool",
                            )
                            .into();
                        Some(val)
                    } else {
                        unreachable!()
                    })
                },
            )))),
            loc: None,
        })),
        Arc::new(RwLock::new(TopLevelDef::Function {
            name: "floor".into(),
            simple_name: "floor".into(),
            signature: primitives.1.add_ty(TypeEnum::TFunc(FunSignature {
                args: vec![FuncArg { name: "n".into(), ty: float, default_value: None }],
                ret: int32,
                vars: Default::default(),
            })),
            var_id: Default::default(),
            instance_to_symbol: Default::default(),
            instance_to_stmt: Default::default(),
            resolver: None,
            codegen_callback: Some(Arc::new(GenCall::new(Box::new(
                |ctx, _, _, args, generator| {
                    let arg = args[0].1.clone().to_basic_value_enum(ctx, generator)?;
                    let floor_intrinsic =
                        ctx.module.get_function("llvm.floor.f64").unwrap_or_else(|| {
                            let float = ctx.ctx.f64_type();
                            let fn_type = float.fn_type(&[float.into()], false);
                            ctx.module.add_function("llvm.floor.f64", fn_type, None)
                        });
                    let val = ctx
                        .builder
                        .build_call(floor_intrinsic, &[arg.into()], "floor")
                        .try_as_basic_value()
                        .left()
                        .unwrap();
                    Ok(Some(
                        ctx.builder
                            .build_float_to_signed_int(
                                val.into_float_value(),
                                ctx.ctx.i32_type(),
                                "fptosi",
                            )
                            .into(),
                    ))
                },
            )))),
            loc: None,
        })),
        Arc::new(RwLock::new(TopLevelDef::Function {
            name: "floor64".into(),
            simple_name: "floor64".into(),
            signature: primitives.1.add_ty(TypeEnum::TFunc(FunSignature {
                args: vec![FuncArg { name: "n".into(), ty: float, default_value: None }],
                ret: int64,
                vars: Default::default(),
            })),
            var_id: Default::default(),
            instance_to_symbol: Default::default(),
            instance_to_stmt: Default::default(),
            resolver: None,
            codegen_callback: Some(Arc::new(GenCall::new(Box::new(
                |ctx, _, _, args, generator| {
                    let arg = args[0].1.clone().to_basic_value_enum(ctx, generator)?;
                    let floor_intrinsic =
                        ctx.module.get_function("llvm.floor.f64").unwrap_or_else(|| {
                            let float = ctx.ctx.f64_type();
                            let fn_type = float.fn_type(&[float.into()], false);
                            ctx.module.add_function("llvm.floor.f64", fn_type, None)
                        });
                    let val = ctx
                        .builder
                        .build_call(floor_intrinsic, &[arg.into()], "floor")
                        .try_as_basic_value()
                        .left()
                        .unwrap();
                    Ok(Some(
                        ctx.builder
                            .build_float_to_signed_int(
                                val.into_float_value(),
                                ctx.ctx.i64_type(),
                                "fptosi",
                            )
                            .into(),
                    ))
                },
            )))),
            loc: None,
        })),
        Arc::new(RwLock::new(TopLevelDef::Function {
            name: "ceil".into(),
            simple_name: "ceil".into(),
            signature: primitives.1.add_ty(TypeEnum::TFunc(FunSignature {
                args: vec![FuncArg { name: "n".into(), ty: float, default_value: None }],
                ret: int32,
                vars: Default::default(),
            })),
            var_id: Default::default(),
            instance_to_symbol: Default::default(),
            instance_to_stmt: Default::default(),
            resolver: None,
            codegen_callback: Some(Arc::new(GenCall::new(Box::new(
                |ctx, _, _, args, generator| {
                    let arg = args[0].1.clone().to_basic_value_enum(ctx, generator)?;
                    let ceil_intrinsic =
                        ctx.module.get_function("llvm.ceil.f64").unwrap_or_else(|| {
                            let float = ctx.ctx.f64_type();
                            let fn_type = float.fn_type(&[float.into()], false);
                            ctx.module.add_function("llvm.ceil.f64", fn_type, None)
                        });
                    let val = ctx
                        .builder
                        .build_call(ceil_intrinsic, &[arg.into()], "ceil")
                        .try_as_basic_value()
                        .left()
                        .unwrap();
                    Ok(Some(
                        ctx.builder
                            .build_float_to_signed_int(
                                val.into_float_value(),
                                ctx.ctx.i32_type(),
                                "fptosi",
                            )
                            .into(),
                    ))
                },
            )))),
            loc: None,
        })),
        Arc::new(RwLock::new(TopLevelDef::Function {
            name: "ceil64".into(),
            simple_name: "ceil64".into(),
            signature: primitives.1.add_ty(TypeEnum::TFunc(FunSignature {
                args: vec![FuncArg { name: "n".into(), ty: float, default_value: None }],
                ret: int64,
                vars: Default::default(),
            })),
            var_id: Default::default(),
            instance_to_symbol: Default::default(),
            instance_to_stmt: Default::default(),
            resolver: None,
            codegen_callback: Some(Arc::new(GenCall::new(Box::new(
                |ctx, _, _, args, generator| {
                    let arg = args[0].1.clone().to_basic_value_enum(ctx, generator)?;
                    let ceil_intrinsic =
                        ctx.module.get_function("llvm.ceil.f64").unwrap_or_else(|| {
                            let float = ctx.ctx.f64_type();
                            let fn_type = float.fn_type(&[float.into()], false);
                            ctx.module.add_function("llvm.ceil.f64", fn_type, None)
                        });
                    let val = ctx
                        .builder
                        .build_call(ceil_intrinsic, &[arg.into()], "ceil")
                        .try_as_basic_value()
                        .left()
                        .unwrap();
                    Ok(Some(
                        ctx.builder
                            .build_float_to_signed_int(
                                val.into_float_value(),
                                ctx.ctx.i64_type(),
                                "fptosi",
                            )
                            .into(),
                    ))
                },
            )))),
            loc: None,
        })),
        Arc::new(RwLock::new({
            let list_var = primitives.1.get_fresh_var(Some("L".into()), None);
            let list = primitives.1.add_ty(TypeEnum::TList { ty: list_var.0 });
            let arg_ty = primitives.1.get_fresh_var_with_range(
                &[list, primitives.0.range],
                Some("I".into()),
                None,
            );
            TopLevelDef::Function {
                name: "len".into(),
                simple_name: "len".into(),
                signature: primitives.1.add_ty(TypeEnum::TFunc(FunSignature {
                    args: vec![FuncArg { name: "ls".into(), ty: arg_ty.0, default_value: None }],
                    ret: int32,
                    vars: vec![(list_var.1, list_var.0), (arg_ty.1, arg_ty.0)]
                        .into_iter()
                        .collect(),
                })),
                var_id: vec![arg_ty.1],
                instance_to_symbol: Default::default(),
                instance_to_stmt: Default::default(),
                resolver: None,
                codegen_callback: Some(Arc::new(GenCall::new(Box::new(
                    |ctx, _, fun, args, generator| {
                        let range_ty = ctx.primitives.range;
                        let arg_ty = fun.0.args[0].ty;
                        let arg = args[0].1.clone().to_basic_value_enum(ctx, generator)?;
                        Ok(if ctx.unifier.unioned(arg_ty, range_ty) {
                            let arg = arg.into_pointer_value();
                            let (start, end, step) = destructure_range(ctx, arg);
                            Some(calculate_len_for_slice_range(ctx, start, end, step).into())
                        } else {
                            let int32 = ctx.ctx.i32_type();
                            let zero = int32.const_zero();
                            let len = ctx
                                .build_gep_and_load(
                                    arg.into_pointer_value(),
                                    &[zero, int32.const_int(1, false)],
                                )
                                .into_int_value();
                            if len.get_type().get_bit_width() != 32 {
                                Some(ctx.builder.build_int_truncate(len, int32, "len2i32").into())
                            } else {
                                Some(len.into())
                            }
                        })
                    },
                )))),
                loc: None,
            }
        })),
        Arc::new(RwLock::new(TopLevelDef::Function {
            name: "min".into(),
            simple_name: "min".into(),
            signature: primitives.1.add_ty(TypeEnum::TFunc(FunSignature {
                args: vec![
                    FuncArg { name: "m".into(), ty: num_ty.0, default_value: None },
                    FuncArg { name: "n".into(), ty: num_ty.0, default_value: None },
                ],
                ret: num_ty.0,
                vars: var_map.clone(),
            })),
            var_id: Default::default(),
            instance_to_symbol: Default::default(),
            instance_to_stmt: Default::default(),
            resolver: None,
            codegen_callback: Some(Arc::new(GenCall::new(Box::new(
                |ctx, _, fun, args, generator| {
                    let boolean = ctx.primitives.bool;
                    let int32 = ctx.primitives.int32;
                    let int64 = ctx.primitives.int64;
                    let uint32 = ctx.primitives.uint32;
                    let uint64 = ctx.primitives.uint64;
                    let float = ctx.primitives.float;
                    let llvm_i1 = ctx.ctx.bool_type().as_basic_type_enum();
                    let llvm_i32 = ctx.ctx.i32_type().as_basic_type_enum();
                    let llvm_i64 = ctx.ctx.i64_type().as_basic_type_enum();
                    let llvm_f64 = ctx.ctx.f64_type().as_basic_type_enum();
                    let m_ty = fun.0.args[0].ty;
                    let n_ty = fun.0.args[1].ty;
                    let m_val = args[0].1.clone().to_basic_value_enum(ctx, generator)?;
                    let n_val = args[1].1.clone().to_basic_value_enum(ctx, generator)?;
                    let mut is_type = |a: Type, b: Type| ctx.unifier.unioned(a, b);
                    let (fun_name, arg_ty) = if is_type(m_ty, n_ty) && is_type(n_ty, boolean) {
                        ("llvm.umin.i1", llvm_i1)
                    } else if is_type(m_ty, n_ty) && is_type(n_ty, int32) {
                        ("llvm.smin.i32", llvm_i32)
                    } else if is_type(m_ty, n_ty) && is_type(n_ty, int64) {
                        ("llvm.smin.i64", llvm_i64)
                    } else if is_type(m_ty, n_ty) && is_type(n_ty, uint32) {
                        ("llvm.umin.i32", llvm_i32)
                    } else if is_type(m_ty, n_ty) && is_type(n_ty, uint64) {
                        ("llvm.umin.i64", llvm_i64)
                    } else if is_type(m_ty, n_ty) && is_type(n_ty, float) {
                        ("llvm.minnum.f64", llvm_f64)
                    } else {
                        unreachable!();
                    };
                    let intrinsic = ctx.module.get_function(fun_name).unwrap_or_else(|| {
                        let fn_type = arg_ty.fn_type(&[arg_ty.into(), arg_ty.into()], false);
                        ctx.module.add_function(fun_name, fn_type, None)
                    });
                    let val = ctx
                        .builder
                        .build_call(intrinsic, &[m_val.into(), n_val.into()], "min")
                        .try_as_basic_value()
                        .left()
                        .unwrap();
                    Ok(val.into())
                },
            )))),
            loc: None,
        })),
        Arc::new(RwLock::new(TopLevelDef::Function {
            name: "max".into(),
            simple_name: "max".into(),
            signature: primitives.1.add_ty(TypeEnum::TFunc(FunSignature {
                args: vec![
                    FuncArg { name: "m".into(), ty: num_ty.0, default_value: None },
                    FuncArg { name: "n".into(), ty: num_ty.0, default_value: None },
                ],
                ret: num_ty.0,
                vars: var_map.clone(),
            })),
            var_id: Default::default(),
            instance_to_symbol: Default::default(),
            instance_to_stmt: Default::default(),
            resolver: None,
            codegen_callback: Some(Arc::new(GenCall::new(Box::new(
                |ctx, _, fun, args, generator| {
                    let boolean = ctx.primitives.bool;
                    let int32 = ctx.primitives.int32;
                    let int64 = ctx.primitives.int64;
                    let uint32 = ctx.primitives.uint32;
                    let uint64 = ctx.primitives.uint64;
                    let float = ctx.primitives.float;
                    let llvm_i1 = ctx.ctx.bool_type().as_basic_type_enum();
                    let llvm_i32 = ctx.ctx.i32_type().as_basic_type_enum();
                    let llvm_i64 = ctx.ctx.i64_type().as_basic_type_enum();
                    let llvm_f64 = ctx.ctx.f64_type().as_basic_type_enum();
                    let m_ty = fun.0.args[0].ty;
                    let n_ty = fun.0.args[1].ty;
                    let m_val = args[0].1.clone().to_basic_value_enum(ctx, generator)?;
                    let n_val = args[1].1.clone().to_basic_value_enum(ctx, generator)?;
                    let mut is_type = |a: Type, b: Type| ctx.unifier.unioned(a, b);
                    let (fun_name, arg_ty) = if is_type(m_ty, n_ty) && is_type(n_ty, boolean) {
                        ("llvm.umax.i1", llvm_i1)
                    } else if is_type(m_ty, n_ty) && is_type(n_ty, int32) {
                        ("llvm.smax.i32", llvm_i32)
                    } else if is_type(m_ty, n_ty) && is_type(n_ty, int64) {
                        ("llvm.smax.i64", llvm_i64)
                    } else if is_type(m_ty, n_ty) && is_type(n_ty, uint32) {
                        ("llvm.umax.i32", llvm_i32)
                    } else if is_type(m_ty, n_ty) && is_type(n_ty, uint64) {
                        ("llvm.umax.i64", llvm_i64)
                    } else if is_type(m_ty, n_ty) && is_type(n_ty, float) {
                        ("llvm.maxnum.f64", llvm_f64)
                    } else {
                        unreachable!();
                    };
                    let intrinsic = ctx.module.get_function(fun_name).unwrap_or_else(|| {
                        let fn_type = arg_ty.fn_type(&[arg_ty.into(), arg_ty.into()], false);
                        ctx.module.add_function(fun_name, fn_type, None)
                    });
                    let val = ctx
                        .builder
                        .build_call(intrinsic, &[m_val.into(), n_val.into()], "max")
                        .try_as_basic_value()
                        .left()
                        .unwrap();
                    Ok(val.into())
                },
            )))),
            loc: None,
        })),
        Arc::new(RwLock::new(TopLevelDef::Function {
            name: "abs".into(),
            simple_name: "abs".into(),
            signature: primitives.1.add_ty(TypeEnum::TFunc(FunSignature {
                args: vec![FuncArg { name: "n".into(), ty: num_ty.0, default_value: None }],
                ret: num_ty.0,
                vars: var_map,
            })),
            var_id: Default::default(),
            instance_to_symbol: Default::default(),
            instance_to_stmt: Default::default(),
            resolver: None,
            codegen_callback: Some(Arc::new(GenCall::new(Box::new(
                |ctx, _, fun, args, generator| {
                    let boolean = ctx.primitives.bool;
                    let int32 = ctx.primitives.int32;
                    let int64 = ctx.primitives.int64;
                    let uint32 = ctx.primitives.uint32;
                    let uint64 = ctx.primitives.uint64;
                    let float = ctx.primitives.float;
                    let llvm_i1 = ctx.ctx.bool_type();
                    let llvm_i32 = ctx.ctx.i32_type().as_basic_type_enum();
                    let llvm_i64 = ctx.ctx.i64_type().as_basic_type_enum();
                    let llvm_f64 = ctx.ctx.f64_type().as_basic_type_enum();
                    let n_ty = fun.0.args[0].ty;
                    let n_val = args[0].1.clone().to_basic_value_enum(ctx, generator)?;
                    let mut is_type = |a: Type, b: Type| ctx.unifier.unioned(a, b);
                    let mut is_float = false;
                    let (fun_name, arg_ty) =
                        if is_type(n_ty, boolean) || is_type(n_ty, uint32) || is_type(n_ty, uint64)
                        {
                            return Ok(n_val.into());
                        } else if is_type(n_ty, int32) {
                            ("llvm.abs.i32", llvm_i32)
                        } else if is_type(n_ty, int64) {
                            ("llvm.abs.i64", llvm_i64)
                        } else if is_type(n_ty, float) {
                            is_float = true;
                            ("llvm.fabs.f64", llvm_f64)
                        } else {
                            unreachable!();
                        };
                    let intrinsic = ctx.module.get_function(fun_name).unwrap_or_else(|| {
                        let fn_type = if is_float {
                            arg_ty.fn_type(&[arg_ty.into()], false)
                        } else {
                            arg_ty.fn_type(&[arg_ty.into(), llvm_i1.into()], false)
                        };
                        ctx.module.add_function(fun_name, fn_type, None)
                    });
                    let val = ctx
                        .builder
                        .build_call(
                            intrinsic,
                            &if is_float {
                                vec![n_val.into()]
                            } else {
                                vec![n_val.into(), llvm_i1.const_int(0, false).into()]
                            },
                            "abs",
                        )
                        .try_as_basic_value()
                        .left()
                        .unwrap();
                    Ok(val.into())
                },
            )))),
            loc: None,
        })),
        Arc::new(RwLock::new(TopLevelDef::Function {
            name: "Some".into(),
            simple_name: "Some".into(),
            signature: primitives.1.add_ty(TypeEnum::TFunc(FunSignature {
                args: vec![FuncArg { name: "n".into(), ty: option_ty_var, default_value: None }],
                ret: primitives.0.option,
                vars: HashMap::from([(option_ty_var_id, option_ty_var)]),
            })),
            var_id: vec![option_ty_var_id],
            instance_to_symbol: Default::default(),
            instance_to_stmt: Default::default(),
            resolver: None,
            codegen_callback: Some(Arc::new(GenCall::new(Box::new(
                |ctx, _, _fun, args, generator| {
                    let arg_val = args[0].1.clone().to_basic_value_enum(ctx, generator)?;
                    let alloca = ctx.builder.build_alloca(arg_val.get_type(), "alloca_some");
                    ctx.builder.build_store(alloca, arg_val);
                    Ok(Some(alloca.into()))
                },
            )))),
            loc: None,
        })),
    ];

    let ast_list: Vec<Option<ast::Stmt<()>>> =
        (0..top_level_def_list.len()).map(|_| None).collect();
    (
        izip!(top_level_def_list, ast_list).collect_vec(),
        &[
            "int32",
            "int64",
            "uint32",
            "uint64",
            "float",
            "round",
            "round64",
            "range",
            "str",
            "bool",
            "floor",
            "floor64",
            "ceil",
            "ceil64",
            "len",
            "min",
            "max",
            "abs",
            "Some",
        ],
    )
}
