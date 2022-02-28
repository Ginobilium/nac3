use super::*;
use crate::{
    codegen::{
        expr::destructure_range, irrt::calculate_len_for_slice_range, stmt::exn_constructor,
    },
    symbol_resolver::SymbolValue,
};
use inkwell::{FloatPredicate, IntPredicate};

type BuiltinInfo = (Vec<(Arc<RwLock<TopLevelDef>>, Option<Stmt>)>, &'static [&'static str]);

pub fn get_builtins(primitives: &mut (PrimitiveStore, Unifier)) -> BuiltinInfo {
    let int32 = primitives.0.int32;
    let int64 = primitives.0.int64;
    let float = primitives.0.float;
    let boolean = primitives.0.bool;
    let range = primitives.0.range;
    let string = primitives.0.str;
    let num_ty = primitives.1.get_fresh_var_with_range(
        &[int32, int64, float, boolean],
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
    let div_by_zero = primitives.1.add_ty(TypeEnum::TObj {
        obj_id: DefinitionId(10),
        fields: exception_fields.iter().map(|(a, b, c)| (*a, (*b, *c))).collect(),
        params: Default::default(),
    });
    let index_error = primitives.1.add_ty(TypeEnum::TObj {
        obj_id: DefinitionId(11),
        fields: exception_fields.iter().map(|(a, b, c)| (*a, (*b, *c))).collect(),
        params: Default::default(),
    });
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
    let div_by_zero_signature = primitives.1.add_ty(TypeEnum::TFunc(FunSignature {
        args: exn_cons_args.clone(),
        ret: div_by_zero,
        vars: Default::default(),
    }));
    let index_error_signature = primitives.1.add_ty(TypeEnum::TFunc(FunSignature {
        args: exn_cons_args,
        ret: index_error,
        vars: Default::default(),
    }));
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
            fields: exception_fields.clone(),
            methods: Default::default(),
            ancestors: vec![],
            constructor: None,
            resolver: None,
            loc: None,
        })),
        Arc::new(RwLock::new(TopLevelDef::Function {
            name: "ZeroDivisionError.__init__".into(),
            simple_name: "__init__".into(),
            signature: div_by_zero_signature,
            var_id: Default::default(),
            instance_to_symbol: Default::default(),
            instance_to_stmt: Default::default(),
            resolver: None,
            codegen_callback: Some(Arc::new(GenCall::new(Box::new(exn_constructor)))),
            loc: None,
        })),
        Arc::new(RwLock::new(TopLevelDef::Function {
            name: "IndexError.__init__".into(),
            simple_name: "__init__".into(),
            signature: index_error_signature,
            var_id: Default::default(),
            instance_to_symbol: Default::default(),
            instance_to_stmt: Default::default(),
            resolver: None,
            codegen_callback: Some(Arc::new(GenCall::new(Box::new(exn_constructor)))),
            loc: None,
        })),
        Arc::new(RwLock::new(TopLevelDef::Class {
            name: "ZeroDivisionError".into(),
            object_id: DefinitionId(10),
            type_vars: Default::default(),
            fields: exception_fields.clone(),
            methods: vec![("__init__".into(), div_by_zero_signature, DefinitionId(8))],
            ancestors: vec![
                TypeAnnotation::CustomClass { id: DefinitionId(10), params: Default::default() },
                TypeAnnotation::CustomClass { id: DefinitionId(7), params: Default::default() },
            ],
            constructor: Some(div_by_zero_signature),
            resolver: None,
            loc: None,
        })),
        Arc::new(RwLock::new(TopLevelDef::Class {
            name: "IndexError".into(),
            object_id: DefinitionId(11),
            type_vars: Default::default(),
            fields: exception_fields,
            methods: vec![("__init__".into(), index_error_signature, DefinitionId(9))],
            ancestors: vec![
                TypeAnnotation::CustomClass { id: DefinitionId(11), params: Default::default() },
                TypeAnnotation::CustomClass { id: DefinitionId(7), params: Default::default() },
            ],
            constructor: Some(index_error_signature),
            resolver: None,
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
                    } else if ctx.unifier.unioned(arg_ty, int32) {
                        Some(arg)
                    } else if ctx.unifier.unioned(arg_ty, int64) {
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
                    let float = ctx.primitives.float;
                    let boolean = ctx.primitives.bool;
                    let arg_ty = fun.0.args[0].ty;
                    let arg = args[0].1.clone().to_basic_value_enum(ctx, generator)?;
                    Ok(
                        if ctx.unifier.unioned(arg_ty, boolean)
                            || ctx.unifier.unioned(arg_ty, int32)
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
                        } else if ctx.unifier.unioned(arg_ty, int64) {
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
                vars: var_map,
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
    ];
    let ast_list: Vec<Option<ast::Stmt<()>>> =
        (0..top_level_def_list.len()).map(|_| None).collect();
    (
        izip!(top_level_def_list, ast_list).collect_vec(),
        &[
            "ZeroDivisionError",
            "IndexError",
            "int32",
            "int64",
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
        ],
    )
}
