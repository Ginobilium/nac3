use std::cell::RefCell;
use inkwell::{IntPredicate::{self, *}, FloatPredicate, values::IntValue};
use crate::{symbol_resolver::SymbolValue, codegen::expr::destructure_range};
use super::*;

type BuiltinInfo = (
    Vec<(Arc<RwLock<TopLevelDef>>, Option<Stmt>)>,
    &'static [&'static str]
);

pub fn get_builtins(primitives: &mut (PrimitiveStore, Unifier)) -> BuiltinInfo {
    let int32 = primitives.0.int32;
    let int64 = primitives.0.int64;
    let float = primitives.0.float;
    let boolean = primitives.0.bool;
    let range = primitives.0.range;
    let string = primitives.0.str;
    let num_ty = primitives.1.get_fresh_var_with_range(&[int32, int64, float, boolean]);
    let var_map: HashMap<_, _> = vec![(num_ty.1, num_ty.0)].into_iter().collect();

    let top_level_def_list = vec![
        Arc::new(RwLock::new(TopLevelComposer::make_top_level_class_def(
            0,
            None,
            "int32".into(),
            None,
        ))),
        Arc::new(RwLock::new(TopLevelComposer::make_top_level_class_def(
            1,
            None,
            "int64".into(),
            None,
        ))),
        Arc::new(RwLock::new(TopLevelComposer::make_top_level_class_def(
            2,
            None,
            "float".into(),
            None,
        ))),
        Arc::new(RwLock::new(TopLevelComposer::make_top_level_class_def(3, None, "bool".into(), None))),
        Arc::new(RwLock::new(TopLevelComposer::make_top_level_class_def(4, None, "none".into(), None))),
        Arc::new(RwLock::new(TopLevelComposer::make_top_level_class_def(
            5,
            None,
            "range".into(),
            None,
        ))),
        Arc::new(RwLock::new(TopLevelComposer::make_top_level_class_def(6, None, "str".into(), None))),
        Arc::new(RwLock::new(TopLevelDef::Function {
            name: "int32".into(),
            simple_name: "int32".into(),
            signature: primitives.1.add_ty(TypeEnum::TFunc(RefCell::new(FunSignature {
                args: vec![FuncArg { name: "_".into(), ty: num_ty.0, default_value: None }],
                ret: int32,
                vars: var_map.clone(),
            }))),
            var_id: Default::default(),
            instance_to_symbol: Default::default(),
            instance_to_stmt: Default::default(),
            resolver: None,
            codegen_callback: Some(Arc::new(GenCall::new(Box::new(
                |ctx, _, fun, args| {
                    let int32 = ctx.primitives.int32;
                    let int64 = ctx.primitives.int64;
                    let float = ctx.primitives.float;
                    let boolean = ctx.primitives.bool;
                    let arg_ty = fun.0.args[0].ty;
                    let arg = args[0].1;
                    if ctx.unifier.unioned(arg_ty, boolean) {
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
                    }
                },
            )))),
        })),
        Arc::new(RwLock::new(TopLevelDef::Function {
            name: "int64".into(),
            simple_name: "int64".into(),
            signature: primitives.1.add_ty(TypeEnum::TFunc(RefCell::new(FunSignature {
                args: vec![FuncArg { name: "_".into(), ty: num_ty.0, default_value: None }],
                ret: int64,
                vars: var_map.clone(),
            }))),
            var_id: Default::default(),
            instance_to_symbol: Default::default(),
            instance_to_stmt: Default::default(),
            resolver: None,
            codegen_callback: Some(Arc::new(GenCall::new(Box::new(
                |ctx, _, fun, args| {
                    let int32 = ctx.primitives.int32;
                    let int64 = ctx.primitives.int64;
                    let float = ctx.primitives.float;
                    let boolean = ctx.primitives.bool;
                    let arg_ty = fun.0.args[0].ty;
                    let arg = args[0].1;
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
                    }
                },
            )))),
        })),
        Arc::new(RwLock::new(TopLevelDef::Function {
            name: "float".into(),
            simple_name: "float".into(),
            signature: primitives.1.add_ty(TypeEnum::TFunc(RefCell::new(FunSignature {
                args: vec![FuncArg { name: "_".into(), ty: num_ty.0, default_value: None }],
                ret: float,
                vars: var_map.clone(),
            }))),
            var_id: Default::default(),
            instance_to_symbol: Default::default(),
            instance_to_stmt: Default::default(),
            resolver: None,
            codegen_callback: Some(Arc::new(GenCall::new(Box::new(
                |ctx, _, fun, args| {
                    let int32 = ctx.primitives.int32;
                    let int64 = ctx.primitives.int64;
                    let boolean = ctx.primitives.bool;
                    let float = ctx.primitives.float;
                    let arg_ty = fun.0.args[0].ty;
                    let arg = args[0].1;
                    if ctx.unifier.unioned(arg_ty, boolean)
                        || ctx.unifier.unioned(arg_ty, int32)
                        || ctx.unifier.unioned(arg_ty, int64)
                    {
                        let arg = args[0].1.into_int_value();
                        let val = ctx
                            .builder
                            .build_signed_int_to_float(arg, ctx.ctx.f64_type(), "sitofp")
                            .into();
                        Some(val)
                    } else if ctx.unifier.unioned(arg_ty, float) {
                        Some(arg)
                    } else {
                        unreachable!()
                    }
                },
            )))),
        })),
        Arc::new(RwLock::new(TopLevelDef::Function {
            name: "round".into(),
            simple_name: "round".into(),
            signature: primitives.1.add_ty(TypeEnum::TFunc(RefCell::new(FunSignature {
                args: vec![FuncArg { name: "_".into(), ty: float, default_value: None }],
                ret: int32,
                vars: Default::default(),
            }))),
            var_id: Default::default(),
            instance_to_symbol: Default::default(),
            instance_to_stmt: Default::default(),
            resolver: None,
            codegen_callback: Some(Arc::new(GenCall::new(Box::new(|ctx, _, _, args| {
                let arg = args[0].1;
                let round_intrinsic =
                    ctx.module.get_function("llvm.round.f64").unwrap_or_else(|| {
                        let float = ctx.ctx.f64_type();
                        let fn_type = float.fn_type(&[float.into()], false);
                        ctx.module.add_function("llvm.round.f64", fn_type, None)
                    });
                let val = ctx
                    .builder
                    .build_call(round_intrinsic, &[arg], "round")
                    .try_as_basic_value()
                    .left()
                    .unwrap();
                Some(
                    ctx.builder
                        .build_float_to_signed_int(
                            val.into_float_value(),
                            ctx.ctx.i32_type(),
                            "fptosi",
                        )
                        .into(),
                )
            })))),
        })),
        Arc::new(RwLock::new(TopLevelDef::Function {
            name: "round64".into(),
            simple_name: "round64".into(),
            signature: primitives.1.add_ty(TypeEnum::TFunc(RefCell::new(FunSignature {
                args: vec![FuncArg { name: "_".into(), ty: float, default_value: None }],
                ret: int64,
                vars: Default::default(),
            }))),
            var_id: Default::default(),
            instance_to_symbol: Default::default(),
            instance_to_stmt: Default::default(),
            resolver: None,
            codegen_callback: Some(Arc::new(GenCall::new(Box::new(|ctx, _, _, args| {
                let arg = args[0].1;
                let round_intrinsic =
                    ctx.module.get_function("llvm.round.f64").unwrap_or_else(|| {
                        let float = ctx.ctx.f64_type();
                        let fn_type = float.fn_type(&[float.into()], false);
                        ctx.module.add_function("llvm.round.f64", fn_type, None)
                    });
                let val = ctx
                    .builder
                    .build_call(round_intrinsic, &[arg], "round")
                    .try_as_basic_value()
                    .left()
                    .unwrap();
                Some(
                    ctx.builder
                        .build_float_to_signed_int(
                            val.into_float_value(),
                            ctx.ctx.i64_type(),
                            "fptosi",
                        )
                        .into(),
                )
            })))),
        })),
        Arc::new(RwLock::new(TopLevelDef::Function {
            name: "range".into(),
            simple_name: "range".into(),
            signature: primitives.1.add_ty(TypeEnum::TFunc(RefCell::new(FunSignature {
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
            }))),
            var_id: Default::default(),
            instance_to_symbol: Default::default(),
            instance_to_stmt: Default::default(),
            resolver: None,
            codegen_callback: Some(Arc::new(GenCall::new(Box::new(|ctx, _, _, args| {
                let mut start = None;
                let mut stop = None;
                let mut step = None;
                let int32 = ctx.ctx.i32_type();
                let zero = int32.const_zero();
                for (i, arg) in args.iter().enumerate() {
                    if arg.0 == Some("start".into()) {
                        start = Some(arg.1);
                    } else if arg.0 == Some("stop".into()) {
                        stop = Some(arg.1);
                    } else if arg.0 == Some("step".into()) {
                        step = Some(arg.1);
                    } else if i == 0 {
                        start = Some(arg.1);
                    } else if i == 1 {
                        stop = Some(arg.1);
                    } else if i == 2 {
                        step = Some(arg.1);
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
                Some(ptr.into())
            })))),
        })),
        Arc::new(RwLock::new(TopLevelDef::Function {
            name: "str".into(),
            simple_name: "str".into(),
            signature: primitives.1.add_ty(TypeEnum::TFunc(RefCell::new(FunSignature {
                args: vec![FuncArg { name: "_".into(), ty: string, default_value: None }],
                ret: string,
                vars: Default::default(),
            }))),
            var_id: Default::default(),
            instance_to_symbol: Default::default(),
            instance_to_stmt: Default::default(),
            resolver: None,
            codegen_callback: Some(Arc::new(GenCall::new(Box::new(|_, _, _, args| {
                Some(args[0].1)
            })))),
        })),
        Arc::new(RwLock::new(TopLevelDef::Function {
            name: "bool".into(),
            simple_name: "bool".into(),
            signature: primitives.1.add_ty(TypeEnum::TFunc(RefCell::new(FunSignature {
                args: vec![FuncArg { name: "_".into(), ty: num_ty.0, default_value: None }],
                ret: primitives.0.bool,
                vars: var_map,
            }))),
            var_id: Default::default(),
            instance_to_symbol: Default::default(),
            instance_to_stmt: Default::default(),
            resolver: None,
            codegen_callback: Some(Arc::new(GenCall::new(Box::new(
                |ctx, _, fun, args| {
                    let int32 = ctx.primitives.int32;
                    let int64 = ctx.primitives.int64;
                    let float = ctx.primitives.float;
                    let boolean = ctx.primitives.bool;
                    let arg_ty = fun.0.args[0].ty;
                    let arg = args[0].1;
                    if ctx.unifier.unioned(arg_ty, boolean) {
                        Some(arg)
                    } else if ctx.unifier.unioned(arg_ty, int32) {
                        Some(ctx.builder.build_int_compare(
                            IntPredicate::NE,
                            ctx.ctx.i32_type().const_zero(),
                            arg.into_int_value(),
                            "bool",
                        ).into())
                    } else if ctx.unifier.unioned(arg_ty, int64) {
                        Some(ctx.builder.build_int_compare(
                            IntPredicate::NE,
                            ctx.ctx.i64_type().const_zero(),
                            arg.into_int_value(),
                            "bool",
                        ).into())
                    } else if ctx.unifier.unioned(arg_ty, float) {
                        let val = ctx.builder.
                            build_float_compare(
                                // UEQ as bool(nan) is True
                                FloatPredicate::UEQ,
                                arg.into_float_value(),
                                ctx.ctx.f64_type().const_zero(),
                                "bool"
                            ).into();
                        Some(val)
                    } else {
                        unreachable!()
                    }
                },
            )))),
        })),
        Arc::new(RwLock::new(TopLevelDef::Function {
            name: "floor".into(),
            simple_name: "floor".into(),
            signature: primitives.1.add_ty(TypeEnum::TFunc(RefCell::new(FunSignature {
                args: vec![FuncArg { name: "_".into(), ty: float, default_value: None }],
                ret: int32,
                vars: Default::default(),
            }))),
            var_id: Default::default(),
            instance_to_symbol: Default::default(),
            instance_to_stmt: Default::default(),
            resolver: None,
            codegen_callback: Some(Arc::new(GenCall::new(Box::new(|ctx, _, _, args| {
                let arg = args[0].1;
                let floor_intrinsic =
                    ctx.module.get_function("llvm.floor.f64").unwrap_or_else(|| {
                        let float = ctx.ctx.f64_type();
                        let fn_type = float.fn_type(&[float.into()], false);
                        ctx.module.add_function("llvm.floor.f64", fn_type, None)
                    });
                let val = ctx
                    .builder
                    .build_call(floor_intrinsic, &[arg], "floor")
                    .try_as_basic_value()
                    .left()
                    .unwrap();
                Some(
                    ctx.builder
                        .build_float_to_signed_int(
                            val.into_float_value(),
                            ctx.ctx.i32_type(),
                            "fptosi",
                        )
                        .into(),
                )
            })))),
        })),
        Arc::new(RwLock::new(TopLevelDef::Function {
            name: "floor64".into(),
            simple_name: "floor64".into(),
            signature: primitives.1.add_ty(TypeEnum::TFunc(RefCell::new(FunSignature {
                args: vec![FuncArg { name: "_".into(), ty: float, default_value: None }],
                ret: int64,
                vars: Default::default(),
            }))),
            var_id: Default::default(),
            instance_to_symbol: Default::default(),
            instance_to_stmt: Default::default(),
            resolver: None,
            codegen_callback: Some(Arc::new(GenCall::new(Box::new(|ctx, _, _, args| {
                let arg = args[0].1;
                let floor_intrinsic =
                    ctx.module.get_function("llvm.floor.f64").unwrap_or_else(|| {
                        let float = ctx.ctx.f64_type();
                        let fn_type = float.fn_type(&[float.into()], false);
                        ctx.module.add_function("llvm.floor.f64", fn_type, None)
                    });
                let val = ctx
                    .builder
                    .build_call(floor_intrinsic, &[arg], "floor")
                    .try_as_basic_value()
                    .left()
                    .unwrap();
                Some(
                    ctx.builder
                        .build_float_to_signed_int(
                            val.into_float_value(),
                            ctx.ctx.i64_type(),
                            "fptosi",
                        )
                        .into(),
                )
            })))),
        })),
        Arc::new(RwLock::new(TopLevelDef::Function {
            name: "ceil".into(),
            simple_name: "ceil".into(),
            signature: primitives.1.add_ty(TypeEnum::TFunc(RefCell::new(FunSignature {
                args: vec![FuncArg { name: "_".into(), ty: float, default_value: None }],
                ret: int32,
                vars: Default::default(),
            }))),
            var_id: Default::default(),
            instance_to_symbol: Default::default(),
            instance_to_stmt: Default::default(),
            resolver: None,
            codegen_callback: Some(Arc::new(GenCall::new(Box::new(|ctx, _, _, args| {
                let arg = args[0].1;
                let ceil_intrinsic =
                    ctx.module.get_function("llvm.ceil.f64").unwrap_or_else(|| {
                        let float = ctx.ctx.f64_type();
                        let fn_type = float.fn_type(&[float.into()], false);
                        ctx.module.add_function("llvm.ceil.f64", fn_type, None)
                    });
                let val = ctx
                    .builder
                    .build_call(ceil_intrinsic, &[arg], "ceil")
                    .try_as_basic_value()
                    .left()
                    .unwrap();
                Some(
                    ctx.builder
                        .build_float_to_signed_int(
                            val.into_float_value(),
                            ctx.ctx.i32_type(),
                            "fptosi",
                        )
                        .into(),
                )
            })))),
        })),
        Arc::new(RwLock::new(TopLevelDef::Function {
            name: "ceil64".into(),
            simple_name: "ceil64".into(),
            signature: primitives.1.add_ty(TypeEnum::TFunc(RefCell::new(FunSignature {
                args: vec![FuncArg { name: "_".into(), ty: float, default_value: None }],
                ret: int64,
                vars: Default::default(),
            }))),
            var_id: Default::default(),
            instance_to_symbol: Default::default(),
            instance_to_stmt: Default::default(),
            resolver: None,
            codegen_callback: Some(Arc::new(GenCall::new(Box::new(|ctx, _, _, args| {
                let arg = args[0].1;
                let ceil_intrinsic =
                    ctx.module.get_function("llvm.ceil.f64").unwrap_or_else(|| {
                        let float = ctx.ctx.f64_type();
                        let fn_type = float.fn_type(&[float.into()], false);
                        ctx.module.add_function("llvm.ceil.f64", fn_type, None)
                    });
                let val = ctx
                    .builder
                    .build_call(ceil_intrinsic, &[arg], "ceil")
                    .try_as_basic_value()
                    .left()
                    .unwrap();
                Some(
                    ctx.builder
                        .build_float_to_signed_int(
                            val.into_float_value(),
                            ctx.ctx.i64_type(),
                            "fptosi",
                        )
                        .into(),
                )
            })))),
        })),
        Arc::new(RwLock::new({
            let list_var = primitives.1.get_fresh_var();
            let list = primitives.1.add_ty(TypeEnum::TList { ty: list_var.0 });
            let arg_ty = primitives.1.get_fresh_var_with_range(&[list, primitives.0.range]);
            TopLevelDef::Function {
                name: "len".into(),
                simple_name: "len".into(),
                signature: primitives.1.add_ty(TypeEnum::TFunc(RefCell::new(FunSignature {
                    args: vec![FuncArg {
                        name: "_".into(),
                        ty: arg_ty.0,
                        default_value: None
                    }],
                    ret: int32,
                    vars: vec![(list_var.1, list_var.0), (arg_ty.1, arg_ty.0)].into_iter().collect(),
                }))),
                var_id: vec![arg_ty.1],
                instance_to_symbol: Default::default(),
                instance_to_stmt: Default::default(),
                resolver: None,
                codegen_callback: Some(Arc::new(GenCall::new(Box::new(
                    |ctx, _, fun, args| {
                        let range_ty = ctx.primitives.range;
                        let arg_ty = fun.0.args[0].ty;
                        let arg = args[0].1;
                        if ctx.unifier.unioned(arg_ty, range_ty) {
                            let arg = arg.into_pointer_value();
                            let (start, end, step) = destructure_range(ctx, arg);
                            Some(calculate_len_for_slice_range(ctx, start, end, step).into())
                        } else {
                            let int32 = ctx.ctx.i32_type();
                            let zero = int32.const_zero();
                            Some(ctx.build_gep_and_load(arg.into_pointer_value(), &[zero, zero]))
                        }
                    },
                )))),
            }
        }))
    ];
    let ast_list: Vec<Option<ast::Stmt<()>>> =
        (0..top_level_def_list.len()).map(|_| None).collect();
    (
        izip!(top_level_def_list, ast_list).collect_vec(),
        &[
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
        ]
    )
}

// equivalent code:
// def length(start, end, step != 0):
//     diff = end - start
//     if diff > 0 and step > 0:
//          return ((diff - 1) // step) + 1
//     elif diff < 0 and step < 0:
//          return ((diff + 1) // step) + 1 
//     else:
//          return 0
pub fn calculate_len_for_slice_range<'ctx, 'a>(
    ctx: &mut CodeGenContext<'ctx, 'a>,
    start: IntValue<'ctx>,
    end: IntValue<'ctx>,
    step: IntValue<'ctx>,
) -> IntValue<'ctx> {
    let int32 = ctx.ctx.i32_type();
    let start = ctx.builder.build_int_s_extend(start, int32, "start");
    let end = ctx.builder.build_int_s_extend(end, int32, "end");
    let step = ctx.builder.build_int_s_extend(step, int32, "step");
    let diff = ctx.builder.build_int_sub(end, start, "diff");
    
    let diff_pos = ctx.builder.build_int_compare(SGT, diff, int32.const_zero(), "diffpos");
    let step_pos = ctx.builder.build_int_compare(SGT, step, int32.const_zero(), "steppos");
    let test_1 = ctx.builder.build_and(diff_pos, step_pos, "bothpos");
    
    let current = ctx.builder.get_insert_block().unwrap().get_parent().unwrap();
    let then_bb = ctx.ctx.append_basic_block(current, "then");
    let else_bb = ctx.ctx.append_basic_block(current, "else");
    let then_bb_2 = ctx.ctx.append_basic_block(current, "then_2");
    let else_bb_2 = ctx.ctx.append_basic_block(current, "else_2");
    let cont_bb_2 = ctx.ctx.append_basic_block(current, "cont_2");
    let cont_bb = ctx.ctx.append_basic_block(current, "cont");
    ctx.builder.build_conditional_branch(test_1, then_bb, else_bb);
    
    ctx.builder.position_at_end(then_bb);
    let length_pos = {
        let diff_pos_min_1 = ctx.builder.build_int_sub(diff, int32.const_int(1, false), "diffminone");
        let length_pos = ctx.builder.build_int_signed_div(diff_pos_min_1, step, "div");
        ctx.builder.build_int_add(length_pos, int32.const_int(1, false), "add1")
    };
    ctx.builder.build_unconditional_branch(cont_bb);
    
    ctx.builder.position_at_end(else_bb);
    let phi_1 = {
        let diff_neg = ctx.builder.build_int_compare(SLT, diff, int32.const_zero(), "diffneg");
        let step_neg = ctx.builder.build_int_compare(SLT, step, int32.const_zero(), "stepneg");
        let test_2 = ctx.builder.build_and(diff_neg, step_neg, "bothneg");
        
        ctx.builder.build_conditional_branch(test_2, then_bb_2, else_bb_2);
        
        ctx.builder.position_at_end(then_bb_2);
        let length_neg = {
            let diff_neg_add_1 = ctx.builder.build_int_add(diff, int32.const_int(1, false), "diffminone");
            let length_neg = ctx.builder.build_int_signed_div(diff_neg_add_1, step, "div");
            ctx.builder.build_int_add(length_neg, int32.const_int(1, false), "add1")
        };
        ctx.builder.build_unconditional_branch(cont_bb_2);
        
        ctx.builder.position_at_end(else_bb_2);
        let length_zero = int32.const_zero();
        ctx.builder.build_unconditional_branch(cont_bb_2);
        
        ctx.builder.position_at_end(cont_bb_2);
        let phi_1 = ctx.builder.build_phi(int32, "lenphi1");
        phi_1.add_incoming(&[(&length_neg, then_bb_2), (&length_zero, else_bb_2)]);
        phi_1.as_basic_value().into_int_value()
    };
    ctx.builder.build_unconditional_branch(cont_bb);
    
    ctx.builder.position_at_end(cont_bb);
    let phi = ctx.builder.build_phi(int32, "lenphi");
    phi.add_incoming(&[(&length_pos, then_bb), (&phi_1, cont_bb_2)]);
    phi.as_basic_value().into_int_value()
}
