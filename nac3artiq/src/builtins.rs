use inkwell::{values::BasicValueEnum, AddressSpace, AtomicOrdering};
use nac3core::{
    toplevel::GenCall,
    typecheck::{
        type_inferencer::PrimitiveStore,
        typedef::{FunSignature, FuncArg},
    },
};
use rustpython_parser::ast::StrRef;
use std::{collections::HashMap, sync::Arc};

// ARTIQ timeline control with now-pinning optimization.
pub fn timeline_builtins(primitive: &PrimitiveStore) -> Vec<(StrRef, FunSignature, Arc<GenCall>)> {
    vec![
        (
            "now_mu".into(),
            FunSignature {
                args: vec![],
                ret: primitive.int64,
                vars: HashMap::new(),
            },
            Arc::new(GenCall::new(Box::new(|ctx, _, _, _| {
                let i64_type = ctx.ctx.i64_type();
                let now = ctx
                    .module
                    .get_global("now")
                    .unwrap_or_else(|| ctx.module.add_global(i64_type, None, "now"));
                let now_raw = ctx.builder.build_load(now.as_pointer_value(), "now");
                if let BasicValueEnum::IntValue(now_raw) = now_raw {
                    let i64_32 = i64_type.const_int(32, false).into();
                    let now_lo = ctx.builder.build_left_shift(now_raw, i64_32, "now_shl");
                    let now_hi = ctx
                        .builder
                        .build_right_shift(now_raw, i64_32, false, "now_lshr")
                        .into();
                    Some(ctx.builder.build_or(now_lo, now_hi, "now_or").into())
                } else {
                    unreachable!()
                }
            }))),
        ),
        (
            "at_mu".into(),
            FunSignature {
                args: vec![FuncArg {
                    name: "t".into(),
                    ty: primitive.int64,
                    default_value: None,
                }],
                ret: primitive.none,
                vars: HashMap::new(),
            },
            Arc::new(GenCall::new(Box::new(|ctx, _, _, args| {
                let i32_type = ctx.ctx.i32_type();
                let i64_type = ctx.ctx.i64_type();
                let i64_32 = i64_type.const_int(32, false).into();
                if let BasicValueEnum::IntValue(time) = args[0].1 {
                    let time_hi = ctx.builder.build_int_truncate(
                        ctx.builder
                            .build_right_shift(time, i64_32, false, "now_lshr"),
                        i32_type,
                        "now_trunc",
                    );
                    let time_lo = ctx.builder.build_int_truncate(time, i32_type, "now_trunc");
                    let now = ctx
                        .module
                        .get_global("now")
                        .unwrap_or_else(|| ctx.module.add_global(i64_type, None, "now"));
                    let now_hiptr = ctx.builder.build_bitcast(
                        now,
                        i32_type.ptr_type(AddressSpace::Generic),
                        "now_bitcast",
                    );
                    if let BasicValueEnum::PointerValue(now_hiptr) = now_hiptr {
                        let now_loptr = unsafe {
                            ctx.builder.build_gep(
                                now_hiptr,
                                &[i32_type.const_int(1, false).into()],
                                "now_gep",
                            )
                        };
                        ctx.builder
                            .build_store(now_hiptr, time_hi)
                            .set_atomic_ordering(AtomicOrdering::SequentiallyConsistent)
                            .unwrap();
                        ctx.builder
                            .build_store(now_loptr, time_lo)
                            .set_atomic_ordering(AtomicOrdering::SequentiallyConsistent)
                            .unwrap();
                        None
                    } else {
                        unreachable!();
                    }
                } else {
                    unreachable!();
                }
            }))),
        ),
        (
            "delay_mu".into(),
            FunSignature {
                args: vec![FuncArg {
                    name: "dt".into(),
                    ty: primitive.int64,
                    default_value: None,
                }],
                ret: primitive.none,
                vars: HashMap::new(),
            },
            Arc::new(GenCall::new(Box::new(|ctx, _, _, args| {
                let i32_type = ctx.ctx.i32_type();
                let i64_type = ctx.ctx.i64_type();
                let i64_32 = i64_type.const_int(32, false).into();
                let now = ctx
                    .module
                    .get_global("now")
                    .unwrap_or_else(|| ctx.module.add_global(i64_type, None, "now"));
                let now_raw = ctx.builder.build_load(now.as_pointer_value(), "now");
                if let (BasicValueEnum::IntValue(now_raw), BasicValueEnum::IntValue(dt)) =
                    (now_raw, args[0].1)
                {
                    let now_lo = ctx.builder.build_left_shift(now_raw, i64_32, "now_shl");
                    let now_hi = ctx
                        .builder
                        .build_right_shift(now_raw, i64_32, false, "now_lshr")
                        .into();
                    let now_val = ctx.builder.build_or(now_lo, now_hi, "now_or");
                    let time = ctx.builder.build_int_add(now_val, dt, "now_add");
                    let time_hi = ctx.builder.build_int_truncate(
                        ctx.builder
                            .build_right_shift(time, i64_32, false, "now_lshr"),
                        i32_type,
                        "now_trunc",
                    );
                    let time_lo = ctx.builder.build_int_truncate(time, i32_type, "now_trunc");
                    let now_hiptr = ctx.builder.build_bitcast(
                        now,
                        i32_type.ptr_type(AddressSpace::Generic),
                        "now_bitcast",
                    );
                    if let BasicValueEnum::PointerValue(now_hiptr) = now_hiptr {
                        let now_loptr = unsafe {
                            ctx.builder.build_gep(
                                now_hiptr,
                                &[i32_type.const_int(1, false).into()],
                                "now_gep",
                            )
                        };
                        ctx.builder
                            .build_store(now_hiptr, time_hi)
                            .set_atomic_ordering(AtomicOrdering::SequentiallyConsistent)
                            .unwrap();
                        ctx.builder
                            .build_store(now_loptr, time_lo)
                            .set_atomic_ordering(AtomicOrdering::SequentiallyConsistent)
                            .unwrap();
                        None
                    } else {
                        unreachable!();
                    }
                } else {
                    unreachable!();
                }
            }))),
        ),
    ]
}
