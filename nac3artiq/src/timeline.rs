use nac3core::codegen::CodeGenContext;
use inkwell::{values::BasicValueEnum, AddressSpace, AtomicOrdering};

pub trait TimeFns {
    fn emit_now_mu<'ctx, 'a>(&self, ctx: &mut CodeGenContext<'ctx, 'a>) -> BasicValueEnum<'ctx>;
    fn emit_at_mu<'ctx, 'a>(&self, ctx: &mut CodeGenContext<'ctx, 'a>, t: BasicValueEnum<'ctx>);
    fn emit_delay_mu<'ctx, 'a>(&self, ctx: &mut CodeGenContext<'ctx, 'a>, dt: BasicValueEnum<'ctx>);
}

pub struct NowPinningTimeFns {}

impl TimeFns for NowPinningTimeFns {
    fn emit_now_mu<'ctx, 'a>(&self, ctx: &mut CodeGenContext<'ctx, 'a>) -> BasicValueEnum<'ctx> {
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
            ctx.builder.build_or(now_lo, now_hi, "now_or").into()
        } else {
            unreachable!();
        }
    }

    fn emit_at_mu<'ctx, 'a>(&self, ctx: &mut CodeGenContext<'ctx, 'a>, t: BasicValueEnum<'ctx>) {
        let i32_type = ctx.ctx.i32_type();
        let i64_type = ctx.ctx.i64_type();
        let i64_32 = i64_type.const_int(32, false).into();
        if let BasicValueEnum::IntValue(time) = t {
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
            } else {
                unreachable!();
            }
        } else {
            unreachable!();
        }
    }

    fn emit_delay_mu<'ctx, 'a>(&self, ctx: &mut CodeGenContext<'ctx, 'a>, dt: BasicValueEnum<'ctx>) {
        let i32_type = ctx.ctx.i32_type();
        let i64_type = ctx.ctx.i64_type();
        let i64_32 = i64_type.const_int(32, false).into();
        let now = ctx
            .module
            .get_global("now")
            .unwrap_or_else(|| ctx.module.add_global(i64_type, None, "now"));
        let now_raw = ctx.builder.build_load(now.as_pointer_value(), "now");
        if let (BasicValueEnum::IntValue(now_raw), BasicValueEnum::IntValue(dt)) =
            (now_raw, dt)
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
            } else {
                unreachable!();
            }
        } else {
            unreachable!();
        }
    }
}

pub static NOW_PINNING_TIME_FNS: NowPinningTimeFns = NowPinningTimeFns {};

pub struct ExternTimeFns {}

impl TimeFns for ExternTimeFns {
    fn emit_now_mu<'ctx, 'a>(&self, ctx: &mut CodeGenContext<'ctx, 'a>) -> BasicValueEnum<'ctx> {
        let now_mu = ctx
            .module
            .get_function("now_mu")
            .unwrap_or_else(|| ctx.module.add_function("now_mu", ctx.ctx.i64_type().fn_type(&[], false), None));
        ctx.builder
            .build_call(now_mu, &[], "now_mu")
            .try_as_basic_value()
            .left()
            .unwrap()
    }

    fn emit_at_mu<'ctx, 'a>(&self, ctx: &mut CodeGenContext<'ctx, 'a>, t: BasicValueEnum<'ctx>) {
        let at_mu = ctx
            .module
            .get_function("at_mu")
            .unwrap_or_else(|| ctx.module.add_function("at_mu", ctx.ctx.void_type().fn_type(&[ctx.ctx.i64_type().into()], false), None));
        ctx.builder
            .build_call(at_mu, &[t], "at_mu");
    }

    fn emit_delay_mu<'ctx, 'a>(&self, ctx: &mut CodeGenContext<'ctx, 'a>, dt: BasicValueEnum<'ctx>) {
        let delay_mu = ctx
            .module
            .get_function("delay_mu")
            .unwrap_or_else(|| ctx.module.add_function("delay_mu", ctx.ctx.void_type().fn_type(&[ctx.ctx.i64_type().into()], false), None));
        ctx.builder
            .build_call(delay_mu, &[dt], "delay_mu");
    }
}

pub static EXTERN_TIME_FNS: ExternTimeFns = ExternTimeFns {};
