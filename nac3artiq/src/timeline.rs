use inkwell::{values::BasicValueEnum, AddressSpace, AtomicOrdering};
use nac3core::codegen::CodeGenContext;

pub trait TimeFns {
    fn emit_now_mu<'ctx, 'a>(&self, ctx: &mut CodeGenContext<'ctx, 'a>) -> BasicValueEnum<'ctx>;
    fn emit_at_mu<'ctx, 'a>(&self, ctx: &mut CodeGenContext<'ctx, 'a>, t: BasicValueEnum<'ctx>);
    fn emit_delay_mu<'ctx, 'a>(&self, ctx: &mut CodeGenContext<'ctx, 'a>, dt: BasicValueEnum<'ctx>);
}

pub struct NowPinningTimeFns64 {}

// For FPGA design reasons, on VexRiscv with 64-bit data bus, the "now" CSR is split into two 32-bit
// values that are each padded to 64-bits.
impl TimeFns for NowPinningTimeFns64 {
    fn emit_now_mu<'ctx, 'a>(&self, ctx: &mut CodeGenContext<'ctx, 'a>) -> BasicValueEnum<'ctx> {
        let i64_type = ctx.ctx.i64_type();
        let i32_type = ctx.ctx.i32_type();
        let now = ctx
            .module
            .get_global("now")
            .unwrap_or_else(|| ctx.module.add_global(i64_type, None, "now"));
        let now_hiptr =
            ctx.builder.build_bitcast(now, i32_type.ptr_type(AddressSpace::Generic), "now_hiptr");
        if let BasicValueEnum::PointerValue(now_hiptr) = now_hiptr {
            let now_loptr = unsafe {
                ctx.builder.build_gep(now_hiptr, &[i32_type.const_int(2, false)], "now_gep")
            };
            if let (BasicValueEnum::IntValue(now_hi), BasicValueEnum::IntValue(now_lo)) = (
                ctx.builder.build_load(now_hiptr, "now_hi"),
                ctx.builder.build_load(now_loptr, "now_lo"),
            ) {
                let zext_hi = ctx.builder.build_int_z_extend(now_hi, i64_type, "now_zext_hi");
                let shifted_hi = ctx.builder.build_left_shift(
                    zext_hi,
                    i64_type.const_int(32, false),
                    "now_shifted_zext_hi",
                );
                let zext_lo = ctx.builder.build_int_z_extend(now_lo, i64_type, "now_zext_lo");
                ctx.builder.build_or(shifted_hi, zext_lo, "now_or").into()
            } else {
                unreachable!();
            }
        } else {
            unreachable!();
        }
    }

    fn emit_at_mu<'ctx, 'a>(&self, ctx: &mut CodeGenContext<'ctx, 'a>, t: BasicValueEnum<'ctx>) {
        let i32_type = ctx.ctx.i32_type();
        let i64_type = ctx.ctx.i64_type();
        let i64_32 = i64_type.const_int(32, false);
        if let BasicValueEnum::IntValue(time) = t {
            let time_hi = ctx.builder.build_int_truncate(
                ctx.builder.build_right_shift(time, i64_32, false, "now_lshr"),
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
                    ctx.builder.build_gep(now_hiptr, &[i32_type.const_int(2, false)], "now_gep")
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

    fn emit_delay_mu<'ctx, 'a>(
        &self,
        ctx: &mut CodeGenContext<'ctx, 'a>,
        dt: BasicValueEnum<'ctx>,
    ) {
        let i64_type = ctx.ctx.i64_type();
        let i32_type = ctx.ctx.i32_type();
        let now = ctx
            .module
            .get_global("now")
            .unwrap_or_else(|| ctx.module.add_global(i64_type, None, "now"));
        let now_hiptr =
            ctx.builder.build_bitcast(now, i32_type.ptr_type(AddressSpace::Generic), "now_hiptr");
        if let BasicValueEnum::PointerValue(now_hiptr) = now_hiptr {
            let now_loptr = unsafe {
                ctx.builder.build_gep(now_hiptr, &[i32_type.const_int(2, false)], "now_loptr")
            };
            if let (
                BasicValueEnum::IntValue(now_hi),
                BasicValueEnum::IntValue(now_lo),
                BasicValueEnum::IntValue(dt),
            ) = (
                ctx.builder.build_load(now_hiptr, "now_hi"),
                ctx.builder.build_load(now_loptr, "now_lo"),
                dt,
            ) {
                let zext_hi = ctx.builder.build_int_z_extend(now_hi, i64_type, "now_zext_hi");
                let shifted_hi = ctx.builder.build_left_shift(
                    zext_hi,
                    i64_type.const_int(32, false),
                    "now_shifted_zext_hi",
                );
                let zext_lo = ctx.builder.build_int_z_extend(now_lo, i64_type, "now_zext_lo");
                let now_val = ctx.builder.build_or(shifted_hi, zext_lo, "now_or");

                let time = ctx.builder.build_int_add(now_val, dt, "now_add");
                let time_hi = ctx.builder.build_int_truncate(
                    ctx.builder.build_right_shift(
                        time,
                        i64_type.const_int(32, false),
                        false,
                        "now_lshr",
                    ),
                    i32_type,
                    "now_trunc",
                );
                let time_lo = ctx.builder.build_int_truncate(time, i32_type, "now_trunc");

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
        };
    }
}

pub static NOW_PINNING_TIME_FNS_64: NowPinningTimeFns64 = NowPinningTimeFns64 {};

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
            let i64_32 = i64_type.const_int(32, false);
            let now_lo = ctx.builder.build_left_shift(now_raw, i64_32, "now_shl");
            let now_hi = ctx.builder.build_right_shift(now_raw, i64_32, false, "now_lshr");
            ctx.builder.build_or(now_lo, now_hi, "now_or").into()
        } else {
            unreachable!();
        }
    }

    fn emit_at_mu<'ctx, 'a>(&self, ctx: &mut CodeGenContext<'ctx, 'a>, t: BasicValueEnum<'ctx>) {
        let i32_type = ctx.ctx.i32_type();
        let i64_type = ctx.ctx.i64_type();
        let i64_32 = i64_type.const_int(32, false);
        if let BasicValueEnum::IntValue(time) = t {
            let time_hi = ctx.builder.build_int_truncate(
                ctx.builder.build_right_shift(time, i64_32, false, "now_lshr"),
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
                    ctx.builder.build_gep(now_hiptr, &[i32_type.const_int(1, false)], "now_gep")
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

    fn emit_delay_mu<'ctx, 'a>(
        &self,
        ctx: &mut CodeGenContext<'ctx, 'a>,
        dt: BasicValueEnum<'ctx>,
    ) {
        let i32_type = ctx.ctx.i32_type();
        let i64_type = ctx.ctx.i64_type();
        let i64_32 = i64_type.const_int(32, false);
        let now = ctx
            .module
            .get_global("now")
            .unwrap_or_else(|| ctx.module.add_global(i64_type, None, "now"));
        let now_raw = ctx.builder.build_load(now.as_pointer_value(), "now");
        if let (BasicValueEnum::IntValue(now_raw), BasicValueEnum::IntValue(dt)) = (now_raw, dt) {
            let now_lo = ctx.builder.build_left_shift(now_raw, i64_32, "now_shl");
            let now_hi = ctx.builder.build_right_shift(now_raw, i64_32, false, "now_lshr");
            let now_val = ctx.builder.build_or(now_lo, now_hi, "now_or");
            let time = ctx.builder.build_int_add(now_val, dt, "now_add");
            let time_hi = ctx.builder.build_int_truncate(
                ctx.builder.build_right_shift(time, i64_32, false, "now_lshr"),
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
                    ctx.builder.build_gep(now_hiptr, &[i32_type.const_int(1, false)], "now_gep")
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
        let now_mu = ctx.module.get_function("now_mu").unwrap_or_else(|| {
            ctx.module.add_function("now_mu", ctx.ctx.i64_type().fn_type(&[], false), None)
        });
        ctx.builder.build_call(now_mu, &[], "now_mu").try_as_basic_value().left().unwrap()
    }

    fn emit_at_mu<'ctx, 'a>(&self, ctx: &mut CodeGenContext<'ctx, 'a>, t: BasicValueEnum<'ctx>) {
        let at_mu = ctx.module.get_function("at_mu").unwrap_or_else(|| {
            ctx.module.add_function(
                "at_mu",
                ctx.ctx.void_type().fn_type(&[ctx.ctx.i64_type().into()], false),
                None,
            )
        });
        ctx.builder.build_call(at_mu, &[t.into()], "at_mu");
    }

    fn emit_delay_mu<'ctx, 'a>(
        &self,
        ctx: &mut CodeGenContext<'ctx, 'a>,
        dt: BasicValueEnum<'ctx>,
    ) {
        let delay_mu = ctx.module.get_function("delay_mu").unwrap_or_else(|| {
            ctx.module.add_function(
                "delay_mu",
                ctx.ctx.void_type().fn_type(&[ctx.ctx.i64_type().into()], false),
                None,
            )
        });
        ctx.builder.build_call(delay_mu, &[dt.into()], "delay_mu");
    }
}

pub static EXTERN_TIME_FNS: ExternTimeFns = ExternTimeFns {};
