use crate::typecheck::typedef::Type;

use super::{CodeGenContext, CodeGenerator};
use inkwell::{
    attributes::{Attribute, AttributeLoc},
    context::Context,
    memory_buffer::MemoryBuffer,
    module::Module,
    types::{BasicTypeEnum, IntType},
    values::{IntValue, PointerValue},
    AddressSpace, IntPredicate,
};
use nac3parser::ast::Expr;

pub fn load_irrt(ctx: &Context) -> Module {
    let bitcode_buf = MemoryBuffer::create_from_memory_range(
        include_bytes!(concat!(env!("OUT_DIR"), "/irrt.bc")),
        "irrt_bitcode_buffer",
    );
    let irrt_mod = Module::parse_bitcode_from_buffer(&bitcode_buf, ctx).unwrap();
    let inline_attr = Attribute::get_named_enum_kind_id("alwaysinline");
    for symbol in &[
        "__nac3_int_exp_int32_t",
        "__nac3_int_exp_int64_t",
        "__nac3_range_slice_len",
        "__nac3_slice_index_bound",
    ] {
        let function = irrt_mod.get_function(symbol).unwrap();
        function.add_attribute(AttributeLoc::Function, ctx.create_enum_attribute(inline_attr, 0));
    }
    irrt_mod
}

// repeated squaring method adapted from GNU Scientific Library:
// https://git.savannah.gnu.org/cgit/gsl.git/tree/sys/pow_int.c
pub fn integer_power<'ctx, 'a>(
    ctx: &mut CodeGenContext<'ctx, 'a>,
    base: IntValue<'ctx>,
    exp: IntValue<'ctx>,
) -> IntValue<'ctx> {
    let symbol = match (base.get_type().get_bit_width(), exp.get_type().get_bit_width()) {
        (32, 32) => "__nac3_int_exp_int32_t",
        (64, 64) => "__nac3_int_exp_int64_t",
        _ => unreachable!(),
    };
    let base_type = base.get_type();
    let pow_fun = ctx.module.get_function(symbol).unwrap_or_else(|| {
        let fn_type = base_type.fn_type(&[base_type.into(), base_type.into()], false);
        ctx.module.add_function(symbol, fn_type, None)
    });
    // TODO: throw exception when exp < 0
    ctx.builder
        .build_call(pow_fun, &[base.into(), exp.into()], "call_int_pow")
        .try_as_basic_value()
        .unwrap_left()
        .into_int_value()
}

pub fn calculate_len_for_slice_range<'ctx, 'a>(
    ctx: &mut CodeGenContext<'ctx, 'a>,
    start: IntValue<'ctx>,
    end: IntValue<'ctx>,
    step: IntValue<'ctx>,
) -> IntValue<'ctx> {
    const SYMBOL: &str = "__nac3_range_slice_len";
    let len_func = ctx.module.get_function(SYMBOL).unwrap_or_else(|| {
        let i32_t = ctx.ctx.i32_type();
        let fn_t = i32_t.fn_type(&[i32_t.into(), i32_t.into(), i32_t.into()], false);
        ctx.module.add_function(SYMBOL, fn_t, None)
    });

    // TODO: assert step != 0, throw exception if not
    ctx.builder
        .build_call(len_func, &[start.into(), end.into(), step.into()], "calc_len")
        .try_as_basic_value()
        .left()
        .unwrap()
        .into_int_value()
}

/// NOTE: the output value of the end index of this function should be compared ***inclusively***,
/// because python allows `a[2::-1]`, whose semantic is `[a[2], a[1], a[0]]`, which is equivalent to
/// NO numeric slice in python.
///
/// equivalent code:
/// ```pseudo_code
/// match (start, end, step):
///     case (s, e, None | Some(step)) if step > 0:
///         return (
///             match s:
///                 case None:
///                     0
///                 case Some(s):
///                     handle_in_bound(s)
///             ,match e:
///                 case None:
///                     length - 1
///                 case Some(e):
///                     handle_in_bound(e) - 1
///             ,step == None ? 1 : step
///         )
///     case (s, e, Some(step)) if step < 0:
///         return (
///             match s:
///                 case None:
///                     length - 1
///                 case Some(s):
///                     s = handle_in_bound(s)
///                     if s == length:
///                         s - 1
///                     else:
///                         s
///             ,match e:
///                 case None:
///                     0
///                 case Some(e):
///                     handle_in_bound(e) + 1
///             ,step
///         )
/// ```
pub fn handle_slice_indices<'a, 'ctx, G: CodeGenerator>(
    start: &Option<Box<Expr<Option<Type>>>>,
    end: &Option<Box<Expr<Option<Type>>>>,
    step: &Option<Box<Expr<Option<Type>>>>,
    ctx: &mut CodeGenContext<'ctx, 'a>,
    generator: &mut G,
    list: PointerValue<'ctx>,
) -> Result<(IntValue<'ctx>, IntValue<'ctx>, IntValue<'ctx>), String> {
    // TODO: throw exception when step is 0
    let int32 = ctx.ctx.i32_type();
    let zero = int32.const_zero();
    let one = int32.const_int(1, false);
    let length = ctx.build_gep_and_load(list, &[zero, one]).into_int_value();
    let length = ctx.builder.build_int_truncate_or_bit_cast(length, int32, "leni32");
    Ok(match (start, end, step) {
        (s, e, None) => (
            s.as_ref().map_or_else(
                || Ok(int32.const_zero()),
                |s| handle_slice_index_bound(s, ctx, generator, length),
            )?,
            {
                let e = e.as_ref().map_or_else(
                    || Ok(length),
                    |e| handle_slice_index_bound(e, ctx, generator, length),
                )?;
                ctx.builder.build_int_sub(e, one, "final_end")
            },
            one,
        ),
        (s, e, Some(step)) => {
            let step = generator
                .gen_expr(ctx, step)?
                .unwrap()
                .to_basic_value_enum(ctx, generator)
                .into_int_value();
            let len_id = ctx.builder.build_int_sub(length, one, "lenmin1");
            let neg = ctx.builder.build_int_compare(IntPredicate::SLT, step, zero, "step_is_neg");
            (
                match s {
                    Some(s) => {
                        let s = handle_slice_index_bound(s, ctx, generator, length)?;
                        ctx.builder
                            .build_select(
                                ctx.builder.build_and(
                                    ctx.builder.build_int_compare(
                                        IntPredicate::EQ,
                                        s,
                                        length,
                                        "s_eq_len",
                                    ),
                                    neg,
                                    "should_minus_one",
                                ),
                                ctx.builder.build_int_sub(s, one, "s_min"),
                                s,
                                "final_start",
                            )
                            .into_int_value()
                    }
                    None => ctx.builder.build_select(neg, len_id, zero, "stt").into_int_value(),
                },
                match e {
                    Some(e) => {
                        let e = handle_slice_index_bound(e, ctx, generator, length)?;
                        ctx.builder
                            .build_select(
                                neg,
                                ctx.builder.build_int_add(e, one, "end_add_one"),
                                ctx.builder.build_int_sub(e, one, "end_sub_one"),
                                "final_end",
                            )
                            .into_int_value()
                    }
                    None => ctx.builder.build_select(neg, zero, len_id, "end").into_int_value(),
                },
                step,
            )
        }
    })
}

/// this function allows index out of range, since python
/// allows index out of range in slice (`a = [1,2,3]; a[1:10] == [2,3]`).
pub fn handle_slice_index_bound<'a, 'ctx, G: CodeGenerator>(
    i: &Expr<Option<Type>>,
    ctx: &mut CodeGenContext<'ctx, 'a>,
    generator: &mut G,
    length: IntValue<'ctx>,
) -> Result<IntValue<'ctx>, String> {
    const SYMBOL: &str = "__nac3_slice_index_bound";
    let func = ctx.module.get_function(SYMBOL).unwrap_or_else(|| {
        let i32_t = ctx.ctx.i32_type();
        let fn_t = i32_t.fn_type(&[i32_t.into(), i32_t.into()], false);
        ctx.module.add_function(SYMBOL, fn_t, None)
    });

    let i = generator.gen_expr(ctx, i)?.unwrap().to_basic_value_enum(ctx, generator);
    Ok(ctx.builder
        .build_call(func, &[i.into(), length.into()], "bounded_ind")
        .try_as_basic_value()
        .left()
        .unwrap()
        .into_int_value())
}

/// This function handles 'end' **inclusively**.
/// Order of tuples assign_idx and value_idx is ('start', 'end', 'step').
/// Negative index should be handled before entering this function
pub fn list_slice_assignment<'ctx, 'a>(
    ctx: &mut CodeGenContext<'ctx, 'a>,
    size_ty: IntType<'ctx>,
    ty: BasicTypeEnum<'ctx>,
    dest_arr: PointerValue<'ctx>,
    dest_idx: (IntValue<'ctx>, IntValue<'ctx>, IntValue<'ctx>),
    src_arr: PointerValue<'ctx>,
    src_idx: (IntValue<'ctx>, IntValue<'ctx>, IntValue<'ctx>),
) {
    let int8_ptr = ctx.ctx.i8_type().ptr_type(AddressSpace::Generic);
    let int32 = ctx.ctx.i32_type();
    let (fun_symbol, elem_ptr_type) = ("__nac3_list_slice_assign_var_size", int8_ptr);
    let slice_assign_fun = {
        let ty_vec = vec![
            int32.into(),         // dest start idx
            int32.into(),         // dest end idx
            int32.into(),         // dest step
            elem_ptr_type.into(), // dest arr ptr
            int32.into(),         // dest arr len
            int32.into(),         // src start idx
            int32.into(),         // src end idx
            int32.into(),         // src step
            elem_ptr_type.into(), // src arr ptr
            int32.into(),         // src arr len
            int32.into(),         // size
        ];
        ctx.module.get_function(fun_symbol).unwrap_or_else(|| {
            let fn_t = int32.fn_type(ty_vec.as_slice(), false);
            ctx.module.add_function(fun_symbol, fn_t, None)
        })
    };

    let zero = int32.const_zero();
    let one = int32.const_int(1, false);
    let dest_arr_ptr = ctx.build_gep_and_load(dest_arr, &[zero, zero]);
    let dest_arr_ptr = ctx.builder.build_pointer_cast(
        dest_arr_ptr.into_pointer_value(),
        elem_ptr_type,
        "dest_arr_ptr_cast",
    );
    let dest_len = ctx.build_gep_and_load(dest_arr, &[zero, one]).into_int_value();
    let dest_len = ctx.builder.build_int_truncate_or_bit_cast(dest_len, int32, "srclen32");
    let src_arr_ptr = ctx.build_gep_and_load(src_arr, &[zero, zero]);
    let src_arr_ptr = ctx.builder.build_pointer_cast(
        src_arr_ptr.into_pointer_value(),
        elem_ptr_type,
        "src_arr_ptr_cast",
    );
    let src_len = ctx.build_gep_and_load(src_arr, &[zero, one]).into_int_value();
    let src_len = ctx.builder.build_int_truncate_or_bit_cast(src_len, int32, "srclen32");

    // index in bound and positive should be done
    // TODO: assert if dest.step == 1 then len(src) <= len(dest) else len(src) == len(dest), and
    // throw exception if not satisfied
    let new_len = {
        let args = vec![
            dest_idx.0.into(),   // dest start idx
            dest_idx.1.into(),   // dest end idx
            dest_idx.2.into(),   // dest step
            dest_arr_ptr.into(), // dest arr ptr
            dest_len.into(),     // dest arr len
            src_idx.0.into(),    // src start idx
            src_idx.1.into(),    // src end idx
            src_idx.2.into(),    // src step
            src_arr_ptr.into(),  // src arr ptr
            src_len.into(),      // src arr len
            {
                let s = match ty {
                    BasicTypeEnum::FloatType(t) => t.size_of(),
                    BasicTypeEnum::IntType(t) => t.size_of(),
                    BasicTypeEnum::PointerType(t) => t.size_of(),
                    BasicTypeEnum::StructType(t) => t.size_of().unwrap(),
                    _ => unreachable!(),
                };
                ctx.builder.build_int_truncate_or_bit_cast(s, int32, "size")
            }
            .into(),
        ];
        ctx.builder
            .build_call(slice_assign_fun, args.as_slice(), "slice_assign")
            .try_as_basic_value()
            .unwrap_left()
            .into_int_value()
    };
    // update length
    let need_update =
        ctx.builder.build_int_compare(IntPredicate::NE, new_len, dest_len, "need_update");
    let current = ctx.builder.get_insert_block().unwrap().get_parent().unwrap();
    let update_bb = ctx.ctx.append_basic_block(current, "update");
    let cont_bb = ctx.ctx.append_basic_block(current, "cont");
    ctx.builder.build_conditional_branch(need_update, update_bb, cont_bb);
    ctx.builder.position_at_end(update_bb);
    let dest_len_ptr = unsafe { ctx.builder.build_gep(dest_arr, &[zero, one], "dest_len_ptr") };
    let new_len = ctx.builder.build_int_z_extend_or_bit_cast(new_len, size_ty, "new_len");
    ctx.builder.build_store(dest_len_ptr, new_len);
    ctx.builder.build_unconditional_branch(cont_bb);
    ctx.builder.position_at_end(cont_bb);
}
