use super::CodeGenContext;
use inkwell::{
    attributes::AttributeLoc, context::Context, memory_buffer::MemoryBuffer, module::Module,
    values::IntValue,
};

pub fn load_irrt(ctx: &Context) -> Module {
    let bitcode_buf = MemoryBuffer::create_from_memory_range(
        include_bytes!(concat!(env!("OUT_DIR"), "/irrt.bc")),
        "irrt_bitcode_buffer",
    );
    let irrt_mod = Module::parse_bitcode_from_buffer(&bitcode_buf, ctx).unwrap();
    // add alwaysinline attributes to power function to help them get inlined
    // alwaysinline enum = 1, see release/13.x/llvm/include/llvm/IR/Attributes.td
    for symbol in &["__nac3_irrt_int_exp_int32_t", "__nac3_irrt_int_exp_int64_t"] {
        let function = irrt_mod.get_function(symbol).unwrap();
        function.add_attribute(AttributeLoc::Function, ctx.create_enum_attribute(1, 0));
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
        (32, 32) => "__nac3_irrt_int_exp_int32_t",
        (64, 64) => "__nac3_irrt_int_exp_int64_t",
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
