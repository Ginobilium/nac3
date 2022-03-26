use super::{
    super::symbol_resolver::ValueEnum,
    expr::destructure_range,
    irrt::{handle_slice_indices, list_slice_assignment},
    CodeGenContext, CodeGenerator,
};
use crate::{
    codegen::expr::gen_binop_expr,
    toplevel::{DefinitionId, TopLevelDef},
    typecheck::typedef::{FunSignature, Type, TypeEnum},
};
use inkwell::{
    attributes::{Attribute, AttributeLoc},
    basic_block::BasicBlock,
    types::BasicTypeEnum,
    values::{BasicValue, BasicValueEnum, FunctionValue, PointerValue},
    IntPredicate::EQ,
};
use nac3parser::ast::{
    Constant, ExcepthandlerKind, Expr, ExprKind, Location, Stmt, StmtKind, StrRef,
};
use std::convert::TryFrom;

pub fn gen_var<'ctx, 'a>(
    ctx: &mut CodeGenContext<'ctx, 'a>,
    ty: BasicTypeEnum<'ctx>,
) -> Result<PointerValue<'ctx>, String> {
    // put the alloca in init block
    let current = ctx.builder.get_insert_block().unwrap();
    // position before the last branching instruction...
    ctx.builder.position_before(&ctx.init_bb.get_last_instruction().unwrap());
    let ptr = ctx.builder.build_alloca(ty, "tmp");
    ctx.builder.position_at_end(current);
    Ok(ptr)
}

pub fn gen_store_target<'ctx, 'a, G: CodeGenerator>(
    generator: &mut G,
    ctx: &mut CodeGenContext<'ctx, 'a>,
    pattern: &Expr<Option<Type>>,
) -> Result<PointerValue<'ctx>, String> {
    // very similar to gen_expr, but we don't do an extra load at the end
    // and we flatten nested tuples
    Ok(match &pattern.node {
        ExprKind::Name { id, .. } => {
            ctx.var_assignment.get(id).map(|v| Ok(v.0) as Result<_, String>).unwrap_or_else(
                || {
                    let ptr_ty = ctx.get_llvm_type(generator, pattern.custom.unwrap());
                    let ptr = generator.gen_var_alloc(ctx, ptr_ty)?;
                    ctx.var_assignment.insert(*id, (ptr, None, 0));
                    Ok(ptr)
                },
            )?
        }
        ExprKind::Attribute { value, attr, .. } => {
            let index = ctx.get_attr_index(value.custom.unwrap(), *attr);
            let val = generator.gen_expr(ctx, value)?.unwrap().to_basic_value_enum(ctx, generator)?;
            let ptr = if let BasicValueEnum::PointerValue(v) = val {
                v
            } else {
                unreachable!();
            };
            unsafe {
                ctx.builder.build_in_bounds_gep(
                    ptr,
                    &[
                        ctx.ctx.i32_type().const_zero(),
                        ctx.ctx.i32_type().const_int(index as u64, false),
                    ],
                    "attr",
                )
            }
        }
        ExprKind::Subscript { value, slice, .. } => {
            let i32_type = ctx.ctx.i32_type();
            let v = generator
                .gen_expr(ctx, value)?
                .unwrap()
                .to_basic_value_enum(ctx, generator)?
                .into_pointer_value();
            let index = generator
                .gen_expr(ctx, slice)?
                .unwrap()
                .to_basic_value_enum(ctx, generator)?
                .into_int_value();
            unsafe {
                let arr_ptr = ctx
                    .build_gep_and_load(v, &[i32_type.const_zero(), i32_type.const_zero()])
                    .into_pointer_value();
                ctx.builder.build_gep(arr_ptr, &[index], "loadarrgep")
            }
        }
        _ => unreachable!(),
    })
}

pub fn gen_assign<'ctx, 'a, G: CodeGenerator>(
    generator: &mut G,
    ctx: &mut CodeGenContext<'ctx, 'a>,
    target: &Expr<Option<Type>>,
    value: ValueEnum<'ctx>,
) -> Result<(), String> {
    match &target.node {
        ExprKind::Tuple { elts, .. } => {
            if let BasicValueEnum::StructValue(v) = value.to_basic_value_enum(ctx, generator)? {
                for (i, elt) in elts.iter().enumerate() {
                    let v = ctx
                        .builder
                        .build_extract_value(v, u32::try_from(i).unwrap(), "struct_elem")
                        .unwrap();
                    generator.gen_assign(ctx, elt, v.into())?;
                }
            } else {
                unreachable!()
            }
        }
        ExprKind::Subscript { value: ls, slice, .. }
            if matches!(&slice.node, ExprKind::Slice { .. }) =>
        {
            if let ExprKind::Slice { lower, upper, step } = &slice.node {
                let ls = generator
                    .gen_expr(ctx, ls)?
                    .unwrap()
                    .to_basic_value_enum(ctx, generator)?
                    .into_pointer_value();
                let (start, end, step) =
                    handle_slice_indices(lower, upper, step, ctx, generator, ls)?;
                let value = value.to_basic_value_enum(ctx, generator)?.into_pointer_value();
                let ty =
                    if let TypeEnum::TList { ty } = &*ctx.unifier.get_ty(target.custom.unwrap()) {
                        ctx.get_llvm_type(generator, *ty)
                    } else {
                        unreachable!()
                    };
                let src_ind = handle_slice_indices(&None, &None, &None, ctx, generator, value)?;
                list_slice_assignment(
                    ctx,
                    generator.get_size_type(ctx.ctx),
                    ty,
                    ls,
                    (start, end, step),
                    value,
                    src_ind,
                )
            } else {
                unreachable!()
            }
        }
        _ => {
            let ptr = generator.gen_store_target(ctx, target)?;
            if let ExprKind::Name { id, .. } = &target.node {
                let (_, static_value, counter) = ctx.var_assignment.get_mut(id).unwrap();
                *counter += 1;
                if let ValueEnum::Static(s) = &value {
                    *static_value = Some(s.clone());
                }
            }
            let val = value.to_basic_value_enum(ctx, generator)?;
            ctx.builder.build_store(ptr, val);
        }
    };
    Ok(())
}

pub fn gen_for<'ctx, 'a, G: CodeGenerator>(
    generator: &mut G,
    ctx: &mut CodeGenContext<'ctx, 'a>,
    stmt: &Stmt<Option<Type>>,
) -> Result<(), String> {
    if let StmtKind::For { iter, target, body, orelse, .. } = &stmt.node {
        // var_assignment static values may be changed in another branch
        // if so, remove the static value as it may not be correct in this branch
        let var_assignment = ctx.var_assignment.clone();

        let int32 = ctx.ctx.i32_type();
        let size_t = generator.get_size_type(ctx.ctx);
        let zero = int32.const_zero();
        let current = ctx.builder.get_insert_block().unwrap().get_parent().unwrap();
        let test_bb = ctx.ctx.append_basic_block(current, "test");
        let body_bb = ctx.ctx.append_basic_block(current, "body");
        let cont_bb = ctx.ctx.append_basic_block(current, "cont");
        // if there is no orelse, we just go to cont_bb
        let orelse_bb =
            if orelse.is_empty() { cont_bb } else { ctx.ctx.append_basic_block(current, "orelse") };
        // store loop bb information and restore it later
        let loop_bb = ctx.loop_target.replace((test_bb, cont_bb));

        let iter_val = generator.gen_expr(ctx, iter)?.unwrap().to_basic_value_enum(ctx, generator)?;
        if ctx.unifier.unioned(iter.custom.unwrap(), ctx.primitives.range) {
            // setup
            let iter_val = iter_val.into_pointer_value();
            let i = generator.gen_var_alloc(ctx, int32.into())?;
            let user_i = generator.gen_store_target(ctx, target)?;
            let (start, end, step) = destructure_range(ctx, iter_val);
            ctx.builder.build_store(i, ctx.builder.build_int_sub(start, step, "start_init"));
            ctx.builder.build_unconditional_branch(test_bb);
            ctx.builder.position_at_end(test_bb);
            let sign = ctx.builder.build_int_compare(
                inkwell::IntPredicate::SGT,
                step,
                int32.const_zero(),
                "sign",
            );
            // add and test
            let tmp = ctx.builder.build_int_add(
                ctx.builder.build_load(i, "i").into_int_value(),
                step,
                "start_loop",
            );
            ctx.builder.build_store(i, tmp);
            ctx.builder.build_store(user_i, tmp);
            // // if step > 0, continue when i < end
            let cmp1 = ctx.builder.build_int_compare(inkwell::IntPredicate::SLT, tmp, end, "cmp1");
            // if step < 0, continue when i > end
            let cmp2 = ctx.builder.build_int_compare(inkwell::IntPredicate::SGT, tmp, end, "cmp2");
            let pos = ctx.builder.build_and(sign, cmp1, "pos");
            let neg = ctx.builder.build_and(ctx.builder.build_not(sign, "inv"), cmp2, "neg");
            ctx.builder.build_conditional_branch(
                ctx.builder.build_or(pos, neg, "or"),
                body_bb,
                orelse_bb,
            );
            ctx.builder.position_at_end(body_bb);
        } else {
            let counter = generator.gen_var_alloc(ctx, size_t.into())?;
            // counter = -1
            ctx.builder.build_store(counter, size_t.const_int(u64::max_value(), true));
            let len = ctx
                .build_gep_and_load(
                    iter_val.into_pointer_value(),
                    &[zero, int32.const_int(1, false)],
                )
                .into_int_value();
            ctx.builder.build_unconditional_branch(test_bb);
            ctx.builder.position_at_end(test_bb);
            let tmp = ctx.builder.build_load(counter, "i").into_int_value();
            let tmp = ctx.builder.build_int_add(tmp, size_t.const_int(1, false), "inc");
            ctx.builder.build_store(counter, tmp);
            let cmp = ctx.builder.build_int_compare(inkwell::IntPredicate::SLT, tmp, len, "cmp");
            ctx.builder.build_conditional_branch(cmp, body_bb, orelse_bb);
            ctx.builder.position_at_end(body_bb);
            let arr_ptr = ctx
                .build_gep_and_load(iter_val.into_pointer_value(), &[zero, zero])
                .into_pointer_value();
            let val = ctx.build_gep_and_load(arr_ptr, &[tmp]);
            generator.gen_assign(ctx, target, val.into())?;
        }

        gen_block(generator, ctx, body.iter())?;
        for (k, (_, _, counter)) in var_assignment.iter() {
            let (_, static_val, counter2) = ctx.var_assignment.get_mut(k).unwrap();
            if counter != counter2 {
                *static_val = None;
            }
        }
        if !ctx.is_terminated() {
            ctx.builder.build_unconditional_branch(test_bb);
        }
        if !orelse.is_empty() {
            ctx.builder.position_at_end(orelse_bb);
            gen_block(generator, ctx, orelse.iter())?;
            if !ctx.is_terminated() {
                ctx.builder.build_unconditional_branch(cont_bb);
            }
        }
        for (k, (_, _, counter)) in var_assignment.iter() {
            let (_, static_val, counter2) = ctx.var_assignment.get_mut(k).unwrap();
            if counter != counter2 {
                *static_val = None;
            }
        }
        ctx.builder.position_at_end(cont_bb);
        ctx.loop_target = loop_bb;
    } else {
        unreachable!()
    }
    Ok(())
}

pub fn gen_while<'ctx, 'a, G: CodeGenerator>(
    generator: &mut G,
    ctx: &mut CodeGenContext<'ctx, 'a>,
    stmt: &Stmt<Option<Type>>,
) -> Result<(), String> {
    if let StmtKind::While { test, body, orelse, .. } = &stmt.node {
        // var_assignment static values may be changed in another branch
        // if so, remove the static value as it may not be correct in this branch
        let var_assignment = ctx.var_assignment.clone();

        let current = ctx.builder.get_insert_block().unwrap().get_parent().unwrap();
        let test_bb = ctx.ctx.append_basic_block(current, "test");
        let body_bb = ctx.ctx.append_basic_block(current, "body");
        let cont_bb = ctx.ctx.append_basic_block(current, "cont");
        // if there is no orelse, we just go to cont_bb
        let orelse_bb =
            if orelse.is_empty() { cont_bb } else { ctx.ctx.append_basic_block(current, "orelse") };
        // store loop bb information and restore it later
        let loop_bb = ctx.loop_target.replace((test_bb, cont_bb));
        ctx.builder.build_unconditional_branch(test_bb);
        ctx.builder.position_at_end(test_bb);
        let test = generator.gen_expr(ctx, test)?.unwrap().to_basic_value_enum(ctx, generator)?;
        if let BasicValueEnum::IntValue(test) = test {
            ctx.builder.build_conditional_branch(test, body_bb, orelse_bb);
        } else {
            unreachable!()
        };
        ctx.builder.position_at_end(body_bb);
        gen_block(generator, ctx, body.iter())?;
        for (k, (_, _, counter)) in var_assignment.iter() {
            let (_, static_val, counter2) = ctx.var_assignment.get_mut(k).unwrap();
            if counter != counter2 {
                *static_val = None;
            }
        }
        if !ctx.is_terminated() {
            ctx.builder.build_unconditional_branch(test_bb);
        }
        if !orelse.is_empty() {
            ctx.builder.position_at_end(orelse_bb);
            gen_block(generator, ctx, orelse.iter())?;
            if !ctx.is_terminated() {
                ctx.builder.build_unconditional_branch(cont_bb);
            }
        }
        for (k, (_, _, counter)) in var_assignment.iter() {
            let (_, static_val, counter2) = ctx.var_assignment.get_mut(k).unwrap();
            if counter != counter2 {
                *static_val = None;
            }
        }
        ctx.builder.position_at_end(cont_bb);
        ctx.loop_target = loop_bb;
    } else {
        unreachable!()
    }
    Ok(())
}

pub fn gen_if<'ctx, 'a, G: CodeGenerator>(
    generator: &mut G,
    ctx: &mut CodeGenContext<'ctx, 'a>,
    stmt: &Stmt<Option<Type>>,
) -> Result<(), String> {
    if let StmtKind::If { test, body, orelse, .. } = &stmt.node {
        // var_assignment static values may be changed in another branch
        // if so, remove the static value as it may not be correct in this branch
        let var_assignment = ctx.var_assignment.clone();

        let current = ctx.builder.get_insert_block().unwrap().get_parent().unwrap();
        let test_bb = ctx.ctx.append_basic_block(current, "test");
        let body_bb = ctx.ctx.append_basic_block(current, "body");
        let mut cont_bb = None;
        // if there is no orelse, we just go to cont_bb
        let orelse_bb = if orelse.is_empty() {
            cont_bb = Some(ctx.ctx.append_basic_block(current, "cont"));
            cont_bb.unwrap()
        } else {
            ctx.ctx.append_basic_block(current, "orelse")
        };
        ctx.builder.build_unconditional_branch(test_bb);
        ctx.builder.position_at_end(test_bb);
        let test = generator.gen_expr(ctx, test)?.unwrap().to_basic_value_enum(ctx, generator)?;
        if let BasicValueEnum::IntValue(test) = test {
            ctx.builder.build_conditional_branch(test, body_bb, orelse_bb);
        } else {
            unreachable!()
        };
        ctx.builder.position_at_end(body_bb);
        gen_block(generator, ctx, body.iter())?;
        for (k, (_, _, counter)) in var_assignment.iter() {
            let (_, static_val, counter2) = ctx.var_assignment.get_mut(k).unwrap();
            if counter != counter2 {
                *static_val = None;
            }
        }

        if !ctx.is_terminated() {
            if cont_bb.is_none() {
                cont_bb = Some(ctx.ctx.append_basic_block(current, "cont"));
            }
            ctx.builder.build_unconditional_branch(cont_bb.unwrap());
        }
        if !orelse.is_empty() {
            ctx.builder.position_at_end(orelse_bb);
            gen_block(generator, ctx, orelse.iter())?;
            if !ctx.is_terminated() {
                if cont_bb.is_none() {
                    cont_bb = Some(ctx.ctx.append_basic_block(current, "cont"));
                }
                ctx.builder.build_unconditional_branch(cont_bb.unwrap());
            }
        }
        if let Some(cont_bb) = cont_bb {
            ctx.builder.position_at_end(cont_bb);
        }
        for (k, (_, _, counter)) in var_assignment.iter() {
            let (_, static_val, counter2) = ctx.var_assignment.get_mut(k).unwrap();
            if counter != counter2 {
                *static_val = None;
            }
        }
    } else {
        unreachable!()
    }
    Ok(())
}

pub fn final_proxy<'ctx, 'a>(
    ctx: &mut CodeGenContext<'ctx, 'a>,
    target: BasicBlock<'ctx>,
    block: BasicBlock<'ctx>,
    final_data: &mut (PointerValue, Vec<BasicBlock<'ctx>>, Vec<BasicBlock<'ctx>>),
) {
    let (final_state, final_targets, final_paths) = final_data;
    let prev = ctx.builder.get_insert_block().unwrap();
    ctx.builder.position_at_end(block);
    unsafe {
        ctx.builder.build_store(*final_state, target.get_address().unwrap());
    }
    ctx.builder.position_at_end(prev);
    final_targets.push(target);
    final_paths.push(block);
}

pub fn get_builtins<'ctx, 'a, G: CodeGenerator>(
    generator: &mut G,
    ctx: &mut CodeGenContext<'ctx, 'a>,
    symbol: &str,
) -> FunctionValue<'ctx> {
    ctx.module.get_function(symbol).unwrap_or_else(|| {
        let ty = match symbol {
            "__nac3_raise" => ctx
                .ctx
                .void_type()
                .fn_type(&[ctx.get_llvm_type(generator, ctx.primitives.exception).into()], false),
            "__nac3_resume" => ctx.ctx.void_type().fn_type(&[], false),
            "__nac3_end_catch" => ctx.ctx.void_type().fn_type(&[], false),
            _ => unimplemented!(),
        };
        let fun = ctx.module.add_function(symbol, ty, None);
        if symbol == "__nac3_raise" || symbol == "__nac3_resume" {
            fun.add_attribute(
                AttributeLoc::Function,
                ctx.ctx.create_enum_attribute(Attribute::get_named_enum_kind_id("noreturn"), 1),
            );
        }
        fun
    })
}

pub fn exn_constructor<'ctx, 'a>(
    ctx: &mut CodeGenContext<'ctx, 'a>,
    obj: Option<(Type, ValueEnum<'ctx>)>,
    _fun: (&FunSignature, DefinitionId),
    mut args: Vec<(Option<StrRef>, ValueEnum<'ctx>)>,
    generator: &mut dyn CodeGenerator,
) -> Result<Option<BasicValueEnum<'ctx>>, String> {
    let (zelf_ty, zelf) = obj.unwrap();
    let zelf = zelf.to_basic_value_enum(ctx, generator)?.into_pointer_value();
    let int32 = ctx.ctx.i32_type();
    let zero = int32.const_zero();
    let zelf_id = {
        if let TypeEnum::TObj { obj_id, .. } = &*ctx.unifier.get_ty(zelf_ty) {
            obj_id.0
        } else {
            unreachable!()
        }
    };
    let defs = ctx.top_level.definitions.read();
    let def = defs[zelf_id].read();
    let zelf_name =
        if let TopLevelDef::Class { name, .. } = &*def { *name } else { unreachable!() };
    let exception_name = format!("{}:{}", ctx.resolver.get_exception_id(zelf_id), zelf_name);
    unsafe {
        let id_ptr = ctx.builder.build_in_bounds_gep(zelf, &[zero, zero], "exn.id");
        let id = ctx.resolver.get_string_id(&exception_name);
        ctx.builder.build_store(id_ptr, int32.const_int(id as u64, false));
        let empty_string = ctx.gen_const(generator, &Constant::Str("".into()), ctx.primitives.str);
        let ptr =
            ctx.builder.build_in_bounds_gep(zelf, &[zero, int32.const_int(5, false)], "exn.msg");
        let msg = if !args.is_empty() {
            args.remove(0).1.to_basic_value_enum(ctx, generator)?
        } else {
            empty_string
        };
        ctx.builder.build_store(ptr, msg);
        for i in [6, 7, 8].iter() {
            let value = if !args.is_empty() {
                args.remove(0).1.to_basic_value_enum(ctx, generator)?
            } else {
                ctx.ctx.i64_type().const_zero().into()
            };
            let ptr = ctx.builder.build_in_bounds_gep(
                zelf,
                &[zero, int32.const_int(*i, false)],
                "exn.param",
            );
            ctx.builder.build_store(ptr, value);
        }
        // set file, func to empty string
        for i in [1, 4].iter() {
            let ptr = ctx.builder.build_in_bounds_gep(
                zelf,
                &[zero, int32.const_int(*i, false)],
                "exn.str",
            );
            ctx.builder.build_store(ptr, empty_string);
        }
        // set ints to zero
        for i in [2, 3].iter() {
            let ptr = ctx.builder.build_in_bounds_gep(
                zelf,
                &[zero, int32.const_int(*i, false)],
                "exn.ints",
            );
            ctx.builder.build_store(ptr, zero);
        }
    }
    Ok(Some(zelf.into()))
}

pub fn gen_raise<'ctx, 'a, G: CodeGenerator>(
    generator: &mut G,
    ctx: &mut CodeGenContext<'ctx, 'a>,
    exception: Option<&BasicValueEnum<'ctx>>,
    loc: Location,
) {
    if let Some(exception) = exception {
        unsafe {
            let int32 = ctx.ctx.i32_type();
            let zero = int32.const_zero();
            let exception = exception.into_pointer_value();
            let file_ptr = ctx.builder.build_in_bounds_gep(
                exception,
                &[zero, int32.const_int(1, false)],
                "file_ptr",
            );
            let filename = ctx.gen_string(generator, loc.file.0);
            ctx.builder.build_store(file_ptr, filename);
            let row_ptr = ctx.builder.build_in_bounds_gep(
                exception,
                &[zero, int32.const_int(2, false)],
                "row_ptr",
            );
            ctx.builder.build_store(row_ptr, int32.const_int(loc.row as u64, false));
            let col_ptr = ctx.builder.build_in_bounds_gep(
                exception,
                &[zero, int32.const_int(3, false)],
                "col_ptr",
            );
            ctx.builder.build_store(col_ptr, int32.const_int(loc.column as u64, false));

            let current_fun = ctx.builder.get_insert_block().unwrap().get_parent().unwrap();
            let fun_name = ctx.gen_string(generator, current_fun.get_name().to_str().unwrap());
            let name_ptr = ctx.builder.build_in_bounds_gep(
                exception,
                &[zero, int32.const_int(4, false)],
                "name_ptr",
            );
            ctx.builder.build_store(name_ptr, fun_name);
        }

        let raise = get_builtins(generator, ctx, "__nac3_raise");
        let exception = *exception;
        ctx.build_call_or_invoke(raise, &[exception], "raise");
    } else {
        let resume = get_builtins(generator, ctx, "__nac3_resume");
        ctx.build_call_or_invoke(resume, &[], "resume");
    }
    ctx.builder.build_unreachable();
}

pub fn gen_try<'ctx, 'a, G: CodeGenerator>(
    generator: &mut G,
    ctx: &mut CodeGenContext<'ctx, 'a>,
    target: &Stmt<Option<Type>>,
) -> Result<(), String> {
    if let StmtKind::Try { body, handlers, orelse, finalbody, .. } = &target.node {
        // if we need to generate anything related to exception, we must have personality defined
        let personality_symbol = ctx.top_level.personality_symbol.as_ref().unwrap();
        let personality = ctx.module.get_function(personality_symbol).unwrap_or_else(|| {
            let ty = ctx.ctx.i32_type().fn_type(&[], true);
            ctx.module.add_function(personality_symbol, ty, None)
        });
        let exception_type = ctx.get_llvm_type(generator, ctx.primitives.exception);
        let ptr_type = ctx.ctx.i8_type().ptr_type(inkwell::AddressSpace::Generic);
        let current_block = ctx.builder.get_insert_block().unwrap();
        let current_fun = current_block.get_parent().unwrap();
        let landingpad = ctx.ctx.append_basic_block(current_fun, "try.landingpad");
        let dispatcher = ctx.ctx.append_basic_block(current_fun, "try.dispatch");
        let mut dispatcher_end = dispatcher;
        ctx.builder.position_at_end(dispatcher);
        let exn = ctx.builder.build_phi(exception_type, "exn");
        ctx.builder.position_at_end(current_block);

        let mut cleanup = None;
        let mut old_loop_target = None;
        let mut old_return = None;
        let mut final_data = None;
        let has_cleanup = !finalbody.is_empty();
        if has_cleanup {
            let final_state = generator.gen_var_alloc(ctx, ptr_type.into())?;
            final_data = Some((final_state, Vec::new(), Vec::new()));
            if let Some((continue_target, break_target)) = ctx.loop_target {
                let break_proxy = ctx.ctx.append_basic_block(current_fun, "try.break");
                let continue_proxy = ctx.ctx.append_basic_block(current_fun, "try.continue");
                final_proxy(ctx, break_target, break_proxy, final_data.as_mut().unwrap());
                final_proxy(ctx, continue_target, continue_proxy, final_data.as_mut().unwrap());
                old_loop_target = ctx.loop_target.replace((continue_proxy, break_proxy));
            }
            let return_proxy = ctx.ctx.append_basic_block(current_fun, "try.return");
            if let Some(return_target) = ctx.return_target {
                final_proxy(ctx, return_target, return_proxy, final_data.as_mut().unwrap());
            } else {
                let return_target = ctx.ctx.append_basic_block(current_fun, "try.return_target");
                ctx.builder.position_at_end(return_target);
                let return_value = ctx.return_buffer.map(|v| ctx.builder.build_load(v, "$ret"));
                ctx.builder.build_return(return_value.as_ref().map(|v| v as &dyn BasicValue));
                ctx.builder.position_at_end(current_block);
                final_proxy(ctx, return_target, return_proxy, final_data.as_mut().unwrap());
            }
            old_return = ctx.return_target.replace(return_proxy);
            cleanup = Some(ctx.ctx.append_basic_block(current_fun, "try.cleanup"));
        }

        let mut clauses = Vec::new();
        let mut found_catch_all = false;
        for handler_node in handlers.iter() {
            let ExcepthandlerKind::ExceptHandler { type_, .. } = &handler_node.node;
            // none or Exception
            if type_.is_none()
                || ctx
                    .unifier
                    .unioned(type_.as_ref().unwrap().custom.unwrap(), ctx.primitives.exception)
            {
                clauses.push(None);
                found_catch_all = true;
                break;
            } else {
                let type_ = type_.as_ref().unwrap();
                let exn_name = ctx.resolver.get_type_name(
                    &ctx.top_level.definitions.read(),
                    &mut ctx.unifier,
                    type_.custom.unwrap(),
                );
                let obj_id = if let TypeEnum::TObj { obj_id, .. } = &*ctx.unifier.get_ty(type_.custom.unwrap()) {
                    *obj_id
                } else {
                    unreachable!()
                };
                let exception_name = format!("{}:{}", ctx.resolver.get_exception_id(obj_id.0), exn_name);
                let exn_id = ctx.resolver.get_string_id(&exception_name);
                let exn_id_global =
                    ctx.module.add_global(ctx.ctx.i32_type(), None, &format!("exn.{}", exn_id));
                exn_id_global.set_initializer(&ctx.ctx.i32_type().const_int(exn_id as u64, false));
                clauses.push(Some(exn_id_global.as_pointer_value().as_basic_value_enum()));
            }
        }
        let mut all_clauses = clauses.clone();
        if let Some(old_clauses) = &ctx.outer_catch_clauses {
            if !found_catch_all {
                all_clauses.extend_from_slice(&old_clauses.0)
            }
        }
        let old_clauses = ctx.outer_catch_clauses.replace((all_clauses, dispatcher, exn));
        let old_unwind = ctx.unwind_target.replace(landingpad);
        gen_block(generator, ctx, body.iter())?;
        if ctx.builder.get_insert_block().unwrap().get_terminator().is_none() {
            gen_block(generator, ctx, orelse.iter())?;
        }
        let body = ctx.builder.get_insert_block().unwrap();
        // reset old_clauses and old_unwind
        let (all_clauses, _, _) = ctx.outer_catch_clauses.take().unwrap();
        ctx.outer_catch_clauses = old_clauses;
        ctx.unwind_target = old_unwind;
        ctx.return_target = old_return;
        ctx.loop_target = old_loop_target;
        old_loop_target = None;

        let old_unwind = if !finalbody.is_empty() {
            let final_landingpad = ctx.ctx.append_basic_block(current_fun, "try.catch.final");
            ctx.builder.position_at_end(final_landingpad);
            ctx.builder.build_landing_pad(
                ctx.ctx.struct_type(&[ptr_type.into(), exception_type], false),
                personality,
                &[],
                true,
                "try.catch.final",
            );
            ctx.builder.build_unconditional_branch(cleanup.unwrap());
            ctx.builder.position_at_end(body);
            ctx.unwind_target.replace(final_landingpad)
        } else {
            None
        };

        // run end_catch before continue/break/return
        let mut final_proxy_lambda =
            |ctx: &mut CodeGenContext<'ctx, 'a>,
             target: BasicBlock<'ctx>,
             block: BasicBlock<'ctx>| final_proxy(ctx, target, block, final_data.as_mut().unwrap());
        let mut redirect_lambda = |ctx: &mut CodeGenContext<'ctx, 'a>,
                                   target: BasicBlock<'ctx>,
                                   block: BasicBlock<'ctx>| {
            ctx.builder.position_at_end(block);
            ctx.builder.build_unconditional_branch(target);
            ctx.builder.position_at_end(body);
        };
        let redirect = if has_cleanup {
            &mut final_proxy_lambda
                as &mut dyn FnMut(&mut CodeGenContext<'ctx, 'a>, BasicBlock<'ctx>, BasicBlock<'ctx>)
        } else {
            &mut redirect_lambda
                as &mut dyn FnMut(&mut CodeGenContext<'ctx, 'a>, BasicBlock<'ctx>, BasicBlock<'ctx>)
        };
        let resume = get_builtins(generator, ctx, "__nac3_resume");
        let end_catch = get_builtins(generator, ctx, "__nac3_end_catch");
        if let Some((continue_target, break_target)) = ctx.loop_target.take() {
            let break_proxy = ctx.ctx.append_basic_block(current_fun, "try.break");
            let continue_proxy = ctx.ctx.append_basic_block(current_fun, "try.continue");
            ctx.builder.position_at_end(break_proxy);
            ctx.builder.build_call(end_catch, &[], "end_catch");
            ctx.builder.position_at_end(continue_proxy);
            ctx.builder.build_call(end_catch, &[], "end_catch");
            ctx.builder.position_at_end(body);
            redirect(ctx, break_target, break_proxy);
            redirect(ctx, continue_target, continue_proxy);
            ctx.loop_target = Some((continue_proxy, break_proxy));
            old_loop_target = Some((continue_target, break_target));
        }
        let return_proxy = ctx.ctx.append_basic_block(current_fun, "try.return");
        ctx.builder.position_at_end(return_proxy);
        ctx.builder.build_call(end_catch, &[], "end_catch");
        let return_target = ctx.return_target.take().unwrap_or_else(|| {
            let doreturn = ctx.ctx.append_basic_block(current_fun, "try.doreturn");
            ctx.builder.position_at_end(doreturn);
            let return_value = ctx.return_buffer.map(|v| ctx.builder.build_load(v, "$ret"));
            ctx.builder.build_return(return_value.as_ref().map(|v| v as &dyn BasicValue));
            doreturn
        });
        redirect(ctx, return_target, return_proxy);
        ctx.return_target = Some(return_proxy);
        old_return = Some(return_target);

        let mut post_handlers = Vec::new();

        let exnid = if !handlers.is_empty() {
            ctx.builder.position_at_end(dispatcher);
            unsafe {
                let zero = ctx.ctx.i32_type().const_zero();
                let exnid_ptr = ctx.builder.build_gep(
                    exn.as_basic_value().into_pointer_value(),
                    &[zero, zero],
                    "exnidptr",
                );
                Some(ctx.builder.build_load(exnid_ptr, "exnid"))
            }
        } else {
            None
        };

        for (handler_node, exn_type) in handlers.iter().zip(clauses.iter()) {
            let ExcepthandlerKind::ExceptHandler { type_, name, body } = &handler_node.node;
            let handler_bb = ctx.ctx.append_basic_block(current_fun, "try.handler");
            ctx.builder.position_at_end(handler_bb);
            if let Some(name) = name {
                let exn_ty = ctx.get_llvm_type(generator, type_.as_ref().unwrap().custom.unwrap());
                let exn_store = generator.gen_var_alloc(ctx, exn_ty)?;
                ctx.var_assignment.insert(*name, (exn_store, None, 0));
                ctx.builder.build_store(exn_store, exn.as_basic_value());
            }
            gen_block(generator, ctx, body.iter())?;
            let current = ctx.builder.get_insert_block().unwrap();
            // only need to call end catch if not terminated
            // otherwise, we already handled in return/break/continue/raise
            if current.get_terminator().is_none() {
                ctx.builder.build_call(end_catch, &[], "end_catch");
            }
            post_handlers.push(current);
            ctx.builder.position_at_end(dispatcher_end);
            if let Some(exn_type) = exn_type {
                let dispatcher_cont =
                    ctx.ctx.append_basic_block(current_fun, "try.dispatcher_cont");
                let actual_id = exnid.unwrap().into_int_value();
                let expected_id = ctx
                    .builder
                    .build_load(exn_type.into_pointer_value(), "expected_id")
                    .into_int_value();
                let result = ctx.builder.build_int_compare(EQ, actual_id, expected_id, "exncheck");
                ctx.builder.build_conditional_branch(result, handler_bb, dispatcher_cont);
                dispatcher_end = dispatcher_cont;
            } else {
                ctx.builder.build_unconditional_branch(handler_bb);
                break;
            }
        }

        ctx.unwind_target = old_unwind;
        ctx.loop_target = old_loop_target;
        ctx.return_target = old_return;

        ctx.builder.position_at_end(landingpad);
        let clauses: Vec<_> = if finalbody.is_empty() { &all_clauses } else { &clauses }
            .iter()
            .map(|v| v.unwrap_or(ptr_type.const_zero().into()))
            .collect();
        let landingpad_value = ctx
            .builder
            .build_landing_pad(
                ctx.ctx.struct_type(&[ptr_type.into(), exception_type], false),
                personality,
                &clauses,
                has_cleanup,
                "try.landingpad",
            )
            .into_struct_value();
        let exn_val = ctx.builder.build_extract_value(landingpad_value, 1, "exn").unwrap();
        ctx.builder.build_unconditional_branch(dispatcher);
        exn.add_incoming(&[(&exn_val, landingpad)]);

        if dispatcher_end.get_terminator().is_none() {
            ctx.builder.position_at_end(dispatcher_end);
            if let Some(cleanup) = cleanup {
                ctx.builder.build_unconditional_branch(cleanup);
            } else if let Some((_, outer_dispatcher, phi)) = ctx.outer_catch_clauses {
                phi.add_incoming(&[(&exn_val, dispatcher_end)]);
                ctx.builder.build_unconditional_branch(outer_dispatcher);
            } else {
                ctx.build_call_or_invoke(resume, &[], "resume");
                ctx.builder.build_unreachable();
            }
        }

        if finalbody.is_empty() {
            let tail = ctx.ctx.append_basic_block(current_fun, "try.tail");
            if body.get_terminator().is_none() {
                ctx.builder.position_at_end(body);
                ctx.builder.build_unconditional_branch(tail);
            }
            if matches!(cleanup, Some(cleanup) if cleanup.get_terminator().is_none()) {
                ctx.builder.position_at_end(cleanup.unwrap());
                ctx.builder.build_unconditional_branch(tail);
            }
            for post_handler in post_handlers {
                if post_handler.get_terminator().is_none() {
                    ctx.builder.position_at_end(post_handler);
                    ctx.builder.build_unconditional_branch(tail);
                }
            }
            ctx.builder.position_at_end(tail);
        } else {
            // exception path
            let cleanup = cleanup.unwrap();
            ctx.builder.position_at_end(cleanup);
            gen_block(generator, ctx, finalbody.iter())?;
            if !ctx.is_terminated() {
                ctx.build_call_or_invoke(resume, &[], "resume");
                ctx.builder.build_unreachable();
            }

            // normal path
            let (final_state, mut final_targets, final_paths) = final_data.unwrap();
            let tail = ctx.ctx.append_basic_block(current_fun, "try.tail");
            final_targets.push(tail);
            let finalizer = ctx.ctx.append_basic_block(current_fun, "try.finally");
            ctx.builder.position_at_end(finalizer);
            gen_block(generator, ctx, finalbody.iter())?;
            if !ctx.is_terminated() {
                let dest = ctx.builder.build_load(final_state, "final_dest");
                ctx.builder.build_indirect_branch(dest, &final_targets);
            }
            for block in final_paths.iter() {
                if block.get_terminator().is_none() {
                    ctx.builder.position_at_end(*block);
                    ctx.builder.build_unconditional_branch(finalizer);
                }
            }
            for block in [body].iter().chain(post_handlers.iter()) {
                if block.get_terminator().is_none() {
                    ctx.builder.position_at_end(*block);
                    unsafe {
                        ctx.builder.build_store(final_state, tail.get_address().unwrap());
                    }
                    ctx.builder.build_unconditional_branch(finalizer);
                }
            }
            ctx.builder.position_at_end(tail);
        }
        Ok(())
    } else {
        unreachable!()
    }
}

pub fn gen_with<'ctx, 'a, G: CodeGenerator>(
    _: &mut G,
    _: &mut CodeGenContext<'ctx, 'a>,
    stmt: &Stmt<Option<Type>>,
) -> Result<(), String> {
    // TODO: Implement with statement after finishing exceptions
    Err(format!("With statement with custom types is not yet supported (at {})", stmt.location))
}

pub fn gen_return<'ctx, 'a, G: CodeGenerator>(
    generator: &mut G,
    ctx: &mut CodeGenContext<'ctx, 'a>,
    value: &Option<Box<Expr<Option<Type>>>>,
) -> Result<(), String> {
    let value = value
        .as_ref()
        .map(|v| generator.gen_expr(ctx, v).and_then(|v| v.unwrap().to_basic_value_enum(ctx, generator)))
        .transpose()?;
    if let Some(return_target) = ctx.return_target {
        if let Some(value) = value {
            ctx.builder.build_store(ctx.return_buffer.unwrap(), value);
        }
        ctx.builder.build_unconditional_branch(return_target);
    } else if ctx.need_sret {
        // sret
        ctx.builder.build_store(ctx.return_buffer.unwrap(), value.unwrap());
        ctx.builder.build_return(None);
    } else {
        let value = value.as_ref().map(|v| v as &dyn BasicValue);
        ctx.builder.build_return(value);
    }
    Ok(())
}

pub fn gen_stmt<'ctx, 'a, G: CodeGenerator>(
    generator: &mut G,
    ctx: &mut CodeGenContext<'ctx, 'a>,
    stmt: &Stmt<Option<Type>>,
) -> Result<(), String> {
    match &stmt.node {
        StmtKind::Pass { .. } => {}
        StmtKind::Expr { value, .. } => {
            generator.gen_expr(ctx, value)?;
        }
        StmtKind::Return { value, .. } => {
            gen_return(generator, ctx, value)?;
        }
        StmtKind::AnnAssign { target, value, .. } => {
            if let Some(value) = value {
                let value = generator.gen_expr(ctx, value)?.unwrap();
                generator.gen_assign(ctx, target, value)?;
            }
        }
        StmtKind::Assign { targets, value, .. } => {
            let value = generator.gen_expr(ctx, value)?.unwrap();
            for target in targets.iter() {
                generator.gen_assign(ctx, target, value.clone())?;
            }
        }
        StmtKind::Continue { .. } => {
            ctx.builder.build_unconditional_branch(ctx.loop_target.unwrap().0);
        }
        StmtKind::Break { .. } => {
            ctx.builder.build_unconditional_branch(ctx.loop_target.unwrap().1);
        }
        StmtKind::If { .. } => generator.gen_if(ctx, stmt)?,
        StmtKind::While { .. } => generator.gen_while(ctx, stmt)?,
        StmtKind::For { .. } => generator.gen_for(ctx, stmt)?,
        StmtKind::With { .. } => generator.gen_with(ctx, stmt)?,
        StmtKind::AugAssign { target, op, value, .. } => {
            let value = gen_binop_expr(generator, ctx, target, op, value)?;
            generator.gen_assign(ctx, target, value)?;
        }
        StmtKind::Try { .. } => gen_try(generator, ctx, stmt)?,
        StmtKind::Raise { exc, .. } => {
            if let Some(exc) = exc {
                let exc =
                    generator.gen_expr(ctx, exc)?.unwrap().to_basic_value_enum(ctx, generator)?;
                gen_raise(generator, ctx, Some(&exc), stmt.location);
            } else {
                gen_raise(generator, ctx, None, stmt.location);
            }
        }
        _ => unimplemented!(),
    };
    Ok(())
}

pub fn gen_block<'ctx, 'a, 'b, G: CodeGenerator, I: Iterator<Item = &'b Stmt<Option<Type>>>>(
    generator: &mut G,
    ctx: &mut CodeGenContext<'ctx, 'a>,
    stmts: I,
) -> Result<(), String> {
    for stmt in stmts {
        generator.gen_stmt(ctx, stmt)?;
        if ctx.is_terminated() {
            break;
        }
    }
    Ok(())
}
