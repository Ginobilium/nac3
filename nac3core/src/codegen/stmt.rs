use super::{
    super::symbol_resolver::ValueEnum,
    expr::destructure_range,
    irrt::{handle_slice_indices, list_slice_assignment},
    CodeGenContext, CodeGenerator,
};
use crate::{
    codegen::expr::gen_binop_expr,
    typecheck::typedef::{Type, TypeEnum},
};
use inkwell::{
    types::BasicTypeEnum,
    values::{BasicValue, BasicValueEnum, PointerValue},
};
use nac3parser::ast::{Expr, ExprKind, Stmt, StmtKind};

pub fn gen_var<'ctx, 'a>(
    ctx: &mut CodeGenContext<'ctx, 'a>,
    ty: BasicTypeEnum<'ctx>,
) -> PointerValue<'ctx> {
    // put the alloca in init block
    let current = ctx.builder.get_insert_block().unwrap();
    // position before the last branching instruction...
    ctx.builder.position_before(&ctx.init_bb.get_last_instruction().unwrap());
    let ptr = ctx.builder.build_alloca(ty, "tmp");
    ctx.builder.position_at_end(current);
    ptr
}

pub fn gen_store_target<'ctx, 'a, G: CodeGenerator>(
    generator: &mut G,
    ctx: &mut CodeGenContext<'ctx, 'a>,
    pattern: &Expr<Option<Type>>,
) -> PointerValue<'ctx> {
    // very similar to gen_expr, but we don't do an extra load at the end
    // and we flatten nested tuples
    match &pattern.node {
        ExprKind::Name { id, .. } => ctx.var_assignment.get(id).map(|v| v.0).unwrap_or_else(|| {
            let ptr_ty = ctx.get_llvm_type(generator, pattern.custom.unwrap());
            let ptr = generator.gen_var_alloc(ctx, ptr_ty);
            ctx.var_assignment.insert(*id, (ptr, None, 0));
            ptr
        }),
        ExprKind::Attribute { value, attr, .. } => {
            let index = ctx.get_attr_index(value.custom.unwrap(), *attr);
            let val = generator.gen_expr(ctx, value).unwrap().to_basic_value_enum(ctx, generator);
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
                .gen_expr(ctx, value)
                .unwrap()
                .to_basic_value_enum(ctx, generator)
                .into_pointer_value();
            let index = generator
                .gen_expr(ctx, slice)
                .unwrap()
                .to_basic_value_enum(ctx, generator)
                .into_int_value();
            unsafe {
                let arr_ptr = ctx
                    .build_gep_and_load(v, &[i32_type.const_zero(), i32_type.const_zero()])
                    .into_pointer_value();
                ctx.builder.build_gep(arr_ptr, &[index], "loadarrgep")
            }
        }
        _ => unreachable!(),
    }
}

pub fn gen_assign<'ctx, 'a, G: CodeGenerator>(
    generator: &mut G,
    ctx: &mut CodeGenContext<'ctx, 'a>,
    target: &Expr<Option<Type>>,
    value: ValueEnum<'ctx>,
) {
    if let ExprKind::Tuple { elts, .. } = &target.node {
        if let BasicValueEnum::PointerValue(ptr) = value.to_basic_value_enum(ctx, generator) {
            let i32_type = ctx.ctx.i32_type();
            for (i, elt) in elts.iter().enumerate() {
                let v = ctx.build_gep_and_load(
                    ptr,
                    &[i32_type.const_zero(), i32_type.const_int(i as u64, false)],
                );
                generator.gen_assign(ctx, elt, v.into());
            }
        } else {
            unreachable!()
        }
    } else {
        match &target.node {
            ExprKind::Subscript { value: ls, slice, .. }
                if matches!(&slice.node, ExprKind::Slice { .. }) =>
            {
                if let ExprKind::Slice { lower, upper, step } = &slice.node {
                    let ls = generator
                        .gen_expr(ctx, ls)
                        .unwrap()
                        .to_basic_value_enum(ctx, generator)
                        .into_pointer_value();
                    let (start, end, step) =
                        handle_slice_indices(lower, upper, step, ctx, generator, ls);
                    let value = value.to_basic_value_enum(ctx, generator).into_pointer_value();
                    let ty = if let TypeEnum::TList { ty } =
                        &*ctx.unifier.get_ty(target.custom.unwrap())
                    {
                        ctx.get_llvm_type(generator, *ty)
                    } else {
                        unreachable!()
                    };
                    let src_ind = handle_slice_indices(&None, &None, &None, ctx, generator, value);
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
                let ptr = generator.gen_store_target(ctx, target);
                if let ExprKind::Name { id, .. } = &target.node {
                    let (_, static_value, counter) = ctx.var_assignment.get_mut(id).unwrap();
                    *counter += 1;
                    if let ValueEnum::Static(s) = &value {
                        *static_value = Some(s.clone());
                    }
                }
                let val = value.to_basic_value_enum(ctx, generator);
                ctx.builder.build_store(ptr, val);
            }
        }
    }
}

pub fn gen_for<'ctx, 'a, G: CodeGenerator>(
    generator: &mut G,
    ctx: &mut CodeGenContext<'ctx, 'a>,
    stmt: &Stmt<Option<Type>>,
) {
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
        let loop_bb = ctx.loop_bb.replace((test_bb, cont_bb));

        let iter_val = generator.gen_expr(ctx, iter).unwrap().to_basic_value_enum(ctx, generator);
        if ctx.unifier.unioned(iter.custom.unwrap(), ctx.primitives.range) {
            // setup
            let iter_val = iter_val.into_pointer_value();
            let i = generator.gen_store_target(ctx, target);
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
            let counter = generator.gen_var_alloc(ctx, size_t.into());
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
            generator.gen_assign(ctx, target, val.into());
        }

        for stmt in body.iter() {
            generator.gen_stmt(ctx, stmt);
        }
        for (k, (_, _, counter)) in var_assignment.iter() {
            let (_, static_val, counter2) = ctx.var_assignment.get_mut(k).unwrap();
            if counter != counter2 {
                *static_val = None;
            }
        }
        ctx.builder.build_unconditional_branch(test_bb);
        if !orelse.is_empty() {
            ctx.builder.position_at_end(orelse_bb);
            for stmt in orelse.iter() {
                generator.gen_stmt(ctx, stmt);
            }
            ctx.builder.build_unconditional_branch(cont_bb);
        }
        for (k, (_, _, counter)) in var_assignment.iter() {
            let (_, static_val, counter2) = ctx.var_assignment.get_mut(k).unwrap();
            if counter != counter2 {
                *static_val = None;
            }
        }
        ctx.builder.position_at_end(cont_bb);
        ctx.loop_bb = loop_bb;
    } else {
        unreachable!()
    }
}

pub fn gen_while<'ctx, 'a, G: CodeGenerator>(
    generator: &mut G,
    ctx: &mut CodeGenContext<'ctx, 'a>,
    stmt: &Stmt<Option<Type>>,
) {
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
        let loop_bb = ctx.loop_bb.replace((test_bb, cont_bb));
        ctx.builder.build_unconditional_branch(test_bb);
        ctx.builder.position_at_end(test_bb);
        let test = generator.gen_expr(ctx, test).unwrap().to_basic_value_enum(ctx, generator);
        if let BasicValueEnum::IntValue(test) = test {
            ctx.builder.build_conditional_branch(test, body_bb, orelse_bb);
        } else {
            unreachable!()
        };
        ctx.builder.position_at_end(body_bb);
        for stmt in body.iter() {
            generator.gen_stmt(ctx, stmt);
        }
        for (k, (_, _, counter)) in var_assignment.iter() {
            let (_, static_val, counter2) = ctx.var_assignment.get_mut(k).unwrap();
            if counter != counter2 {
                *static_val = None;
            }
        }
        ctx.builder.build_unconditional_branch(test_bb);
        if !orelse.is_empty() {
            ctx.builder.position_at_end(orelse_bb);
            for stmt in orelse.iter() {
                generator.gen_stmt(ctx, stmt);
            }
            ctx.builder.build_unconditional_branch(cont_bb);
        }
        for (k, (_, _, counter)) in var_assignment.iter() {
            let (_, static_val, counter2) = ctx.var_assignment.get_mut(k).unwrap();
            if counter != counter2 {
                *static_val = None;
            }
        }
        ctx.builder.position_at_end(cont_bb);
        ctx.loop_bb = loop_bb;
    } else {
        unreachable!()
    }
}

pub fn gen_if<'ctx, 'a, G: CodeGenerator>(
    generator: &mut G,
    ctx: &mut CodeGenContext<'ctx, 'a>,
    stmt: &Stmt<Option<Type>>,
) -> bool {
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
        let test = generator.gen_expr(ctx, test).unwrap().to_basic_value_enum(ctx, generator);
        if let BasicValueEnum::IntValue(test) = test {
            ctx.builder.build_conditional_branch(test, body_bb, orelse_bb);
        } else {
            unreachable!()
        };
        ctx.builder.position_at_end(body_bb);
        let mut exited = false;
        for stmt in body.iter() {
            exited = generator.gen_stmt(ctx, stmt);
            if exited {
                break;
            }
        }
        for (k, (_, _, counter)) in var_assignment.iter() {
            let (_, static_val, counter2) = ctx.var_assignment.get_mut(k).unwrap();
            if counter != counter2 {
                *static_val = None;
            }
        }

        if !exited {
            if cont_bb.is_none() {
                cont_bb = Some(ctx.ctx.append_basic_block(current, "cont"));
            }
            ctx.builder.build_unconditional_branch(cont_bb.unwrap());
        }
        let then_exited = exited;
        let else_exited = if !orelse.is_empty() {
            exited = false;
            ctx.builder.position_at_end(orelse_bb);
            for stmt in orelse.iter() {
                exited = generator.gen_stmt(ctx, stmt);
                if exited {
                    break;
                }
            }
            if !exited {
                if cont_bb.is_none() {
                    cont_bb = Some(ctx.ctx.append_basic_block(current, "cont"));
                }
                ctx.builder.build_unconditional_branch(cont_bb.unwrap());
            }
            exited
        } else {
            false
        };
        if let Some(cont_bb) = cont_bb {
            ctx.builder.position_at_end(cont_bb);
        }
        for (k, (_, _, counter)) in var_assignment.iter() {
            let (_, static_val, counter2) = ctx.var_assignment.get_mut(k).unwrap();
            if counter != counter2 {
                *static_val = None;
            }
        }
        then_exited && else_exited
    } else {
        unreachable!()
    }
}

pub fn gen_with<'ctx, 'a, G: CodeGenerator>(
    _: &mut G,
    _: &mut CodeGenContext<'ctx, 'a>,
    _: &Stmt<Option<Type>>,
) -> bool {
    // TODO: Implement with statement after finishing exceptions
    unimplemented!()
}

pub fn gen_stmt<'ctx, 'a, G: CodeGenerator>(
    generator: &mut G,
    ctx: &mut CodeGenContext<'ctx, 'a>,
    stmt: &Stmt<Option<Type>>,
) -> bool {
    match &stmt.node {
        StmtKind::Pass { .. } => {}
        StmtKind::Expr { value, .. } => {
            generator.gen_expr(ctx, value);
        }
        StmtKind::Return { value, .. } => {
            let value = value
                .as_ref()
                .map(|v| generator.gen_expr(ctx, v).unwrap().to_basic_value_enum(ctx, generator));
            let value = value.as_ref().map(|v| v as &dyn BasicValue);
            ctx.builder.build_return(value);
            return true;
        }
        StmtKind::AnnAssign { target, value, .. } => {
            if let Some(value) = value {
                let value = generator.gen_expr(ctx, value).unwrap();
                generator.gen_assign(ctx, target, value);
            }
        }
        StmtKind::Assign { targets, value, .. } => {
            let value = generator.gen_expr(ctx, value).unwrap();
            for target in targets.iter() {
                generator.gen_assign(ctx, target, value.clone());
            }
        }
        StmtKind::Continue { .. } => {
            ctx.builder.build_unconditional_branch(ctx.loop_bb.unwrap().0);
            return true;
        }
        StmtKind::Break { .. } => {
            ctx.builder.build_unconditional_branch(ctx.loop_bb.unwrap().1);
            return true;
        }
        StmtKind::If { .. } => return generator.gen_if(ctx, stmt),
        StmtKind::While { .. } => return generator.gen_while(ctx, stmt),
        StmtKind::For { .. } => return generator.gen_for(ctx, stmt),
        StmtKind::With { .. } => return generator.gen_with(ctx, stmt),
        StmtKind::AugAssign { target, op, value, .. } => {
            let value = gen_binop_expr(generator, ctx, target, op, value);
            generator.gen_assign(ctx, target, value);
        }
        _ => unimplemented!(),
    };
    false
}
