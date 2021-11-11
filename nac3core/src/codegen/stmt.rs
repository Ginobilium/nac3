use super::{expr::destructure_range, CodeGenContext, CodeGenerator};
use crate::typecheck::typedef::Type;
use inkwell::values::{BasicValue, BasicValueEnum, PointerValue};
use nac3parser::ast::{Expr, ExprKind, Stmt, StmtKind};

pub fn gen_var<'ctx, 'a>(ctx: &mut CodeGenContext<'ctx, 'a>, ty: Type) -> PointerValue<'ctx> {
    // put the alloca in init block
    let current = ctx.builder.get_insert_block().unwrap();
    // position before the last branching instruction...
    ctx.builder.position_before(&ctx.init_bb.get_last_instruction().unwrap());
    let ty = ctx.get_llvm_type(ty);
    let ptr = ctx.builder.build_alloca(ty, "tmp");
    ctx.builder.position_at_end(current);
    ptr
}

pub fn gen_store_target<'ctx, 'a, G: CodeGenerator + ?Sized>(
    generator: &mut G,
    ctx: &mut CodeGenContext<'ctx, 'a>,
    pattern: &Expr<Option<Type>>,
) -> PointerValue<'ctx> {
    // very similar to gen_expr, but we don't do an extra load at the end
    // and we flatten nested tuples
    match &pattern.node {
        ExprKind::Name { id, .. } => ctx.var_assignment.get(id).cloned().unwrap_or_else(|| {
            let ptr = generator.gen_var_alloc(ctx, pattern.custom.unwrap());
            ctx.var_assignment.insert(*id, ptr);
            ptr
        }),
        ExprKind::Attribute { value, attr, .. } => {
            let index = ctx.get_attr_index(value.custom.unwrap(), *attr);
            let val = generator.gen_expr(ctx, value).unwrap();
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
            let v = generator.gen_expr(ctx, value).unwrap().into_pointer_value();
            let index = generator.gen_expr(ctx, slice).unwrap().into_int_value();
            unsafe {
                let arr_ptr = ctx
                    .build_gep_and_load(v, &[i32_type.const_zero(), i32_type.const_int(1, false)])
                    .into_pointer_value();
                ctx.builder.build_gep(arr_ptr, &[index], "loadarrgep")
            }
        }
        _ => unreachable!(),
    }
}

pub fn gen_assign<'ctx, 'a, G: CodeGenerator + ?Sized>(
    generator: &mut G,
    ctx: &mut CodeGenContext<'ctx, 'a>,
    target: &Expr<Option<Type>>,
    value: BasicValueEnum<'ctx>,
) {
    let i32_type = ctx.ctx.i32_type();
    if let ExprKind::Tuple { elts, .. } = &target.node {
        if let BasicValueEnum::PointerValue(ptr) = value {
            for (i, elt) in elts.iter().enumerate() {
                let v = ctx.build_gep_and_load(
                    ptr,
                    &[i32_type.const_zero(), i32_type.const_int(i as u64, false)],
                );
                generator.gen_assign(ctx, elt, v);
            }
        } else {
            unreachable!()
        }
    } else {
        let ptr = generator.gen_store_target(ctx, target);
        ctx.builder.build_store(ptr, value);
    }
}

pub fn gen_for<'ctx, 'a, G: CodeGenerator + ?Sized>(
    generator: &mut G,
    ctx: &mut CodeGenContext<'ctx, 'a>,
    stmt: &Stmt<Option<Type>>,
) {
    if let StmtKind::For { iter, target, body, orelse, .. } = &stmt.node {
        let int32 = ctx.ctx.i32_type();
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

        let iter_val = generator.gen_expr(ctx, iter).unwrap();
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
            let counter = generator.gen_var_alloc(ctx, ctx.primitives.int32);
            // counter = -1
            ctx.builder.build_store(counter, ctx.ctx.i32_type().const_int(u64::max_value(), true));
            let len = ctx
                .build_gep_and_load(iter_val.into_pointer_value(), &[zero, zero])
                .into_int_value();
            ctx.builder.build_unconditional_branch(test_bb);
            ctx.builder.position_at_end(test_bb);
            let tmp = ctx.builder.build_load(counter, "i").into_int_value();
            let tmp = ctx.builder.build_int_add(tmp, int32.const_int(1, false), "inc");
            ctx.builder.build_store(counter, tmp);
            let cmp = ctx.builder.build_int_compare(inkwell::IntPredicate::SLT, tmp, len, "cmp");
            ctx.builder.build_conditional_branch(cmp, body_bb, orelse_bb);
            ctx.builder.position_at_end(body_bb);
            let arr_ptr = ctx
                .build_gep_and_load(
                    iter_val.into_pointer_value(),
                    &[zero, int32.const_int(1, false)],
                )
                .into_pointer_value();
            let val = ctx.build_gep_and_load(arr_ptr, &[tmp]);
            generator.gen_assign(ctx, target, val);
        }

        for stmt in body.iter() {
            generator.gen_stmt(ctx, stmt);
        }
        ctx.builder.build_unconditional_branch(test_bb);
        if !orelse.is_empty() {
            ctx.builder.position_at_end(orelse_bb);
            for stmt in orelse.iter() {
                generator.gen_stmt(ctx, stmt);
            }
            ctx.builder.build_unconditional_branch(cont_bb);
        }
        ctx.builder.position_at_end(cont_bb);
        ctx.loop_bb = loop_bb;
    } else {
        unreachable!()
    }
}

pub fn gen_while<'ctx, 'a, G: CodeGenerator + ?Sized>(
    generator: &mut G,
    ctx: &mut CodeGenContext<'ctx, 'a>,
    stmt: &Stmt<Option<Type>>,
) {
    if let StmtKind::While { test, body, orelse, .. } = &stmt.node {
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
        let test = generator.gen_expr(ctx, test).unwrap();
        if let BasicValueEnum::IntValue(test) = test {
            ctx.builder.build_conditional_branch(test, body_bb, orelse_bb);
        } else {
            unreachable!()
        };
        ctx.builder.position_at_end(body_bb);
        for stmt in body.iter() {
            generator.gen_stmt(ctx, stmt);
        }
        ctx.builder.build_unconditional_branch(test_bb);
        if !orelse.is_empty() {
            ctx.builder.position_at_end(orelse_bb);
            for stmt in orelse.iter() {
                generator.gen_stmt(ctx, stmt);
            }
            ctx.builder.build_unconditional_branch(cont_bb);
        }
        ctx.builder.position_at_end(cont_bb);
        ctx.loop_bb = loop_bb;
    } else {
        unreachable!()
    }
}

pub fn gen_if<'ctx, 'a, G: CodeGenerator + ?Sized>(
    generator: &mut G,
    ctx: &mut CodeGenContext<'ctx, 'a>,
    stmt: &Stmt<Option<Type>>,
) -> bool {
    if let StmtKind::If { test, body, orelse, .. } = &stmt.node {
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
        let test = generator.gen_expr(ctx, test).unwrap();
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
        then_exited && else_exited
    } else {
        unreachable!()
    }
}

pub fn gen_with<'ctx, 'a, G: CodeGenerator + ?Sized>(
    _: &mut G,
    _: &mut CodeGenContext<'ctx, 'a>,
    _: &Stmt<Option<Type>>,
) -> bool {
    // TODO: Implement with statement after finishing exceptions
    unimplemented!()
}

pub fn gen_stmt<'ctx, 'a, G: CodeGenerator + ?Sized>(
    generator: &mut G,
    ctx: &mut CodeGenContext<'ctx, 'a>,
    stmt: &Stmt<Option<Type>>,
) -> bool {
    match &stmt.node {
        StmtKind::Pass { .. }  => {}
        StmtKind::Expr { value, .. } => {
            generator.gen_expr(ctx, value);
        }
        StmtKind::Return { value, .. } => {
            let value = value.as_ref().map(|v| generator.gen_expr(ctx, v).unwrap());
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
                generator.gen_assign(ctx, target, value);
            }
        }
        StmtKind::Continue { .. } => {
            ctx.builder.build_unconditional_branch(ctx.loop_bb.unwrap().0);
            return true;
        }
        StmtKind::Break { .. }=> {
            ctx.builder.build_unconditional_branch(ctx.loop_bb.unwrap().1);
            return true;
        }
        StmtKind::If { .. } => return generator.gen_if(ctx, stmt),
        StmtKind::While { .. } => return generator.gen_while(ctx, stmt),
        StmtKind::For { .. } => return generator.gen_for(ctx, stmt),
        StmtKind::With { .. } => return generator.gen_with(ctx, stmt),
        StmtKind::AugAssign { target, op, value, .. } => {
            let value = {
                let ty1 = ctx.unifier.get_representative(target.custom.unwrap());
                let ty2 = ctx.unifier.get_representative(value.custom.unwrap());
                let left = generator.gen_expr(ctx, target).unwrap();
                let right = generator.gen_expr(ctx, value).unwrap();

                // we can directly compare the types, because we've got their representatives
                // which would be unchanged until further unification, which we would never do
                // when doing code generation for function instances
                if ty1 == ty2 && [ctx.primitives.int32, ctx.primitives.int64].contains(&ty1) {
                    ctx.gen_int_ops(op, left, right)
                } else if ty1 == ty2 && ctx.primitives.float == ty1 {
                    ctx.gen_float_ops(op, left, right)
                } else {
                    unimplemented!()
                }
            };
            generator.gen_assign(ctx, target, value);
        }
        _ => unimplemented!(),
    };
    false
}
