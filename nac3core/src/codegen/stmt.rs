use super::{
    super::symbol_resolver::ValueEnum,
    expr::destructure_range,
    irrt::{handle_slice_indices, list_slice_assignment},
    CodeGenContext, CodeGenerator,
};
use crate::{
    codegen::expr::gen_binop_expr,
    toplevel::{DefinitionId, TopLevelDef},
    typecheck::typedef::{Type, TypeEnum, FunSignature}
};
use inkwell::{
    attributes::{Attribute, AttributeLoc},
    basic_block::BasicBlock,
    types::BasicTypeEnum,
    values::{BasicValue, BasicValueEnum, FunctionValue, PointerValue},
    IntPredicate::EQ,
};
use nac3parser::ast::{ExcepthandlerKind, Expr, ExprKind, Location, Stmt, StmtKind, StrRef, Constant};
use std::convert::TryFrom;

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
    match &target.node {
        ExprKind::Tuple { elts, .. } => {
            if let BasicValueEnum::StructValue(v) = value.to_basic_value_enum(ctx, generator) {
                for (i, elt) in elts.iter().enumerate() {
                    let v = ctx
                        .builder
                        .build_extract_value(v, u32::try_from(i).unwrap(), "struct_elem")
                        .unwrap();
                    generator.gen_assign(ctx, elt, v.into());
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
        let loop_bb = ctx.loop_target.replace((test_bb, cont_bb));

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

        gen_block(generator, ctx, body.iter());
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
            gen_block(generator, ctx, orelse.iter());
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
        let loop_bb = ctx.loop_target.replace((test_bb, cont_bb));
        ctx.builder.build_unconditional_branch(test_bb);
        ctx.builder.position_at_end(test_bb);
        let test = generator.gen_expr(ctx, test).unwrap().to_basic_value_enum(ctx, generator);
        if let BasicValueEnum::IntValue(test) = test {
            ctx.builder.build_conditional_branch(test, body_bb, orelse_bb);
        } else {
            unreachable!()
        };
        ctx.builder.position_at_end(body_bb);
        gen_block(generator, ctx, body.iter());
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
            gen_block(generator, ctx, orelse.iter());
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
}

pub fn gen_if<'ctx, 'a, G: CodeGenerator>(
    generator: &mut G,
    ctx: &mut CodeGenContext<'ctx, 'a>,
    stmt: &Stmt<Option<Type>>,
) {
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
        gen_block(generator, ctx, body.iter());
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
            gen_block(generator, ctx, orelse.iter());
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
}

pub fn exn_constructor<'ctx, 'a>(
    ctx: &mut CodeGenContext<'ctx, 'a>,
    obj: Option<(Type, ValueEnum<'ctx>)>,
    _fun: (&FunSignature, DefinitionId),
    mut args: Vec<(Option<StrRef>, ValueEnum<'ctx>)>,
    generator: &mut dyn CodeGenerator
) -> Option<BasicValueEnum<'ctx>> {
    let (zelf_ty, zelf) = obj.unwrap();
    let zelf = zelf.to_basic_value_enum(ctx, generator).into_pointer_value();
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
    let zelf_name = if let TopLevelDef::Class { name, .. } = &*def {
        *name
    } else {
        unreachable!()
    };
    let exception_name = format!("0:{}", zelf_name);
    unsafe {
        let id_ptr = ctx.builder.build_in_bounds_gep(zelf, &[zero, zero], "exn.id");
        let id = ctx.resolver.get_string_id(&exception_name);
        ctx.builder.build_store(id_ptr, int32.const_int(id as u64, false));
        let empty_string = ctx.gen_const(generator, &Constant::Str("".into()), ctx.primitives.str);
        let ptr = ctx.builder.build_in_bounds_gep(
            zelf, &[zero, int32.const_int(5, false)], "exn.msg");
        let msg = if !args.is_empty() {
            args.remove(0).1.to_basic_value_enum(ctx, generator)
        } else {
            empty_string
        };
        ctx.builder.build_store(ptr, msg);
        for i in [6, 7, 8].iter() {
            let value = if !args.is_empty() {
                args.remove(0).1.to_basic_value_enum(ctx, generator)
            } else {
                ctx.ctx.i64_type().const_zero().into()
            };
            let ptr = ctx.builder.build_in_bounds_gep(
                zelf, &[zero, int32.const_int(*i, false)], "exn.param");
            ctx.builder.build_store(ptr, value);
        }
        // set file, func to empty string
        for i in [1, 4].iter() {
            let ptr = ctx.builder.build_in_bounds_gep(
                zelf, &[zero, int32.const_int(*i, false)], "exn.str");
            ctx.builder.build_store(ptr, empty_string);
        }
        // set ints to zero
        for i in [2, 3].iter() {
            let ptr = ctx.builder.build_in_bounds_gep(
                zelf, &[zero, int32.const_int(*i, false)], "exn.ints");
            ctx.builder.build_store(ptr, zero);
        }
    }
    Some(zelf.into())
}

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

pub fn gen_return<'ctx, 'a, G: CodeGenerator>(
    generator: &mut G,
    ctx: &mut CodeGenContext<'ctx, 'a>,
    value: &Option<Box<Expr<Option<Type>>>>,
) {
    let value = value
        .as_ref()
        .map(|v| generator.gen_expr(ctx, v).unwrap().to_basic_value_enum(ctx, generator));
    if let Some(return_target) = ctx.return_target {
        if let Some(value) = value {
            ctx.builder.build_store(ctx.return_buffer.unwrap(), value);
        }
        ctx.builder.build_unconditional_branch(return_target);
    } else {
        let value = value.as_ref().map(|v| v as &dyn BasicValue);
        ctx.builder.build_return(value);
    }
}

pub fn gen_stmt<'ctx, 'a, G: CodeGenerator>(
    generator: &mut G,
    ctx: &mut CodeGenContext<'ctx, 'a>,
    stmt: &Stmt<Option<Type>>,
) {
    match &stmt.node {
        StmtKind::Pass { .. } => {}
        StmtKind::Expr { value, .. } => {
            generator.gen_expr(ctx, value);
        }
        StmtKind::Return { value, .. } => {
            gen_return(generator, ctx, value);
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
            ctx.builder.build_unconditional_branch(ctx.loop_target.unwrap().0);
        }
        StmtKind::Break { .. } => {
            ctx.builder.build_unconditional_branch(ctx.loop_target.unwrap().1);
        }
        StmtKind::If { .. } => generator.gen_if(ctx, stmt),
        StmtKind::While { .. } => generator.gen_while(ctx, stmt),
        StmtKind::For { .. } => generator.gen_for(ctx, stmt),
        StmtKind::With { .. } => generator.gen_with(ctx, stmt),
        StmtKind::AugAssign { target, op, value, .. } => {
            let value = gen_binop_expr(generator, ctx, target, op, value);
            generator.gen_assign(ctx, target, value);
        }
        _ => unimplemented!(),
    };
    false
pub fn gen_block<'ctx, 'a, 'b, G: CodeGenerator, I: Iterator<Item = &'b Stmt<Option<Type>>>>(
    generator: &mut G,
    ctx: &mut CodeGenContext<'ctx, 'a>,
    stmts: I,
) {
    for stmt in stmts {
        generator.gen_stmt(ctx, stmt);
        if ctx.is_terminated() {
            break;
        }
    }
}
