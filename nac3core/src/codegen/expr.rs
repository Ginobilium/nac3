use std::{collections::HashMap, convert::TryInto, iter::once};

use crate::{
    codegen::{get_llvm_type, CodeGenContext, CodeGenTask},
    symbol_resolver::SymbolValue,
    toplevel::{DefinitionId, TopLevelDef},
    typecheck::typedef::{FunSignature, FuncArg, Type, TypeEnum, Unifier},
};
use inkwell::{
    types::{BasicType, BasicTypeEnum},
    values::{BasicValueEnum, IntValue, PointerValue},
    AddressSpace,
};
use itertools::{chain, izip, zip, Itertools};
use rustpython_parser::ast::{self, Boolop, Constant, Expr, ExprKind, Operator, StrRef};

use super::CodeGenerator;

pub fn assert_int_val(val: BasicValueEnum<'_>) -> IntValue<'_> {
    if let BasicValueEnum::IntValue(v) = val {
        v
    } else {
        unreachable!()
    }
}

pub fn assert_pointer_val(val: BasicValueEnum<'_>) -> PointerValue<'_> {
    if let BasicValueEnum::PointerValue(v) = val {
        v
    } else {
        unreachable!()
    }
}

impl<'ctx, 'a> CodeGenContext<'ctx, 'a> {
    fn get_subst_key(
        &mut self,
        obj: Option<Type>,
        fun: &FunSignature,
        filter: Option<&Vec<u32>>,
    ) -> String {
        let mut vars = obj
            .map(|ty| {
                if let TypeEnum::TObj { params, .. } = &*self.unifier.get_ty(ty) {
                    params.borrow().clone()
                } else {
                    unreachable!()
                }
            })
            .unwrap_or_default();
        vars.extend(fun.vars.iter());
        let sorted =
            vars.keys().filter(|id| filter.map(|v| v.contains(id)).unwrap_or(true)).sorted();
        sorted
            .map(|id| {
                self.unifier.stringify(vars[id], &mut |id| id.to_string(), &mut |id| id.to_string())
            })
            .join(", ")
    }

    pub fn get_attr_index(&mut self, ty: Type, attr: StrRef) -> usize {
        let obj_id = match &*self.unifier.get_ty(ty) {
            TypeEnum::TObj { obj_id, .. } => *obj_id,
            // we cannot have other types, virtual type should be handled by function calls
            _ => unreachable!(),
        };
        let def = &self.top_level.definitions.read()[obj_id.0];
        let index = if let TopLevelDef::Class { fields, .. } = &*def.read() {
            fields.iter().find_position(|x| x.0 == attr).unwrap().0
        } else {
            unreachable!()
        };
        index
    }

    fn gen_symbol_val(&mut self, val: &SymbolValue) -> BasicValueEnum<'ctx> {
        match val {
            SymbolValue::I32(v) => self.ctx.i32_type().const_int(*v as u64, true).into(),
            SymbolValue::I64(v) => self.ctx.i64_type().const_int(*v as u64, true).into(),
            SymbolValue::Bool(v) => self.ctx.bool_type().const_int(*v as u64, true).into(),
            SymbolValue::Double(v) => self.ctx.f64_type().const_float(*v).into(),
            SymbolValue::Tuple(ls) => {
                let vals = ls.iter().map(|v| self.gen_symbol_val(v)).collect_vec();
                let fields = vals.iter().map(|v| v.get_type()).collect_vec();
                let ty = self.ctx.struct_type(&fields, false);
                let ptr = self.builder.build_alloca(ty, "tuple");
                let zero = self.ctx.i32_type().const_zero();
                unsafe {
                    for (i, val) in vals.into_iter().enumerate() {
                        let p = self.builder.build_in_bounds_gep(
                            ptr,
                            &[zero, self.ctx.i32_type().const_int(i as u64, false)],
                            "elemptr",
                        );
                        self.builder.build_store(p, val);
                    }
                }
                ptr.into()
            }
        }
    }

    pub fn get_llvm_type(&mut self, ty: Type) -> BasicTypeEnum<'ctx> {
        get_llvm_type(self.ctx, &mut self.unifier, self.top_level, &mut self.type_cache, ty)
    }

    fn gen_const(&mut self, value: &Constant, ty: Type) -> BasicValueEnum<'ctx> {
        match value {
            Constant::Bool(v) => {
                assert!(self.unifier.unioned(ty, self.primitives.bool));
                let ty = self.ctx.bool_type();
                ty.const_int(if *v { 1 } else { 0 }, false).into()
            }
            Constant::Int(v) => {
                let ty = if self.unifier.unioned(ty, self.primitives.int32) {
                    self.ctx.i32_type()
                } else if self.unifier.unioned(ty, self.primitives.int64) {
                    self.ctx.i64_type()
                } else {
                    unreachable!();
                };
                let val: i64 = v.try_into().unwrap();
                ty.const_int(val as u64, false).into()
            }
            Constant::Float(v) => {
                assert!(self.unifier.unioned(ty, self.primitives.float));
                let ty = self.ctx.f64_type();
                ty.const_float(*v).into()
            }
            Constant::Tuple(v) => {
                let ty = self.unifier.get_ty(ty);
                let types =
                    if let TypeEnum::TTuple { ty } = &*ty { ty.clone() } else { unreachable!() };
                let values = zip(types.into_iter(), v.iter())
                    .map(|(ty, v)| self.gen_const(v, ty))
                    .collect_vec();
                let types = values.iter().map(BasicValueEnum::get_type).collect_vec();
                let ty = self.ctx.struct_type(&types, false);
                ty.const_named_struct(&values).into()
            }
            _ => unreachable!(),
        }
    }

    fn gen_int_ops(
        &mut self,
        op: &Operator,
        lhs: BasicValueEnum<'ctx>,
        rhs: BasicValueEnum<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let (lhs, rhs) =
            if let (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) = (lhs, rhs) {
                (lhs, rhs)
            } else {
                unreachable!()
            };
        match op {
            Operator::Add => self.builder.build_int_add(lhs, rhs, "add").into(),
            Operator::Sub => self.builder.build_int_sub(lhs, rhs, "sub").into(),
            Operator::Mult => self.builder.build_int_mul(lhs, rhs, "mul").into(),
            Operator::Div => {
                let float = self.ctx.f64_type();
                let left = self.builder.build_signed_int_to_float(lhs, float, "i2f");
                let right = self.builder.build_signed_int_to_float(rhs, float, "i2f");
                self.builder.build_float_div(left, right, "fdiv").into()
            }
            Operator::Mod => self.builder.build_int_signed_rem(lhs, rhs, "mod").into(),
            Operator::BitOr => self.builder.build_or(lhs, rhs, "or").into(),
            Operator::BitXor => self.builder.build_xor(lhs, rhs, "xor").into(),
            Operator::BitAnd => self.builder.build_and(lhs, rhs, "and").into(),
            Operator::LShift => self.builder.build_left_shift(lhs, rhs, "lshift").into(),
            Operator::RShift => self.builder.build_right_shift(lhs, rhs, true, "rshift").into(),
            Operator::FloorDiv => self.builder.build_int_signed_div(lhs, rhs, "floordiv").into(),
            // special implementation?
            Operator::Pow => unimplemented!(),
            Operator::MatMult => unreachable!(),
        }
    }

    fn gen_float_ops(
        &mut self,
        op: &Operator,
        lhs: BasicValueEnum<'ctx>,
        rhs: BasicValueEnum<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let (lhs, rhs) = if let (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) =
            (lhs, rhs)
        {
            (lhs, rhs)
        } else {
            unreachable!()
        };
        match op {
            Operator::Add => self.builder.build_float_add(lhs, rhs, "fadd").into(),
            Operator::Sub => self.builder.build_float_sub(lhs, rhs, "fsub").into(),
            Operator::Mult => self.builder.build_float_mul(lhs, rhs, "fmul").into(),
            Operator::Div => self.builder.build_float_div(lhs, rhs, "fdiv").into(),
            Operator::Mod => self.builder.build_float_rem(lhs, rhs, "fmod").into(),
            Operator::FloorDiv => {
                let div = self.builder.build_float_div(lhs, rhs, "fdiv");
                let floor_intrinsic =
                    self.module.get_function("llvm.floor.f64").unwrap_or_else(|| {
                        let float = self.ctx.f64_type();
                        let fn_type = float.fn_type(&[float.into()], false);
                        self.module.add_function("llvm.floor.f64", fn_type, None)
                    });
                self.builder
                    .build_call(floor_intrinsic, &[div.into()], "floor")
                    .try_as_basic_value()
                    .left()
                    .unwrap()
            }
            // special implementation?
            _ => unimplemented!(),
        }
    }
}

pub fn gen_constructor<'ctx, 'a, G: CodeGenerator + ?Sized>(
    generator: &mut G,
    ctx: &mut CodeGenContext<'ctx, 'a>,
    signature: &FunSignature,
    def: &TopLevelDef,
    params: Vec<(Option<StrRef>, BasicValueEnum<'ctx>)>,
) -> BasicValueEnum<'ctx> {
    match def {
        TopLevelDef::Class { methods, .. } => {
            // TODO: what about other fields that require alloca?
            let mut fun_id = None;
            for (name, _, id) in methods.iter() {
                if name == &"__init__".into() {
                    fun_id = Some(*id);
                }
            }
            let ty = ctx.get_llvm_type(signature.ret).into_pointer_type();
            let zelf_ty: BasicTypeEnum = ty.get_element_type().try_into().unwrap();
            let zelf = ctx.builder.build_alloca(zelf_ty, "alloca").into();
            // call `__init__` if there is one
            if let Some(fun_id) = fun_id {
                let mut sign = signature.clone();
                sign.ret = ctx.primitives.none;
                generator.gen_call(ctx, Some((signature.ret, zelf)), (&sign, fun_id), params);
            }
            zelf
        }
        _ => unreachable!(),
    }
}

pub fn gen_func_instance<'ctx, 'a>(
    ctx: &mut CodeGenContext<'ctx, 'a>,
    obj: Option<(Type, BasicValueEnum<'ctx>)>,
    fun: (&FunSignature, &mut TopLevelDef, String),
) -> String {
    if let (
        sign,
        TopLevelDef::Function {
            name, instance_to_symbol, instance_to_stmt, var_id, resolver, ..
        },
        key,
    ) = fun
    {
        instance_to_symbol.get(&key).cloned().unwrap_or_else(|| {
            let symbol = format!("{}.{}", name, instance_to_symbol.len());
            instance_to_symbol.insert(key, symbol.clone());
            let key = ctx.get_subst_key(obj.map(|a| a.0), sign, Some(var_id));
            let instance = instance_to_stmt.get(&key).unwrap();
            let unifiers = ctx.top_level.unifiers.read();
            let (unifier, primitives) = &unifiers[instance.unifier_id];
            let mut unifier = Unifier::from_shared_unifier(unifier);

            let mut type_cache = [
                (ctx.primitives.int32, primitives.int32),
                (ctx.primitives.int64, primitives.int64),
                (ctx.primitives.float, primitives.float),
                (ctx.primitives.bool, primitives.bool),
                (ctx.primitives.none, primitives.none),
            ]
            .iter()
            .map(|(a, b)| (ctx.unifier.get_representative(*a), unifier.get_representative(*b)))
            .collect();

            let subst = sign
                .vars
                .iter()
                .map(|(id, ty)| {
                    (
                        *instance.subst.get(id).unwrap(),
                        unifier.copy_from(&mut ctx.unifier, *ty, &mut type_cache),
                    )
                })
                .collect();

            let mut signature = FunSignature {
                args: sign
                    .args
                    .iter()
                    .map(|arg| FuncArg {
                        name: arg.name,
                        ty: unifier.copy_from(&mut ctx.unifier, arg.ty, &mut type_cache),
                        default_value: arg.default_value.clone(),
                    })
                    .collect(),
                ret: unifier.copy_from(&mut ctx.unifier, sign.ret, &mut type_cache),
                vars: sign
                    .vars
                    .iter()
                    .map(|(id, ty)| {
                        (*id, unifier.copy_from(&mut ctx.unifier, *ty, &mut type_cache))
                    })
                    .collect(),
            };

            if let Some(obj) = &obj {
                signature
                    .args
                    .insert(0, FuncArg { name: "self".into(), ty: obj.0, default_value: None });
            }

            let unifier = (unifier.get_shared_unifier(), *primitives);

            ctx.registry.add_task(CodeGenTask {
                symbol_name: symbol.clone(),
                body: instance.body.clone(),
                resolver: resolver.as_ref().unwrap().clone(),
                calls: instance.calls.clone(),
                subst,
                signature,
                unifier,
            });
            symbol
        })
    } else {
        unreachable!()
    }
}

pub fn gen_call<'ctx, 'a, G: CodeGenerator + ?Sized>(
    generator: &mut G,
    ctx: &mut CodeGenContext<'ctx, 'a>,
    obj: Option<(Type, BasicValueEnum<'ctx>)>,
    fun: (&FunSignature, DefinitionId),
    params: Vec<(Option<StrRef>, BasicValueEnum<'ctx>)>,
) -> Option<BasicValueEnum<'ctx>> {
    let definition = ctx.top_level.definitions.read().get(fun.1 .0).cloned().unwrap();
    let key = ctx.get_subst_key(obj.map(|a| a.0), fun.0, None);
    let symbol = {
        // make sure this lock guard is dropped at the end of this scope...
        let def = definition.read();
        match &*def {
            TopLevelDef::Function { instance_to_symbol, codegen_callback, .. } => {
                if let Some(callback) = codegen_callback {
                    return callback.run(ctx, obj, fun, params);
                }
                instance_to_symbol.get(&key).cloned()
            }
            TopLevelDef::Class { .. } => {
                return Some(generator.gen_constructor(ctx, fun.0, &*def, params))
            }
        }
    }
    .unwrap_or_else(|| {
        generator.gen_func_instance(ctx, obj, (fun.0, &mut *definition.write(), key))
    });
    let fun_val = ctx.module.get_function(&symbol).unwrap_or_else(|| {
        let mut args = fun.0.args.clone();
        if let Some(obj) = &obj {
            args.insert(0, FuncArg { name: "self".into(), ty: obj.0, default_value: None });
        }
        let params = args.iter().map(|arg| ctx.get_llvm_type(arg.ty)).collect_vec();
        let fun_ty = if ctx.unifier.unioned(fun.0.ret, ctx.primitives.none) {
            ctx.ctx.void_type().fn_type(&params, false)
        } else {
            ctx.get_llvm_type(fun.0.ret).fn_type(&params, false)
        };
        ctx.module.add_function(&symbol, fun_ty, None)
    });
    let mut keys = fun.0.args.clone();
    let mut mapping = HashMap::new();
    for (key, value) in params.into_iter() {
        mapping.insert(key.unwrap_or_else(|| keys.remove(0).name), value);
    }
    // default value handling
    for k in keys.into_iter() {
        mapping.insert(k.name, ctx.gen_symbol_val(&k.default_value.unwrap()));
    }
    // reorder the parameters
    let mut params = fun.0.args.iter().map(|arg| mapping.remove(&arg.name).unwrap()).collect_vec();
    if let Some(obj) = obj {
        params.insert(0, obj.1);
    }
    ctx.builder.build_call(fun_val, &params, "call").try_as_basic_value().left()
}

pub fn gen_expr<'ctx, 'a, G: CodeGenerator + ?Sized>(
    generator: &mut G,
    ctx: &mut CodeGenContext<'ctx, 'a>,
    expr: &Expr<Option<Type>>,
) -> Option<BasicValueEnum<'ctx>> {
    let zero = ctx.ctx.i32_type().const_int(0, false);
    Some(match &expr.node {
        ExprKind::Constant { value, .. } => {
            let ty = expr.custom.unwrap();
            ctx.gen_const(value, ty)
        }
        ExprKind::Name { id, .. } => {
            let ptr = ctx.var_assignment.get(id);
            if let Some(ptr) = ptr {
                ctx.builder.build_load(*ptr, "load")
            } else {
                let resolver = ctx.resolver.clone();
                resolver.get_symbol_value(*id, ctx).unwrap()
            }
        }
        ExprKind::List { elts, .. } => {
            // this shall be optimized later for constant primitive lists...
            // we should use memcpy for that instead of generating thousands of stores
            let elements = elts.iter().map(|x| generator.gen_expr(ctx, x).unwrap()).collect_vec();
            let ty = if elements.is_empty() {
                ctx.ctx.i32_type().into()
            } else {
                elements[0].get_type()
            };
            let arr_ptr = ctx.builder.build_array_alloca(
                ty,
                ctx.ctx.i32_type().const_int(elements.len() as u64, false),
                "tmparr",
            );
            let arr_ty = ctx.ctx.struct_type(
                &[ctx.ctx.i32_type().into(), ty.ptr_type(AddressSpace::Generic).into()],
                false,
            );
            let arr_str_ptr = ctx.builder.build_alloca(arr_ty, "tmparrstr");
            unsafe {
                let len_ptr =
                    ctx.builder.build_in_bounds_gep(arr_str_ptr, &[zero, zero], "len_ptr");
                ctx.builder.build_store(
                    len_ptr,
                    ctx.ctx.i32_type().const_int(elements.len() as u64, false),
                );
                let ptr_to_arr = ctx.builder.build_in_bounds_gep(
                    arr_str_ptr,
                    &[zero, ctx.ctx.i32_type().const_int(1, false)],
                    "ptr_to_arr",
                );
                ctx.builder.build_store(ptr_to_arr, arr_ptr);
                let i32_type = ctx.ctx.i32_type();
                for (i, v) in elements.iter().enumerate() {
                    let elem_ptr = ctx.builder.build_in_bounds_gep(
                        arr_ptr,
                        &[i32_type.const_int(i as u64, false)],
                        "elem_ptr",
                    );
                    ctx.builder.build_store(elem_ptr, *v);
                }
            }
            arr_str_ptr.into()
        }
        ExprKind::Tuple { elts, .. } => {
            let element_val =
                elts.iter().map(|x| generator.gen_expr(ctx, x).unwrap()).collect_vec();
            let element_ty = element_val.iter().map(BasicValueEnum::get_type).collect_vec();
            let tuple_ty = ctx.ctx.struct_type(&element_ty, false);
            let tuple_ptr = ctx.builder.build_alloca(tuple_ty, "tuple");
            for (i, v) in element_val.into_iter().enumerate() {
                unsafe {
                    let ptr = ctx.builder.build_in_bounds_gep(
                        tuple_ptr,
                        &[zero, ctx.ctx.i32_type().const_int(i as u64, false)],
                        "ptr",
                    );
                    ctx.builder.build_store(ptr, v);
                }
            }
            tuple_ptr.into()
        }
        ExprKind::Attribute { value, attr, .. } => {
            // note that we would handle class methods directly in calls
            let index = ctx.get_attr_index(value.custom.unwrap(), *attr);
            let val = generator.gen_expr(ctx, value).unwrap();
            let ptr = assert_pointer_val(val);
            unsafe {
                let ptr = ctx.builder.build_in_bounds_gep(
                    ptr,
                    &[zero, ctx.ctx.i32_type().const_int(index as u64, false)],
                    "attr",
                );
                ctx.builder.build_load(ptr, "field")
            }
        }
        ExprKind::BoolOp { op, values } => {
            // requires conditional branches for short-circuiting...
            let left = assert_int_val(generator.gen_expr(ctx, &values[0]).unwrap());
            let current = ctx.builder.get_insert_block().unwrap().get_parent().unwrap();
            let a_bb = ctx.ctx.append_basic_block(current, "a");
            let b_bb = ctx.ctx.append_basic_block(current, "b");
            let cont_bb = ctx.ctx.append_basic_block(current, "cont");
            ctx.builder.build_conditional_branch(left, a_bb, b_bb);
            let (a, b) = match op {
                Boolop::Or => {
                    ctx.builder.position_at_end(a_bb);
                    let a = ctx.ctx.bool_type().const_int(1, false);
                    ctx.builder.build_unconditional_branch(cont_bb);
                    ctx.builder.position_at_end(b_bb);
                    let b = assert_int_val(generator.gen_expr(ctx, &values[1]).unwrap());
                    ctx.builder.build_unconditional_branch(cont_bb);
                    (a, b)
                }
                Boolop::And => {
                    ctx.builder.position_at_end(a_bb);
                    let a = assert_int_val(generator.gen_expr(ctx, &values[1]).unwrap());
                    ctx.builder.build_unconditional_branch(cont_bb);
                    ctx.builder.position_at_end(b_bb);
                    let b = ctx.ctx.bool_type().const_int(0, false);
                    ctx.builder.build_unconditional_branch(cont_bb);
                    (a, b)
                }
            };
            ctx.builder.position_at_end(cont_bb);
            let phi = ctx.builder.build_phi(ctx.ctx.bool_type(), "phi");
            phi.add_incoming(&[(&a, a_bb), (&b, b_bb)]);
            phi.as_basic_value()
        }
        ExprKind::BinOp { op, left, right } => {
            let ty1 = ctx.unifier.get_representative(left.custom.unwrap());
            let ty2 = ctx.unifier.get_representative(right.custom.unwrap());
            let left = generator.gen_expr(ctx, left).unwrap();
            let right = generator.gen_expr(ctx, right).unwrap();

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
        }
        ExprKind::UnaryOp { op, operand } => {
            let ty = ctx.unifier.get_representative(operand.custom.unwrap());
            let val = generator.gen_expr(ctx, operand).unwrap();
            if ty == ctx.primitives.bool {
                let val = assert_int_val(val);
                match op {
                    ast::Unaryop::Invert | ast::Unaryop::Not => {
                        ctx.builder.build_not(val, "not").into()
                    }
                    _ => val.into(),
                }
            } else if [ctx.primitives.int32, ctx.primitives.int64].contains(&ty) {
                let val = assert_int_val(val);
                match op {
                    ast::Unaryop::USub => ctx.builder.build_int_neg(val, "neg").into(),
                    ast::Unaryop::Invert => ctx.builder.build_not(val, "not").into(),
                    ast::Unaryop::Not => ctx
                        .builder
                        .build_int_compare(
                            inkwell::IntPredicate::EQ,
                            val,
                            val.get_type().const_zero(),
                            "not",
                        )
                        .into(),
                    _ => val.into(),
                }
            } else if ty == ctx.primitives.float {
                let val =
                    if let BasicValueEnum::FloatValue(val) = val { val } else { unreachable!() };
                match op {
                    ast::Unaryop::USub => ctx.builder.build_float_neg(val, "neg").into(),
                    ast::Unaryop::Not => ctx
                        .builder
                        .build_float_compare(
                            inkwell::FloatPredicate::OEQ,
                            val,
                            val.get_type().const_zero(),
                            "not",
                        )
                        .into(),
                    _ => val.into(),
                }
            } else {
                unimplemented!()
            }
        }
        ExprKind::Compare { left, ops, comparators } => {
            izip!(chain(once(left.as_ref()), comparators.iter()), comparators.iter(), ops.iter(),)
                .fold(None, |prev, (lhs, rhs, op)| {
                    let ty = ctx.unifier.get_representative(lhs.custom.unwrap());
                    let current =
                        if [ctx.primitives.int32, ctx.primitives.int64, ctx.primitives.bool]
                            .contains(&ty)
                        {
                            let (lhs, rhs) = if let (
                                BasicValueEnum::IntValue(lhs),
                                BasicValueEnum::IntValue(rhs),
                            ) = (
                                generator.gen_expr(ctx, lhs).unwrap(),
                                generator.gen_expr(ctx, rhs).unwrap(),
                            ) {
                                (lhs, rhs)
                            } else {
                                unreachable!()
                            };
                            let op = match op {
                                ast::Cmpop::Eq | ast::Cmpop::Is => inkwell::IntPredicate::EQ,
                                ast::Cmpop::NotEq => inkwell::IntPredicate::NE,
                                ast::Cmpop::Lt => inkwell::IntPredicate::SLT,
                                ast::Cmpop::LtE => inkwell::IntPredicate::SLE,
                                ast::Cmpop::Gt => inkwell::IntPredicate::SGT,
                                ast::Cmpop::GtE => inkwell::IntPredicate::SGE,
                                _ => unreachable!(),
                            };
                            ctx.builder.build_int_compare(op, lhs, rhs, "cmp")
                        } else if ty == ctx.primitives.float {
                            let (lhs, rhs) = if let (
                                BasicValueEnum::FloatValue(lhs),
                                BasicValueEnum::FloatValue(rhs),
                            ) = (
                                generator.gen_expr(ctx, lhs).unwrap(),
                                generator.gen_expr(ctx, rhs).unwrap(),
                            ) {
                                (lhs, rhs)
                            } else {
                                unreachable!()
                            };
                            let op = match op {
                                ast::Cmpop::Eq | ast::Cmpop::Is => inkwell::FloatPredicate::OEQ,
                                ast::Cmpop::NotEq => inkwell::FloatPredicate::ONE,
                                ast::Cmpop::Lt => inkwell::FloatPredicate::OLT,
                                ast::Cmpop::LtE => inkwell::FloatPredicate::OLE,
                                ast::Cmpop::Gt => inkwell::FloatPredicate::OGT,
                                ast::Cmpop::GtE => inkwell::FloatPredicate::OGE,
                                _ => unreachable!(),
                            };
                            ctx.builder.build_float_compare(op, lhs, rhs, "cmp")
                        } else {
                            unimplemented!()
                        };
                    prev.map(|v| ctx.builder.build_and(v, current, "cmp")).or(Some(current))
                })
                .unwrap()
                .into() // as there should be at least 1 element, it should never be none
        }
        ExprKind::IfExp { test, body, orelse } => {
            let test = assert_int_val(generator.gen_expr(ctx, test).unwrap());
            let current = ctx.builder.get_insert_block().unwrap().get_parent().unwrap();
            let then_bb = ctx.ctx.append_basic_block(current, "then");
            let else_bb = ctx.ctx.append_basic_block(current, "else");
            let cont_bb = ctx.ctx.append_basic_block(current, "cont");
            ctx.builder.build_conditional_branch(test, then_bb, else_bb);
            ctx.builder.position_at_end(then_bb);
            let a = generator.gen_expr(ctx, body).unwrap();
            ctx.builder.build_unconditional_branch(cont_bb);
            ctx.builder.position_at_end(else_bb);
            let b = generator.gen_expr(ctx, orelse).unwrap();
            ctx.builder.build_unconditional_branch(cont_bb);
            ctx.builder.position_at_end(cont_bb);
            let phi = ctx.builder.build_phi(a.get_type(), "ifexpr");
            phi.add_incoming(&[(&a, then_bb), (&b, else_bb)]);
            phi.as_basic_value()
        }
        ExprKind::Call { func, args, keywords } => {
            let mut params =
                args.iter().map(|arg| (None, generator.gen_expr(ctx, arg).unwrap())).collect_vec();
            let kw_iter = keywords.iter().map(|kw| {
                (
                    Some(*kw.node.arg.as_ref().unwrap()),
                    generator.gen_expr(ctx, &kw.node.value).unwrap(),
                )
            });
            params.extend(kw_iter);
            let call = ctx.calls.get(&expr.location.into());
            let signature = match call {
                Some(call) => ctx.unifier.get_call_signature(*call).unwrap(),
                None => {
                    let ty = func.custom.unwrap();
                    if let TypeEnum::TFunc(sign) = &*ctx.unifier.get_ty(ty) {
                        sign.borrow().clone()
                    } else {
                        unreachable!()
                    }
                }
            };
            match &func.as_ref().node {
                ExprKind::Name { id, .. } => {
                    // TODO: handle primitive casts and function pointers
                    let fun = ctx.resolver.get_identifier_def(*id).expect("Unknown identifier");
                    return generator.gen_call(ctx, None, (&signature, fun), params);
                }
                ExprKind::Attribute { value, attr, .. } => {
                    let val = generator.gen_expr(ctx, value).unwrap();
                    let id = if let TypeEnum::TObj { obj_id, .. } =
                        &*ctx.unifier.get_ty(value.custom.unwrap())
                    {
                        *obj_id
                    } else {
                        unreachable!()
                    };
                    let fun_id = {
                        let defs = ctx.top_level.definitions.read();
                        let obj_def = defs.get(id.0).unwrap().read();
                        if let TopLevelDef::Class { methods, .. } = &*obj_def {
                            let mut fun_id = None;
                            for (name, _, id) in methods.iter() {
                                if name == attr {
                                    fun_id = Some(*id);
                                }
                            }
                            fun_id.unwrap()
                        } else {
                            unreachable!()
                        }
                    };
                    return generator.gen_call(
                        ctx,
                        Some((value.custom.unwrap(), val)),
                        (&signature, fun_id),
                        params,
                    );
                }
                _ => unimplemented!(),
            }
        }
        ExprKind::Subscript { value, slice, .. } => {
            if let TypeEnum::TList { .. } = &*ctx.unifier.get_ty(value.custom.unwrap()) {
                if let ExprKind::Slice { .. } = slice.node {
                    unimplemented!()
                } else {
                    // TODO: bound check
                    let i32_type = ctx.ctx.i32_type();
                    let v = assert_pointer_val(generator.gen_expr(ctx, value).unwrap());
                    let index = assert_int_val(generator.gen_expr(ctx, slice).unwrap());
                    unsafe {
                        let ptr_to_arr = ctx.builder.build_in_bounds_gep(
                            v,
                            &[i32_type.const_zero(), i32_type.const_int(1, false)],
                            "ptr_to_arr",
                        );
                        let arr_ptr =
                            assert_pointer_val(ctx.builder.build_load(ptr_to_arr, "loadptr"));
                        let ptr = ctx.builder.build_gep(arr_ptr, &[index], "loadarrgep");
                        ctx.builder.build_load(ptr, "loadarr")
                    }
                }
            } else {
                let i32_type = ctx.ctx.i32_type();
                let v = assert_pointer_val(generator.gen_expr(ctx, value).unwrap());
                let index = assert_int_val(generator.gen_expr(ctx, slice).unwrap());
                unsafe {
                    let ptr_to_elem = ctx.builder.build_in_bounds_gep(
                        v,
                        &[i32_type.const_zero(), index],
                        "ptr_to_elem",
                    );
                    ctx.builder.build_load(ptr_to_elem, "loadelem")
                }
            }
        }
        _ => unimplemented!(),
    })
}
