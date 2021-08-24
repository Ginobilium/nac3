use std::{collections::HashMap, convert::TryInto, iter::once};

use super::{get_llvm_type, CodeGenContext};
use crate::{
    symbol_resolver::SymbolValue,
    toplevel::{DefinitionId, TopLevelDef},
    typecheck::typedef::{FunSignature, Type, TypeEnum},
};
use inkwell::{
    types::{BasicType, BasicTypeEnum},
    values::{BasicValueEnum, IntValue, PointerValue},
    AddressSpace,
};
use itertools::{chain, izip, zip, Itertools};
use rustpython_parser::ast::{self, Boolop, Constant, Expr, ExprKind, Operator};

pub fn assert_int_val<'ctx>(val: BasicValueEnum<'ctx>) -> IntValue<'ctx> {
    if let BasicValueEnum::IntValue(v) = val {
        v
    } else {
        unreachable!()
    }
}

pub fn assert_pointer_val<'ctx>(val: BasicValueEnum<'ctx>) -> PointerValue<'ctx> {
    if let BasicValueEnum::PointerValue(v) = val {
        v
    } else {
        unreachable!()
    }
}

impl<'ctx, 'a> CodeGenContext<'ctx, 'a> {
    fn get_subst_key(&mut self, obj: Option<Type>, fun: &FunSignature) -> String {
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
        let sorted = vars.keys().sorted();
        sorted
            .map(|id| {
                self.unifier.stringify(vars[id], &mut |id| id.to_string(), &mut |id| id.to_string())
            })
            .join(", ")
    }

    pub fn get_attr_index(&mut self, ty: Type, attr: &str) -> usize {
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

    fn gen_call(
        &mut self,
        obj: Option<(Type, BasicValueEnum<'ctx>)>,
        fun: (&FunSignature, DefinitionId),
        params: Vec<(Option<String>, BasicValueEnum<'ctx>)>,
        ret: Type,
    ) -> Option<BasicValueEnum<'ctx>> {
        let key = self.get_subst_key(obj.map(|(a, _)| a), fun.0);
        let top_level_defs = self.top_level.definitions.read();
        let definition = top_level_defs.get(fun.1 .0).unwrap();
        let val = if let TopLevelDef::Function { instance_to_symbol, .. } = &*definition.read() {
            let symbol = instance_to_symbol.get(&key).unwrap_or_else(|| {
                // TODO: codegen for function that are not yet generated
                unimplemented!()
            });
            let fun_val = self.module.get_function(symbol).unwrap_or_else(|| {
                let params = fun.0.args.iter().map(|arg| self.get_llvm_type(arg.ty)).collect_vec();
                let fun_ty = if self.unifier.unioned(ret, self.primitives.none) {
                    self.ctx.void_type().fn_type(&params, false)
                } else {
                    self.get_llvm_type(ret).fn_type(&params, false)
                };
                self.module.add_function(symbol, fun_ty, None)
            });
            let mut keys = fun.0.args.clone();
            let mut mapping = HashMap::new();
            for (key, value) in params.into_iter() {
                mapping.insert(key.unwrap_or_else(|| keys.remove(0).name), value);
            }
            // default value handling
            for k in keys.into_iter() {
                mapping.insert(k.name, self.gen_symbol_val(&k.default_value.unwrap()));
            }
            // reorder the parameters
            let params =
                fun.0.args.iter().map(|arg| mapping.remove(&arg.name).unwrap()).collect_vec();
            self.builder.build_call(fun_val, &params, "call").try_as_basic_value().left()
        } else {
            unreachable!()
        };
        val
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
                ty.const_int(v.try_into().unwrap(), false).into()
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

    pub fn gen_expr(&mut self, expr: &Expr<Option<Type>>) -> Option<BasicValueEnum<'ctx>> {
        let zero = self.ctx.i32_type().const_int(0, false);
        Some(match &expr.node {
            ExprKind::Constant { value, .. } => {
                let ty = expr.custom.unwrap();
                self.gen_const(value, ty)
            }
            ExprKind::Name { id, .. } => {
                let ptr = self.var_assignment.get(id).unwrap();
                self.builder.build_load(*ptr, "load")
            }
            ExprKind::List { elts, .. } => {
                // this shall be optimized later for constant primitive lists...
                // we should use memcpy for that instead of generating thousands of stores
                let elements = elts.iter().map(|x| self.gen_expr(x).unwrap()).collect_vec();
                let ty = if elements.is_empty() {
                    self.ctx.i32_type().into()
                } else {
                    elements[0].get_type()
                };
                let arr_ptr = self.builder.build_array_alloca(
                    ty,
                    self.ctx.i32_type().const_int(elements.len() as u64, false),
                    "tmparr",
                );
                let arr_ty = self.ctx.struct_type(
                    &[self.ctx.i32_type().into(), ty.ptr_type(AddressSpace::Generic).into()],
                    false,
                );
                let arr_str_ptr = self.builder.build_alloca(arr_ty, "tmparrstr");
                unsafe {
                    let len_ptr =
                        self.builder.build_in_bounds_gep(arr_str_ptr, &[zero, zero], "len_ptr");
                    self.builder.build_store(
                        len_ptr,
                        self.ctx.i32_type().const_int(elements.len() as u64, false),
                    );
                    let ptr_to_arr = self.builder.build_in_bounds_gep(
                        arr_str_ptr,
                        &[zero, self.ctx.i32_type().const_int(1, false)],
                        "ptr_to_arr",
                    );
                    self.builder.build_store(ptr_to_arr, arr_ptr);
                    let i32_type = self.ctx.i32_type();
                    for (i, v) in elements.iter().enumerate() {
                        let elem_ptr = self.builder.build_in_bounds_gep(
                            arr_ptr,
                            &[i32_type.const_int(i as u64, false)],
                            "elem_ptr",
                        );
                        self.builder.build_store(elem_ptr, *v);
                    }
                }
                arr_str_ptr.into()
            }
            ExprKind::Tuple { elts, .. } => {
                let element_val = elts.iter().map(|x| self.gen_expr(x).unwrap()).collect_vec();
                let element_ty = element_val.iter().map(BasicValueEnum::get_type).collect_vec();
                let tuple_ty = self.ctx.struct_type(&element_ty, false);
                let tuple_ptr = self.builder.build_alloca(tuple_ty, "tuple");
                for (i, v) in element_val.into_iter().enumerate() {
                    unsafe {
                        let ptr = self.builder.build_in_bounds_gep(
                            tuple_ptr,
                            &[zero, self.ctx.i32_type().const_int(i as u64, false)],
                            "ptr",
                        );
                        self.builder.build_store(ptr, v);
                    }
                }
                tuple_ptr.into()
            }
            ExprKind::Attribute { value, attr, .. } => {
                // note that we would handle class methods directly in calls
                let index = self.get_attr_index(value.custom.unwrap(), attr);
                let val = self.gen_expr(value).unwrap();
                let ptr = assert_pointer_val(val);
                unsafe {
                    let ptr = self.builder.build_in_bounds_gep(
                        ptr,
                        &[zero, self.ctx.i32_type().const_int(index as u64, false)],
                        "attr",
                    );
                    self.builder.build_load(ptr, "field")
                }
            }
            ExprKind::BoolOp { op, values } => {
                // requires conditional branches for short-circuiting...
                let left = assert_int_val(self.gen_expr(&values[0]).unwrap());
                let current = self.builder.get_insert_block().unwrap().get_parent().unwrap();
                let a_bb = self.ctx.append_basic_block(current, "a");
                let b_bb = self.ctx.append_basic_block(current, "b");
                let cont_bb = self.ctx.append_basic_block(current, "cont");
                self.builder.build_conditional_branch(left, a_bb, b_bb);
                let (a, b) = match op {
                    Boolop::Or => {
                        self.builder.position_at_end(a_bb);
                        let a = self.ctx.bool_type().const_int(1, false);
                        self.builder.build_unconditional_branch(cont_bb);
                        self.builder.position_at_end(b_bb);
                        let b = assert_int_val(self.gen_expr(&values[1]).unwrap());
                        self.builder.build_unconditional_branch(cont_bb);
                        (a, b)
                    }
                    Boolop::And => {
                        self.builder.position_at_end(a_bb);
                        let a = assert_int_val(self.gen_expr(&values[1]).unwrap());
                        self.builder.build_unconditional_branch(cont_bb);
                        self.builder.position_at_end(b_bb);
                        let b = self.ctx.bool_type().const_int(0, false);
                        self.builder.build_unconditional_branch(cont_bb);
                        (a, b)
                    }
                };
                self.builder.position_at_end(cont_bb);
                let phi = self.builder.build_phi(self.ctx.bool_type(), "phi");
                phi.add_incoming(&[(&a, a_bb), (&b, b_bb)]);
                phi.as_basic_value()
            }
            ExprKind::BinOp { op, left, right } => {
                let ty1 = self.unifier.get_representative(left.custom.unwrap());
                let ty2 = self.unifier.get_representative(right.custom.unwrap());
                let left = self.gen_expr(left).unwrap();
                let right = self.gen_expr(right).unwrap();

                // we can directly compare the types, because we've got their representatives
                // which would be unchanged until further unification, which we would never do
                // when doing code generation for function instances
                if ty1 == ty2 && [self.primitives.int32, self.primitives.int64].contains(&ty1) {
                    self.gen_int_ops(op, left, right)
                } else if ty1 == ty2 && self.primitives.float == ty1 {
                    self.gen_float_ops(op, left, right)
                } else {
                    unimplemented!()
                }
            }
            ExprKind::UnaryOp { op, operand } => {
                let ty = self.unifier.get_representative(operand.custom.unwrap());
                let val = self.gen_expr(operand).unwrap();
                if ty == self.primitives.bool {
                    let val = assert_int_val(val);
                    match op {
                        ast::Unaryop::Invert | ast::Unaryop::Not => {
                            self.builder.build_not(val, "not").into()
                        }
                        _ => val.into(),
                    }
                } else if [self.primitives.int32, self.primitives.int64].contains(&ty) {
                    let val = assert_int_val(val);
                    match op {
                        ast::Unaryop::USub => self.builder.build_int_neg(val, "neg").into(),
                        ast::Unaryop::Invert => self.builder.build_not(val, "not").into(),
                        ast::Unaryop::Not => self
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
                } else if ty == self.primitives.float {
                    let val = if let BasicValueEnum::FloatValue(val) = val {
                        val
                    } else {
                        unreachable!()
                    };
                    match op {
                        ast::Unaryop::USub => self.builder.build_float_neg(val, "neg").into(),
                        ast::Unaryop::Not => self
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
                izip!(
                    chain(once(left.as_ref()), comparators.iter()),
                    comparators.iter(),
                    ops.iter(),
                )
                .fold(None, |prev, (lhs, rhs, op)| {
                    let ty = self.unifier.get_representative(lhs.custom.unwrap());
                    let current =
                        if [self.primitives.int32, self.primitives.int64, self.primitives.bool]
                            .contains(&ty)
                        {
                            let (lhs, rhs) = if let (
                                BasicValueEnum::IntValue(lhs),
                                BasicValueEnum::IntValue(rhs),
                            ) =
                                (self.gen_expr(lhs).unwrap(), self.gen_expr(rhs).unwrap())
                            {
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
                            self.builder.build_int_compare(op, lhs, rhs, "cmp")
                        } else if ty == self.primitives.float {
                            let (lhs, rhs) = if let (
                                BasicValueEnum::FloatValue(lhs),
                                BasicValueEnum::FloatValue(rhs),
                            ) =
                                (self.gen_expr(lhs).unwrap(), self.gen_expr(rhs).unwrap())
                            {
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
                            self.builder.build_float_compare(op, lhs, rhs, "cmp")
                        } else {
                            unimplemented!()
                        };
                    prev.map(|v| self.builder.build_and(v, current, "cmp")).or(Some(current))
                })
                .unwrap()
                .into() // as there should be at least 1 element, it should never be none
            }
            ExprKind::IfExp { test, body, orelse } => {
                let test = assert_int_val(self.gen_expr(test).unwrap());
                let current = self.builder.get_insert_block().unwrap().get_parent().unwrap();
                let then_bb = self.ctx.append_basic_block(current, "then");
                let else_bb = self.ctx.append_basic_block(current, "else");
                let cont_bb = self.ctx.append_basic_block(current, "cont");
                self.builder.build_conditional_branch(test, then_bb, else_bb);
                self.builder.position_at_end(then_bb);
                let a = self.gen_expr(body).unwrap();
                self.builder.build_unconditional_branch(cont_bb);
                self.builder.position_at_end(else_bb);
                let b = self.gen_expr(orelse).unwrap();
                self.builder.build_unconditional_branch(cont_bb);
                self.builder.position_at_end(cont_bb);
                let phi = self.builder.build_phi(a.get_type(), "ifexpr");
                phi.add_incoming(&[(&a, then_bb), (&b, else_bb)]);
                phi.as_basic_value()
            }
            ExprKind::Call { func, args, keywords } => {
                if let ExprKind::Name { id, .. } = &func.as_ref().node {
                    // TODO: handle primitive casts
                    let fun = self.resolver.get_identifier_def(&id).expect("Unknown identifier");
                    let ret = expr.custom.unwrap();
                    let mut params =
                        args.iter().map(|arg| (None, self.gen_expr(arg).unwrap())).collect_vec();
                    let kw_iter = keywords.iter().map(|kw| {
                        (
                            Some(kw.node.arg.as_ref().unwrap().clone()),
                            self.gen_expr(&kw.node.value).unwrap(),
                        )
                    });
                    params.extend(kw_iter);
                    let signature = self
                        .unifier
                        .get_call_signature(*self.calls.get(&expr.location.into()).unwrap())
                        .unwrap();
                    return self.gen_call(None, (&signature, fun), params, ret);
                } else {
                    unimplemented!()
                }
            }
            ExprKind::Subscript { value, slice, .. } => {
                if let TypeEnum::TList { .. } = &*self.unifier.get_ty(value.custom.unwrap()) {
                    if let ExprKind::Slice { .. } = slice.node {
                        unimplemented!()
                    } else {
                        // TODO: bound check
                        let i32_type = self.ctx.i32_type();
                        let v = assert_pointer_val(self.gen_expr(value).unwrap());
                        let index = assert_int_val(self.gen_expr(slice).unwrap());
                        unsafe {
                            let ptr_to_arr = self.builder.build_in_bounds_gep(
                                v,
                                &[i32_type.const_zero(), i32_type.const_int(1, false)],
                                "ptr_to_arr",
                            );
                            let arr_ptr =
                                assert_pointer_val(self.builder.build_load(ptr_to_arr, "loadptr"));
                            let ptr = self.builder.build_gep(arr_ptr, &[index], "loadarrgep");
                            self.builder.build_load(ptr, "loadarr")
                        }
                    }
                } else {
                    let i32_type = self.ctx.i32_type();
                    let v = assert_pointer_val(self.gen_expr(value).unwrap());
                    let index = assert_int_val(self.gen_expr(slice).unwrap());
                    unsafe {
                        let ptr_to_elem = self.builder.build_in_bounds_gep(
                            v,
                            &[i32_type.const_zero(), index],
                            "ptr_to_elem",
                        );
                        self.builder.build_load(ptr_to_elem, "loadelem")
                    }
                }
            }
            _ => unimplemented!(),
        })
    }
}
