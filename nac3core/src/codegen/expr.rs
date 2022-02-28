use std::{collections::HashMap, convert::TryInto, iter::once};

use crate::{
    codegen::{
        concrete_type::{ConcreteFuncArg, ConcreteTypeEnum, ConcreteTypeStore},
        get_llvm_type,
        irrt::*,
        stmt::gen_raise,
        CodeGenContext, CodeGenTask,
    },
    symbol_resolver::{SymbolValue, ValueEnum},
    toplevel::{DefinitionId, TopLevelDef},
    typecheck::typedef::{FunSignature, FuncArg, Type, TypeEnum, Unifier},
};
use inkwell::{
    types::{BasicType, BasicTypeEnum},
    values::{BasicValueEnum, FunctionValue, IntValue, PointerValue},
    AddressSpace,
};
use itertools::{chain, izip, zip, Itertools};
use nac3parser::ast::{
    self, Boolop, Comprehension, Constant, Expr, ExprKind, Location, Operator, StrRef,
};

use super::CodeGenerator;

pub fn get_subst_key(
    unifier: &mut Unifier,
    obj: Option<Type>,
    fun_vars: &HashMap<u32, Type>,
    filter: Option<&Vec<u32>>,
) -> String {
    let mut vars = obj
        .map(|ty| {
            if let TypeEnum::TObj { params, .. } = &*unifier.get_ty(ty) {
                params.clone()
            } else {
                unreachable!()
            }
        })
        .unwrap_or_default();
    vars.extend(fun_vars.iter());
    let sorted = vars.keys().filter(|id| filter.map(|v| v.contains(id)).unwrap_or(true)).sorted();
    sorted
        .map(|id| {
            unifier.internal_stringify(
                vars[id],
                &mut |id| id.to_string(),
                &mut |id| id.to_string(),
                &mut None,
            )
        })
        .join(", ")
}

impl<'ctx, 'a> CodeGenContext<'ctx, 'a> {
    pub fn build_gep_and_load(
        &mut self,
        ptr: PointerValue<'ctx>,
        index: &[IntValue<'ctx>],
    ) -> BasicValueEnum<'ctx> {
        unsafe { self.builder.build_load(self.builder.build_gep(ptr, index, "gep"), "load") }
    }

    fn get_subst_key(
        &mut self,
        obj: Option<Type>,
        fun: &FunSignature,
        filter: Option<&Vec<u32>>,
    ) -> String {
        get_subst_key(&mut self.unifier, obj, &fun.vars, filter)
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

    pub fn gen_symbol_val(
        &mut self,
        generator: &mut dyn CodeGenerator,
        val: &SymbolValue,
    ) -> BasicValueEnum<'ctx> {
        match val {
            SymbolValue::I32(v) => self.ctx.i32_type().const_int(*v as u64, true).into(),
            SymbolValue::I64(v) => self.ctx.i64_type().const_int(*v as u64, true).into(),
            SymbolValue::Bool(v) => self.ctx.bool_type().const_int(*v as u64, true).into(),
            SymbolValue::Double(v) => self.ctx.f64_type().const_float(*v).into(),
            SymbolValue::Str(v) => {
                let str_ptr =
                    self.builder.build_global_string_ptr(v, "const").as_pointer_value().into();
                let size = generator.get_size_type(self.ctx).const_int(v.len() as u64, false);
                let ty = self.get_llvm_type(generator, self.primitives.str).into_struct_type();
                ty.const_named_struct(&[str_ptr, size.into()]).into()
            }
            SymbolValue::Tuple(ls) => {
                let vals = ls.iter().map(|v| self.gen_symbol_val(generator, v)).collect_vec();
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
                self.builder.build_load(ptr, "tup_val")
            }
        }
    }

    pub fn get_llvm_type(
        &mut self,
        generator: &mut dyn CodeGenerator,
        ty: Type,
    ) -> BasicTypeEnum<'ctx> {
        get_llvm_type(
            self.ctx,
            generator,
            &mut self.unifier,
            self.top_level,
            &mut self.type_cache,
            ty,
        )
    }

    pub fn gen_const(
        &mut self,
        generator: &mut dyn CodeGenerator,
        value: &Constant,
        ty: Type,
    ) -> BasicValueEnum<'ctx> {
        match value {
            Constant::Bool(v) => {
                assert!(self.unifier.unioned(ty, self.primitives.bool));
                let ty = self.ctx.bool_type();
                ty.const_int(if *v { 1 } else { 0 }, false).into()
            }
            Constant::Int(Some(val)) => {
                let ty = if self.unifier.unioned(ty, self.primitives.int32) {
                    self.ctx.i32_type()
                } else if self.unifier.unioned(ty, self.primitives.int64) {
                    self.ctx.i64_type()
                } else {
                    unreachable!();
                };
                ty.const_int(*val as u64, false).into()
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
                    .map(|(ty, v)| self.gen_const(generator, v, ty))
                    .collect_vec();
                let types = values.iter().map(BasicValueEnum::get_type).collect_vec();
                let ty = self.ctx.struct_type(&types, false);
                ty.const_named_struct(&values).into()
            }
            Constant::Str(v) => {
                assert!(self.unifier.unioned(ty, self.primitives.str));
                if let Some(v) = self.const_strings.get(v) {
                    *v
                } else {
                    let str_ptr =
                        self.builder.build_global_string_ptr(v, "const").as_pointer_value().into();
                    let size = generator.get_size_type(self.ctx).const_int(v.len() as u64, false);
                    let ty = self.get_llvm_type(generator, self.primitives.str);
                    let val =
                        ty.into_struct_type().const_named_struct(&[str_ptr, size.into()]).into();
                    self.const_strings.insert(v.to_string(), val);
                    val
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn gen_int_ops(
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
            Operator::Pow => integer_power(self, lhs, rhs).into(),
            // special implementation?
            Operator::MatMult => unreachable!(),
        }
    }

    pub fn gen_float_ops(
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
        let float = self.ctx.f64_type();
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
                        let fn_type = float.fn_type(&[float.into()], false);
                        self.module.add_function("llvm.floor.f64", fn_type, None)
                    });
                self.builder
                    .build_call(floor_intrinsic, &[div.into()], "floor")
                    .try_as_basic_value()
                    .left()
                    .unwrap()
            }
            Operator::Pow => {
                let pow_intrinsic = self.module.get_function("llvm.pow.f64").unwrap_or_else(|| {
                    let fn_type = float.fn_type(&[float.into(), float.into()], false);
                    self.module.add_function("llvm.pow.f64", fn_type, None)
                });
                self.builder
                    .build_call(pow_intrinsic, &[lhs.into(), rhs.into()], "f_pow")
                    .try_as_basic_value()
                    .unwrap_left()
            }
            // special implementation?
            _ => unimplemented!(),
        }
    }

    pub fn build_call_or_invoke(
        &self,
        fun: FunctionValue<'ctx>,
        params: &[BasicValueEnum<'ctx>],
        call_name: &str,
    ) -> Option<BasicValueEnum<'ctx>> {
        if let Some(target) = self.unwind_target {
            let current = self.builder.get_insert_block().unwrap().get_parent().unwrap();
            let then_block = self.ctx.append_basic_block(current, &format!("after.{}", call_name));
            let result = self
                .builder
                .build_invoke(fun, params, then_block, target, call_name)
                .try_as_basic_value()
                .left();
            self.builder.position_at_end(then_block);
            result
        } else {
            let param: Vec<_> = params.iter().map(|v| (*v).into()).collect();
            self.builder.build_call(fun, &param, call_name).try_as_basic_value().left()
        }
    }

    pub fn gen_string<G: CodeGenerator, S: Into<String>>(
        &mut self,
        generator: &mut G,
        s: S,
    ) -> BasicValueEnum<'ctx> {
        self.gen_const(generator, &nac3parser::ast::Constant::Str(s.into()), self.primitives.str)
    }

    pub fn raise_exn<G: CodeGenerator>(
        &mut self,
        generator: &mut G,
        name: &str,
        msg: BasicValueEnum<'ctx>,
        params: [Option<IntValue<'ctx>>; 3],
        loc: Location,
    ) {
        let ty = self.get_llvm_type(generator, self.primitives.exception).into_pointer_type();
        let zelf_ty: BasicTypeEnum = ty.get_element_type().into_struct_type().into();
        let zelf = self.builder.build_alloca(zelf_ty, "alloca");
        let int32 = self.ctx.i32_type();
        let zero = int32.const_zero();
        unsafe {
            let id_ptr = self.builder.build_in_bounds_gep(zelf, &[zero, zero], "exn.id");
            let id = self.resolver.get_string_id(name);
            self.builder.build_store(id_ptr, int32.const_int(id as u64, false));
            let ptr = self.builder.build_in_bounds_gep(
                zelf,
                &[zero, int32.const_int(5, false)],
                "exn.msg",
            );
            self.builder.build_store(ptr, msg);
            let i64_zero = self.ctx.i64_type().const_zero();
            for (i, attr_ind) in [6, 7, 8].iter().enumerate() {
                let ptr = self.builder.build_in_bounds_gep(
                    zelf,
                    &[zero, int32.const_int(*attr_ind, false)],
                    "exn.param",
                );
                let val = params[i].map_or(i64_zero, |v| {
                    self.builder.build_int_s_extend(v, self.ctx.i64_type(), "sext")
                });
                self.builder.build_store(ptr, val);
            }
        }
        gen_raise(generator, self, Some(&zelf.into()), loc);
    }

    pub fn make_assert<G: CodeGenerator>(
        &mut self,
        generator: &mut G,
        cond: IntValue<'ctx>,
        err_name: &str,
        err_msg: &str,
        params: [Option<IntValue<'ctx>>; 3],
        loc: Location,
    ) {
        let i1 = self.ctx.bool_type();
        let i1_true = i1.const_all_ones();
        let expect_fun = self.module.get_function("llvm.expect.i1").unwrap_or_else(|| {
            self.module.add_function(
                "llvm.expect",
                i1.fn_type(&[i1.into(), i1.into()], false),
                None,
            )
        });
        // we assume that the condition is most probably true, so the normal path is the most
        // probable path
        // even if this assumption is violated, it does not matter as exception unwinding is
        // slow anyway...
        let cond = self
            .builder
            .build_call(expect_fun, &[cond.into(), i1_true.into()], "expect")
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_int_value();
        let current_fun = self.builder.get_insert_block().unwrap().get_parent().unwrap();
        let then_block = self.ctx.append_basic_block(current_fun, "succ");
        let exn_block = self.ctx.append_basic_block(current_fun, "fail");
        self.builder.build_conditional_branch(cond, then_block, exn_block);
        self.builder.position_at_end(exn_block);
        let err_msg = self.gen_string(generator, err_msg);
        self.raise_exn(generator, err_name, err_msg, params, loc);
        self.builder.position_at_end(then_block);
    }
}

pub fn gen_constructor<'ctx, 'a, G: CodeGenerator>(
    generator: &mut G,
    ctx: &mut CodeGenContext<'ctx, 'a>,
    signature: &FunSignature,
    def: &TopLevelDef,
    params: Vec<(Option<StrRef>, ValueEnum<'ctx>)>,
) -> Result<BasicValueEnum<'ctx>, String> {
    match def {
        TopLevelDef::Class { methods, .. } => {
            // TODO: what about other fields that require alloca?
            let mut fun_id = None;
            for (name, _, id) in methods.iter() {
                if name == &"__init__".into() {
                    fun_id = Some(*id);
                }
            }
            let ty = ctx.get_llvm_type(generator, signature.ret).into_pointer_type();
            let zelf_ty: BasicTypeEnum = ty.get_element_type().try_into().unwrap();
            let zelf: BasicValueEnum<'ctx> = ctx.builder.build_alloca(zelf_ty, "alloca").into();
            // call `__init__` if there is one
            if let Some(fun_id) = fun_id {
                let mut sign = signature.clone();
                sign.ret = ctx.primitives.none;
                generator.gen_call(
                    ctx,
                    Some((signature.ret, zelf.into())),
                    (&sign, fun_id),
                    params,
                )?;
            }
            Ok(zelf)
        }
        _ => unreachable!(),
    }
}

pub fn gen_func_instance<'ctx, 'a>(
    ctx: &mut CodeGenContext<'ctx, 'a>,
    obj: Option<(Type, ValueEnum<'ctx>)>,
    fun: (&FunSignature, &mut TopLevelDef, String),
    id: usize,
) -> Result<String, String> {
    if let (
        sign,
        TopLevelDef::Function {
            name, instance_to_symbol, instance_to_stmt, var_id, resolver, ..
        },
        key,
    ) = fun
    {
        if let Some(sym) = instance_to_symbol.get(&key) {
            return Ok(sym.clone());
        }
        let symbol = format!("{}.{}", name, instance_to_symbol.len());
        instance_to_symbol.insert(key, symbol.clone());
        let key = ctx.get_subst_key(obj.as_ref().map(|a| a.0), sign, Some(var_id));
        let instance = instance_to_stmt.get(&key).unwrap();

        let mut store = ConcreteTypeStore::new();
        let mut cache = HashMap::new();

        let subst = sign
            .vars
            .iter()
            .map(|(id, ty)| {
                (
                    *instance.subst.get(id).unwrap(),
                    store.from_unifier_type(&mut ctx.unifier, &ctx.primitives, *ty, &mut cache),
                )
            })
            .collect();

        let mut signature =
            store.from_signature(&mut ctx.unifier, &ctx.primitives, sign, &mut cache);

        if let Some(obj) = &obj {
            let zelf =
                store.from_unifier_type(&mut ctx.unifier, &ctx.primitives, obj.0, &mut cache);
            if let ConcreteTypeEnum::TFunc { args, .. } = &mut signature {
                args.insert(
                    0,
                    ConcreteFuncArg { name: "self".into(), ty: zelf, default_value: None },
                )
            } else {
                unreachable!()
            }
        }
        let signature = store.add_cty(signature);

        ctx.registry.add_task(CodeGenTask {
            symbol_name: symbol.clone(),
            body: instance.body.clone(),
            resolver: resolver.as_ref().unwrap().clone(),
            calls: instance.calls.clone(),
            subst,
            signature,
            store,
            unifier_index: instance.unifier_id,
            id,
        });
        Ok(symbol)
    } else {
        unreachable!()
    }
}

pub fn gen_call<'ctx, 'a, G: CodeGenerator>(
    generator: &mut G,
    ctx: &mut CodeGenContext<'ctx, 'a>,
    obj: Option<(Type, ValueEnum<'ctx>)>,
    fun: (&FunSignature, DefinitionId),
    params: Vec<(Option<StrRef>, ValueEnum<'ctx>)>,
) -> Result<Option<BasicValueEnum<'ctx>>, String> {
    let definition = ctx.top_level.definitions.read().get(fun.1 .0).cloned().unwrap();
    let id;
    let key;
    let param_vals;
    let symbol = {
        // make sure this lock guard is dropped at the end of this scope...
        let def = definition.read();
        match &*def {
            TopLevelDef::Function {
                instance_to_symbol,
                instance_to_stmt,
                codegen_callback,
                ..
            } => {
                if let Some(callback) = codegen_callback {
                    return callback.run(ctx, obj, fun, params, generator);
                }
                let old_key = ctx.get_subst_key(obj.as_ref().map(|a| a.0), fun.0, None);
                let mut keys = fun.0.args.clone();
                let mut mapping = HashMap::new();
                for (key, value) in params.into_iter() {
                    mapping.insert(key.unwrap_or_else(|| keys.remove(0).name), value);
                }
                // default value handling
                for k in keys.into_iter() {
                    if mapping.get(&k.name).is_some() {
                        continue;
                    }
                    mapping.insert(
                        k.name,
                        ctx.gen_symbol_val(generator, &k.default_value.unwrap()).into(),
                    );
                }
                // reorder the parameters
                let mut real_params =
                    fun.0.args.iter().map(|arg| mapping.remove(&arg.name).unwrap()).collect_vec();
                if let Some(obj) = &obj {
                    real_params.insert(0, obj.1.clone());
                }
                let static_params = real_params
                    .iter()
                    .enumerate()
                    .filter_map(|(i, v)| {
                        if let ValueEnum::Static(s) = v {
                            Some((i, s.clone()))
                        } else {
                            None
                        }
                    })
                    .collect_vec();
                id = {
                    let ids = static_params
                        .iter()
                        .map(|(i, v)| (*i, v.get_unique_identifier()))
                        .collect_vec();
                    let mut store = ctx.static_value_store.lock();
                    match store.lookup.get(&ids) {
                        Some(index) => *index,
                        None => {
                            let length = store.store.len();
                            store.lookup.insert(ids, length);
                            store.store.push(static_params.into_iter().collect());
                            length
                        }
                    }
                };
                // special case: extern functions
                key = if instance_to_stmt.is_empty() {
                    "".to_string()
                } else {
                    format!("{}:{}", id, old_key)
                };
                param_vals = real_params
                    .into_iter()
                    .map(|p| p.to_basic_value_enum(ctx, generator))
                    .collect::<Result<Vec<_>, String>>()?;
                instance_to_symbol.get(&key).cloned().ok_or_else(|| "".into())
            }
            TopLevelDef::Class { .. } => {
                return Ok(Some(generator.gen_constructor(ctx, fun.0, &*def, params)?))
            }
        }
    }
    .or_else(|_: String| {
        generator.gen_func_instance(ctx, obj.clone(), (fun.0, &mut *definition.write(), key), id)
    })?;
    let fun_val = ctx.module.get_function(&symbol).unwrap_or_else(|| {
        let mut args = fun.0.args.clone();
        if let Some(obj) = &obj {
            args.insert(0, FuncArg { name: "self".into(), ty: obj.0, default_value: None });
        }
        let params =
            args.iter().map(|arg| ctx.get_llvm_type(generator, arg.ty).into()).collect_vec();
        let fun_ty = if ctx.unifier.unioned(fun.0.ret, ctx.primitives.none) {
            ctx.ctx.void_type().fn_type(&params, false)
        } else {
            ctx.get_llvm_type(generator, fun.0.ret).fn_type(&params, false)
        };
        ctx.module.add_function(&symbol, fun_ty, None)
    });
    Ok(ctx.build_call_or_invoke(fun_val, &param_vals, "call"))
}

pub fn destructure_range<'ctx, 'a>(
    ctx: &mut CodeGenContext<'ctx, 'a>,
    range: PointerValue<'ctx>,
) -> (IntValue<'ctx>, IntValue<'ctx>, IntValue<'ctx>) {
    let int32 = ctx.ctx.i32_type();
    let start = ctx
        .build_gep_and_load(range, &[int32.const_zero(), int32.const_int(0, false)])
        .into_int_value();
    let end = ctx
        .build_gep_and_load(range, &[int32.const_zero(), int32.const_int(1, false)])
        .into_int_value();
    let step = ctx
        .build_gep_and_load(range, &[int32.const_zero(), int32.const_int(2, false)])
        .into_int_value();
    (start, end, step)
}

pub fn allocate_list<'ctx, 'a, G: CodeGenerator>(
    generator: &mut G,
    ctx: &mut CodeGenContext<'ctx, 'a>,
    ty: BasicTypeEnum<'ctx>,
    length: IntValue<'ctx>,
) -> PointerValue<'ctx> {
    let arr_ptr = ctx.builder.build_array_alloca(ty, length, "tmparr");
    let size_t = generator.get_size_type(ctx.ctx);
    let i32_t = ctx.ctx.i32_type();
    let arr_ty =
        ctx.ctx.struct_type(&[ty.ptr_type(AddressSpace::Generic).into(), size_t.into()], false);
    let zero = ctx.ctx.i32_type().const_zero();
    let arr_str_ptr = ctx.builder.build_alloca(arr_ty, "tmparrstr");
    unsafe {
        let len_ptr = ctx.builder.build_in_bounds_gep(
            arr_str_ptr,
            &[zero, i32_t.const_int(1, false)],
            "len_ptr",
        );
        let length = ctx.builder.build_int_z_extend(length, size_t, "zext");
        ctx.builder.build_store(len_ptr, length);
        let ptr_to_arr =
            ctx.builder.build_in_bounds_gep(arr_str_ptr, &[zero, i32_t.const_zero()], "ptr_to_arr");
        ctx.builder.build_store(ptr_to_arr, arr_ptr);
        arr_str_ptr
    }
}

pub fn gen_comprehension<'ctx, 'a, G: CodeGenerator>(
    generator: &mut G,
    ctx: &mut CodeGenContext<'ctx, 'a>,
    expr: &Expr<Option<Type>>,
) -> Result<BasicValueEnum<'ctx>, String> {
    if let ExprKind::ListComp { elt, generators } = &expr.node {
        let current = ctx.builder.get_insert_block().unwrap().get_parent().unwrap();
        let test_bb = ctx.ctx.append_basic_block(current, "test");
        let body_bb = ctx.ctx.append_basic_block(current, "body");
        let cont_bb = ctx.ctx.append_basic_block(current, "cont");

        let Comprehension { target, iter, ifs, .. } = &generators[0];
        let iter_val = generator.gen_expr(ctx, iter)?.unwrap().to_basic_value_enum(ctx, generator)?;
        let int32 = ctx.ctx.i32_type();
        let size_t = generator.get_size_type(ctx.ctx);
        let zero_size_t = size_t.const_zero();
        let zero_32 = int32.const_zero();

        let index = generator.gen_var_alloc(ctx, size_t.into())?;
        ctx.builder.build_store(index, zero_size_t);

        let elem_ty = ctx.get_llvm_type(generator, elt.custom.unwrap());
        let is_range = ctx.unifier.unioned(iter.custom.unwrap(), ctx.primitives.range);
        let list;
        let list_content;

        if is_range {
            let iter_val = iter_val.into_pointer_value();
            let (start, end, step) = destructure_range(ctx, iter_val);
            let diff = ctx.builder.build_int_sub(end, start, "diff");
            // add 1 to the length as the value is rounded to zero
            // the length may be 1 more than the actual length if the division is exact, but the
            // length is a upper bound only anyway so it does not matter.
            let length = ctx.builder.build_int_signed_div(diff, step, "div");
            let length = ctx.builder.build_int_add(length, int32.const_int(1, false), "add1");
            // in case length is non-positive
            let is_valid =
                ctx.builder.build_int_compare(inkwell::IntPredicate::SGT, length, zero_32, "check");
            let normal = ctx.ctx.append_basic_block(current, "normal_list");
            let empty = ctx.ctx.append_basic_block(current, "empty_list");
            let list_init = ctx.ctx.append_basic_block(current, "list_init");
            ctx.builder.build_conditional_branch(is_valid, normal, empty);
            // normal: allocate a list
            ctx.builder.position_at_end(normal);
            let list_a = allocate_list(
                generator,
                ctx,
                elem_ty,
                ctx.builder.build_int_z_extend_or_bit_cast(length, size_t, "z_ext_len"),
            );
            ctx.builder.build_unconditional_branch(list_init);
            ctx.builder.position_at_end(empty);
            let list_b = allocate_list(generator, ctx, elem_ty, zero_size_t);
            ctx.builder.build_unconditional_branch(list_init);
            ctx.builder.position_at_end(list_init);
            let phi = ctx.builder.build_phi(list_a.get_type(), "phi");
            phi.add_incoming(&[(&list_a, normal), (&list_b, empty)]);
            list = phi.as_basic_value().into_pointer_value();
            list_content =
                ctx.build_gep_and_load(list, &[zero_size_t, zero_32]).into_pointer_value();

            let i = generator.gen_store_target(ctx, target)?;
            ctx.builder.build_store(i, ctx.builder.build_int_sub(start, step, "start_init"));
            ctx.builder.build_unconditional_branch(test_bb);
            ctx.builder.position_at_end(test_bb);
            let sign =
                ctx.builder.build_int_compare(inkwell::IntPredicate::SGT, step, zero_32, "sign");
            // add and test
            let tmp = ctx.builder.build_int_add(
                ctx.builder.build_load(i, "i").into_int_value(),
                step,
                "start_loop",
            );
            ctx.builder.build_store(i, tmp);
            // if step > 0, continue when i < end
            let cmp1 = ctx.builder.build_int_compare(inkwell::IntPredicate::SLT, tmp, end, "cmp1");
            // if step < 0, continue when i > end
            let cmp2 = ctx.builder.build_int_compare(inkwell::IntPredicate::SGT, tmp, end, "cmp2");
            let pos = ctx.builder.build_and(sign, cmp1, "pos");
            let neg = ctx.builder.build_and(ctx.builder.build_not(sign, "inv"), cmp2, "neg");
            ctx.builder.build_conditional_branch(
                ctx.builder.build_or(pos, neg, "or"),
                body_bb,
                cont_bb,
            );
            ctx.builder.position_at_end(body_bb);
        } else {
            let length = ctx
                .build_gep_and_load(
                    iter_val.into_pointer_value(),
                    &[zero_size_t, int32.const_int(1, false)],
                )
                .into_int_value();
            list = allocate_list(generator, ctx, elem_ty, length);
            list_content =
                ctx.build_gep_and_load(list, &[zero_size_t, zero_32]).into_pointer_value();
            let counter = generator.gen_var_alloc(ctx, size_t.into())?;
            // counter = -1
            ctx.builder.build_store(counter, size_t.const_int(u64::max_value(), true));
            ctx.builder.build_unconditional_branch(test_bb);
            ctx.builder.position_at_end(test_bb);
            let tmp = ctx.builder.build_load(counter, "i").into_int_value();
            let tmp = ctx.builder.build_int_add(tmp, size_t.const_int(1, false), "inc");
            ctx.builder.build_store(counter, tmp);
            let cmp = ctx.builder.build_int_compare(inkwell::IntPredicate::SLT, tmp, length, "cmp");
            ctx.builder.build_conditional_branch(cmp, body_bb, cont_bb);
            ctx.builder.position_at_end(body_bb);
            let arr_ptr = ctx
                .build_gep_and_load(iter_val.into_pointer_value(), &[zero_size_t, zero_32])
                .into_pointer_value();
            let val = ctx.build_gep_and_load(arr_ptr, &[tmp]);
            generator.gen_assign(ctx, target, val.into())?;
        }
        for cond in ifs.iter() {
            let result = generator
                .gen_expr(ctx, cond)?
                .unwrap()
                .to_basic_value_enum(ctx, generator)?
                .into_int_value();
            let succ = ctx.ctx.append_basic_block(current, "then");
            ctx.builder.build_conditional_branch(result, succ, test_bb);
            ctx.builder.position_at_end(succ);
        }
        let elem = generator.gen_expr(ctx, elt)?.unwrap();
        let i = ctx.builder.build_load(index, "i").into_int_value();
        let elem_ptr = unsafe { ctx.builder.build_gep(list_content, &[i], "elem_ptr") };
        let val = elem.to_basic_value_enum(ctx, generator)?;
        ctx.builder.build_store(elem_ptr, val);
        ctx.builder
            .build_store(index, ctx.builder.build_int_add(i, size_t.const_int(1, false), "inc"));
        ctx.builder.build_unconditional_branch(test_bb);
        ctx.builder.position_at_end(cont_bb);
        let len_ptr = unsafe {
            ctx.builder.build_gep(list, &[zero_size_t, int32.const_int(1, false)], "length")
        };
        ctx.builder.build_store(len_ptr, ctx.builder.build_load(index, "index"));
        Ok(list.into())
    } else {
        unreachable!()
    }
}

pub fn gen_binop_expr<'ctx, 'a, G: CodeGenerator>(
    generator: &mut G,
    ctx: &mut CodeGenContext<'ctx, 'a>,
    left: &Expr<Option<Type>>,
    op: &Operator,
    right: &Expr<Option<Type>>,
) -> Result<ValueEnum<'ctx>, String> {
    let ty1 = ctx.unifier.get_representative(left.custom.unwrap());
    let ty2 = ctx.unifier.get_representative(right.custom.unwrap());
    let left = generator.gen_expr(ctx, left)?.unwrap().to_basic_value_enum(ctx, generator)?;
    let right = generator.gen_expr(ctx, right)?.unwrap().to_basic_value_enum(ctx, generator)?;

    // we can directly compare the types, because we've got their representatives
    // which would be unchanged until further unification, which we would never do
    // when doing code generation for function instances
    Ok(if ty1 == ty2 && [ctx.primitives.int32, ctx.primitives.int64].contains(&ty1) {
        ctx.gen_int_ops(op, left, right)
    } else if ty1 == ty2 && ctx.primitives.float == ty1 {
        ctx.gen_float_ops(op, left, right)
    } else if ty1 == ctx.primitives.float && ty2 == ctx.primitives.int32 {
        // Pow is the only operator that would pass typecheck between float and int
        assert!(*op == Operator::Pow);
        // TODO: throw exception when rhs is out of i16 bound
        // since llvm intrinsic only support to i16 for f64
        let i16_t = ctx.ctx.i16_type();
        let pow_intr = ctx.module.get_function("llvm.powi.f64.i16").unwrap_or_else(|| {
            let f64_t = ctx.ctx.f64_type();
            let ty = f64_t.fn_type(&[f64_t.into(), i16_t.into()], false);
            ctx.module.add_function("llvm.powi.f64.i16", ty, None)
        });
        let right = ctx.builder.build_int_truncate(right.into_int_value(), i16_t, "r_pow");
        ctx.builder
            .build_call(pow_intr, &[left.into(), right.into()], "f_pow_i")
            .try_as_basic_value()
            .unwrap_left()
    } else {
        unimplemented!()
    }
    .into())
}

pub fn gen_expr<'ctx, 'a, G: CodeGenerator>(
    generator: &mut G,
    ctx: &mut CodeGenContext<'ctx, 'a>,
    expr: &Expr<Option<Type>>,
) -> Result<Option<ValueEnum<'ctx>>, String> {
    let int32 = ctx.ctx.i32_type();
    let zero = int32.const_int(0, false);
    Ok(Some(match &expr.node {
        ExprKind::Constant { value, .. } => {
            let ty = expr.custom.unwrap();
            ctx.gen_const(generator, value, ty).into()
        }
        ExprKind::Name { id, .. } => match ctx.var_assignment.get(id) {
            Some((ptr, None, _)) => ctx.builder.build_load(*ptr, "load").into(),
            Some((_, Some(static_value), _)) => ValueEnum::Static(static_value.clone()),
            None => {
                let resolver = ctx.resolver.clone();
                let val = resolver.get_symbol_value(*id, ctx).unwrap();
                // if is tuple, need to deref it to handle tuple as value
                if let (TypeEnum::TTuple { .. }, BasicValueEnum::PointerValue(ptr)) = (
                    &*ctx.unifier.get_ty(expr.custom.unwrap()),
                    resolver
                        .get_symbol_value(*id, ctx)
                        .unwrap()
                        .to_basic_value_enum(ctx, generator)?,
                ) {
                    ctx.builder.build_load(ptr, "tup_val").into()
                } else {
                    val
                }
            }
        },
        ExprKind::List { elts, .. } => {
            // this shall be optimized later for constant primitive lists...
            // we should use memcpy for that instead of generating thousands of stores
            let elements = elts
                .iter()
                .map(|x| {
                    generator
                        .gen_expr(ctx, x)
                        .map_or_else(|e| Err(e), |v| v.unwrap().to_basic_value_enum(ctx, generator))
                })
                .collect::<Result<Vec<_>, _>>()?;
            let ty = if elements.is_empty() {
                if let TypeEnum::TList { ty } = &*ctx.unifier.get_ty(expr.custom.unwrap()) {
                    ctx.get_llvm_type(generator, *ty)
                } else {
                    unreachable!()
                }
            } else {
                elements[0].get_type()
            };
            let length = generator.get_size_type(ctx.ctx).const_int(elements.len() as u64, false);
            let arr_str_ptr = allocate_list(generator, ctx, ty, length);
            let arr_ptr = ctx.build_gep_and_load(arr_str_ptr, &[zero, zero]).into_pointer_value();
            unsafe {
                for (i, v) in elements.iter().enumerate() {
                    let elem_ptr = ctx.builder.build_gep(
                        arr_ptr,
                        &[int32.const_int(i as u64, false)],
                        "elem_ptr",
                    );
                    ctx.builder.build_store(elem_ptr, *v);
                }
            }
            arr_str_ptr.into()
        }
        ExprKind::Tuple { elts, .. } => {
            let element_val = elts
                .iter()
                .map(|x| {
                    generator
                        .gen_expr(ctx, x)
                        .map_or_else(|e| Err(e), |v| v.unwrap().to_basic_value_enum(ctx, generator))
                })
                .collect::<Result<Vec<_>, _>>()?;
            let element_ty = element_val.iter().map(BasicValueEnum::get_type).collect_vec();
            let tuple_ty = ctx.ctx.struct_type(&element_ty, false);
            let tuple_ptr = ctx.builder.build_alloca(tuple_ty, "tuple");
            for (i, v) in element_val.into_iter().enumerate() {
                unsafe {
                    let ptr = ctx.builder.build_in_bounds_gep(
                        tuple_ptr,
                        &[zero, int32.const_int(i as u64, false)],
                        "ptr",
                    );
                    ctx.builder.build_store(ptr, v);
                }
            }
            ctx.builder.build_load(tuple_ptr, "tup_val").into()
        }
        ExprKind::Attribute { value, attr, .. } => {
            // note that we would handle class methods directly in calls
            match generator.gen_expr(ctx, value)?.unwrap() {
                ValueEnum::Static(v) => v.get_field(*attr, ctx).map_or_else(|| {
                    let v = v.to_basic_value_enum(ctx, generator)?;
                    let index = ctx.get_attr_index(value.custom.unwrap(), *attr);
                    Ok(ValueEnum::Dynamic(ctx.build_gep_and_load(
                        v.into_pointer_value(),
                        &[zero, int32.const_int(index as u64, false)],
                    ))) as Result<_, String>
                }, |v| Ok(v))?,
                ValueEnum::Dynamic(v) => {
                    let index = ctx.get_attr_index(value.custom.unwrap(), *attr);
                    ValueEnum::Dynamic(ctx.build_gep_and_load(
                        v.into_pointer_value(),
                        &[zero, int32.const_int(index as u64, false)],
                    ))
                }
            }
        }
        ExprKind::BoolOp { op, values } => {
            // requires conditional branches for short-circuiting...
            let left = generator
                .gen_expr(ctx, &values[0])?
                .unwrap()
                .to_basic_value_enum(ctx, generator)?
                .into_int_value();
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
                    let b = generator
                        .gen_expr(ctx, &values[1])?
                        .unwrap()
                        .to_basic_value_enum(ctx, generator)?
                        .into_int_value();
                    ctx.builder.build_unconditional_branch(cont_bb);
                    (a, b)
                }
                Boolop::And => {
                    ctx.builder.position_at_end(a_bb);
                    let a = generator
                        .gen_expr(ctx, &values[1])?
                        .unwrap()
                        .to_basic_value_enum(ctx, generator)?
                        .into_int_value();
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
            phi.as_basic_value().into()
        }
        ExprKind::BinOp { op, left, right } => gen_binop_expr(generator, ctx, left, op, right)?,
        ExprKind::UnaryOp { op, operand } => {
            let ty = ctx.unifier.get_representative(operand.custom.unwrap());
            let val =
                generator.gen_expr(ctx, operand)?.unwrap().to_basic_value_enum(ctx, generator)?;
            if ty == ctx.primitives.bool {
                let val = val.into_int_value();
                match op {
                    ast::Unaryop::Invert | ast::Unaryop::Not => {
                        ctx.builder.build_not(val, "not").into()
                    }
                    _ => val.into(),
                }
            } else if [ctx.primitives.int32, ctx.primitives.int64].contains(&ty) {
                let val = val.into_int_value();
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
                .fold(Ok(None), |prev: Result<Option<_>, String>, (lhs, rhs, op)| {
                    let ty = ctx.unifier.get_representative(lhs.custom.unwrap());
                    let current =
                        if [ctx.primitives.int32, ctx.primitives.int64, ctx.primitives.bool]
                            .contains(&ty)
                        {
                            let (lhs, rhs) = if let (
                                BasicValueEnum::IntValue(lhs),
                                BasicValueEnum::IntValue(rhs),
                            ) = (
                                generator
                                    .gen_expr(ctx, lhs)?
                                    .unwrap()
                                    .to_basic_value_enum(ctx, generator)?,
                                generator
                                    .gen_expr(ctx, rhs)?
                                    .unwrap()
                                    .to_basic_value_enum(ctx, generator)?,
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
                                generator
                                    .gen_expr(ctx, lhs)?
                                    .unwrap()
                                    .to_basic_value_enum(ctx, generator)?,
                                generator
                                    .gen_expr(ctx, rhs)?
                                    .unwrap()
                                    .to_basic_value_enum(ctx, generator)?,
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
                    Ok(prev?.map(|v| ctx.builder.build_and(v, current, "cmp")).or(Some(current)))
                })?
                .unwrap()
                .into() // as there should be at least 1 element, it should never be none
        }
        ExprKind::IfExp { test, body, orelse } => {
            let test = generator
                .gen_expr(ctx, test)?
                .unwrap()
                .to_basic_value_enum(ctx, generator)?
                .into_int_value();
            let current = ctx.builder.get_insert_block().unwrap().get_parent().unwrap();
            let then_bb = ctx.ctx.append_basic_block(current, "then");
            let else_bb = ctx.ctx.append_basic_block(current, "else");
            let cont_bb = ctx.ctx.append_basic_block(current, "cont");
            ctx.builder.build_conditional_branch(test, then_bb, else_bb);
            ctx.builder.position_at_end(then_bb);
            let a = generator.gen_expr(ctx, body)?.unwrap().to_basic_value_enum(ctx, generator)?;
            ctx.builder.build_unconditional_branch(cont_bb);
            ctx.builder.position_at_end(else_bb);
            let b = generator.gen_expr(ctx, orelse)?.unwrap().to_basic_value_enum(ctx, generator)?;
            ctx.builder.build_unconditional_branch(cont_bb);
            ctx.builder.position_at_end(cont_bb);
            let phi = ctx.builder.build_phi(a.get_type(), "ifexpr");
            phi.add_incoming(&[(&a, then_bb), (&b, else_bb)]);
            phi.as_basic_value().into()
        }
        ExprKind::Call { func, args, keywords } => {
            let mut params = args
                .iter()
                .map(|arg| Ok((None, generator.gen_expr(ctx, arg)?.unwrap())) as Result<_, String>)
                .collect::<Result<Vec<_>, _>>()?;
            let kw_iter = keywords.iter().map(|kw| {
                Ok((
                    Some(*kw.node.arg.as_ref().unwrap()),
                    generator.gen_expr(ctx, &kw.node.value)?.unwrap(),
                )) as Result<_, String>
            });
            let kw_iter = kw_iter.collect::<Result<Vec<_>, _>>()?;
            params.extend(kw_iter);
            let call = ctx.calls.get(&expr.location.into());
            let signature = match call {
                Some(call) => ctx.unifier.get_call_signature(*call).unwrap(),
                None => {
                    let ty = func.custom.unwrap();
                    if let TypeEnum::TFunc(sign) = &*ctx.unifier.get_ty(ty) {
                        sign.clone()
                    } else {
                        unreachable!()
                    }
                }
            };
            let func = func.as_ref();
            match &func.node {
                ExprKind::Name { id, .. } => {
                    // TODO: handle primitive casts and function pointers
                    let fun = ctx
                        .resolver
                        .get_identifier_def(*id)
                        .map_err(|e| format!("{} (at {})", e, func.location))?;
                    return Ok(generator
                        .gen_call(ctx, None, (&signature, fun), params)?
                        .map(|v| v.into()));
                }
                ExprKind::Attribute { value, attr, .. } => {
                    let val = generator.gen_expr(ctx, value)?.unwrap();
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
                    return Ok(generator
                        .gen_call(
                            ctx,
                            Some((value.custom.unwrap(), val)),
                            (&signature, fun_id),
                            params,
                        )?
                        .map(|v| v.into()));
                }
                _ => unimplemented!(),
            }
        }
        ExprKind::Subscript { value, slice, .. } => {
            if let TypeEnum::TList { ty } = &*ctx.unifier.get_ty(value.custom.unwrap()) {
                let v = generator
                    .gen_expr(ctx, value)?
                    .unwrap()
                    .to_basic_value_enum(ctx, generator)?
                    .into_pointer_value();
                let ty = ctx.get_llvm_type(generator, *ty);
                let arr_ptr = ctx.build_gep_and_load(v, &[zero, zero]).into_pointer_value();
                if let ExprKind::Slice { lower, upper, step } = &slice.node {
                    let one = int32.const_int(1, false);
                    let (start, end, step) =
                        handle_slice_indices(lower, upper, step, ctx, generator, v)?;
                    let length = calculate_len_for_slice_range(
                        ctx,
                        start,
                        ctx.builder
                            .build_select(
                                ctx.builder.build_int_compare(
                                    inkwell::IntPredicate::SLT,
                                    step,
                                    zero,
                                    "is_neg",
                                ),
                                ctx.builder.build_int_sub(end, one, "e_min_one"),
                                ctx.builder.build_int_add(end, one, "e_add_one"),
                                "final_e",
                            )
                            .into_int_value(),
                        step,
                    );
                    let res_array_ret = allocate_list(generator, ctx, ty, length);
                    let res_ind =
                        handle_slice_indices(&None, &None, &None, ctx, generator, res_array_ret)?;
                    list_slice_assignment(
                        ctx,
                        generator.get_size_type(ctx.ctx),
                        ty,
                        res_array_ret,
                        res_ind,
                        v,
                        (start, end, step),
                    );
                    res_array_ret.into()
                } else {
                    let len = ctx
                        .build_gep_and_load(v, &[zero, int32.const_int(1, false)])
                        .into_int_value();
                    let raw_index = generator
                        .gen_expr(ctx, slice)?
                        .unwrap()
                        .to_basic_value_enum(ctx, generator)?
                        .into_int_value();
                    let raw_index = ctx.builder.build_int_s_extend(
                        raw_index,
                        generator.get_size_type(ctx.ctx),
                        "sext",
                    );
                    // handle negative index
                    let is_negative = ctx.builder.build_int_compare(
                        inkwell::IntPredicate::SLT,
                        raw_index,
                        generator.get_size_type(ctx.ctx).const_zero(),
                        "is_neg",
                    );
                    let adjusted = ctx.builder.build_int_add(raw_index, len, "adjusted");
                    let index = ctx
                        .builder
                        .build_select(is_negative, adjusted, raw_index, "index")
                        .into_int_value();
                    // unsigned less than is enough, because negative index after adjustment is
                    // bigger than the length (for unsigned cmp)
                    let bound_check = ctx.builder.build_int_compare(
                        inkwell::IntPredicate::ULT,
                        index,
                        len,
                        "inbound",
                    );
                    ctx.make_assert(
                        generator,
                        bound_check,
                        "0:IndexError",
                        "index {0} out of bounds 0:{1}",
                        [Some(raw_index), Some(len), None],
                        expr.location,
                    );
                    ctx.build_gep_and_load(arr_ptr, &[index])
                }
            } else if let TypeEnum::TTuple { .. } = &*ctx.unifier.get_ty(value.custom.unwrap()) {
                let v = generator
                    .gen_expr(ctx, value)?
                    .unwrap()
                    .to_basic_value_enum(ctx, generator)?
                    .into_struct_value();
                let index: u32 =
                    if let ExprKind::Constant { value: ast::Constant::Int(v), .. } = &slice.node {
                        v.unwrap().try_into().unwrap()
                    } else {
                        unreachable!("tuple subscript must be const int after type check");
                    };
                ctx.builder.build_extract_value(v, index, "tup_elem").unwrap()
            } else {
                unreachable!("should not be other subscriptable types after type check");
            }
        }
        .into(),
        ExprKind::ListComp { .. } => gen_comprehension(generator, ctx, expr)?.into(),
        _ => unimplemented!(),
    }))
}
