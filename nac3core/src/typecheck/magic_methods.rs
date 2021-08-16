use crate::typecheck::{
    type_inferencer::*,
    typedef::{FunSignature, FuncArg, Type, TypeEnum, Unifier},
};
use rustpython_parser::ast;
use rustpython_parser::ast::{Cmpop, Operator, Unaryop};
use std::borrow::Borrow;
use std::collections::HashMap;

pub fn binop_name(op: &Operator) -> &'static str {
    match op {
        Operator::Add => "__add__",
        Operator::Sub => "__sub__",
        Operator::Div => "__truediv__",
        Operator::Mod => "__mod__",
        Operator::Mult => "__mul__",
        Operator::Pow => "__pow__",
        Operator::BitOr => "__or__",
        Operator::BitXor => "__xor__",
        Operator::BitAnd => "__and__",
        Operator::LShift => "__lshift__",
        Operator::RShift => "__rshift__",
        Operator::FloorDiv => "__floordiv__",
        Operator::MatMult => "__matmul__",
    }
}

pub fn binop_assign_name(op: &Operator) -> &'static str {
    match op {
        Operator::Add => "__iadd__",
        Operator::Sub => "__isub__",
        Operator::Div => "__itruediv__",
        Operator::Mod => "__imod__",
        Operator::Mult => "__imul__",
        Operator::Pow => "__ipow__",
        Operator::BitOr => "__ior__",
        Operator::BitXor => "__ixor__",
        Operator::BitAnd => "__iand__",
        Operator::LShift => "__ilshift__",
        Operator::RShift => "__irshift__",
        Operator::FloorDiv => "__ifloordiv__",
        Operator::MatMult => "__imatmul__",
    }
}

pub fn unaryop_name(op: &Unaryop) -> &'static str {
    match op {
        Unaryop::UAdd => "__pos__",
        Unaryop::USub => "__neg__",
        Unaryop::Not => "__not__",
        Unaryop::Invert => "__inv__",
    }
}

pub fn comparison_name(op: &Cmpop) -> Option<&'static str> {
    match op {
        Cmpop::Lt => Some("__lt__"),
        Cmpop::LtE => Some("__le__"),
        Cmpop::Gt => Some("__gt__"),
        Cmpop::GtE => Some("__ge__"),
        Cmpop::Eq => Some("__eq__"),
        Cmpop::NotEq => Some("__ne__"),
        _ => None,
    }
}

pub fn impl_binop(
    unifier: &mut Unifier,
    _store: &PrimitiveStore,
    ty: Type,
    other_ty: &[Type],
    ret_ty: Type,
    ops: &[ast::Operator],
) {
    if let TypeEnum::TObj { fields, .. } = unifier.get_ty(ty).borrow() {
        for op in ops {
            fields.borrow_mut().insert(binop_name(op).into(), {
                let other = if other_ty.len() == 1 {
                    other_ty[0]
                } else {
                    unifier.get_fresh_var_with_range(other_ty).0
                };
                unifier.add_ty(TypeEnum::TFunc(FunSignature {
                    ret: ret_ty,
                    vars: HashMap::new(),
                    args: vec![FuncArg { ty: other, default_value: None, name: "other".into() }],
                }.into()))
            });

            fields.borrow_mut().insert(binop_assign_name(op).into(), {
                let other = if other_ty.len() == 1 {
                    other_ty[0]
                } else {
                    unifier.get_fresh_var_with_range(other_ty).0
                };
                unifier.add_ty(TypeEnum::TFunc(FunSignature {
                    ret: ret_ty,
                    vars: HashMap::new(),
                    args: vec![FuncArg { ty: other, default_value: None, name: "other".into() }],
                }.into()))
            });
        }
    } else {
        unreachable!("")
    }
}

pub fn impl_unaryop(
    unifier: &mut Unifier,
    _store: &PrimitiveStore,
    ty: Type,
    ret_ty: Type,
    ops: &[ast::Unaryop],
) {
    if let TypeEnum::TObj { fields, .. } = unifier.get_ty(ty).borrow() {
        for op in ops {
            fields.borrow_mut().insert(
                unaryop_name(op).into(),
                unifier.add_ty(TypeEnum::TFunc(FunSignature {
                    ret: ret_ty,
                    vars: HashMap::new(),
                    args: vec![],
                }.into())),
            );
        }
    } else {
        unreachable!()
    }
}

pub fn impl_cmpop(
    unifier: &mut Unifier,
    store: &PrimitiveStore,
    ty: Type,
    other_ty: Type,
    ops: &[ast::Cmpop],
) {
    if let TypeEnum::TObj { fields, .. } = unifier.get_ty(ty).borrow() {
        for op in ops {
            fields.borrow_mut().insert(
                comparison_name(op).unwrap().into(),
                unifier.add_ty(TypeEnum::TFunc(FunSignature {
                    ret: store.bool,
                    vars: HashMap::new(),
                    args: vec![FuncArg { ty: other_ty, default_value: None, name: "other".into() }],
                }.into())),
            );
        }
    } else {
        unreachable!()
    }
}

/// Add, Sub, Mult, Pow
pub fn impl_basic_arithmetic(
    unifier: &mut Unifier,
    store: &PrimitiveStore,
    ty: Type,
    other_ty: &[Type],
    ret_ty: Type,
) {
    impl_binop(
        unifier,
        store,
        ty,
        other_ty,
        ret_ty,
        &[ast::Operator::Add, ast::Operator::Sub, ast::Operator::Mult],
    )
}

pub fn impl_pow(
    unifier: &mut Unifier,
    store: &PrimitiveStore,
    ty: Type,
    other_ty: &[Type],
    ret_ty: Type,
) {
    impl_binop(unifier, store, ty, other_ty, ret_ty, &[ast::Operator::Pow])
}

/// BitOr, BitXor, BitAnd
pub fn impl_bitwise_arithmetic(unifier: &mut Unifier, store: &PrimitiveStore, ty: Type) {
    impl_binop(
        unifier,
        store,
        ty,
        &[ty],
        ty,
        &[ast::Operator::BitAnd, ast::Operator::BitOr, ast::Operator::BitXor],
    )
}

/// LShift, RShift
pub fn impl_bitwise_shift(unifier: &mut Unifier, store: &PrimitiveStore, ty: Type) {
    impl_binop(unifier, store, ty, &[ty], ty, &[ast::Operator::LShift, ast::Operator::RShift])
}

/// Div
pub fn impl_div(unifier: &mut Unifier, store: &PrimitiveStore, ty: Type, other_ty: &[Type]) {
    impl_binop(unifier, store, ty, other_ty, store.float, &[ast::Operator::Div])
}

/// FloorDiv
pub fn impl_floordiv(
    unifier: &mut Unifier,
    store: &PrimitiveStore,
    ty: Type,
    other_ty: &[Type],
    ret_ty: Type,
) {
    impl_binop(unifier, store, ty, other_ty, ret_ty, &[ast::Operator::FloorDiv])
}

/// Mod
pub fn impl_mod(
    unifier: &mut Unifier,
    store: &PrimitiveStore,
    ty: Type,
    other_ty: &[Type],
    ret_ty: Type,
) {
    impl_binop(unifier, store, ty, other_ty, ret_ty, &[ast::Operator::Mod])
}

/// UAdd, USub
pub fn impl_sign(unifier: &mut Unifier, store: &PrimitiveStore, ty: Type) {
    impl_unaryop(unifier, store, ty, ty, &[ast::Unaryop::UAdd, ast::Unaryop::USub])
}

/// Invert
pub fn impl_invert(unifier: &mut Unifier, store: &PrimitiveStore, ty: Type) {
    impl_unaryop(unifier, store, ty, ty, &[ast::Unaryop::Invert])
}

/// Not
pub fn impl_not(unifier: &mut Unifier, store: &PrimitiveStore, ty: Type) {
    impl_unaryop(unifier, store, ty, store.bool, &[ast::Unaryop::Not])
}

/// Lt, LtE, Gt, GtE
pub fn impl_comparison(unifier: &mut Unifier, store: &PrimitiveStore, ty: Type, other_ty: Type) {
    impl_cmpop(
        unifier,
        store,
        ty,
        other_ty,
        &[ast::Cmpop::Lt, ast::Cmpop::Gt, ast::Cmpop::LtE, ast::Cmpop::GtE],
    )
}

/// Eq, NotEq
pub fn impl_eq(unifier: &mut Unifier, store: &PrimitiveStore, ty: Type) {
    impl_cmpop(unifier, store, ty, ty, &[ast::Cmpop::Eq, ast::Cmpop::NotEq])
}

pub fn set_primitives_magic_methods(store: &PrimitiveStore, unifier: &mut Unifier) {
    let PrimitiveStore { int32: int32_t, int64: int64_t, float: float_t, bool: bool_t, .. } =
        *store;
    /* int32 ======== */
    impl_basic_arithmetic(unifier, store, int32_t, &[int32_t], int32_t);
    impl_pow(unifier, store, int32_t, &[int32_t], int32_t);
    impl_bitwise_arithmetic(unifier, store, int32_t);
    impl_bitwise_shift(unifier, store, int32_t);
    impl_div(unifier, store, int32_t, &[int32_t]);
    impl_floordiv(unifier, store, int32_t, &[int32_t], int32_t);
    impl_mod(unifier, store, int32_t, &[int32_t], int32_t);
    impl_sign(unifier, store, int32_t);
    impl_invert(unifier, store, int32_t);
    impl_not(unifier, store, int32_t);
    impl_comparison(unifier, store, int32_t, int32_t);
    impl_eq(unifier, store, int32_t);

    /* int64 ======== */
    impl_basic_arithmetic(unifier, store, int64_t, &[int64_t], int64_t);
    impl_pow(unifier, store, int64_t, &[int64_t], int64_t);
    impl_bitwise_arithmetic(unifier, store, int64_t);
    impl_bitwise_shift(unifier, store, int64_t);
    impl_div(unifier, store, int64_t, &[int64_t]);
    impl_floordiv(unifier, store, int64_t, &[int64_t], int64_t);
    impl_mod(unifier, store, int64_t, &[int64_t], int64_t);
    impl_sign(unifier, store, int64_t);
    impl_invert(unifier, store, int64_t);
    impl_not(unifier, store, int64_t);
    impl_comparison(unifier, store, int64_t, int64_t);
    impl_eq(unifier, store, int64_t);

    /* float ======== */
    impl_basic_arithmetic(unifier, store, float_t, &[float_t], float_t);
    impl_pow(unifier, store, float_t, &[int32_t, float_t], float_t);
    impl_div(unifier, store, float_t, &[float_t]);
    impl_floordiv(unifier, store, float_t, &[float_t], float_t);
    impl_mod(unifier, store, float_t, &[float_t], float_t);
    impl_sign(unifier, store, float_t);
    impl_not(unifier, store, float_t);
    impl_comparison(unifier, store, float_t, float_t);
    impl_eq(unifier, store, float_t);

    /* bool ======== */
    impl_not(unifier, store, bool_t);
    impl_eq(unifier, store, bool_t);
}
