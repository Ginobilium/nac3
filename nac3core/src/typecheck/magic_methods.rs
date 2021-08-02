use std::borrow::Borrow;
use std::collections::HashMap;
use rustpython_parser::ast::{Cmpop, Operator, Unaryop};

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
        Unaryop::UAdd   => "__pos__",
        Unaryop::USub   => "__neg__",
        Unaryop::Not    => "__not__",
        Unaryop::Invert => "__inv__",
    }
}

pub fn comparison_name(op: &Cmpop) -> Option<&'static str> {
    match op {
        Cmpop::Lt    => Some("__lt__"),
        Cmpop::LtE   => Some("__le__"),
        Cmpop::Gt    => Some("__gt__"),
        Cmpop::GtE   => Some("__ge__"),
        Cmpop::Eq    => Some("__eq__"),
        Cmpop::NotEq => Some("__ne__"),
        _ => None,
    }
}

use crate::typecheck::{type_inferencer::*, typedef::{FunSignature, FuncArg, TypeEnum, Unifier, Type}};
use rustpython_parser::ast;

/// Add, Sub, Mult, Pow
pub fn impl_basic_arithmetic(unifier: &mut Unifier, store: &PrimitiveStore, ty: Type, other_ty: Type, ret_ty: Type) {
    if let TypeEnum::TObj {fields, .. } = unifier.get_ty(ty).borrow() {
        for op in &[
            ast::Operator::Add, 
            ast::Operator::Sub, 
            ast::Operator::Mult,
            ast::Operator::Pow, 
        ] {
            fields.borrow_mut().insert(
                binop_name(op).into(),
                unifier.add_ty(TypeEnum::TFunc(FunSignature {
                    ret: ret_ty,
                    vars: HashMap::new(),
                    args: vec![FuncArg {
                        ty: other_ty,
                        is_optional: false,
                        name: "other".into()
                    }]
                }))
            );

            fields.borrow_mut().insert(
                binop_assign_name(op).into(),
                unifier.add_ty(TypeEnum::TFunc(FunSignature {
                    ret: store.none,
                    vars: HashMap::new(),
                    args: vec![FuncArg {
                        ty: other_ty,
                        is_optional: false,
                        name: "other".into()
                    }]
                }))
            );
        }
    } else { unreachable!() }
}

/// LShift, RShift, BitOr, BitXor, BitAnd
pub fn impl_bitwise_arithmetic(unifier: &mut Unifier, store: &PrimitiveStore, ty: Type) {
    if let TypeEnum::TObj {fields, .. } = unifier.get_ty(ty).borrow() {
        for op in &[
            ast::Operator::LShift, 
            ast::Operator::RShift, 
            ast::Operator::BitOr, 
            ast::Operator::BitXor, 
            ast::Operator::BitAnd, 
        ] {
            fields.borrow_mut().insert(
                binop_name(op).into(), 
                unifier.add_ty(TypeEnum::TFunc(FunSignature {
                    ret: ty,
                    vars: HashMap::new(),
                    args: vec![FuncArg {
                        ty,
                        is_optional: false,
                        name: "other".into()
                    }]
                }))
            );

            fields.borrow_mut().insert(
                binop_assign_name(op).into(), 
                unifier.add_ty(TypeEnum::TFunc(FunSignature {
                    ret: store.none,
                    vars: HashMap::new(),
                    args: vec![FuncArg {
                        ty,
                        is_optional: false,
                        name: "other".into()
                    }]
                }))
            );
        }
    }
}

/// Div
pub fn impl_div(unifier: &mut Unifier, store: &PrimitiveStore, ty: Type, other_ty: Type) {
    if let TypeEnum::TObj {fields, .. } = unifier.get_ty(ty).borrow() {
        fields.borrow_mut().insert(
            binop_name(&ast::Operator::Div).into(), 
            unifier.add_ty(TypeEnum::TFunc(FunSignature{
                ret: store.float,
                vars: HashMap::new(),
                args: vec![FuncArg {
                    ty: other_ty,
                    is_optional: false,
                    name: "other".into()
                }]
            }))
        );

        fields.borrow_mut().insert(
            binop_assign_name(&ast::Operator::Div).into(), 
            unifier.add_ty(TypeEnum::TFunc(FunSignature{
                ret: store.none,
                vars: HashMap::new(),
                args: vec![FuncArg {
                    ty: other_ty,
                    is_optional: false,
                    name: "other".into()
                }]
            }))
        );
    } else { unreachable!() }
}

/// FloorDiv
pub fn impl_floordiv(unifier: &mut Unifier, store: &PrimitiveStore, ty: Type, other_ty: Type, ret_ty: Type) {
    if let TypeEnum::TObj {fields, .. } = unifier.get_ty(ty).borrow() {
        fields.borrow_mut().insert(
            binop_name(&ast::Operator::FloorDiv).into(), 
            unifier.add_ty(TypeEnum::TFunc(FunSignature{
                ret: ret_ty,
                vars: HashMap::new(),
                args: vec![FuncArg {
                    ty: other_ty,
                    is_optional: false,
                    name: "other".into()
                }]
            }))
        );

        fields.borrow_mut().insert(
            binop_assign_name(&ast::Operator::FloorDiv).into(), 
            unifier.add_ty(TypeEnum::TFunc(FunSignature{
                ret: store.none,
                vars: HashMap::new(),
                args: vec![FuncArg {
                    ty: other_ty,
                    is_optional: false,
                    name: "other".into()
                }]
            }))
        );
    } else { unreachable!() }
}

/// Mod
pub fn impl_mod(unifier: &mut Unifier, store: &PrimitiveStore, ty: Type, other_ty: Type, ret_ty: Type) {
    if let TypeEnum::TObj {fields, .. } = unifier.get_ty(ty).borrow() {
        fields.borrow_mut().insert(
            binop_name(&ast::Operator::Mod).into(),
            unifier.add_ty(TypeEnum::TFunc(FunSignature {
                ret: ret_ty,
                vars: HashMap::new(),
                args: vec![FuncArg {
                    ty: other_ty,
                    is_optional: false,
                    name: "other".into()
                }]
            }))
        );

        fields.borrow_mut().insert(
            binop_assign_name(&ast::Operator::Mod).into(),
            unifier.add_ty(TypeEnum::TFunc(FunSignature {
                ret: store.none,
                vars: HashMap::new(),
                args: vec![FuncArg {
                    ty: other_ty,
                    is_optional: false,
                    name: "other".into()
                }]
            }))
        );
    } else { unreachable!() }
}

/// UAdd, USub
pub fn impl_unary_op(unifier: &mut Unifier, _store: &PrimitiveStore, ty: Type) {
    if let TypeEnum::TObj {fields, .. } = unifier.get_ty(ty).borrow() {
        for op in &[
            ast::Unaryop::UAdd,
            ast::Unaryop::USub
        ] {
            fields.borrow_mut().insert(
                unaryop_name(op).into(), 
                unifier.add_ty(TypeEnum::TFunc(FunSignature {
                    ret: ty,
                    vars: HashMap::new(),
                    args: vec![]
                }))
            );
        }
    } else { unreachable!() }
}

/// Invert
pub fn impl_invert(unifier: &mut Unifier, _store: &PrimitiveStore, ty: Type) {
    if let TypeEnum::TObj {fields, .. } = unifier.get_ty(ty).borrow() {
        fields.borrow_mut().insert(
            unaryop_name(&ast::Unaryop::Invert).into(), 
            unifier.add_ty(TypeEnum::TFunc(FunSignature {
                ret: ty,
                vars: HashMap::new(),
                args: vec![]
            }))
        );
    }
}

/// Not
pub fn impl_not(unifier: &mut Unifier, store: &PrimitiveStore, ty: Type) {
    if let TypeEnum::TObj {fields, .. } = unifier.get_ty(ty).borrow() {
        fields.borrow_mut().insert(
            unaryop_name(&ast::Unaryop::Not).into(), 
            unifier.add_ty(TypeEnum::TFunc(FunSignature {
                ret: store.bool,
                vars: HashMap::new(),
                args: vec![]
            }))
        );
    } else { unreachable!() }
}

/// Lt, LtE, Gt, GtE
pub fn impl_comparison(unifier: &mut Unifier, store: &PrimitiveStore, ty: Type, other_ty: Type) {
    if let TypeEnum::TObj {fields, .. } = unifier.get_ty(ty).borrow() {
            for op in &[
            ast::Cmpop::Lt,
            ast::Cmpop::LtE,
            ast::Cmpop::Gt,
            ast::Cmpop::GtE,
        ] {
            fields.borrow_mut().insert(
                comparison_name(op).unwrap().into(), 
                unifier.add_ty(TypeEnum::TFunc(FunSignature {
                    ret: store.bool,
                    vars: HashMap::new(),
                    args: vec![FuncArg {
                        ty: other_ty,
                        is_optional: false,
                        name: "other".into()
                    }]
                }))
            );
        }
    } else { unreachable!() }
}

/// Eq, NotEq
pub fn impl_eq(unifier: &mut Unifier, store: &PrimitiveStore, ty: Type) {
    if let TypeEnum::TObj {fields, .. } = unifier.get_ty(ty).borrow() {
        for op in &[
            ast::Cmpop::Eq,
            ast::Cmpop::NotEq,
        ] {
            fields.borrow_mut().insert(
                comparison_name(op).unwrap().into(), 
                unifier.add_ty(TypeEnum::TFunc(FunSignature {
                    ret: store.bool,
                    vars: HashMap::new(),
                    args: vec![FuncArg {
                        ty,
                        is_optional: false,
                        name: "other".into()
                    }]
                }))
            );
        }
    } else { unreachable!() }
}

pub fn set_primirives_magic_methods(store: &PrimitiveStore, unifier: &mut Unifier) {
    let PrimitiveStore {
        int32: int32_t, 
        int64: int64_t, 
        float: float_t, 
        bool: bool_t,
        none: _none_t
    } = *store;
    /* int32 ======== */
    impl_basic_arithmetic(unifier, store, int32_t, int32_t, int32_t);
    // impl_basic_arithmetic(unifier, store, int32_t, int64_t, int64_t);
    // impl_basic_arithmetic(unifier, store, int32_t, float_t, float_t);
    impl_bitwise_arithmetic(unifier, store, int32_t);
    // impl_div(unifier, store, int32_t, int32_t);
    // impl_div(unifier, store, int32_t, int64_t);
    impl_div(unifier, store, int32_t, int32_t);
    impl_floordiv(unifier, store, int32_t, int32_t, int32_t);
    // impl_floordiv(unifier, store, int32_t, int64_t, int32_t);
    // impl_floordiv(unifier, store, int32_t, float_t, float_t);
    impl_mod(unifier, store, int32_t, int32_t, int32_t);
    // impl_mod(unifier, store, int32_t, int64_t, int32_t);
    // impl_mod(unifier, store, int32_t, float_t, float_t);
    impl_unary_op(unifier, store, int32_t);
    impl_invert(unifier, store, int32_t);
    impl_not(unifier, store, int32_t);
    impl_comparison(unifier, store, int32_t, int32_t);
    // impl_comparison(unifier, store, int32_t, int64_t);
    // impl_comparison(unifier, store, int32_t, float_t);
    impl_eq(unifier, store, int32_t);
    
    /* int64 ======== */ 
    // impl_basic_arithmetic(unifier, store, int64_t, int32_t, int64_t);
    impl_basic_arithmetic(unifier, store, int64_t, int64_t, int64_t);
    // impl_basic_arithmetic(unifier, store, int64_t, float_t, float_t);
    impl_bitwise_arithmetic(unifier, store, int64_t);
    // impl_div(unifier, store, int64_t, int32_t);
    impl_div(unifier, store, int64_t, int64_t);
    // impl_div(unifier, store, int64_t, float_t);
    // impl_floordiv(unifier, store, int64_t, int32_t, int64_t);
    impl_floordiv(unifier, store, int64_t, int64_t, int64_t);
    // impl_floordiv(unifier, store, int64_t, float_t, float_t);
    // impl_mod(unifier, store, int64_t, int32_t, int64_t);
    impl_mod(unifier, store, int64_t, int64_t, int64_t);
    // impl_mod(unifier, store, int64_t, float_t, float_t);
    impl_unary_op(unifier, store, int64_t);
    impl_invert(unifier, store, int64_t);
    impl_not(unifier, store, int64_t);
    // impl_comparison(unifier, store, int64_t, int32_t);
    impl_comparison(unifier, store, int64_t, int64_t);
    // impl_comparison(unifier, store, int64_t, float_t);
    impl_eq(unifier, store, int64_t);
    
    /* float ======== */ 
    // impl_basic_arithmetic(unifier, store, float_t, int32_t, float_t);
    // impl_basic_arithmetic(unifier, store, float_t, int64_t, float_t);
    impl_basic_arithmetic(unifier, store, float_t, float_t, float_t);
    // impl_div(unifier, store, float_t, int32_t);
    // impl_div(unifier, store, float_t, int64_t);
    impl_div(unifier, store, float_t, float_t);
    // impl_floordiv(unifier, store, float_t, int32_t, float_t);
    // impl_floordiv(unifier, store, float_t, int64_t, float_t);
    impl_floordiv(unifier, store, float_t, float_t, float_t);
    // impl_mod(unifier, store, float_t, int32_t, float_t);
    // impl_mod(unifier, store, float_t, int64_t, float_t);
    impl_mod(unifier, store, float_t, float_t, float_t);
    impl_unary_op(unifier, store, float_t);
    impl_not(unifier, store, float_t);
    // impl_comparison(unifier, store, float_t, int32_t);
    // impl_comparison(unifier, store, float_t, int64_t);
    impl_comparison(unifier, store, float_t, float_t);
    impl_eq(unifier, store, float_t);
    
    /* bool ======== */ 
    impl_not(unifier, store, bool_t);
    impl_eq(unifier, store, bool_t);
}