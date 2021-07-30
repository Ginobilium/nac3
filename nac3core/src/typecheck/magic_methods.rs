use std::{collections::HashMap, rc::Rc};
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

use crate::typecheck::{type_inferencer::*, typedef::{FunSignature, FuncArg, TypeEnum, Unifier}};
use rustpython_parser::ast;
pub fn set_primirives_magic_methods(store: &PrimitiveStore, unifier: &mut Unifier) {
    // int32 --------
    if let Some(TypeEnum::TObj {fields, .. }) = Rc::get_mut(&mut unifier.get_ty(store.int32)) {
        for op in &[
        ast::Operator::Add, 
        ast::Operator::Sub, 
        ast::Operator::Mult,
        ast::Operator::Mod, 
        ast::Operator::Pow, 
        ast::Operator::LShift, 
        ast::Operator::RShift, 
        ast::Operator::BitOr, 
        ast::Operator::BitXor, 
        ast::Operator::BitAnd, 
        ast::Operator::FloorDiv
        ] {
            fields.insert(
                binop_name(op).to_string(), 
                unifier.add_ty(TypeEnum::TFunc(FunSignature {
                    ret: store.int32,
                    vars: HashMap::new(),
                    args: vec![FuncArg {
                        ty: store.int32,
                        is_optional: false,
                        name: "other".into() // the name does not matter here
                    }],
                }))
            );

            fields.insert(
                binop_assign_name(op).to_string(), 
                unifier.add_ty(TypeEnum::TFunc(FunSignature {
                    ret: store.none,
                    vars: HashMap::new(),
                    args: vec![FuncArg {
                        ty: store.int32,
                        is_optional: false,
                        name: "other".into()
                    }]
                }))
            );
        };
        // int div int gets float
        fields.insert(
            binop_assign_name(&ast::Operator::Div).into(),
            unifier.add_ty(TypeEnum::TFunc(FunSignature {
                ret: store.float,
                vars: HashMap::new(),
                args: vec![FuncArg {
                    ty: store.int32,
                    is_optional: false,
                    name: "other".into()
                }]
            }))
        );

        for op in &[
            ast::Cmpop::Eq,
            ast::Cmpop::NotEq,
            ast::Cmpop::Lt,
            ast::Cmpop::LtE,
            ast::Cmpop::Gt,
            ast::Cmpop::GtE,
        ] {
            fields.insert(
                comparison_name(op).unwrap().to_string(), 
                unifier.add_ty(TypeEnum::TFunc(FunSignature {
                    ret: store.bool,
                    vars: HashMap::new(),
                    args: vec![FuncArg {
                        ty: store.int32,
                        is_optional: false,
                        name: "other".into()
                    }],
                }))
            );
        }

        for op in &[
            ast::Unaryop::UAdd,
            ast::Unaryop::USub,
            ast::Unaryop::Not,
            ast::Unaryop::Invert,
        ] {
            fields.insert(
                unaryop_name(op).into(), 
                unifier.add_ty(TypeEnum::TFunc(FunSignature {
                    ret: store.int32,
                    vars: HashMap::new(),
                    args: vec![]
                }))
            );
        }
    } else { unreachable!() }
    // int32 --------
    // int64 --------
    if let Some(TypeEnum::TObj {fields, .. }) = Rc::get_mut(&mut unifier.get_ty(store.int64)) {
        for op in &[
        ast::Operator::Add, 
        ast::Operator::Sub, 
        ast::Operator::Mult,
        ast::Operator::Mod, 
        ast::Operator::Pow, 
        ast::Operator::LShift, 
        ast::Operator::RShift, 
        ast::Operator::BitOr, 
        ast::Operator::BitXor, 
        ast::Operator::BitAnd, 
        ast::Operator::FloorDiv
        ] {
            fields.insert(
                binop_name(op).to_string(), 
                unifier.add_ty(TypeEnum::TFunc(FunSignature {
                    ret: store.int64,
                    vars: HashMap::new(),
                    args: vec![FuncArg {
                        ty: store.int64,
                        is_optional: false,
                        name: "other".into() // the name does not matter here
                    }],
                }))
            );

            fields.insert(
                binop_assign_name(op).to_string(), 
                unifier.add_ty(TypeEnum::TFunc(FunSignature {
                    ret: store.none,
                    vars: HashMap::new(),
                    args: vec![FuncArg {
                        ty: store.int64,
                        is_optional: false,
                        name: "other".into()
                    }]
                }))
            );
        };
        fields.insert(
            binop_assign_name(&ast::Operator::Div).into(),
            unifier.add_ty(TypeEnum::TFunc(FunSignature {
                ret: store.float,
                vars: HashMap::new(),
                args: vec![FuncArg {
                    ty: store.int64,
                    is_optional: false,
                    name: "other".into()
                }]
            }))
        );

        for op in &[
            ast::Cmpop::Eq,
            ast::Cmpop::NotEq,
            ast::Cmpop::Lt,
            ast::Cmpop::LtE,
            ast::Cmpop::Gt,
            ast::Cmpop::GtE,
        ] {
            fields.insert(
                comparison_name(op).unwrap().to_string(), 
                unifier.add_ty(TypeEnum::TFunc(FunSignature {
                    ret: store.bool,
                    vars: HashMap::new(),
                    args: vec![FuncArg {
                        ty: store.int64,
                        is_optional: false,
                        name: "other".into()
                    }],
                }))
            );
        }

        for op in &[
            ast::Unaryop::UAdd,
            ast::Unaryop::USub,
            ast::Unaryop::Not,
            ast::Unaryop::Invert,
        ] {
            fields.insert(
                unaryop_name(op).into(), 
                unifier.add_ty(TypeEnum::TFunc(FunSignature {
                    ret: store.int64,
                    vars: HashMap::new(),
                    args: vec![]
                }))
            );
        }
    } else { unreachable!() }
    // int64 --------
    // float --------
    if let Some(TypeEnum::TObj {fields, .. }) = Rc::get_mut(&mut unifier.get_ty(store.float)) {
        for op in &[
        ast::Operator::Add, 
        ast::Operator::Sub, 
        ast::Operator::Mult, 
        ast::Operator::Div, 
        ast::Operator::Mod, 
        ast::Operator::Pow,
        ast::Operator::FloorDiv,
        ] {
            fields.insert(
                binop_name(op).to_string(), 
                unifier.add_ty(TypeEnum::TFunc(FunSignature {
                    ret: store.float,
                    vars: HashMap::new(),
                    args: vec![FuncArg {
                        ty: store.float,
                        is_optional: false,
                        name: "other".into() // the name does not matter here
                    }],
                }))
            );

            fields.insert(
                binop_assign_name(op).to_string(), 
                unifier.add_ty(TypeEnum::TFunc(FunSignature {
                    ret: store.none,
                    vars: HashMap::new(),
                    args: vec![FuncArg {
                        ty: store.float,
                        is_optional: false,
                        name: "other".into()
                    }]
                }))
            );
        };

        for op in &[
            ast::Cmpop::Eq,
            ast::Cmpop::NotEq,
            ast::Cmpop::Lt,
            ast::Cmpop::LtE,
            ast::Cmpop::Gt,
            ast::Cmpop::GtE,
        ] {
            fields.insert(
                comparison_name(op).unwrap().to_string(), 
                unifier.add_ty(TypeEnum::TFunc(FunSignature {
                    ret: store.bool,
                    vars: HashMap::new(),
                    args: vec![FuncArg {
                        ty: store.float,
                        is_optional: false,
                        name: "other".into()
                    }],
                }))
            );
        }

        for op in &[
            ast::Unaryop::UAdd,
            ast::Unaryop::USub,
            ast::Unaryop::Not,
        ] {
            fields.insert(
                unaryop_name(op).into(), 
                unifier.add_ty(TypeEnum::TFunc(FunSignature {
                    ret: store.int64,
                    vars: HashMap::new(),
                    args: vec![]
                }))
            );
        }
    } else { unreachable!() }
    // float --------
    // bool ---------
    if let Some(TypeEnum::TObj {fields, .. }) = Rc::get_mut(&mut unifier.get_ty(store.bool)) {
        for op in &[
        ast::Operator::Add, 
        ast::Operator::Sub, 
        ast::Operator::Mult, 
        ast::Operator::Mod, 
        ast::Operator::Pow, 
        ast::Operator::LShift, 
        ast::Operator::RShift, 
        ast::Operator::FloorDiv
        ] {
            fields.insert(
                binop_name(op).into(),
                unifier.add_ty(TypeEnum::TFunc(FunSignature {
                    ret: store.int32,
                    vars: HashMap::new(),
                    args: vec![FuncArg {
                        ty: store.bool,
                        is_optional: false,
                        name: "other".into()
                    }]
                }))
            );

            fields.insert(
                binop_name(op).into(),
                unifier.add_ty(TypeEnum::TFunc(FunSignature {
                    ret: store.int32,
                    vars: HashMap::new(),
                    args: vec![FuncArg {
                        ty: store.int32,
                        is_optional: false,
                        name: "other".into()
                    }]
                }))
            );

            // binop_assignment will change type?
            /* fields.insert(
                binop_assignment_name(op).into(),
                unifier.add_ty(TypeEnum::TFunc(FunSignature {
                    ret: store.none,
                    vars: HashMap::new(),
                    args: vec![FuncArg {
                        ty: store.bool,
                        is_optional: false,
                        name: "other".into()
                    }]
                }))
            ); */
        };

        for op in &[
            ast::Operator::BitOr,
            ast::Operator::BitXor,
            ast::Operator::BitAnd
        ] {
            fields.insert(
                binop_name(op).into(),
                unifier.add_ty(TypeEnum::TFunc(FunSignature {
                    ret: store.bool,
                    vars: HashMap::new(),
                    args: vec![FuncArg {
                        ty: store.int32,
                        is_optional: false,
                        name: "other".into()
                    }]
                }))
            );
        };

        for op in &[
            ast::Cmpop::Eq,
            ast::Cmpop::NotEq,
            ast::Cmpop::Lt,
            ast::Cmpop::LtE,
            ast::Cmpop::Gt,
            ast::Cmpop::GtE,
        ] {
            fields.insert(
                comparison_name(op).unwrap().into(), 
                unifier.add_ty(TypeEnum::TFunc(FunSignature {
                    ret: store.int32,
                    vars: HashMap::new(),
                    args: vec![FuncArg {
                        ty: store.bool,
                        is_optional: false,
                        name: "other".into()
                    }]
                }))
            );
        }
    }
    // bool --------
}