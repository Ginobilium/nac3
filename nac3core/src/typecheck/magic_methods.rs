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
