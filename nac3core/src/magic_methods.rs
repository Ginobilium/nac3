use rustpython_parser::ast::{Comparison, Operator, UnaryOperator};

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

pub fn unaryop_name(op: &UnaryOperator) -> &'static str {
    match op {
        UnaryOperator::Pos => "__pos__",
        UnaryOperator::Neg => "__neg__",
        UnaryOperator::Not => "__not__",
        UnaryOperator::Inv => "__inv__",
    }
}

pub fn comparison_name(op: &Comparison) -> Option<&'static str> {
    match op {
        Comparison::Less => Some("__lt__"),
        Comparison::LessOrEqual => Some("__le__"),
        Comparison::Greater => Some("__gt__"),
        Comparison::GreaterOrEqual => Some("__ge__"),
        Comparison::Equal => Some("__eq__"),
        Comparison::NotEqual => Some("__ne__"),
        _ => None,
    }
}
