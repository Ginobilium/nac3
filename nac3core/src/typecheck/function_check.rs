use super::type_inferencer::Inferencer;
use super::typedef::Type;
use rustpython_parser::ast::{self, Expr, ExprKind, StmtKind};
use std::iter::once;

impl<'a> Inferencer<'a> {
    fn check_expr(
        &mut self,
        expr: &Expr<Option<Type>>,
        defined_identifiers: &[String],
    ) -> Result<(), String> {
        if let Some(ty) = &expr.custom {
            let ty = self.unifier.get_ty(*ty);
            let ty = ty.as_ref().borrow();
            if ty.is_concrete() {
                return Err(format!(
                    "expected concrete type at {:?} but got {}",
                    expr.location,
                    ty.get_type_name()
                ));
            }
        }
        match &expr.node {
            ExprKind::Name { id, .. } => {
                if !defined_identifiers.contains(id) {
                    return Err(format!("unknown identifier {} (use before def?)", id));
                }
            }
            ExprKind::List { elts, .. }
            | ExprKind::Tuple { elts, .. }
            | ExprKind::BoolOp { values: elts, .. } => {
                for elt in elts.iter() {
                    self.check_expr(elt, defined_identifiers)?;
                }
            }
            ExprKind::Attribute { value, .. } => {
                self.check_expr(value.as_ref(), defined_identifiers)?;
            }
            ExprKind::BinOp { left, right, .. } => {
                self.check_expr(left.as_ref(), defined_identifiers)?;
                self.check_expr(right.as_ref(), defined_identifiers)?;
            }
            ExprKind::UnaryOp { operand, .. } => {
                self.check_expr(operand.as_ref(), defined_identifiers)?;
            }
            ExprKind::Compare {
                left, comparators, ..
            } => {
                for elt in once(left.as_ref()).chain(comparators.iter()) {
                    self.check_expr(elt, defined_identifiers)?;
                }
            }
            ExprKind::Subscript { value, slice, .. } => {
                self.check_expr(value.as_ref(), defined_identifiers)?;
                self.check_expr(slice.as_ref(), defined_identifiers)?;
            }
            ExprKind::IfExp { test, body, orelse } => {
                self.check_expr(test.as_ref(), defined_identifiers)?;
                self.check_expr(body.as_ref(), defined_identifiers)?;
                self.check_expr(orelse.as_ref(), defined_identifiers)?;
            }
            ExprKind::Slice { lower, upper, step } => {
                for elt in [lower.as_ref(), upper.as_ref(), step.as_ref()]
                    .iter()
                    .flatten()
                {
                    self.check_expr(elt, defined_identifiers)?;
                }
            }
            ExprKind::ListComp { .. } => unimplemented!(),
            ExprKind::Lambda { .. } => unimplemented!(),
            _ => {}
        }
        Ok(())
    }
}
