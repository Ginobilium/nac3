use crate::typecheck::typedef::TypeEnum;

use super::type_inferencer::Inferencer;
use super::typedef::Type;
use rustpython_parser::ast::{self, Expr, ExprKind, Stmt, StmtKind};
use std::{collections::HashSet, iter::once};

impl<'a> Inferencer<'a> {
    fn check_pattern(
        &mut self,
        pattern: &Expr<Option<Type>>,
        defined_identifiers: &mut HashSet<String>,
    ) -> Result<(), String> {
        match &pattern.node {
            ExprKind::Name { id, .. } => {
                if !defined_identifiers.contains(id) {
                    defined_identifiers.insert(id.clone());
                }
                Ok(())
            }
            ExprKind::Tuple { elts, .. } => {
                for elt in elts.iter() {
                    self.check_pattern(elt, defined_identifiers)?;
                }
                Ok(())
            }
            ExprKind::Subscript { value, slice, .. } => {
                self.check_expr(value, defined_identifiers)?;
                self.check_expr(slice, defined_identifiers)?;
                if let TypeEnum::TTuple { .. } = &*self.unifier.get_ty(value.custom.unwrap()) {
                    return Err(format!(
                        "Error at {}: cannot assign to tuple element",
                        value.location
                    ));
                }
                Ok(())
            }
            _ => self.check_expr(pattern, defined_identifiers),
        }
    }

    fn check_expr(
        &mut self,
        expr: &Expr<Option<Type>>,
        defined_identifiers: &mut HashSet<String>,
    ) -> Result<(), String> {
        // there are some cases where the custom field is None
        if let Some(ty) = &expr.custom {
            if !self.unifier.is_concrete(*ty, &self.function_data.bound_variables) {
                return Err(format!(
                    "expected concrete type at {} but got {}",
                    expr.location,
                    self.unifier.get_ty(*ty).get_type_name()
                ));
            }
        }
        match &expr.node {
            ExprKind::Name { id, .. } => {
                if !defined_identifiers.contains(id) {
                    if self.function_data.resolver.get_identifier_def(id).is_some() {
                        defined_identifiers.insert(id.clone());
                    } else {
                        return Err(format!(
                            "unknown identifier {} (use before def?) at {}",
                            id, expr.location
                        ));
                    }
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
                self.check_expr(value, defined_identifiers)?;
            }
            ExprKind::BinOp { left, right, .. } => {
                self.check_expr(left, defined_identifiers)?;
                self.check_expr(right, defined_identifiers)?;
            }
            ExprKind::UnaryOp { operand, .. } => {
                self.check_expr(operand, defined_identifiers)?;
            }
            ExprKind::Compare { left, comparators, .. } => {
                for elt in once(left.as_ref()).chain(comparators.iter()) {
                    self.check_expr(elt, defined_identifiers)?;
                }
            }
            ExprKind::Subscript { value, slice, .. } => {
                self.check_expr(value, defined_identifiers)?;
                self.check_expr(slice, defined_identifiers)?;
            }
            ExprKind::IfExp { test, body, orelse } => {
                self.check_expr(test, defined_identifiers)?;
                self.check_expr(body, defined_identifiers)?;
                self.check_expr(orelse, defined_identifiers)?;
            }
            ExprKind::Slice { lower, upper, step } => {
                for elt in [lower.as_ref(), upper.as_ref(), step.as_ref()].iter().flatten() {
                    self.check_expr(elt, defined_identifiers)?;
                }
            }
            ExprKind::Lambda { args, body } => {
                let mut defined_identifiers = defined_identifiers.clone();
                for arg in args.args.iter() {
                    if !defined_identifiers.contains(&arg.node.arg) {
                        defined_identifiers.insert(arg.node.arg.clone());
                    }
                }
                self.check_expr(body, &mut defined_identifiers)?;
            }
            ExprKind::ListComp { elt, generators, .. } => {
                // in our type inference stage, we already make sure that there is only 1 generator
                let ast::Comprehension { target, iter, ifs, .. } = &generators[0];
                self.check_expr(iter, defined_identifiers)?;
                let mut defined_identifiers = defined_identifiers.clone();
                self.check_pattern(target, &mut defined_identifiers)?;
                for term in once(elt.as_ref()).chain(ifs.iter()) {
                    self.check_expr(term, &mut defined_identifiers)?;
                }
            }
            ExprKind::Call { func, args, keywords } => {
                for expr in once(func.as_ref())
                    .chain(args.iter())
                    .chain(keywords.iter().map(|v| v.node.value.as_ref()))
                {
                    self.check_expr(expr, defined_identifiers)?;
                }
            }
            ExprKind::Constant { .. } => {}
            _ => {
                println!("{:?}", expr.node);
                unimplemented!()
            }
        }
        Ok(())
    }

    // check statements for proper identifier def-use and return on all paths
    fn check_stmt(
        &mut self,
        stmt: &Stmt<Option<Type>>,
        defined_identifiers: &mut HashSet<String>,
    ) -> Result<bool, String> {
        match &stmt.node {
            StmtKind::For { target, iter, body, orelse, .. } => {
                self.check_expr(iter, defined_identifiers)?;
                for stmt in orelse.iter() {
                    self.check_stmt(stmt, defined_identifiers)?;
                }
                let mut defined_identifiers = defined_identifiers.clone();
                self.check_pattern(target, &mut defined_identifiers)?;
                for stmt in body.iter() {
                    self.check_stmt(stmt, &mut defined_identifiers)?;
                }
                Ok(false)
            }
            StmtKind::If { test, body, orelse } => {
                self.check_expr(test, defined_identifiers)?;
                let mut body_identifiers = defined_identifiers.clone();
                let mut orelse_identifiers = defined_identifiers.clone();
                let body_returned = self.check_block(body, &mut body_identifiers)?;
                let orelse_returned = self.check_block(orelse, &mut orelse_identifiers)?;

                for ident in body_identifiers.iter() {
                    if !defined_identifiers.contains(ident) && orelse_identifiers.contains(ident) {
                        defined_identifiers.insert(ident.clone());
                    }
                }
                Ok(body_returned && orelse_returned)
            }
            StmtKind::While { test, body, orelse } => {
                self.check_expr(test, defined_identifiers)?;
                let mut defined_identifiers = defined_identifiers.clone();
                self.check_block(body, &mut defined_identifiers)?;
                self.check_block(orelse, &mut defined_identifiers)?;
                Ok(false)
            }
            StmtKind::Expr { value } => {
                self.check_expr(value, defined_identifiers)?;
                Ok(false)
            }
            StmtKind::Assign { targets, value, .. } => {
                self.check_expr(value, defined_identifiers)?;
                for target in targets {
                    self.check_pattern(target, defined_identifiers)?;
                }
                Ok(false)
            }
            StmtKind::AnnAssign { target, value, .. } => {
                if let Some(value) = value {
                    self.check_expr(value, defined_identifiers)?;
                    self.check_pattern(target, defined_identifiers)?;
                }
                Ok(false)
            }
            StmtKind::Return { value } => {
                if let Some(value) = value {
                    self.check_expr(value, defined_identifiers)?;
                }
                Ok(true)
            }
            StmtKind::Raise { exc, .. } => {
                if let Some(value) = exc {
                    self.check_expr(value, defined_identifiers)?;
                }
                Ok(true)
            }
            // break, raise, etc.
            _ => Ok(false),
        }
    }

    pub fn check_block(
        &mut self,
        block: &[Stmt<Option<Type>>],
        defined_identifiers: &mut HashSet<String>,
    ) -> Result<bool, String> {
        let mut ret = false;
        for stmt in block {
            if ret {
                return Err(format!("dead code at {:?}", stmt.location));
            }
            if self.check_stmt(stmt, defined_identifiers)? {
                ret = true;
            }
        }
        Ok(ret)
    }
}
