use super::CodeGenContext;
use crate::typecheck::typedef::Type;
use inkwell::values::{BasicValue, BasicValueEnum, PointerValue};
use rustpython_parser::ast::{Expr, ExprKind, Stmt, StmtKind};

impl<'ctx, 'a> CodeGenContext<'ctx, 'a> {
    fn gen_var(&mut self, ty: Type) -> PointerValue<'ctx> {
        // put the alloca in init block
        let current = self.builder.get_insert_block().unwrap();
        // position before the last branching instruction...
        self.builder.position_before(&self.init_bb.get_last_instruction().unwrap());
        let ty = self.get_llvm_type(ty);
        let ptr = self.builder.build_alloca(ty, "tmp");
        self.builder.position_at_end(current);
        ptr
    }

    fn parse_pattern(&mut self, pattern: &Expr<Option<Type>>) -> PointerValue<'ctx> {
        // very similar to gen_expr, but we don't do an extra load at the end
        // and we flatten nested tuples
        match &pattern.node {
            ExprKind::Name { id, .. } => {
                self.var_assignment.get(id).cloned().unwrap_or_else(|| {
                    let ptr = self.gen_var(pattern.custom.unwrap());
                    self.var_assignment.insert(id.clone(), ptr);
                    ptr
                })
            }
            ExprKind::Attribute { value, attr, .. } => {
                let index = self.get_attr_index(value.custom.unwrap(), attr);
                let val = self.gen_expr(value);
                let ptr = if let BasicValueEnum::PointerValue(v) = val {
                    v
                } else {
                    unreachable!();
                };
                unsafe {
                    ptr.const_in_bounds_gep(&[
                        self.ctx.i32_type().const_zero(),
                        self.ctx.i32_type().const_int(index as u64, false),
                    ])
                }
            }
            ExprKind::Subscript { .. } => unimplemented!(),
            _ => unreachable!(),
        }
    }

    fn gen_assignment(&mut self, target: &Expr<Option<Type>>, value: BasicValueEnum<'ctx>) {
        if let ExprKind::Tuple { elts, .. } = &target.node {
            if let BasicValueEnum::PointerValue(ptr) = value {
                for (i, elt) in elts.iter().enumerate() {
                    unsafe {
                        let t = ptr.const_in_bounds_gep(&[
                            self.ctx.i32_type().const_zero(),
                            self.ctx.i32_type().const_int(i as u64, false),
                        ]);
                        let v = self.builder.build_load(t, "tmpload");
                        self.gen_assignment(elt, v);
                    }
                }
            } else {
                unreachable!()
            }
        } else {
            let ptr = self.parse_pattern(target);
            self.builder.build_store(ptr, value);
        }
    }

    pub fn gen_stmt(&mut self, stmt: &Stmt<Option<Type>>) {
        match &stmt.node {
            StmtKind::Expr { value } => {
                self.gen_expr(&value);
            }
            StmtKind::Return { value } => {
                let value = value.as_ref().map(|v| self.gen_expr(&v));
                let value = value.as_ref().map(|v| v as &dyn BasicValue);
                self.builder.build_return(value);
            }
            StmtKind::AnnAssign { target, value, .. } => {
                if let Some(value) = value {
                    let value = self.gen_expr(&value);
                    self.gen_assignment(target, value);
                }
            }
            StmtKind::Assign { targets, value, .. } => {
                let value = self.gen_expr(&value);
                for target in targets.iter() {
                    self.gen_assignment(target, value);
                }
            }
            StmtKind::Continue => {
                self.builder.build_unconditional_branch(self.loop_bb.unwrap().0);
            }
            StmtKind::Break => {
                self.builder.build_unconditional_branch(self.loop_bb.unwrap().1);
            }
            StmtKind::While { test, body, orelse } => {
                let current = self.builder.get_insert_block().unwrap().get_parent().unwrap();
                let test_bb = self.ctx.append_basic_block(current, "test");
                let body_bb = self.ctx.append_basic_block(current, "body");
                let cont_bb = self.ctx.append_basic_block(current, "cont");
                // if there is no orelse, we just go to cont_bb
                let orelse_bb = if orelse.is_empty() {
                    cont_bb
                } else {
                    self.ctx.append_basic_block(current, "orelse")
                };
                // store loop bb information and restore it later
                let loop_bb = self.loop_bb.replace((test_bb, cont_bb));
                self.builder.build_unconditional_branch(test_bb);
                self.builder.position_at_end(test_bb);
                let test = self.gen_expr(test);
                if let BasicValueEnum::IntValue(test) = test {
                    self.builder.build_conditional_branch(test, body_bb, orelse_bb);
                } else {
                    unreachable!()
                };
                self.builder.position_at_end(body_bb);
                for stmt in body.iter() {
                    self.gen_stmt(stmt);
                }
                self.builder.build_unconditional_branch(test_bb);
                if !orelse.is_empty() {
                    self.builder.position_at_end(orelse_bb);
                    for stmt in orelse.iter() {
                        self.gen_stmt(stmt);
                    }
                    self.builder.build_unconditional_branch(cont_bb);
                }
                self.builder.position_at_end(cont_bb);
                self.loop_bb = loop_bb;
            }
            _ => unimplemented!(),
        }
    }
}
