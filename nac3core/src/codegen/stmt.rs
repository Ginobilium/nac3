use super::{
    expr::{assert_int_val, assert_pointer_val},
    CodeGenContext,
};
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
                let val = self.gen_expr(value).unwrap();
                let ptr = if let BasicValueEnum::PointerValue(v) = val {
                    v
                } else {
                    unreachable!();
                };
                unsafe {
                    self.builder.build_in_bounds_gep(
                        ptr,
                        &[
                            self.ctx.i32_type().const_zero(),
                            self.ctx.i32_type().const_int(index as u64, false),
                        ],
                        "attr",
                    )
                }
            }
            ExprKind::Subscript { value, slice, .. } => {
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
                    self.builder.build_gep(arr_ptr, &[index], "loadarrgep")
                }
            }
            _ => unreachable!(),
        }
    }

    fn gen_assignment(&mut self, target: &Expr<Option<Type>>, value: BasicValueEnum<'ctx>) {
        let i32_type = self.ctx.i32_type();
        if let ExprKind::Tuple { elts, .. } = &target.node {
            if let BasicValueEnum::PointerValue(ptr) = value {
                for (i, elt) in elts.iter().enumerate() {
                    unsafe {
                        let t = self.builder.build_in_bounds_gep(
                            ptr,
                            &[i32_type.const_zero(), i32_type.const_int(i as u64, false)],
                            "elem",
                        );
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

    // return true if it contains terminator
    pub fn gen_stmt(&mut self, stmt: &Stmt<Option<Type>>) -> bool {
        match &stmt.node {
            StmtKind::Expr { value } => {
                self.gen_expr(&value);
            }
            StmtKind::Return { value } => {
                let value = value.as_ref().map(|v| self.gen_expr(&v).unwrap());
                let value = value.as_ref().map(|v| v as &dyn BasicValue);
                self.builder.build_return(value);
                return true;
            }
            StmtKind::AnnAssign { target, value, .. } => {
                if let Some(value) = value {
                    let value = self.gen_expr(&value).unwrap();
                    self.gen_assignment(target, value);
                }
            }
            StmtKind::Assign { targets, value, .. } => {
                let value = self.gen_expr(&value).unwrap();
                for target in targets.iter() {
                    self.gen_assignment(target, value);
                }
            }
            StmtKind::Continue => {
                self.builder.build_unconditional_branch(self.loop_bb.unwrap().0);
                return true;
            }
            StmtKind::Break => {
                self.builder.build_unconditional_branch(self.loop_bb.unwrap().1);
                return true;
            }
            StmtKind::If { test, body, orelse } => {
                let current = self.builder.get_insert_block().unwrap().get_parent().unwrap();
                let test_bb = self.ctx.append_basic_block(current, "test");
                let body_bb = self.ctx.append_basic_block(current, "body");
                let mut cont_bb = None; // self.ctx.append_basic_block(current, "cont");
                                        // if there is no orelse, we just go to cont_bb
                let orelse_bb = if orelse.is_empty() {
                    cont_bb = Some(self.ctx.append_basic_block(current, "cont"));
                    cont_bb.unwrap()
                } else {
                    self.ctx.append_basic_block(current, "orelse")
                };
                self.builder.build_unconditional_branch(test_bb);
                self.builder.position_at_end(test_bb);
                let test = self.gen_expr(test).unwrap();
                if let BasicValueEnum::IntValue(test) = test {
                    self.builder.build_conditional_branch(test, body_bb, orelse_bb);
                } else {
                    unreachable!()
                };
                self.builder.position_at_end(body_bb);
                let mut exited = false;
                for stmt in body.iter() {
                    exited = self.gen_stmt(stmt);
                    if exited {
                        break;
                    }
                }
                if !exited {
                    if cont_bb.is_none() {
                        cont_bb = Some(self.ctx.append_basic_block(current, "cont"));
                    }
                    self.builder.build_unconditional_branch(cont_bb.unwrap());
                }
                if !orelse.is_empty() {
                    exited = false;
                    self.builder.position_at_end(orelse_bb);
                    for stmt in orelse.iter() {
                        exited = self.gen_stmt(stmt);
                        if exited {
                            break;
                        }
                    }
                    if !exited {
                        if cont_bb.is_none() {
                            cont_bb = Some(self.ctx.append_basic_block(current, "cont"));
                        }
                        self.builder.build_unconditional_branch(cont_bb.unwrap());
                    }
                }
                if let Some(cont_bb) = cont_bb {
                    self.builder.position_at_end(cont_bb);
                }
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
                let test = self.gen_expr(test).unwrap();
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
            _ => unimplemented!("{:?}", stmt),
        };
        false
    }
}
