use std::convert::TryInto;

use crate::{top_level::CodeGenContext, typecheck::typedef::Type};
use inkwell::{
    types::BasicTypeEnum,
    values::{BasicValueEnum, PointerValue},
};
use rustpython_parser::ast::{Expr, ExprKind, Stmt, StmtKind};

impl<'ctx> CodeGenContext<'ctx> {
    fn gen_var(&mut self, ty: Type) -> PointerValue<'ctx> {
        let ty = self.get_llvm_type(ty);
        let ty = if let BasicTypeEnum::PointerType(ty) = ty {
            ty.get_element_type().try_into().unwrap()
        } else {
            ty
        };
        self.builder.build_alloca(ty, "tmp")
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
            StmtKind::Assign { targets, value, .. } => {
                let value = self.gen_expr(&value);
                for target in targets.iter() {
                    self.gen_assignment(target, value);
                }
            }
            _ => unimplemented!(),
        }
    }
}
