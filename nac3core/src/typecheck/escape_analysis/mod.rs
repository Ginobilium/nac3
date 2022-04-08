use std::sync::Arc;

use itertools::chain;
use nac3parser::ast::{Comprehension, Constant, Expr, ExprKind, Location, Stmt, StmtKind, StrRef};

use lifetime::{BasicBlockId, LifetimeIR, LifetimeIRBuilder, LifetimeId, LifetimeKind};

use crate::{
    symbol_resolver::SymbolResolver,
    toplevel::{TopLevelContext, TopLevelDef},
};

use super::{
    type_inferencer::PrimitiveStore,
    typedef::{Type, TypeEnum, Unifier},
};

#[cfg(test)]
mod test;

mod lifetime;

pub struct EscapeAnalyzer<'a> {
    builder: LifetimeIRBuilder,
    loop_head: Option<BasicBlockId>,
    loop_tail: Option<BasicBlockId>,
    unifier: &'a mut Unifier,
    primitive_store: &'a PrimitiveStore,
    resolver: Arc<dyn SymbolResolver + Send + Sync>,
    top_level: &'a TopLevelContext,
}

impl<'a> EscapeAnalyzer<'a> {
    pub fn new(
        unifier: &'a mut Unifier,
        primitive_store: &'a PrimitiveStore,
        resolver: Arc<dyn SymbolResolver + Send + Sync>,
        top_level: &'a TopLevelContext,
    ) -> Self {
        Self {
            builder: LifetimeIRBuilder::new(),
            loop_head: None,
            loop_tail: None,
            primitive_store,
            unifier,
            resolver,
            top_level,
        }
    }

    pub fn check_function_lifetime(
        unifier: &'a mut Unifier,
        primitive_store: &'a PrimitiveStore,
        resolver: Arc<dyn SymbolResolver + Send + Sync>,
        top_level: &'a TopLevelContext,
        args: &[(StrRef, Type)],
        body: &[Stmt<Option<Type>>],
        loc: Location,
    ) -> Result<(), String> {
        use LifetimeIR::{CreateLifetime, VarAssign};
        let mut zelf = Self::new(unifier, primitive_store, resolver, top_level);
        let nonlocal_lifetime =
            zelf.builder.append_ir(CreateLifetime { kind: LifetimeKind::NonLocal }, loc);
        for (name, ty) in args.iter().copied() {
            if zelf.need_alloca(ty) {
                zelf.builder.append_ir(VarAssign { var: name, lifetime: nonlocal_lifetime }, loc);
            }
        }
        zelf.handle_statements(body)?;
        zelf.builder.remove_empty_bb();
        zelf.builder.analyze().map_err(|e| {
            format!("{}\nIR: {}", e, zelf.builder.print_ir())
        })
    }

    fn need_alloca(&mut self, ty: Type) -> bool {
        !(self.unifier.unioned(ty, self.primitive_store.int32)
            || self.unifier.unioned(ty, self.primitive_store.int64)
            || self.unifier.unioned(ty, self.primitive_store.uint32)
            || self.unifier.unioned(ty, self.primitive_store.uint64)
            || self.unifier.unioned(ty, self.primitive_store.float)
            || self.unifier.unioned(ty, self.primitive_store.bool)
            || self.unifier.unioned(ty, self.primitive_store.none)
            || self.unifier.unioned(ty, self.primitive_store.range))
    }

    fn is_terminated(&self) -> bool {
        self.builder.is_terminated(self.builder.get_current_block())
    }

    fn handle_unknown_function_call<P: std::borrow::Borrow<Expr<Option<Type>>>>(
        &mut self,
        params: &[P],
        ret_need_alloca: bool,
        loc: Location,
    ) -> Result<Option<LifetimeId>, String> {
        let param_lifetimes = params
            .iter()
            .filter_map(|p| self.handle_expr(p.borrow()).transpose())
            .collect::<Result<Vec<_>, _>>()?;
        self.builder.append_ir(LifetimeIR::PassedToFunc { param_lifetimes }, loc);
        if ret_need_alloca {
            Ok(Some(
                self.builder
                    .append_ir(LifetimeIR::CreateLifetime { kind: LifetimeKind::Unknown }, loc),
            ))
        } else {
            Ok(None)
        }
    }

    fn handle_expr(&mut self, expr: &Expr<Option<Type>>) -> Result<Option<LifetimeId>, String> {
        use LifetimeIR::*;
        use LifetimeKind::*;
        let need_alloca = self.need_alloca(expr.custom.unwrap());
        let loc = expr.location;
        Ok(match &expr.node {
            ExprKind::Name { id, .. } => {
                if need_alloca {
                    Some(self.builder.append_ir(VarAccess { var: *id }, loc))
                } else {
                    None
                }
            }
            ExprKind::Attribute { value, attr, .. } => {
                if need_alloca {
                    let val = self.handle_expr(value)?.unwrap();
                    Some(self.builder.append_ir(FieldAccess { obj: val, field: *attr }, loc))
                } else {
                    self.handle_expr(value)?;
                    None
                }
            }
            ExprKind::Constant { .. } => {
                if need_alloca {
                    Some(self.builder.append_ir(CreateLifetime { kind: Static }, loc))
                } else {
                    None
                }
            }
            ExprKind::List { elts, .. } => {
                let elems =
                    elts.iter().map(|e| self.handle_expr(e)).collect::<Result<Vec<_>, _>>()?;
                let list_lifetime =
                    self.builder.append_ir(CreateLifetime { kind: PreciseLocal }, loc);
                if !elems.is_empty() {
                    if elems[0].is_some() {
                        let elems = elems.into_iter().map(|e| e.unwrap()).collect::<Vec<_>>();
                        let elem_lifetime =
                            self.builder.append_ir(UnifyLifetimes { lifetimes: elems }, loc);
                        self.builder.append_ir(
                            FieldAssign {
                                obj: list_lifetime,
                                field: "$elem".into(),
                                new: elem_lifetime,
                                is_init: true,
                            },
                            loc,
                        );
                    }
                } else {
                    let elem_lifetime =
                        self.builder.append_ir(CreateLifetime { kind: PreciseLocal }, loc);
                    self.builder.append_ir(
                        FieldAssign {
                            obj: list_lifetime,
                            field: "$elem".into(),
                            new: elem_lifetime,
                            is_init: true,
                        },
                        loc,
                    );
                }
                Some(list_lifetime)
            }
            ExprKind::Tuple { elts, .. } => {
                let elems =
                    elts.iter().map(|e| self.handle_expr(e)).collect::<Result<Vec<_>, _>>()?;
                let tuple_lifetime =
                    self.builder.append_ir(CreateLifetime { kind: PreciseLocal }, loc);
                for (i, lifetime) in elems.into_iter().enumerate() {
                    if let Some(lifetime) = lifetime {
                        self.builder.append_ir(
                            FieldAssign {
                                obj: tuple_lifetime,
                                field: format!("$elem{}", i).into(),
                                new: lifetime,
                                is_init: true,
                            },
                            loc,
                        );
                    }
                }
                Some(tuple_lifetime)
            }
            ExprKind::Subscript { value, slice, .. } => {
                let value_lifetime = self.handle_expr(value)?.unwrap();
                match &slice.node {
                    ExprKind::Slice { lower, upper, step } => {
                        for expr in [lower, upper, step].iter().filter_map(|x| x.as_ref()) {
                            self.handle_expr(expr)?;
                        }
                        let slice_lifetime =
                            self.builder.append_ir(CreateLifetime { kind: PreciseLocal }, loc);
                        let slice_elem = self.builder.append_ir(
                            FieldAccess { obj: value_lifetime, field: "$elem".into() },
                            loc,
                        );
                        self.builder.append_ir(
                            FieldAssign {
                                obj: slice_lifetime,
                                field: "$elem".into(),
                                new: slice_elem,
                                is_init: true
                            },
                            loc,
                        );
                        Some(slice_lifetime)
                    }
                    ExprKind::Constant { value: Constant::Int(v), .. }
                        if matches!(
                            &*self.unifier.get_ty(value.custom.unwrap()),
                            TypeEnum::TTuple { .. }
                        ) =>
                    {
                        Some(self.builder.append_ir(
                            FieldAccess {
                                obj: value_lifetime,
                                field: format!("$elem{}", v).into(),
                            },
                            loc,
                        ))
                    }
                    _ => {
                        self.handle_expr(slice)?;
                        if need_alloca {
                            Some(self.builder.append_ir(
                                FieldAccess { obj: value_lifetime, field: "$elem".into() },
                                loc,
                            ))
                        } else {
                            None
                        }
                    }
                }
            }
            ExprKind::Call { func, args, keywords } => {
                let mut lifetimes = vec![];
                for arg in chain!(args.iter(), keywords.iter().map(|k| k.node.value.as_ref())) {
                    if let Some(lifetime) = self.handle_expr(arg)? {
                        lifetimes.push(lifetime);
                    }
                }
                match &func.node {
                    ExprKind::Name { id, .. } => {
                        if !lifetimes.is_empty() {
                            self.builder.append_ir(PassedToFunc { param_lifetimes: lifetimes }, loc);
                        }
                        if need_alloca {
                            let id = self
                                .resolver
                                .get_identifier_def(*id)
                                .map_err(|e| format!("{} (at {})", e, func.location))?;
                            if let TopLevelDef::Class { .. } =
                                &*self.top_level.definitions.read()[id.0].read()
                            {
                                Some(
                                    self.builder
                                        .append_ir(CreateLifetime { kind: PreciseLocal }, loc),
                                )
                            } else {
                                Some(self.builder.append_ir(CreateLifetime { kind: Unknown }, loc))
                            }
                        } else {
                            None
                        }
                    }
                    ExprKind::Attribute { value, .. } => {
                        let obj_lifetime = self.handle_expr(value)?.unwrap();
                        lifetimes.push(obj_lifetime);
                        self.builder.append_ir(PassedToFunc { param_lifetimes: lifetimes }, loc);
                        if need_alloca {
                            Some(self.builder.append_ir(CreateLifetime { kind: Unknown }, loc))
                        } else {
                            None
                        }
                    }
                    _ => unimplemented!(),
                }
            }
            ExprKind::BinOp { left, right, .. } => self.handle_unknown_function_call(
                &[left.as_ref(), right.as_ref()],
                need_alloca,
                loc,
            )?,
            ExprKind::BoolOp { values, .. } => {
                self.handle_unknown_function_call(&values, need_alloca, loc)?
            }
            ExprKind::UnaryOp { operand, .. } => {
                self.handle_unknown_function_call(&[operand.as_ref()], need_alloca, loc)?
            }
            ExprKind::Compare { left, comparators, .. } => {
                self.handle_unknown_function_call(&[left.as_ref()], false, loc)?;
                self.handle_unknown_function_call(&comparators, need_alloca, loc)?
            }
            ExprKind::IfExp { test, body, orelse } => {
                self.handle_expr(test)?;
                let body_bb = self.builder.append_block();
                let else_bb = self.builder.append_block();
                let tail_bb = self.builder.append_block();
                self.builder.append_ir(Branch { targets: vec![body_bb, else_bb] }, test.location);
                self.builder.position_at_end(body_bb);
                let body_lifetime = self.handle_expr(body)?;
                self.builder.append_ir(Branch { targets: vec![tail_bb] }, body.location);
                self.builder.position_at_end(else_bb);
                let else_lifetime = self.handle_expr(body)?;
                self.builder.append_ir(Branch { targets: vec![tail_bb] }, orelse.location);
                self.builder.position_at_end(tail_bb);
                if let (Some(body_lifetime), Some(else_lifetime)) = (body_lifetime, else_lifetime) {
                    Some(self.builder.append_ir(
                        UnifyLifetimes { lifetimes: vec![body_lifetime, else_lifetime] },
                        loc,
                    ))
                } else {
                    None
                }
            }
            ExprKind::ListComp { elt, generators } => {
                let Comprehension { target, iter, ifs, .. } = &generators[0];
                let list_lifetime =
                    self.builder.append_ir(CreateLifetime { kind: PreciseLocal }, loc);
                let iter_elem_lifetime = self.handle_expr(iter)?.map(|obj| {
                    self.builder
                        .append_ir(FieldAccess { obj, field: "$elem".into() }, iter.location)
                });
                let loop_body = self.builder.append_block();
                let loop_tail = self.builder.append_block();
                self.builder.append_ir(Branch { targets: vec![loop_body] }, loc);
                self.builder.position_at_end(loop_body);
                self.handle_assignment(target, iter_elem_lifetime)?;
                for ifexpr in ifs.iter() {
                    self.handle_expr(ifexpr)?;
                }
                let elem_lifetime = self.handle_expr(elt)?;
                if let Some(elem_lifetime) = elem_lifetime {
                    self.builder.append_ir(
                        FieldAssign {
                            obj: list_lifetime,
                            field: "$elem".into(),
                            new: elem_lifetime,
                            is_init: true
                        },
                        elt.location,
                    );
                }
                self.builder.append_ir(Branch { targets: vec![loop_body, loop_tail] }, loc);
                self.builder.position_at_end(loop_tail);
                Some(list_lifetime)
            }
            _ => unimplemented!(),
        })
    }

    fn handle_assignment(
        &mut self,
        lhs: &Expr<Option<Type>>,
        rhs_lifetime: Option<LifetimeId>,
    ) -> Result<(), String> {
        use LifetimeIR::*;
        match &lhs.node {
            ExprKind::Attribute { value, attr, .. } => {
                let value_lifetime = self.handle_expr(value)?.unwrap();
                if let Some(field_lifetime) = rhs_lifetime {
                    self.builder.append_ir(
                        FieldAssign { obj: value_lifetime, field: *attr, new: field_lifetime, is_init: false },
                        lhs.location,
                    );
                }
            }
            ExprKind::Subscript { value, slice, .. } => {
                let value_lifetime = self.handle_expr(value)?.unwrap();
                let elem_lifetime = if let ExprKind::Slice { lower, upper, step } = &slice.node {
                    for expr in [lower, upper, step].iter().filter_map(|x| x.as_ref()) {
                        self.handle_expr(expr)?;
                    }
                    if let Some(rhs_lifetime) = rhs_lifetime {
                        // must be a list
                        Some(self.builder.append_ir(
                            FieldAccess { obj: rhs_lifetime, field: "$elem".into() },
                            lhs.location,
                        ))
                    } else {
                        None
                    }
                } else {
                    self.handle_expr(slice)?;
                    rhs_lifetime
                };
                // must be a list
                if let Some(elem_lifetime) = elem_lifetime {
                    self.builder.append_ir(
                        FieldAssign {
                            obj: value_lifetime,
                            field: "$elem".into(),
                            new: elem_lifetime,
                            is_init: false
                        },
                        lhs.location,
                    );
                }
            }
            ExprKind::Name { id, .. } => {
                if let Some(lifetime) = rhs_lifetime {
                    self.builder.append_ir(VarAssign { var: *id, lifetime }, lhs.location);
                }
            }
            ExprKind::Tuple { elts, .. } => {
                let rhs_lifetime = rhs_lifetime.unwrap();
                for (i, e) in elts.iter().enumerate() {
                    let elem_lifetime = self.builder.append_ir(
                        FieldAccess { obj: rhs_lifetime, field: format!("$elem{}", i).into() },
                        e.location,
                    );
                    self.handle_assignment(e, Some(elem_lifetime))?;
                }
            }
            _ => unreachable!(),
        }
        Ok(())
    }

    fn handle_statement(&mut self, stmt: &Stmt<Option<Type>>) -> Result<(), String> {
        use LifetimeIR::*;
        match &stmt.node {
            StmtKind::Expr { value, .. } => {
                self.handle_expr(value)?;
            }
            StmtKind::Assign { targets, value, .. } => {
                let rhs_lifetime = self.handle_expr(value)?;
                for target in targets {
                    self.handle_assignment(target, rhs_lifetime)?;
                }
            }
            StmtKind::If { test, body, orelse, .. } => {
                // test should return bool
                self.handle_expr(test)?;
                let body_bb = self.builder.append_block();
                let else_bb = self.builder.append_block();
                self.builder.append_ir(Branch { targets: vec![body_bb, else_bb] }, stmt.location);
                self.builder.position_at_end(body_bb);
                self.handle_statements(&body)?;
                let body_terminated = self.is_terminated();
                if orelse.is_empty() {
                    if !body_terminated {
                        // else_bb is the basic block after this if statement
                        self.builder.append_ir(Branch { targets: vec![else_bb] }, stmt.location);
                        self.builder.position_at_end(else_bb);
                    }
                } else {
                    let tail_bb = self.builder.append_block();
                    if !body_terminated {
                        self.builder.append_ir(Branch { targets: vec![tail_bb] }, stmt.location);
                    }
                    self.builder.position_at_end(else_bb);
                    self.handle_statements(&orelse)?;
                    if !self.is_terminated() {
                        self.builder.append_ir(Branch { targets: vec![tail_bb] }, stmt.location);
                    }
                    self.builder.position_at_end(tail_bb);
                }
            }
            StmtKind::While { test, body, orelse, .. } => {
                let old_loop_head = self.loop_head;
                let old_loop_tail = self.loop_tail;
                let loop_head = self.builder.append_block();
                let loop_body = self.builder.append_block();
                let loop_else =
                    if orelse.is_empty() { None } else { Some(self.builder.append_block()) };
                let loop_tail = self.builder.append_block();
                self.loop_head = Some(loop_head);
                self.loop_tail = Some(loop_tail);
                self.builder.append_ir(Branch { targets: vec![loop_head] }, stmt.location);
                self.builder.position_at_end(loop_head);
                self.handle_expr(test)?;
                self.builder.append_ir(
                    Branch { targets: vec![loop_body, loop_else.unwrap_or(loop_tail)] },
                    stmt.location,
                );
                self.builder.position_at_end(loop_body);
                self.handle_statements(&body)?;
                if !self.is_terminated() {
                    self.builder.append_ir(Branch { targets: vec![loop_head] }, stmt.location);
                }

                self.loop_head = old_loop_head;
                self.loop_tail = old_loop_tail;
                if let Some(loop_else) = loop_else {
                    self.builder.position_at_end(loop_else);
                    self.handle_statements(&orelse)?;
                    if !self.is_terminated() {
                        self.builder.append_ir(Branch { targets: vec![loop_tail] }, stmt.location);
                    }
                }
                self.builder.position_at_end(loop_tail);
            }
            StmtKind::For { target, iter, body, orelse, .. } => {
                let old_loop_head = self.loop_head;
                let old_loop_tail = self.loop_tail;
                let loop_head = self.builder.append_block();
                let loop_body = self.builder.append_block();
                let loop_else =
                    if orelse.is_empty() { None } else { Some(self.builder.append_block()) };
                let loop_tail = self.builder.append_block();
                self.loop_head = Some(loop_head);
                self.loop_tail = Some(loop_tail);
                let iter_lifetime = self.handle_expr(iter)?.map(|obj| {
                    self.builder
                        .append_ir(FieldAccess { obj, field: "$elem".into() }, iter.location)
                });
                self.builder.append_ir(Branch { targets: vec![loop_head] }, stmt.location);
                self.builder.position_at_end(loop_head);
                if let Some(iter_lifetime) = iter_lifetime {
                    self.handle_assignment(target, Some(iter_lifetime))?;
                }
                self.builder.append_ir(
                    Branch { targets: vec![loop_body, loop_else.unwrap_or(loop_tail)] },
                    stmt.location,
                );
                self.builder.position_at_end(loop_body);
                self.handle_statements(&body)?;
                if !self.is_terminated() {
                    self.builder.append_ir(Branch { targets: vec![loop_head] }, stmt.location);
                }

                self.loop_head = old_loop_head;
                self.loop_tail = old_loop_tail;
                if let Some(loop_else) = loop_else {
                    self.builder.position_at_end(loop_else);
                    self.handle_statements(&orelse)?;
                    if !self.is_terminated() {
                        self.builder.append_ir(Branch { targets: vec![loop_tail] }, stmt.location);
                    }
                }
                self.builder.position_at_end(loop_tail);
            }

            StmtKind::Continue { .. } => {
                if let Some(loop_head) = self.loop_head {
                    self.builder.append_ir(Branch { targets: vec![loop_head] }, stmt.location);
                } else {
                    return Err(format!("break outside loop"));
                }
            }
            StmtKind::Break { .. } => {
                if let Some(loop_tail) = self.loop_tail {
                    self.builder.append_ir(Branch { targets: vec![loop_tail] }, stmt.location);
                } else {
                    return Err(format!("break outside loop"));
                }
            }
            StmtKind::Return { value, .. } => {
                let val = if let Some(value) = value { self.handle_expr(value)? } else { None };
                self.builder.append_ir(Return { val }, stmt.location);
            }
            StmtKind::Pass { .. } => {}
            _ => unimplemented!("{:?}", stmt.node),
        }
        Ok(())
    }

    fn handle_statements(&mut self, stmts: &[Stmt<Option<Type>>]) -> Result<(), String> {
        for stmt in stmts.iter() {
            if self.builder.is_terminated(self.builder.get_current_block()) {
                break;
            }
            self.handle_statement(stmt)?;
        }
        Ok(())
    }
}
