use nac3core::{
    codegen::{expr::gen_call, stmt::gen_with, CodeGenContext, CodeGenerator},
    symbol_resolver::ValueEnum,
    toplevel::DefinitionId,
    typecheck::typedef::{FunSignature, Type},
};

use nac3parser::ast::{Expr, ExprKind, Located, Stmt, StmtKind, StrRef};

use inkwell::{context::Context, types::IntType, values::BasicValueEnum};

use crate::timeline::TimeFns;

pub struct ArtiqCodeGenerator<'a> {
    name: String,
    size_t: u32,
    name_counter: u32,
    start: Option<Expr<Option<Type>>>,
    end: Option<Expr<Option<Type>>>,
    timeline: &'a (dyn TimeFns + Sync),
}

impl<'a> ArtiqCodeGenerator<'a> {
    pub fn new(name: String, size_t: u32, timeline: &'a (dyn TimeFns + Sync)) -> ArtiqCodeGenerator<'a> {
        assert!(size_t == 32 || size_t == 64);
        ArtiqCodeGenerator {
            name,
            size_t,
            name_counter: 0,
            start: None,
            end: None,
            timeline,
        }
    }
}

impl<'b> CodeGenerator for ArtiqCodeGenerator<'b> {
    fn get_name(&self) -> &str {
        &self.name
    }

    fn get_size_type<'ctx>(&self, ctx: &'ctx Context) -> IntType<'ctx> {
        if self.size_t == 32 {
            ctx.i32_type()
        } else {
            ctx.i64_type()
        }
    }

    fn gen_call<'ctx, 'a>(
        &mut self,
        ctx: &mut CodeGenContext<'ctx, 'a>,
        obj: Option<(Type, ValueEnum<'ctx>)>,
        fun: (&FunSignature, DefinitionId),
        params: Vec<(Option<StrRef>, ValueEnum<'ctx>)>,
    ) -> Option<BasicValueEnum<'ctx>> {
        let result = gen_call(self, ctx, obj, fun, params);
        if let Some(end) = self.end.clone() {
            let old_end = self.gen_expr(ctx, &end).unwrap().to_basic_value_enum(ctx, self);
            let now = self.timeline.emit_now_mu(ctx);
            let smax = ctx.module.get_function("llvm.smax.i64").unwrap_or_else(|| {
                let i64 = ctx.ctx.i64_type();
                ctx.module.add_function(
                    "llvm.smax.i64",
                    i64.fn_type(&[i64.into(), i64.into()], false),
                    None,
                )
            });
            let max = ctx
                .builder
                .build_call(smax, &[old_end.into(), now.into()], "smax")
                .try_as_basic_value()
                .left()
                .unwrap();
            let end_store = self.gen_store_target(ctx, &end);
            ctx.builder.build_store(end_store, max);
        }
        if let Some(start) = self.start.clone() {
            let start_val = self.gen_expr(ctx, &start).unwrap().to_basic_value_enum(ctx, self);
            self.timeline.emit_at_mu(ctx, start_val);
        }
        result
    }

    fn gen_with<'ctx, 'a>(
        &mut self,
        ctx: &mut CodeGenContext<'ctx, 'a>,
        stmt: &Stmt<Option<Type>>,
    ) -> bool {
        if let StmtKind::With { items, body, .. } = &stmt.node {
            if items.len() == 1 && items[0].optional_vars.is_none() {
                let item = &items[0];
                // Behavior of parallel and sequential:
                // Each function call (indirectly, can be inside a sequential block) within a parallel
                // block will update the end variable to the maximum now_mu in the block.
                // Each function call directly inside a parallel block will reset the timeline after
                // execution. A parallel block within a sequential block (or not within any block) will
                // set the timeline to the max now_mu within the block (and the outer max now_mu will also
                // be updated).
                //
                // Implementation: We track the start and end separately.
                // - If there is a start variable, it indicates that we are directly inside a
                // parallel block and we have to reset the timeline after every function call.
                // - If there is a end variable, it indicates that we are (indirectly) inside a
                // parallel block, and we should update the max end value.
                if let ExprKind::Name { id, ctx: name_ctx } = &item.context_expr.node {
                    if id == &"parallel".into() {
                        let old_start = self.start.take();
                        let old_end = self.end.take();
                        let now = if let Some(old_start) = &old_start {
                            self.gen_expr(ctx, old_start)
                                .unwrap()
                                .to_basic_value_enum(ctx, self)
                        } else {
                            self.timeline.emit_now_mu(ctx)
                        };
                        // Emulate variable allocation, as we need to use the CodeGenContext
                        // HashMap to store our variable due to lifetime limitation
                        // Note: we should be able to store variables directly if generic
                        // associative type is used by limiting the lifetime of CodeGenerator to
                        // the LLVM Context.
                        // The name is guaranteed to be unique as users cannot use this as variable
                        // name.
                        self.start = old_start.clone().or_else(|| {
                            let start = format!("with-{}-start", self.name_counter).into();
                            let start_expr = Located {
                                // location does not matter at this point
                                location: stmt.location,
                                node: ExprKind::Name {
                                    id: start,
                                    ctx: name_ctx.clone(),
                                },
                                custom: Some(ctx.primitives.int64),
                            };
                            let start = self.gen_store_target(ctx, &start_expr);
                            ctx.builder.build_store(start, now);
                            Some(start_expr)
                        });
                        let end = format!("with-{}-end", self.name_counter).into();
                        let end_expr = Located {
                            // location does not matter at this point
                            location: stmt.location,
                            node: ExprKind::Name {
                                id: end,
                                ctx: name_ctx.clone(),
                            },
                            custom: Some(ctx.primitives.int64),
                        };
                        let end = self.gen_store_target(ctx, &end_expr);
                        ctx.builder.build_store(end, now);
                        self.end = Some(end_expr);
                        self.name_counter += 1;
                        let mut exited = false;
                        for stmt in body.iter() {
                            if self.gen_stmt(ctx, stmt) {
                                exited = true;
                                break;
                            }
                        }
                        // set duration
                        let end_expr = self.end.take().unwrap();
                        let end_val = self
                            .gen_expr(ctx, &end_expr)
                            .unwrap()
                            .to_basic_value_enum(ctx, self);

                        // inside an sequential block
                        if old_start.is_none() {
                            self.timeline.emit_at_mu(ctx, end_val);
                        }
                        // inside a parallel block, should update the outer max now_mu
                        if let Some(old_end) = &old_end {
                            let outer_end_val = self
                                .gen_expr(ctx, old_end)
                                .unwrap()
                                .to_basic_value_enum(ctx, self);
                            let smax =
                                ctx.module.get_function("llvm.smax.i64").unwrap_or_else(|| {
                                    let i64 = ctx.ctx.i64_type();
                                    ctx.module.add_function(
                                        "llvm.smax.i64",
                                        i64.fn_type(&[i64.into(), i64.into()], false),
                                        None,
                                    )
                                });
                            let max = ctx
                                .builder
                                .build_call(smax, &[end_val.into(), outer_end_val.into()], "smax")
                                .try_as_basic_value()
                                .left()
                                .unwrap();
                            let outer_end = self.gen_store_target(ctx, old_end);
                            ctx.builder.build_store(outer_end, max);
                        }
                        self.start = old_start;
                        self.end = old_end;
                        return exited;
                    } else if id == &"sequential".into() {
                        let start = self.start.take();
                        for stmt in body.iter() {
                            if self.gen_stmt(ctx, stmt) {
                                self.start = start;
                                return true;
                            }
                        }
                        self.start = start;
                        return false;
                    }
                }
            }
            // not parallel/sequential
            gen_with(self, ctx, stmt)
        } else {
            unreachable!()
        }
    }
}
