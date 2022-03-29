use nac3core::{
    codegen::{
        expr::gen_call,
        stmt::{gen_block, gen_with},
        CodeGenContext, CodeGenerator,
    },
    symbol_resolver::ValueEnum,
    toplevel::{DefinitionId, GenCall},
    typecheck::typedef::{FunSignature, FuncArg, Type, TypeEnum}
};

use nac3parser::ast::{Expr, ExprKind, Located, Stmt, StmtKind, StrRef};

use inkwell::{
    context::Context, module::Linkage, types::IntType, values::BasicValueEnum, AddressSpace,
};

use pyo3::{PyObject, PyResult, Python, types::{PyDict, PyList}};

use crate::{symbol_resolver::InnerResolver, timeline::TimeFns};

use std::{
    collections::hash_map::DefaultHasher,
    collections::HashMap,
    hash::{Hash, Hasher},
    sync::Arc,
};

pub struct ArtiqCodeGenerator<'a> {
    name: String,
    size_t: u32,
    name_counter: u32,
    start: Option<Expr<Option<Type>>>,
    end: Option<Expr<Option<Type>>>,
    timeline: &'a (dyn TimeFns + Sync),
}

impl<'a> ArtiqCodeGenerator<'a> {
    pub fn new(
        name: String,
        size_t: u32,
        timeline: &'a (dyn TimeFns + Sync),
    ) -> ArtiqCodeGenerator<'a> {
        assert!(size_t == 32 || size_t == 64);
        ArtiqCodeGenerator { name, size_t, name_counter: 0, start: None, end: None, timeline }
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
    ) -> Result<Option<BasicValueEnum<'ctx>>, String> {
        let result = gen_call(self, ctx, obj, fun, params)?;
        if let Some(end) = self.end.clone() {
            let old_end = self.gen_expr(ctx, &end)?.unwrap().to_basic_value_enum(ctx, self)?;
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
            let end_store = self.gen_store_target(ctx, &end)?;
            ctx.builder.build_store(end_store, max);
        }
        if let Some(start) = self.start.clone() {
            let start_val = self.gen_expr(ctx, &start)?.unwrap().to_basic_value_enum(ctx, self)?;
            self.timeline.emit_at_mu(ctx, start_val);
        }
        Ok(result)
    }

    fn gen_with<'ctx, 'a>(
        &mut self,
        ctx: &mut CodeGenContext<'ctx, 'a>,
        stmt: &Stmt<Option<Type>>,
    ) -> Result<(), String> {
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
                            self.gen_expr(ctx, old_start)?.unwrap().to_basic_value_enum(ctx, self)?
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
                        self.start = old_start.clone().map_or_else(
                            || {
                                let start = format!("with-{}-start", self.name_counter).into();
                                let start_expr = Located {
                                    // location does not matter at this point
                                    location: stmt.location,
                                    node: ExprKind::Name { id: start, ctx: name_ctx.clone() },
                                    custom: Some(ctx.primitives.int64),
                                };
                                let start = self.gen_store_target(ctx, &start_expr)?;
                                ctx.builder.build_store(start, now);
                                Ok(Some(start_expr)) as Result<_, String>
                            },
                            |v| Ok(Some(v)),
                        )?;
                        let end = format!("with-{}-end", self.name_counter).into();
                        let end_expr = Located {
                            // location does not matter at this point
                            location: stmt.location,
                            node: ExprKind::Name { id: end, ctx: name_ctx.clone() },
                            custom: Some(ctx.primitives.int64),
                        };
                        let end = self.gen_store_target(ctx, &end_expr)?;
                        ctx.builder.build_store(end, now);
                        self.end = Some(end_expr);
                        self.name_counter += 1;
                        gen_block(self, ctx, body.iter())?;
                        let current = ctx.builder.get_insert_block().unwrap();
                        // if the current block is terminated, move before the terminator
                        // we want to set the timeline before reaching the terminator
                        // TODO: This may be unsound if there are multiple exit paths in the
                        // block... e.g.
                        // if ...:
                        //     return
                        // Perhaps we can fix this by using actual with block?
                        let reset_position = if let Some(terminator) = current.get_terminator() {
                            ctx.builder.position_before(&terminator);
                            true
                        } else {
                            false
                        };
                        // set duration
                        let end_expr = self.end.take().unwrap();
                        let end_val =
                            self.gen_expr(ctx, &end_expr)?.unwrap().to_basic_value_enum(ctx, self)?;

                        // inside a sequential block
                        if old_start.is_none() {
                            self.timeline.emit_at_mu(ctx, end_val);
                        }
                        // inside a parallel block, should update the outer max now_mu
                        if let Some(old_end) = &old_end {
                            let outer_end_val = self
                                .gen_expr(ctx, old_end)?
                                .unwrap()
                                .to_basic_value_enum(ctx, self)?;
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
                            let outer_end = self.gen_store_target(ctx, old_end)?;
                            ctx.builder.build_store(outer_end, max);
                        }
                        self.start = old_start;
                        self.end = old_end;
                        if reset_position {
                            ctx.builder.position_at_end(current);
                        }
                        return Ok(());
                    } else if id == &"sequential".into() {
                        let start = self.start.take();
                        for stmt in body.iter() {
                            self.gen_stmt(ctx, stmt)?;
                            if ctx.is_terminated() {
                                break;
                            }
                        }
                        self.start = start;
                        return Ok(());
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

fn gen_rpc_tag<'ctx, 'a>(
    ctx: &mut CodeGenContext<'ctx, 'a>,
    ty: Type,
    buffer: &mut Vec<u8>,
) -> Result<(), String> {
    use nac3core::typecheck::typedef::TypeEnum::*;

    let int32 = ctx.primitives.int32;
    let int64 = ctx.primitives.int64;
    let float = ctx.primitives.float;
    let bool = ctx.primitives.bool;
    let str = ctx.primitives.str;
    let none = ctx.primitives.none;

    if ctx.unifier.unioned(ty, int32) {
        buffer.push(b'i');
    } else if ctx.unifier.unioned(ty, int64) {
        buffer.push(b'I');
    } else if ctx.unifier.unioned(ty, float) {
        buffer.push(b'f');
    } else if ctx.unifier.unioned(ty, bool) {
        buffer.push(b'b');
    } else if ctx.unifier.unioned(ty, str) {
        buffer.push(b's');
    } else if ctx.unifier.unioned(ty, none) {
        buffer.push(b'n');
    } else {
        let ty_enum = ctx.unifier.get_ty(ty);
        match &*ty_enum {
            TTuple { ty } => {
                buffer.push(b't');
                buffer.push(ty.len() as u8);
                for ty in ty {
                    gen_rpc_tag(ctx, *ty, buffer)?;
                }
            }
            TList { ty } => {
                buffer.push(b'l');
                gen_rpc_tag(ctx, *ty, buffer)?;
            }
            _ => return Err(format!("Unsupported type: {:?}", ctx.unifier.stringify(ty))),
        }
    }
    Ok(())
}

fn rpc_codegen_callback_fn<'ctx, 'a>(
    ctx: &mut CodeGenContext<'ctx, 'a>,
    obj: Option<(Type, ValueEnum<'ctx>)>,
    fun: (&FunSignature, DefinitionId),
    args: Vec<(Option<StrRef>, ValueEnum<'ctx>)>,
    generator: &mut dyn CodeGenerator,
) -> Result<Option<BasicValueEnum<'ctx>>, String> {
    let ptr_type = ctx.ctx.i8_type().ptr_type(inkwell::AddressSpace::Generic);
    let size_type = generator.get_size_type(ctx.ctx);
    let int8 = ctx.ctx.i8_type();
    let int32 = ctx.ctx.i32_type();
    let tag_ptr_type = ctx.ctx.struct_type(&[ptr_type.into(), size_type.into()], false);

    let service_id = int32.const_int(fun.1.0 as u64, false);
    // -- setup rpc tags
    let mut tag = Vec::new();
    if obj.is_some() {
        tag.push(b'O');
    }
    for arg in fun.0.args.iter() {
        gen_rpc_tag(ctx, arg.ty, &mut tag)?;
    }
    tag.push(b':');
    gen_rpc_tag(ctx, fun.0.ret, &mut tag)?;

    let mut hasher = DefaultHasher::new();
    tag.hash(&mut hasher);
    let hash = format!("{}", hasher.finish());

    let tag_ptr = ctx
        .module
        .get_global(hash.as_str())
        .unwrap_or_else(|| {
            let tag_arr_ptr = ctx.module.add_global(
                int8.array_type(tag.len() as u32),
                None,
                format!("tagptr{}", fun.1 .0).as_str(),
            );
            tag_arr_ptr.set_initializer(&int8.const_array(
                &tag.iter().map(|v| int8.const_int(*v as u64, false)).collect::<Vec<_>>(),
            ));
            tag_arr_ptr.set_linkage(Linkage::Private);
            let tag_ptr = ctx.module.add_global(tag_ptr_type, None, &hash);
            tag_ptr.set_linkage(Linkage::Private);
            tag_ptr.set_initializer(&ctx.ctx.const_struct(
                &[
                    tag_arr_ptr.as_pointer_value().const_cast(ptr_type).into(),
                    size_type.const_int(tag.len() as u64, false).into(),
                ],
                false,
            ));
            tag_ptr
        })
        .as_pointer_value();

    let arg_length = args.len() + if obj.is_some() { 1 } else { 0 };

    let stacksave = ctx.module.get_function("llvm.stacksave").unwrap_or_else(|| {
        ctx.module.add_function("llvm.stacksave", ptr_type.fn_type(&[], false), None)
    });
    let stackrestore = ctx.module.get_function("llvm.stackrestore").unwrap_or_else(|| {
        ctx.module.add_function(
            "llvm.stackrestore",
            ctx.ctx.void_type().fn_type(&[ptr_type.into()], false),
            None,
        )
    });

    let stackptr = ctx.builder.build_call(stacksave, &[], "rpc.stack");
    let args_ptr = ctx.builder.build_array_alloca(
        ptr_type,
        ctx.ctx.i32_type().const_int(arg_length as u64, false),
        "argptr",
    );

    // -- rpc args handling
    let mut keys = fun.0.args.clone();
    let mut mapping = HashMap::new();
    for (key, value) in args.into_iter() {
        mapping.insert(key.unwrap_or_else(|| keys.remove(0).name), value);
    }
    // default value handling
    for k in keys.into_iter() {
        mapping.insert(
            k.name,
            ctx.gen_symbol_val(generator, &k.default_value.unwrap(), k.ty).into()
        );
    }
    // reorder the parameters
    let mut real_params = fun
        .0
        .args
        .iter()
        .map(|arg| mapping.remove(&arg.name).unwrap().to_basic_value_enum(ctx, generator))
        .collect::<Result<Vec<_>, _>>()?;
    if let Some(obj) = obj {
        if let ValueEnum::Static(obj) = obj.1 {
            real_params.insert(0, obj.get_const_obj(ctx, generator));
        } else {
            // should be an error here...
            panic!("only host object is allowed");
        }
    }

    for (i, arg) in real_params.iter().enumerate() {
        let arg_slot = ctx.builder.build_alloca(arg.get_type(), &format!("rpc.arg{}", i));
        ctx.builder.build_store(arg_slot, *arg);
        let arg_slot = ctx.builder.build_bitcast(arg_slot, ptr_type, "rpc.arg");
        let arg_ptr = unsafe {
            ctx.builder.build_gep(
                args_ptr,
                &[int32.const_int(i as u64, false)],
                &format!("rpc.arg{}", i),
            )
        };
        ctx.builder.build_store(arg_ptr, arg_slot);
    }

    // call
    let rpc_send = ctx.module.get_function("rpc_send").unwrap_or_else(|| {
        ctx.module.add_function(
            "rpc_send",
            ctx.ctx.void_type().fn_type(
                &[
                    int32.into(),
                    tag_ptr_type.ptr_type(AddressSpace::Generic).into(),
                    ptr_type.ptr_type(AddressSpace::Generic).into(),
                ],
                false,
            ),
            None,
        )
    });
    ctx.builder.build_call(
        rpc_send,
        &[service_id.into(), tag_ptr.into(), args_ptr.into()],
        "rpc.send",
    );

    // reclaim stack space used by arguments
    ctx.builder.build_call(
        stackrestore,
        &[stackptr.try_as_basic_value().unwrap_left().into()],
        "rpc.stackrestore",
    );

    // -- receive value:
    // T result = {
    //   void *ret_ptr = alloca(sizeof(T));
    //   void *ptr = ret_ptr;
    //   loop: int size = rpc_recv(ptr);
    //   // Non-zero: Provide `size` bytes of extra storage for variable-length data.
    //   if(size) { ptr = alloca(size); goto loop; }
    //   else *(T*)ret_ptr
    // }
    let rpc_recv = ctx.module.get_function("rpc_recv").unwrap_or_else(|| {
        ctx.module.add_function("rpc_recv", int32.fn_type(&[ptr_type.into()], false), None)
    });

    if ctx.unifier.unioned(fun.0.ret, ctx.primitives.none) {
        ctx.build_call_or_invoke(rpc_recv, &[ptr_type.const_null().into()], "rpc_recv");
        return Ok(None);
    }

    let prehead_bb = ctx.builder.get_insert_block().unwrap();
    let current_function = prehead_bb.get_parent().unwrap();
    let head_bb = ctx.ctx.append_basic_block(current_function, "rpc.head");
    let alloc_bb = ctx.ctx.append_basic_block(current_function, "rpc.continue");
    let tail_bb = ctx.ctx.append_basic_block(current_function, "rpc.tail");

    let ret_ty = ctx.get_llvm_type(generator, fun.0.ret);
    let need_load = !ret_ty.is_pointer_type();
    let slot = ctx.builder.build_alloca(ret_ty, "rpc.ret.slot");
    let slotgen = ctx.builder.build_bitcast(slot, ptr_type, "rpc.ret.ptr");
    ctx.builder.build_unconditional_branch(head_bb);
    ctx.builder.position_at_end(head_bb);

    let phi = ctx.builder.build_phi(ptr_type, "rpc.ptr");
    phi.add_incoming(&[(&slotgen, prehead_bb)]);
    let alloc_size = ctx
        .build_call_or_invoke(rpc_recv, &[phi.as_basic_value()], "rpc.size.next")
        .unwrap()
        .into_int_value();
    let is_done = ctx.builder.build_int_compare(
        inkwell::IntPredicate::EQ,
        int32.const_zero(),
        alloc_size,
        "rpc.done",
    );

    ctx.builder.build_conditional_branch(is_done, tail_bb, alloc_bb);
    ctx.builder.position_at_end(alloc_bb);

    let alloc_ptr = ctx.builder.build_array_alloca(ptr_type, alloc_size, "rpc.alloc");
    let alloc_ptr = ctx.builder.build_bitcast(alloc_ptr, ptr_type, "rpc.alloc.ptr");
    phi.add_incoming(&[(&alloc_ptr, alloc_bb)]);
    ctx.builder.build_unconditional_branch(head_bb);

    ctx.builder.position_at_end(tail_bb);

    let result = ctx.builder.build_load(slot, "rpc.result");
    if need_load {
        ctx.builder.build_call(
            stackrestore,
            &[stackptr.try_as_basic_value().unwrap_left().into()],
            "rpc.stackrestore",
        );
    }
    Ok(Some(result))
}

pub fn attributes_writeback<'ctx, 'a>(
    ctx: &mut CodeGenContext<'ctx, 'a>,
    generator: &mut dyn CodeGenerator,
    inner_resolver: &InnerResolver,
    host_attributes: PyObject,
) -> Result<(), String> {
    Python::with_gil(|py| -> PyResult<Result<(), String>> {
        let host_attributes = host_attributes.cast_as::<PyList>(py)?;
        let top_levels = ctx.top_level.definitions.read();
        let globals = inner_resolver.global_value_ids.read();
        let int32 = ctx.ctx.i32_type();
        let zero = int32.const_zero();
        let mut values = Vec::new();
        let mut scratch_buffer = Vec::new();
        for (_, val) in globals.iter() {
            let val = val.as_ref(py);
            let ty = inner_resolver.get_obj_type(py, val, &mut ctx.unifier, &top_levels, &ctx.primitives)?;
            if let Err(ty) = ty {
                return Ok(Err(ty))
            }
            let ty = ty.unwrap();
            match &*ctx.unifier.get_ty(ty) {
                TypeEnum::TObj { fields, .. } => {
                    // we only care about primitive attributes
                    // for non-primitive attributes, they should be in another global
                    let mut attributes = Vec::new();
                    let obj = inner_resolver.get_obj_value(py, val, ctx, generator)?.unwrap();
                    for (name, (field_ty, is_mutable)) in fields.iter() {
                        if !is_mutable {
                            continue
                        }
                        if gen_rpc_tag(ctx, *field_ty, &mut scratch_buffer).is_ok() {
                            attributes.push(name.to_string());
                            let index = ctx.get_attr_index(ty, *name);
                            values.push((*field_ty, ctx.build_gep_and_load(
                                            obj.into_pointer_value(),
                                            &[zero, int32.const_int(index as u64, false)])));
                        }
                    }
                    if !attributes.is_empty() {
                        let pydict = PyDict::new(py);
                        pydict.set_item("obj", val)?;
                        pydict.set_item("fields", attributes)?;
                        host_attributes.append(pydict)?;
                    }
                },
                TypeEnum::TList { ty: elem_ty } => {
                    if gen_rpc_tag(ctx, *elem_ty, &mut scratch_buffer).is_ok() {
                        let pydict = PyDict::new(py);
                        pydict.set_item("obj", val)?;
                        host_attributes.append(pydict)?;
                        values.push((ty, inner_resolver.get_obj_value(py, val, ctx, generator)?.unwrap()));
                    }
                },
                _ => {}
            }
        }
        let fun = FunSignature {
            args: values.iter().enumerate().map(|(i, (ty, _))| FuncArg {
                name: i.to_string().into(),
                ty: *ty,
                default_value: None
            }).collect(),
            ret: ctx.primitives.none,
            vars: Default::default()
        };
        let args: Vec<_> = values.into_iter().map(|(_, val)| (None, ValueEnum::Dynamic(val))).collect();
        if let Err(e) = rpc_codegen_callback_fn(ctx, None, (&fun, DefinitionId(0)), args, generator) {
            return Ok(Err(e));
        }
        Ok(Ok(()))
    }).unwrap()?;
    Ok(())
}

pub fn rpc_codegen_callback() -> Arc<GenCall> {
    Arc::new(GenCall::new(Box::new(|ctx, obj, fun, args, generator| {
        rpc_codegen_callback_fn(ctx, obj, fun, args, generator)
    })))
}
