use crate::{
    symbol_resolver::{StaticValue, SymbolResolver},
    toplevel::{TopLevelContext, TopLevelDef},
    typecheck::{
        type_inferencer::{CodeLocation, PrimitiveStore},
        typedef::{CallId, FuncArg, Type, TypeEnum, Unifier},
    },
};
use crossbeam::channel::{unbounded, Receiver, Sender};
use inkwell::{
    AddressSpace,
    OptimizationLevel,
    attributes::{Attribute, AttributeLoc},
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::Module,
    passes::{PassManager, PassManagerBuilder},
    types::{AnyType, BasicType, BasicTypeEnum},
    values::{BasicValueEnum, FunctionValue, PhiValue, PointerValue},
    debug_info::{
        DebugInfoBuilder, DICompileUnit, DISubprogram, AsDIScope, DIFlagsConstants, DIScope
    },
};
use itertools::Itertools;
use nac3parser::ast::{Stmt, StrRef, Location};
use parking_lot::{Condvar, Mutex};
use std::collections::{HashMap, HashSet};
use std::sync::{
    atomic::{AtomicBool, Ordering},
    Arc,
};
use std::thread;

pub mod concrete_type;
pub mod expr;
mod generator;
pub mod irrt;
pub mod stmt;

#[cfg(test)]
mod test;

use concrete_type::{ConcreteType, ConcreteTypeEnum, ConcreteTypeStore};
pub use generator::{CodeGenerator, DefaultCodeGenerator};

#[derive(Default)]
pub struct StaticValueStore {
    pub lookup: HashMap<Vec<(usize, u64)>, usize>,
    pub store: Vec<HashMap<usize, Arc<dyn StaticValue + Send + Sync>>>,
}

pub type VarValue<'ctx> = (PointerValue<'ctx>, Option<Arc<dyn StaticValue + Send + Sync>>, i64);

pub struct CodeGenContext<'ctx, 'a> {
    pub ctx: &'ctx Context,
    pub builder: Builder<'ctx>,
    pub debug_info: (DebugInfoBuilder<'ctx>, DICompileUnit<'ctx>, DIScope<'ctx>),
    pub module: Module<'ctx>,
    pub top_level: &'a TopLevelContext,
    pub unifier: Unifier,
    pub resolver: Arc<dyn SymbolResolver + Send + Sync>,
    pub static_value_store: Arc<Mutex<StaticValueStore>>,
    pub var_assignment: HashMap<StrRef, VarValue<'ctx>>,
    pub type_cache: HashMap<Type, BasicTypeEnum<'ctx>>,
    pub primitives: PrimitiveStore,
    pub calls: Arc<HashMap<CodeLocation, CallId>>,
    pub registry: &'a WorkerRegistry,
    // const string cache
    pub const_strings: HashMap<String, BasicValueEnum<'ctx>>,
    // stores the alloca for variables
    pub init_bb: BasicBlock<'ctx>,
    // the first one is the test_bb, and the second one is bb after the loop
    pub loop_target: Option<(BasicBlock<'ctx>, BasicBlock<'ctx>)>,
    // unwind target bb
    pub unwind_target: Option<BasicBlock<'ctx>>,
    // return target bb, just emit ret if no such target
    pub return_target: Option<BasicBlock<'ctx>>,
    pub return_buffer: Option<PointerValue<'ctx>>,
    // outer catch clauses
    pub outer_catch_clauses:
        Option<(Vec<Option<BasicValueEnum<'ctx>>>, BasicBlock<'ctx>, PhiValue<'ctx>)>,
    pub need_sret: bool,
    pub current_loc: Location,
}

impl<'ctx, 'a> CodeGenContext<'ctx, 'a> {
    pub fn is_terminated(&self) -> bool {
        self.builder.get_insert_block().unwrap().get_terminator().is_some()
    }
}

type Fp = Box<dyn Fn(&Module) + Send + Sync>;

pub struct WithCall {
    fp: Fp,
}

impl WithCall {
    pub fn new(fp: Fp) -> WithCall {
        WithCall { fp }
    }

    pub fn run<'ctx>(&self, m: &Module<'ctx>) {
        (self.fp)(m)
    }
}

pub struct WorkerRegistry {
    sender: Arc<Sender<Option<CodeGenTask>>>,
    receiver: Arc<Receiver<Option<CodeGenTask>>>,
    panicked: AtomicBool,
    task_count: Mutex<usize>,
    thread_count: usize,
    wait_condvar: Condvar,
    top_level_ctx: Arc<TopLevelContext>,
    static_value_store: Arc<Mutex<StaticValueStore>>,
}

impl WorkerRegistry {
    pub fn create_workers<G: CodeGenerator + Send + 'static>(
        generators: Vec<Box<G>>,
        top_level_ctx: Arc<TopLevelContext>,
        f: Arc<WithCall>,
    ) -> (Arc<WorkerRegistry>, Vec<thread::JoinHandle<()>>) {
        let (sender, receiver) = unbounded();
        let task_count = Mutex::new(0);
        let wait_condvar = Condvar::new();

        // init: 0 to be empty
        let mut static_value_store: StaticValueStore = Default::default();
        static_value_store.lookup.insert(Default::default(), 0);
        static_value_store.store.push(Default::default());

        let registry = Arc::new(WorkerRegistry {
            sender: Arc::new(sender),
            receiver: Arc::new(receiver),
            thread_count: generators.len(),
            panicked: AtomicBool::new(false),
            static_value_store: Arc::new(Mutex::new(static_value_store)),
            task_count,
            wait_condvar,
            top_level_ctx,
        });

        let mut handles = Vec::new();
        for mut generator in generators.into_iter() {
            let registry = registry.clone();
            let registry2 = registry.clone();
            let f = f.clone();
            let handle = thread::spawn(move || {
                registry.worker_thread(generator.as_mut(), f);
            });
            let handle = thread::spawn(move || {
                if let Err(e) = handle.join() {
                    if let Some(e) = e.downcast_ref::<&'static str>() {
                        eprintln!("Got an error: {}", e);
                    } else {
                        eprintln!("Got an unknown error: {:?}", e);
                    }
                    registry2.panicked.store(true, Ordering::SeqCst);
                    registry2.wait_condvar.notify_all();
                }
            });
            handles.push(handle);
        }
        (registry, handles)
    }

    pub fn wait_tasks_complete(&self, handles: Vec<thread::JoinHandle<()>>) {
        {
            let mut count = self.task_count.lock();
            while *count != 0 {
                if self.panicked.load(Ordering::SeqCst) {
                    break;
                }
                self.wait_condvar.wait(&mut count);
            }
        }
        for _ in 0..self.thread_count {
            self.sender.send(None).unwrap();
        }
        {
            let mut count = self.task_count.lock();
            while *count != self.thread_count {
                if self.panicked.load(Ordering::SeqCst) {
                    break;
                }
                self.wait_condvar.wait(&mut count);
            }
        }
        for handle in handles {
            handle.join().unwrap();
        }
        if self.panicked.load(Ordering::SeqCst) {
            panic!("tasks panicked");
        }
    }

    pub fn add_task(&self, task: CodeGenTask) {
        *self.task_count.lock() += 1;
        self.sender.send(Some(task)).unwrap();
    }

    fn worker_thread<G: CodeGenerator>(&self, generator: &mut G, f: Arc<WithCall>) {
        let context = Context::create();
        let mut builder = context.create_builder();
        let module = context.create_module(generator.get_name());

        module.add_basic_value_flag(
            "Debug Info Version",
            inkwell::module::FlagBehavior::Warning,
            context.i32_type().const_int(3, false),
        );
        module.add_basic_value_flag(
            "Dwarf Version",
            inkwell::module::FlagBehavior::Warning,
            context.i32_type().const_int(4, false),
        );

        let pass_builder = PassManagerBuilder::create();
        pass_builder.set_optimization_level(OptimizationLevel::Default);
        let passes = PassManager::create(&module);
        pass_builder.populate_function_pass_manager(&passes);

        let mut errors = HashSet::new();
        while let Some(task) = self.receiver.recv().unwrap() {
            let tmp_module = context.create_module("tmp");
            match gen_func(&context, generator, self, builder, tmp_module, task) {
                Ok(result) => {
                    builder = result.0;
                    passes.run_on(&result.2);
                    module.link_in_module(result.1).unwrap();
                }
                Err((old_builder, e)) => {
                    builder = old_builder;
                    errors.insert(e);
                }
            }
            *self.task_count.lock() -= 1;
            self.wait_condvar.notify_all();
        }
        if !errors.is_empty() {
            panic!("Codegen error: {}", errors.into_iter().sorted().join("\n----------\n"));
        }

        let result = module.verify();
        if let Err(err) = result {
            println!("{}", module.print_to_string().to_str().unwrap());
            println!("{}", err);
            panic!()
        }
        f.run(&module);
        let mut lock = self.task_count.lock();
        *lock += 1;
        self.wait_condvar.notify_all();
    }
}

pub struct CodeGenTask {
    pub subst: Vec<(Type, ConcreteType)>,
    pub store: ConcreteTypeStore,
    pub symbol_name: String,
    pub signature: ConcreteType,
    pub body: Arc<Vec<Stmt<Option<Type>>>>,
    pub calls: Arc<HashMap<CodeLocation, CallId>>,
    pub unifier_index: usize,
    pub resolver: Arc<dyn SymbolResolver + Send + Sync>,
    pub id: usize,
}

fn get_llvm_type<'ctx>(
    ctx: &'ctx Context,
    generator: &mut dyn CodeGenerator,
    unifier: &mut Unifier,
    top_level: &TopLevelContext,
    type_cache: &mut HashMap<Type, BasicTypeEnum<'ctx>>,
    primitives: &PrimitiveStore,
    ty: Type,
) -> BasicTypeEnum<'ctx> {
    use TypeEnum::*;
    // we assume the type cache should already contain primitive types,
    // and they should be passed by value instead of passing as pointer.
    type_cache.get(&unifier.get_representative(ty)).cloned().unwrap_or_else(|| {
        let ty_enum = unifier.get_ty(ty);
        let result = match &*ty_enum {
            TObj { obj_id, fields, .. } => {
                // check to avoid treating primitives other than Option as classes
                if obj_id.0 <= 10 {
                    match (unifier.get_ty(ty).as_ref(), unifier.get_ty(primitives.option).as_ref())
                    {
                        (
                            TypeEnum::TObj { obj_id, params, .. },
                            TypeEnum::TObj { obj_id: opt_id, .. },
                        ) if *obj_id == *opt_id => {
                            return get_llvm_type(
                                ctx,
                                generator,
                                unifier,
                                top_level,
                                type_cache,
                                primitives,
                                *params.iter().next().unwrap().1,
                            )
                            .ptr_type(AddressSpace::Generic)
                            .into();
                        }
                        _ => unreachable!("must be option type"),
                    }
                }
                // a struct with fields in the order of declaration
                let top_level_defs = top_level.definitions.read();
                let definition = top_level_defs.get(obj_id.0).unwrap();
                let ty = if let TopLevelDef::Class { name, fields: fields_list, .. } =
                    &*definition.read()
                {
                    let struct_type = ctx.opaque_struct_type(&name.to_string());
                    type_cache.insert(unifier.get_representative(ty), struct_type.ptr_type(AddressSpace::Generic).into());
                    let fields = fields_list
                        .iter()
                        .map(|f| {
                            get_llvm_type(
                                ctx,
                                generator,
                                unifier,
                                top_level,
                                type_cache,
                                primitives,
                                fields[&f.0].0,
                            )
                        })
                        .collect_vec();
                    struct_type.set_body(&fields, false);
                    struct_type.ptr_type(AddressSpace::Generic).into()
                } else {
                    unreachable!()
                };
                return ty;
            }
            TTuple { ty } => {
                // a struct with fields in the order present in the tuple
                let fields = ty
                    .iter()
                    .map(|ty| get_llvm_type(ctx, generator, unifier, top_level, type_cache, primitives, *ty))
                    .collect_vec();
                ctx.struct_type(&fields, false).into()
            }
            TList { ty } => {
                // a struct with an integer and a pointer to an array
                let element_type =
                    get_llvm_type(ctx, generator, unifier, top_level, type_cache, primitives, *ty);
                let fields = [
                    element_type.ptr_type(AddressSpace::Generic).into(),
                    generator.get_size_type(ctx).into(),
                ];
                ctx.struct_type(&fields, false).ptr_type(AddressSpace::Generic).into()
            }
            TVirtual { .. } => unimplemented!(),
            _ => unreachable!("{}", ty_enum.get_type_name()),
        };
        type_cache.insert(unifier.get_representative(ty), result);
        result
    })
}

fn need_sret<'ctx>(ctx: &'ctx Context, ty: BasicTypeEnum<'ctx>) -> bool {
    fn need_sret_impl<'ctx>(ctx: &'ctx Context, ty: BasicTypeEnum<'ctx>, maybe_large: bool) -> bool {
        match ty {
            BasicTypeEnum::IntType(_) | BasicTypeEnum::PointerType(_) => false,
            BasicTypeEnum::FloatType(_) if maybe_large => false,
            BasicTypeEnum::StructType(ty) if maybe_large && ty.count_fields() <= 2 =>
                ty.get_field_types().iter().any(|ty| need_sret_impl(ctx, *ty, false)),
            _ => true,
        }
    }
    need_sret_impl(ctx, ty, true)
}

pub fn gen_func_impl<'ctx, G: CodeGenerator, F: FnOnce(&mut G, &mut CodeGenContext) -> Result<(), String>> (
    context: &'ctx Context,
    generator: &mut G,
    registry: &WorkerRegistry,
    builder: Builder<'ctx>,
    module: Module<'ctx>,
    task: CodeGenTask,
    codegen_function: F
) -> Result<(Builder<'ctx>, Module<'ctx>, FunctionValue<'ctx>), (Builder<'ctx>, String)> {
    let top_level_ctx = registry.top_level_ctx.clone();
    let static_value_store = registry.static_value_store.clone();
    let (mut unifier, primitives) = {
        let (unifier, primitives) = &top_level_ctx.unifiers.read()[task.unifier_index];
        (Unifier::from_shared_unifier(unifier), *primitives)
    };
    unifier.top_level = Some(top_level_ctx.clone());

    let mut cache = HashMap::new();
    for (a, b) in task.subst.iter() {
        // this should be unification between variables and concrete types
        // and should not cause any problem...
        let b = task.store.to_unifier_type(&mut unifier, &primitives, *b, &mut cache);
        unifier
            .unify(*a, b)
            .or_else(|err| {
                if matches!(&*unifier.get_ty(*a), TypeEnum::TRigidVar { .. }) {
                    unifier.replace_rigid_var(*a, b);
                    Ok(())
                } else {
                    Err(err)
                }
            })
            .unwrap()
    }

    // rebuild primitive store with unique representatives
    let primitives = PrimitiveStore {
        int32: unifier.get_representative(primitives.int32),
        int64: unifier.get_representative(primitives.int64),
        uint32: unifier.get_representative(primitives.uint32),
        uint64: unifier.get_representative(primitives.uint64),
        float: unifier.get_representative(primitives.float),
        bool: unifier.get_representative(primitives.bool),
        none: unifier.get_representative(primitives.none),
        range: unifier.get_representative(primitives.range),
        str: unifier.get_representative(primitives.str),
        exception: unifier.get_representative(primitives.exception),
        option: unifier.get_representative(primitives.option),
    };

    let mut type_cache: HashMap<_, _> = [
        (primitives.int32, context.i32_type().into()),
        (primitives.int64, context.i64_type().into()),
        (primitives.uint32, context.i32_type().into()),
        (primitives.uint64, context.i64_type().into()),
        (primitives.float, context.f64_type().into()),
        (primitives.bool, context.bool_type().into()),
        (primitives.str, {
            let str_type = context.opaque_struct_type("str");
            let fields = [
                context.i8_type().ptr_type(AddressSpace::Generic).into(),
                generator.get_size_type(context).into(),
            ];
            str_type.set_body(&fields, false);
            str_type.into()
        }),
        (primitives.range, context.i32_type().array_type(3).ptr_type(AddressSpace::Generic).into()),
    ]
    .iter()
    .cloned()
    .collect();
    type_cache.insert(primitives.exception, {
        let exception = context.opaque_struct_type("Exception");
        let int32 = context.i32_type().into();
        let int64 = context.i64_type().into();
        let str_ty = *type_cache.get(&primitives.str).unwrap();
        let fields = [int32, str_ty, int32, int32, str_ty, str_ty, int64, int64, int64];
        exception.set_body(&fields, false);
        exception.ptr_type(AddressSpace::Generic).into()
    });
    // NOTE: special handling of option cannot use this type cache since it contains type var,
    // handled inside get_llvm_type instead

    let (args, ret) = if let ConcreteTypeEnum::TFunc { args, ret, .. } =
        task.store.get(task.signature)
    {
        (
            args.iter()
                .map(|arg| FuncArg {
                    name: arg.name,
                    ty: task.store.to_unifier_type(&mut unifier, &primitives, arg.ty, &mut cache),
                    default_value: arg.default_value.clone(),
                })
                .collect_vec(),
            task.store.to_unifier_type(&mut unifier, &primitives, *ret, &mut cache),
        )
    } else {
        unreachable!()
    };
    let ret_type = if unifier.unioned(ret, primitives.none) {
        None
    } else {
        Some(get_llvm_type(context, generator, &mut unifier, top_level_ctx.as_ref(), &mut type_cache, &primitives, ret))
    };

    let has_sret = ret_type.map_or(false, |ty| need_sret(context, ty));
    let mut params = args
        .iter()
        .map(|arg| {
            get_llvm_type(
                context,
                generator,
                &mut unifier,
                top_level_ctx.as_ref(),
                &mut type_cache,
                &primitives,
                arg.ty,
            )
            .into()
        })
        .collect_vec();

    if has_sret {
        params.insert(0, ret_type.unwrap().ptr_type(AddressSpace::Generic).into());
    }

    let fn_type = match ret_type {
        Some(ret_type) if !has_sret => ret_type.fn_type(&params, false),
        _ => context.void_type().fn_type(&params, false)
    };

    let symbol = &task.symbol_name;
    let fn_val =
        module.get_function(symbol).unwrap_or_else(|| module.add_function(symbol, fn_type, None));

    if let Some(personality) = &top_level_ctx.personality_symbol {
        let personality = module.get_function(personality).unwrap_or_else(|| {
            let ty = context.i32_type().fn_type(&[], true);
            module.add_function(personality, ty, None)
        });
        fn_val.set_personality_function(personality);
    }
    if has_sret {
        fn_val.add_attribute(AttributeLoc::Param(0),
            context.create_type_attribute(Attribute::get_named_enum_kind_id("sret"),
            ret_type.unwrap().as_any_type_enum()));
    }

    let init_bb = context.append_basic_block(fn_val, "init");
    builder.position_at_end(init_bb);
    let body_bb = context.append_basic_block(fn_val, "body");

    let mut var_assignment = HashMap::new();
    let offset = if has_sret { 1 } else { 0 };
    for (n, arg) in args.iter().enumerate() {
        let param = fn_val.get_nth_param((n as u32) + offset).unwrap();
        let alloca = builder.build_alloca(
            get_llvm_type(
                context,
                generator,
                &mut unifier,
                top_level_ctx.as_ref(),
                &mut type_cache,
                &primitives,
                arg.ty,
            ),
            &arg.name.to_string(),
        );
        builder.build_store(alloca, param);
        var_assignment.insert(arg.name, (alloca, None, 0));
    }

    let return_buffer = if has_sret {
        Some(fn_val.get_nth_param(0).unwrap().into_pointer_value())
    } else {
        fn_type.get_return_type().map(|v| builder.build_alloca(v, "$ret"))
    };

    let static_values = {
        let store = registry.static_value_store.lock();
        store.store[task.id].clone()
    };
    for (k, v) in static_values.into_iter() {
        let (_, static_val, _) = var_assignment.get_mut(&args[k].name).unwrap();
        *static_val = Some(v);
    }

    builder.build_unconditional_branch(body_bb);
    builder.position_at_end(body_bb);

    let (dibuilder, compile_unit) = module.create_debug_info_builder(
        /* allow_unresolved */ true,
        /* language */ inkwell::debug_info::DWARFSourceLanguage::Python,
        /* filename */
        &task
            .body
            .get(0)
            .map_or_else(
                || "<nac3_internal>".to_string(),
                |f| f.location.file.0.to_string(),
            ),
        /* directory */ "",
        /* producer */ "NAC3",
        /* is_optimized */ true,
        /* compiler command line flags */ "",
        /* runtime_ver */ 0,
        /* split_name */ "",
        /* kind */ inkwell::debug_info::DWARFEmissionKind::Full,
        /* dwo_id */ 0,
        /* split_debug_inling */ true,
        /* debug_info_for_profiling */ false,
        /* sysroot */ "",
        /* sdk */ "",
    );
    let subroutine_type = dibuilder.create_subroutine_type(
        compile_unit.get_file(),
        Some(
            dibuilder
                .create_basic_type("_", 0_u64, 0x00, inkwell::debug_info::DIFlags::PUBLIC)
                .unwrap()
                .as_type(),
        ),
        &[],
        inkwell::debug_info::DIFlags::PUBLIC,
    );
    let (row, col) =
        task.body.get(0).map_or_else(|| (0, 0), |b| (b.location.row, b.location.column));
    let func_scope: DISubprogram<'_> = dibuilder.create_function(
        /* scope */ compile_unit.as_debug_info_scope(),
        /* func name */ symbol,
        /* linkage_name */ None,
        /* file */ compile_unit.get_file(),
        /* line_no */ row as u32,
        /* DIType */ subroutine_type,
        /* is_local_to_unit */ false,
        /* is_definition */ true,
        /* scope_line */ row as u32,
        /* flags */ inkwell::debug_info::DIFlags::PUBLIC,
        /* is_optimized */ true,
    );
    fn_val.set_subprogram(func_scope);

    let mut code_gen_context = CodeGenContext {
        ctx: context,
        resolver: task.resolver,
        top_level: top_level_ctx.as_ref(),
        calls: task.calls,
        loop_target: None,
        return_target: None,
        return_buffer,
        unwind_target: None,
        outer_catch_clauses: None,
        const_strings: Default::default(),
        registry,
        var_assignment,
        type_cache,
        primitives,
        init_bb,
        builder,
        module,
        unifier,
        static_value_store,
        need_sret: has_sret,
        current_loc: Default::default(),
        debug_info: (dibuilder, compile_unit, func_scope.as_debug_info_scope()),
    };

    let loc = code_gen_context.debug_info.0.create_debug_location(
        context,
        row as u32,
        col as u32,
        func_scope.as_debug_info_scope(),
        None
    );
    code_gen_context.builder.set_current_debug_location(context, loc);
    
    let result = codegen_function(generator, &mut code_gen_context);

    // after static analysis, only void functions can have no return at the end.
    if !code_gen_context.is_terminated() {
        code_gen_context.builder.build_return(None);
    }

    code_gen_context.builder.unset_current_debug_location();
    code_gen_context.debug_info.0.finalize();

    let CodeGenContext { builder, module, .. } = code_gen_context;
    if let Err(e) = result {
        return Err((builder, e));
    }

    Ok((builder, module, fn_val))
}

pub fn gen_func<'ctx, G: CodeGenerator>(
    context: &'ctx Context,
    generator: &mut G,
    registry: &WorkerRegistry,
    builder: Builder<'ctx>,
    module: Module<'ctx>,
    task: CodeGenTask,
) -> Result<(Builder<'ctx>, Module<'ctx>, FunctionValue<'ctx>), (Builder<'ctx>, String)> {
    let body = task.body.clone();
    gen_func_impl(context, generator, registry, builder, module, task, |generator, ctx| {
        for stmt in body.iter() {
            generator.gen_stmt(ctx, stmt)?;
        }
        Ok(())
    })
}
