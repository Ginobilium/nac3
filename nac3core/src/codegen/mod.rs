use crate::{
    symbol_resolver::SymbolResolver,
    toplevel::{TopLevelContext, TopLevelDef},
    typecheck::{
        type_inferencer::{CodeLocation, PrimitiveStore},
        typedef::{CallId, FuncArg, Type, TypeEnum, Unifier},
    },
};
use crossbeam::channel::{unbounded, Receiver, Sender};
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::Module,
    passes::{PassManager, PassManagerBuilder},
    types::{BasicType, BasicTypeEnum},
    values::{FunctionValue, PointerValue},
    AddressSpace, OptimizationLevel,
};
use itertools::Itertools;
use parking_lot::{Condvar, Mutex};
use nac3parser::ast::{Stmt, StrRef};
use std::collections::HashMap;
use std::sync::{
    atomic::{AtomicBool, Ordering},
    Arc,
};
use std::thread;

pub mod concrete_type;
pub mod expr;
pub mod stmt;
mod generator;

#[cfg(test)]
mod test;

use concrete_type::{ConcreteType, ConcreteTypeEnum, ConcreteTypeStore};
pub use generator::{CodeGenerator, DefaultCodeGenerator};

pub struct CodeGenContext<'ctx, 'a> {
    pub ctx: &'ctx Context,
    pub builder: Builder<'ctx>,
    pub module: Module<'ctx>,
    pub top_level: &'a TopLevelContext,
    pub unifier: Unifier,
    pub resolver: Arc<dyn SymbolResolver + Send + Sync>,
    pub var_assignment: HashMap<StrRef, PointerValue<'ctx>>,
    pub type_cache: HashMap<Type, BasicTypeEnum<'ctx>>,
    pub primitives: PrimitiveStore,
    pub calls: Arc<HashMap<CodeLocation, CallId>>,
    pub registry: &'a WorkerRegistry,
    // stores the alloca for variables
    pub init_bb: BasicBlock<'ctx>,
    // where continue and break should go to respectively
    // the first one is the test_bb, and the second one is bb after the loop
    pub loop_bb: Option<(BasicBlock<'ctx>, BasicBlock<'ctx>)>,
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

        let registry = Arc::new(WorkerRegistry {
            sender: Arc::new(sender),
            receiver: Arc::new(receiver),
            thread_count: generators.len(),
            panicked: AtomicBool::new(false),
            task_count,
            wait_condvar,
        });

        let mut handles = Vec::new();
        for mut generator in generators.into_iter() {
            let top_level_ctx = top_level_ctx.clone();
            let registry = registry.clone();
            let registry2 = registry.clone();
            let f = f.clone();
            let handle = thread::spawn(move || {
                registry.worker_thread(generator.as_mut(), top_level_ctx, f);
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

    fn worker_thread<G: CodeGenerator>(
        &self,
        generator: &mut G,
        top_level_ctx: Arc<TopLevelContext>,
        f: Arc<WithCall>,
    ) {
        let context = Context::create();
        let mut builder = context.create_builder();
        let mut module = context.create_module(generator.get_name());

        let pass_builder = PassManagerBuilder::create();
        pass_builder.set_optimization_level(OptimizationLevel::Default);
        let passes = PassManager::create(&module);
        pass_builder.populate_function_pass_manager(&passes);

        while let Some(task) = self.receiver.recv().unwrap() {
            let result =
                gen_func(&context, generator, self, builder, module, task, top_level_ctx.clone());
            builder = result.0;
            module = result.1;
            passes.run_on(&result.2);
            *self.task_count.lock() -= 1;
            self.wait_condvar.notify_all();
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
}

fn get_llvm_type<'ctx>(
    ctx: &'ctx Context,
    unifier: &mut Unifier,
    top_level: &TopLevelContext,
    type_cache: &mut HashMap<Type, BasicTypeEnum<'ctx>>,
    ty: Type,
) -> BasicTypeEnum<'ctx> {
    use TypeEnum::*;
    // we assume the type cache should already contain primitive types,
    // and they should be passed by value instead of passing as pointer.
    type_cache.get(&unifier.get_representative(ty)).cloned().unwrap_or_else(|| {
        let ty = unifier.get_ty(ty);
        match &*ty {
            TObj { obj_id, fields, .. } => {
                // a struct with fields in the order of declaration
                let top_level_defs = top_level.definitions.read();
                let definition = top_level_defs.get(obj_id.0).unwrap();
                let ty = if let TopLevelDef::Class { fields: fields_list, .. } = &*definition.read()
                {
                    let fields = fields.borrow();
                    let fields = fields_list
                        .iter()
                        .map(|f| get_llvm_type(ctx, unifier, top_level, type_cache, fields[&f.0].0))
                        .collect_vec();
                    ctx.struct_type(&fields, false).ptr_type(AddressSpace::Generic).into()
                } else {
                    unreachable!()
                };
                ty
            }
            TTuple { ty } => {
                // a struct with fields in the order present in the tuple
                let fields = ty
                    .iter()
                    .map(|ty| get_llvm_type(ctx, unifier, top_level, type_cache, *ty))
                    .collect_vec();
                ctx.struct_type(&fields, false).ptr_type(AddressSpace::Generic).into()
            }
            TList { ty } => {
                // a struct with an integer and a pointer to an array
                let element_type = get_llvm_type(ctx, unifier, top_level, type_cache, *ty);
                let fields =
                    [ctx.i32_type().into(), element_type.ptr_type(AddressSpace::Generic).into()];
                ctx.struct_type(&fields, false).ptr_type(AddressSpace::Generic).into()
            }
            TVirtual { .. } => unimplemented!(),
            _ => unreachable!("{}", ty.get_type_name()),
        }
    })
}

pub fn gen_func<'ctx, G: CodeGenerator + ?Sized>(
    context: &'ctx Context,
    generator: &mut G,
    registry: &WorkerRegistry,
    builder: Builder<'ctx>,
    module: Module<'ctx>,
    task: CodeGenTask,
    top_level_ctx: Arc<TopLevelContext>,
) -> (Builder<'ctx>, Module<'ctx>, FunctionValue<'ctx>) {
    let (mut unifier, primitives) = {
        let (unifier, primitives) = &top_level_ctx.unifiers.read()[task.unifier_index];
        (Unifier::from_shared_unifier(unifier), *primitives)
    };

    let mut cache = HashMap::new();
    for (a, b) in task.subst.iter() {
        // this should be unification between variables and concrete types
        // and should not cause any problem...
        let b = task.store.to_unifier_type(&mut unifier, &primitives, *b, &mut cache);
        unifier.unify(*a, b).unwrap();
    }

    // rebuild primitive store with unique representatives
    let primitives = PrimitiveStore {
        int32: unifier.get_representative(primitives.int32),
        int64: unifier.get_representative(primitives.int64),
        float: unifier.get_representative(primitives.float),
        bool: unifier.get_representative(primitives.bool),
        none: unifier.get_representative(primitives.none),
        range: unifier.get_representative(primitives.range),
        str: unifier.get_representative(primitives.str),
    };

    let mut type_cache: HashMap<_, _> = [
        (unifier.get_representative(primitives.int32), context.i32_type().into()),
        (unifier.get_representative(primitives.int64), context.i64_type().into()),
        (unifier.get_representative(primitives.float), context.f64_type().into()),
        (unifier.get_representative(primitives.bool), context.bool_type().into()),
        (unifier.get_representative(primitives.str), context.i8_type().ptr_type(AddressSpace::Generic).into()),
    ]
    .iter()
    .cloned()
    .collect();

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
    let params = args
        .iter()
        .map(|arg| {
            get_llvm_type(context, &mut unifier, top_level_ctx.as_ref(), &mut type_cache, arg.ty)
        })
        .collect_vec();

    let fn_type = if unifier.unioned(ret, primitives.none) {
        context.void_type().fn_type(&params, false)
    } else {
        get_llvm_type(context, &mut unifier, top_level_ctx.as_ref(), &mut type_cache, ret)
            .fn_type(&params, false)
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

    let init_bb = context.append_basic_block(fn_val, "init");
    builder.position_at_end(init_bb);
    let body_bb = context.append_basic_block(fn_val, "body");

    let mut var_assignment = HashMap::new();
    for (n, arg) in args.iter().enumerate() {
        let param = fn_val.get_nth_param(n as u32).unwrap();
        let alloca = builder.build_alloca(
            get_llvm_type(context, &mut unifier, top_level_ctx.as_ref(), &mut type_cache, arg.ty),
            &arg.name.to_string(),
        );
        builder.build_store(alloca, param);
        var_assignment.insert(arg.name, alloca);
    }
    builder.build_unconditional_branch(body_bb);
    builder.position_at_end(body_bb);

    let mut code_gen_context = CodeGenContext {
        ctx: context,
        resolver: task.resolver,
        top_level: top_level_ctx.as_ref(),
        calls: task.calls,
        loop_bb: None,
        registry,
        var_assignment,
        type_cache,
        primitives,
        init_bb,
        builder,
        module,
        unifier,
    };

    let mut returned = false;
    for stmt in task.body.iter() {
        returned = generator.gen_stmt(&mut code_gen_context, stmt);
        if returned {
            break;
        }
    }
    // after static analysis, only void functions can have no return at the end.
    if !returned {
        code_gen_context.builder.build_return(None);
    }

    let CodeGenContext { builder, module, .. } = code_gen_context;

    (builder, module, fn_val)
}
