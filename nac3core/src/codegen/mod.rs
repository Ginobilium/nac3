use crate::{
    symbol_resolver::SymbolResolver,
    toplevel::{TopLevelContext, TopLevelDef},
    typecheck::{
        type_inferencer::{CodeLocation, PrimitiveStore},
        typedef::{CallId, FunSignature, SharedUnifier, Type, TypeEnum, Unifier},
    },
};
use crossbeam::channel::{unbounded, Receiver, Sender};
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::Module,
    types::{BasicType, BasicTypeEnum},
    values::PointerValue,
    AddressSpace,
};
use itertools::Itertools;
use parking_lot::{Condvar, Mutex};
use rustpython_parser::ast::Stmt;
use std::collections::HashMap;
use std::sync::{
    atomic::{AtomicBool, Ordering},
    Arc,
};
use std::thread;

mod expr;
mod stmt;

#[cfg(test)]
mod test;

pub struct CodeGenContext<'ctx, 'a> {
    pub ctx: &'ctx Context,
    pub builder: Builder<'ctx>,
    pub module: Module<'ctx>,
    pub top_level: &'a TopLevelContext,
    pub unifier: Unifier,
    pub resolver: Arc<Mutex<Box<dyn SymbolResolver + Send + Sync>>>,
    pub var_assignment: HashMap<String, PointerValue<'ctx>>,
    pub type_cache: HashMap<Type, BasicTypeEnum<'ctx>>,
    pub primitives: PrimitiveStore,
    pub calls: HashMap<CodeLocation, CallId>,
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
    pub fn create_workers(
        names: &[&str],
        top_level_ctx: Arc<TopLevelContext>,
        f: Arc<WithCall>,
    ) -> (Arc<WorkerRegistry>, Vec<thread::JoinHandle<()>>) {
        let (sender, receiver) = unbounded();
        let task_count = Mutex::new(0);
        let wait_condvar = Condvar::new();

        let registry = Arc::new(WorkerRegistry {
            sender: Arc::new(sender),
            receiver: Arc::new(receiver),
            thread_count: names.len(),
            panicked: AtomicBool::new(false),
            task_count,
            wait_condvar,
        });

        let mut handles = Vec::new();
        for name in names.iter() {
            let top_level_ctx = top_level_ctx.clone();
            let registry = registry.clone();
            let registry2 = registry.clone();
            let name = name.to_string();
            let f = f.clone();
            let handle = thread::spawn(move || {
                registry.worker_thread(name, top_level_ctx, f);
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

    fn worker_thread(
        &self,
        module_name: String,
        top_level_ctx: Arc<TopLevelContext>,
        f: Arc<WithCall>,
    ) {
        let context = Context::create();
        let mut builder = context.create_builder();
        let mut module = context.create_module(&module_name);

        while let Some(task) = self.receiver.recv().unwrap() {
            let result = gen_func(&context, self, builder, module, task, top_level_ctx.clone());
            builder = result.0;
            module = result.1;
            *self.task_count.lock() -= 1;
            self.wait_condvar.notify_all();
        }

        // do whatever...
        let mut lock = self.task_count.lock();
        module.verify().unwrap();
        f.run(&module);
        *lock += 1;
        self.wait_condvar.notify_all();
    }
}

pub struct CodeGenTask {
    pub subst: Vec<(Type, Type)>,
    pub symbol_name: String,
    pub signature: FunSignature,
    pub body: Vec<Stmt<Option<Type>>>,
    pub calls: HashMap<CodeLocation, CallId>,
    pub unifier: (SharedUnifier, PrimitiveStore),
    pub resolver: Arc<Mutex<Box<dyn SymbolResolver + Send + Sync>>>,
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
        match &*unifier.get_ty(ty) {
            TObj { obj_id, fields, .. } => {
                // a struct with fields in the order of declaration
                let top_level_defs = top_level.definitions.read();
                let definition = top_level_defs.get(obj_id.0).unwrap();
                let ty = if let TopLevelDef::Class { fields: fields_list, .. } = &*definition.read()
                {
                    let fields = fields.borrow();
                    let fields = fields_list
                        .iter()
                        .map(|f| get_llvm_type(ctx, unifier, top_level, type_cache, fields[&f.0]))
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
            _ => unreachable!(),
        }
    })
}

pub fn gen_func<'ctx>(
    context: &'ctx Context,
    registry: &WorkerRegistry,
    builder: Builder<'ctx>,
    module: Module<'ctx>,
    task: CodeGenTask,
    top_level_ctx: Arc<TopLevelContext>,
) -> (Builder<'ctx>, Module<'ctx>) {
    // unwrap_or(0) is for unit tests without using rayon
    let (mut unifier, primitives) = {
        let (unifier, primitives) = task.unifier;
        (Unifier::from_shared_unifier(&unifier), primitives)
    };

    for (a, b) in task.subst.iter() {
        // this should be unification between variables and concrete types
        // and should not cause any problem...
        unifier.unify(*a, *b).unwrap();
    }

    // rebuild primitive store with unique representatives
    let primitives = PrimitiveStore {
        int32: unifier.get_representative(primitives.int32),
        int64: unifier.get_representative(primitives.int64),
        float: unifier.get_representative(primitives.float),
        bool: unifier.get_representative(primitives.bool),
        none: unifier.get_representative(primitives.none),
    };

    let mut type_cache: HashMap<_, _> = [
        (unifier.get_representative(primitives.int32), context.i32_type().into()),
        (unifier.get_representative(primitives.int64), context.i64_type().into()),
        (unifier.get_representative(primitives.float), context.f64_type().into()),
        (unifier.get_representative(primitives.bool), context.bool_type().into()),
    ]
    .iter()
    .cloned()
    .collect();

    let params = task
        .signature
        .args
        .iter()
        .map(|arg| {
            get_llvm_type(&context, &mut unifier, top_level_ctx.as_ref(), &mut type_cache, arg.ty)
        })
        .collect_vec();

    let fn_type = if unifier.unioned(task.signature.ret, primitives.none) {
        context.void_type().fn_type(&params, false)
    } else {
        get_llvm_type(
            &context,
            &mut unifier,
            top_level_ctx.as_ref(),
            &mut type_cache,
            task.signature.ret,
        )
        .fn_type(&params, false)
    };

    let symbol = &task.symbol_name;
    let fn_val = module
        .get_function(symbol)
        .unwrap_or_else(|| module.add_function(symbol, fn_type, None));

    let init_bb = context.append_basic_block(fn_val, "init");
    builder.position_at_end(init_bb);
    let body_bb = context.append_basic_block(fn_val, "body");

    let mut var_assignment = HashMap::new();
    for (n, arg) in task.signature.args.iter().enumerate() {
        let param = fn_val.get_nth_param(n as u32).unwrap();
        let alloca = builder.build_alloca(
            get_llvm_type(&context, &mut unifier, top_level_ctx.as_ref(), &mut type_cache, arg.ty),
            &arg.name,
        );
        builder.build_store(alloca, param);
        var_assignment.insert(arg.name.clone(), alloca);
    }
    builder.build_unconditional_branch(body_bb);
    builder.position_at_end(body_bb);

    let mut code_gen_context = CodeGenContext {
        ctx: &context,
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

    for stmt in task.body.iter() {
        code_gen_context.gen_stmt(stmt);
    }

    let CodeGenContext { builder, module, .. } = code_gen_context;

    (builder, module)
}
