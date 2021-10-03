use std::collections::HashMap;
use std::fs;
use std::path::Path;
use std::process::Command;
use std::sync::Arc;

use inkwell::{
    AddressSpace, AtomicOrdering,
    values::BasicValueEnum,
    passes::{PassManager, PassManagerBuilder},
    targets::*,
    OptimizationLevel,
};
use pyo3::prelude::*;
use pyo3::{exceptions, types::PyList};
use rustpython_parser::{
    ast::{self, StrRef},
    parser,
};

use parking_lot::RwLock;

use nac3core::{
    codegen::{CodeGenTask, WithCall, WorkerRegistry},
    symbol_resolver::SymbolResolver,
    toplevel::{composer::TopLevelComposer, TopLevelContext, TopLevelDef, GenCall},
    typecheck::typedef::{FunSignature, FuncArg},
};
use nac3core::{
    toplevel::DefinitionId,
    typecheck::{type_inferencer::PrimitiveStore, typedef::Type},
};

use crate::symbol_resolver::Resolver;

mod symbol_resolver;

#[derive(PartialEq, Clone, Copy)]
enum Isa {
    RiscV,
    CortexA9,
}

// TopLevelComposer is unsendable as it holds the unification table, which is
// unsendable due to Rc. Arc would cause a performance hit.
#[pyclass(unsendable, name = "NAC3")]
struct Nac3 {
    isa: Isa,
    primitive: PrimitiveStore,
    builtins_ty: HashMap<StrRef, Type>,
    builtins_def: HashMap<StrRef, DefinitionId>,
    pyid_to_def: Arc<RwLock<HashMap<u64, DefinitionId>>>,
    pyid_to_type: Arc<RwLock<HashMap<u64, Type>>>,
    composer: TopLevelComposer,
    top_level: Option<Arc<TopLevelContext>>,
    to_be_registered: Vec<PyObject>,
}

impl Nac3 {
    fn register_module_impl(&mut self, obj: PyObject) -> PyResult<()> {
        let mut name_to_pyid: HashMap<StrRef, u64> = HashMap::new();
        let (module_name, source_file) = Python::with_gil(|py| -> PyResult<(String, String)> {
            let obj: &PyAny = obj.extract(py)?;
            let builtins = PyModule::import(py, "builtins")?;
            let id_fn = builtins.getattr("id")?;
            let members: &PyList = PyModule::import(py, "inspect")?
                .getattr("getmembers")?
                .call1((obj,))?
                .cast_as()?;
            for member in members.iter() {
                let key: &str = member.get_item(0)?.extract()?;
                let val = id_fn.call1((member.get_item(1)?,))?.extract()?;
                name_to_pyid.insert(key.into(), val);
            }
            Ok((
                obj.getattr("__name__")?.extract()?,
                obj.getattr("__file__")?.extract()?,
            ))
        })?;

        let source = fs::read_to_string(source_file).map_err(|e| {
            exceptions::PyIOError::new_err(format!("failed to read input file: {}", e))
        })?;
        let parser_result = parser::parse_program(&source).map_err(|e| {
            exceptions::PySyntaxError::new_err(format!("parse error: {}", e))
        })?;
        let resolver = Arc::new(Box::new(Resolver {
            id_to_type: self.builtins_ty.clone().into(),
            id_to_def: self.builtins_def.clone().into(),
            pyid_to_def: self.pyid_to_def.clone(),
            pyid_to_type: self.pyid_to_type.clone(),
            class_names: Default::default(),
            name_to_pyid: name_to_pyid.clone(),
        }) as Box<dyn SymbolResolver + Send + Sync>);
        let mut name_to_def = HashMap::new();
        let mut name_to_type = HashMap::new();

        for mut stmt in parser_result.into_iter() {
            let include = match stmt.node {
                ast::StmtKind::ClassDef {
                    ref decorator_list,
                    ref mut body,
                    ..
                } => {
                    let kernels = decorator_list.iter().any(|decorator| {
                        if let ast::ExprKind::Name { id, .. } = decorator.node {
                            id.to_string() == "kernel" || id.to_string() == "portable"
                        } else {
                            false
                        }
                    });
                    body.retain(|stmt| {
                        if let ast::StmtKind::FunctionDef {
                            ref decorator_list, ..
                        } = stmt.node
                        {
                            decorator_list.iter().any(|decorator| {
                                if let ast::ExprKind::Name { id, .. } = decorator.node {
                                    id.to_string() == "kernel" || id.to_string() == "portable"
                                } else {
                                    false
                                }
                            })
                        } else {
                            true
                        }
                    });
                    kernels
                }
                ast::StmtKind::FunctionDef {
                    ref decorator_list, ..
                } => decorator_list.iter().any(|decorator| {
                    if let ast::ExprKind::Name { id, .. } = decorator.node {
                        let id = id.to_string();
                        id == "extern" || id == "portable" || id == "kernel"
                    } else {
                        false
                    }
                }),
                _ => false,
            };

            if include {
                let (name, def_id, ty) = self
                    .composer
                    .register_top_level(stmt, Some(resolver.clone()), module_name.clone())
                    .unwrap();
                name_to_def.insert(name, def_id);
                if let Some(ty) = ty {
                    name_to_type.insert(name, ty);
                }
            }
        }
        let mut map = self.pyid_to_def.write();
        for (name, def) in name_to_def.into_iter() {
            map.insert(*name_to_pyid.get(&name).unwrap(), def);
        }
        let mut map = self.pyid_to_type.write();
        for (name, ty) in name_to_type.into_iter() {
            map.insert(*name_to_pyid.get(&name).unwrap(), ty);
        }
        Ok(())
    }
}

// ARTIQ timeline control with now-pinning optimization.
fn timeline_builtins(primitive: &PrimitiveStore) -> Vec<(StrRef, FunSignature, Arc<GenCall>)> {
    vec![(
        "now_mu".into(),
        FunSignature {
            args: vec![],
            ret: primitive.int64,
            vars: HashMap::new(),
        },
        Arc::new(GenCall::new(Box::new(
            |ctx, _, _, _| {
                let i64_type = ctx.ctx.i64_type();
                let now = ctx.module.get_global("now").unwrap_or_else(|| ctx.module.add_global(i64_type, None, "now"));
                let now_raw = ctx.builder.build_load(now.as_pointer_value(), "now");
                if let BasicValueEnum::IntValue(now_raw) = now_raw {
                    let i64_32 = i64_type.const_int(32, false).into();
                    let now_lo = ctx.builder.build_left_shift(now_raw, i64_32, "now_shl");
                    let now_hi = ctx.builder.build_right_shift(now_raw, i64_32, false, "now_lshr").into();
                    Some(ctx.builder.build_or(now_lo, now_hi, "now_or").into())
                } else {
                    unreachable!()
                }
            }
        )))
    ),(
        "at_mu".into(),
        FunSignature {
            args: vec![FuncArg {
                name: "t".into(),
                ty: primitive.int64,
                default_value: None,
            }],
            ret: primitive.none,
            vars: HashMap::new(),
        },
        Arc::new(GenCall::new(Box::new(
            |ctx, _, _, args| {
                let i32_type = ctx.ctx.i32_type();
                let i64_type = ctx.ctx.i64_type();
                let i64_32 = i64_type.const_int(32, false).into();
                if let BasicValueEnum::IntValue(time) = args[0].1 {
                    let time_hi = ctx.builder.build_int_truncate(ctx.builder.build_right_shift(time, i64_32, false, "now_lshr"), i32_type, "now_trunc");
                    let time_lo = ctx.builder.build_int_truncate(time, i32_type, "now_trunc");
                    let now = ctx.module.get_global("now").unwrap_or_else(|| ctx.module.add_global(i64_type, None, "now"));
                    let now_hiptr = ctx.builder.build_bitcast(now, i32_type.ptr_type(AddressSpace::Generic), "now_bitcast");
                    if let BasicValueEnum::PointerValue(now_hiptr) = now_hiptr {
                        let now_loptr = unsafe { ctx.builder.build_gep(now_hiptr, &[i32_type.const_int(1, false).into()], "now_gep") };
                        ctx.builder.build_store(now_hiptr, time_hi).set_atomic_ordering(AtomicOrdering::SequentiallyConsistent).unwrap();
                        ctx.builder.build_store(now_loptr, time_lo).set_atomic_ordering(AtomicOrdering::SequentiallyConsistent).unwrap();
                        None
                    } else {
                        unreachable!();
                    }
                } else {
                    unreachable!();
                }
            }
        )))
    ),(
        "delay_mu".into(),
        FunSignature {
            args: vec![FuncArg {
                name: "dt".into(),
                ty: primitive.int64,
                default_value: None,
            }],
            ret: primitive.none,
            vars: HashMap::new(),
        },
        Arc::new(GenCall::new(Box::new(
            |ctx, _, _, args| {
                let i32_type = ctx.ctx.i32_type();
                let i64_type = ctx.ctx.i64_type();
                let i64_32 = i64_type.const_int(32, false).into();
                let now = ctx.module.get_global("now").unwrap_or_else(|| ctx.module.add_global(i64_type, None, "now"));
                let now_raw = ctx.builder.build_load(now.as_pointer_value(), "now");
                if let (BasicValueEnum::IntValue(now_raw), BasicValueEnum::IntValue(dt)) = (now_raw, args[0].1) {
                    let now_lo = ctx.builder.build_left_shift(now_raw, i64_32, "now_shl");
                    let now_hi = ctx.builder.build_right_shift(now_raw, i64_32, false, "now_lshr").into();
                    let now_val = ctx.builder.build_or(now_lo, now_hi, "now_or");
                    let time = ctx.builder.build_int_add(now_val, dt, "now_add");
                    let time_hi = ctx.builder.build_int_truncate(ctx.builder.build_right_shift(time, i64_32, false, "now_lshr"), i32_type, "now_trunc");
                    let time_lo = ctx.builder.build_int_truncate(time, i32_type, "now_trunc");
                    let now_hiptr = ctx.builder.build_bitcast(now, i32_type.ptr_type(AddressSpace::Generic), "now_bitcast");
                    if let BasicValueEnum::PointerValue(now_hiptr) = now_hiptr {
                        let now_loptr = unsafe { ctx.builder.build_gep(now_hiptr, &[i32_type.const_int(1, false).into()], "now_gep") };
                        ctx.builder.build_store(now_hiptr, time_hi).set_atomic_ordering(AtomicOrdering::SequentiallyConsistent).unwrap();
                        ctx.builder.build_store(now_loptr, time_lo).set_atomic_ordering(AtomicOrdering::SequentiallyConsistent).unwrap();
                        None
                    } else {
                        unreachable!();
                    }
                } else {
                    unreachable!();
                }
            }
        )))
    )]
}

#[pymethods]
impl Nac3 {
    #[new]
    fn new(isa: &str) -> PyResult<Self> {
        let isa = match isa {
            "riscv" => Isa::RiscV,
            "cortexa9" => Isa::CortexA9,
            _ => return Err(exceptions::PyValueError::new_err("invalid ISA")),
        };
        let primitive: PrimitiveStore = TopLevelComposer::make_primitives().0;
        let builtins = if isa == Isa::RiscV { timeline_builtins(&primitive) } else { vec![] };
        let (composer, builtins_def, builtins_ty) = TopLevelComposer::new(builtins);
        Ok(Nac3 {
            isa,
            primitive,
            builtins_ty,
            builtins_def,
            composer,
            top_level: None,
            pyid_to_def: Default::default(),
            pyid_to_type: Default::default(),
            to_be_registered: Default::default(),
        })
    }

    fn register_module(&mut self, obj: PyObject) {
        self.to_be_registered.push(obj);
    }

    fn analyze(&mut self) -> PyResult<()> {
        for obj in std::mem::take(&mut self.to_be_registered).into_iter() {
            self.register_module_impl(obj)?;
        }
        self.composer.start_analysis(true).unwrap();
        self.top_level = Some(Arc::new(self.composer.make_top_level_context()));
        Ok(())
    }

    fn compile_method(&mut self, class: u64, method_name: String) -> PyResult<()> {
        let top_level = self.top_level.as_ref().unwrap();
        let module_resolver;
        let instance = {
            let defs = top_level.definitions.read();
            let class_def = defs[self.pyid_to_def.read().get(&class).unwrap().0].write();
            let mut method_def = if let TopLevelDef::Class {
                methods, resolver, ..
            } = &*class_def
            {
                module_resolver = Some(resolver.clone().unwrap());
                if let Some((_name, _unification_key, definition_id)) = methods
                    .iter()
                    .find(|method| method.0.to_string() == method_name)
                {
                    defs[definition_id.0].write()
                } else {
                    return Err(exceptions::PyValueError::new_err("method not found"));
                }
            } else {
                return Err(exceptions::PyTypeError::new_err(
                    "parent object is not a class",
                ));
            };

            // FIXME: what is this for? What happens if the kernel is called twice?
            if let TopLevelDef::Function {
                instance_to_stmt,
                instance_to_symbol,
                ..
            } = &mut *method_def
            {
                instance_to_symbol.insert("".to_string(), method_name);
                instance_to_stmt[""].clone()
            } else {
                unreachable!()
            }
        };
        let signature = FunSignature {
            args: vec![],
            ret: self.primitive.none,
            vars: HashMap::new(),
        };
        let task = CodeGenTask {
            subst: Default::default(),
            symbol_name: "__modinit__".to_string(),
            body: instance.body,
            signature,
            resolver: module_resolver.unwrap(),
            unifier: top_level.unifiers.read()[instance.unifier_id].clone(),
            calls: instance.calls,
        };
        let isa = self.isa;
        let f = Arc::new(WithCall::new(Box::new(move |module| {
            let builder = PassManagerBuilder::create();
            builder.set_optimization_level(OptimizationLevel::Aggressive);
            let passes = PassManager::create(());
            builder.populate_module_pass_manager(&passes);
            passes.run_on(module);

            let (triple, features) = match isa {
                Isa::RiscV => (TargetTriple::create("riscv32-unknown-linux"), "+a,+m"),
                Isa::CortexA9 => (
                    TargetTriple::create("armv7-unknown-linux-gnueabihf"),
                    "+dsp,+fp16,+neon,+vfp3",
                ),
            };
            let target =
                Target::from_triple(&triple).expect("couldn't create target from target triple");
            let target_machine = target
                .create_target_machine(
                    &triple,
                    "",
                    features,
                    OptimizationLevel::Default,
                    RelocMode::PIC,
                    CodeModel::Default,
                )
                .expect("couldn't create target machine");
            target_machine
                .write_to_file(
                    module,
                    FileType::Object,
                    Path::new(&format!("{}.o", module.get_name().to_str().unwrap())),
                )
                .expect("couldn't write module to file");
        })));
        let thread_names: Vec<String> = (0..4).map(|i| format!("module{}", i)).collect();
        let threads: Vec<_> = thread_names.iter().map(|s| s.as_str()).collect();
        let (registry, handles) = WorkerRegistry::create_workers(&threads, top_level.clone(), f);
        registry.add_task(task);
        registry.wait_tasks_complete(handles);

        let mut linker_args = vec![
            "-shared".to_string(),
            "--eh-frame-hdr".to_string(),
            "-Tkernel.ld".to_string(),
            "-x".to_string(),
            "-o".to_string(),
            "module.elf".to_string(),
        ];
        linker_args.extend(thread_names.iter().map(|name| name.to_owned() + ".o"));
        if let Ok(linker_status) = Command::new("ld.lld").args(linker_args).status() {
            if !linker_status.success() {
                return Err(exceptions::PyRuntimeError::new_err(
                    "failed to start linker",
                ));
            }
        } else {
            return Err(exceptions::PyRuntimeError::new_err(
                "linker returned non-zero status code",
            ));
        }

        Ok(())
    }
}

#[pymodule]
fn nac3artiq(_py: Python, m: &PyModule) -> PyResult<()> {
    Target::initialize_all(&InitializationConfig::default());
    m.add_class::<Nac3>()?;
    Ok(())
}
