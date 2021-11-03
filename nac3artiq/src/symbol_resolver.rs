use inkwell::{types::BasicType, values::BasicValueEnum, AddressSpace};
use nac3core::{
    codegen::CodeGenContext,
    location::Location,
    symbol_resolver::SymbolResolver,
    toplevel::{DefinitionId, TopLevelDef},
    typecheck::{
        type_inferencer::PrimitiveStore,
        typedef::{Type, TypeEnum, Unifier},
    },
};
use parking_lot::{Mutex, RwLock};
use pyo3::{
    types::{PyList, PyModule, PyTuple},
    PyAny, PyObject, PyResult, Python,
};
use nac3parser::ast::StrRef;
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    sync::Arc,
};

use crate::PrimitivePythonId;

pub struct Resolver {
    pub id_to_type: Mutex<HashMap<StrRef, Type>>,
    pub id_to_def: Mutex<HashMap<StrRef, DefinitionId>>,
    pub global_value_ids: Arc<Mutex<HashSet<u64>>>,
    pub class_names: Mutex<HashMap<StrRef, Type>>,
    pub pyid_to_def: Arc<RwLock<HashMap<u64, DefinitionId>>>,
    pub pyid_to_type: Arc<RwLock<HashMap<u64, Type>>>,
    pub primitive_ids: PrimitivePythonId,
    // module specific
    pub name_to_pyid: HashMap<StrRef, u64>,
    pub module: PyObject,
}

struct PythonHelper<'a> {
    type_fn: &'a PyAny,
    len_fn: &'a PyAny,
    id_fn: &'a PyAny,
}

impl Resolver {
    fn get_list_elem_type(
        &self,
        list: &PyAny,
        len: usize,
        helper: &PythonHelper,
        unifier: &mut Unifier,
        defs: &[Arc<RwLock<TopLevelDef>>],
        primitives: &PrimitiveStore,
    ) -> PyResult<Option<Type>> {
        let first = self.get_obj_type(list.get_item(0)?, helper, unifier, defs, primitives)?;
        Ok((1..len).fold(first, |a, i| {
            let b = list
                .get_item(i)
                .map(|elem| self.get_obj_type(elem, helper, unifier, defs, primitives));
            a.and_then(|a| {
                if let Ok(Ok(Some(ty))) = b {
                    if unifier.unify(a, ty).is_ok() {
                        Some(a)
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
        }))
    }

    fn get_obj_type(
        &self,
        obj: &PyAny,
        helper: &PythonHelper,
        unifier: &mut Unifier,
        defs: &[Arc<RwLock<TopLevelDef>>],
        primitives: &PrimitiveStore,
    ) -> PyResult<Option<Type>> {
        let ty_id: u64 = helper
            .id_fn
            .call1((helper.type_fn.call1((obj,))?,))?
            .extract()?;

        if ty_id == self.primitive_ids.int || ty_id == self.primitive_ids.int32 {
            Ok(Some(primitives.int32))
        } else if ty_id == self.primitive_ids.int64 {
            Ok(Some(primitives.int64))
        } else if ty_id == self.primitive_ids.bool {
            Ok(Some(primitives.bool))
        } else if ty_id == self.primitive_ids.float {
            Ok(Some(primitives.float))
        } else if ty_id == self.primitive_ids.list {
            let len: usize = helper.len_fn.call1((obj,))?.extract()?;
            if len == 0 {
                let var = unifier.get_fresh_var().0;
                let list = unifier.add_ty(TypeEnum::TList { ty: var });
                Ok(Some(list))
            } else {
                let ty = self.get_list_elem_type(obj, len, helper, unifier, defs, primitives)?;
                Ok(ty.map(|ty| unifier.add_ty(TypeEnum::TList { ty })))
            }
        } else if ty_id == self.primitive_ids.tuple {
            let elements: &PyTuple = obj.cast_as()?;
            let types: Result<Option<Vec<_>>, _> = elements
                .iter()
                .map(|elem| self.get_obj_type(elem, helper, unifier, defs, primitives))
                .collect();
            let types = types?;
            Ok(types.map(|types| unifier.add_ty(TypeEnum::TTuple { ty: types })))
        } else if let Some(def_id) = self.pyid_to_def.read().get(&ty_id) {
            let def = defs[def_id.0].read();
            if let TopLevelDef::Class {
                object_id,
                type_vars,
                fields,
                methods,
                ..
            } = &*def
            {
                let var_map: HashMap<_, _> = type_vars
                    .iter()
                    .map(|var| {
                        (
                            if let TypeEnum::TVar { id, .. } = &*unifier.get_ty(*var) {
                                *id
                            } else {
                                unreachable!()
                            },
                            unifier.get_fresh_var().0,
                        )
                    })
                    .collect();
                let mut fields_ty = HashMap::new();
                for method in methods.iter() {
                    fields_ty.insert(method.0, method.1);
                }
                for field in fields.iter() {
                    let name: String = field.0.into();
                    let field_data = obj.getattr(&name)?;
                    let ty = self
                        .get_obj_type(field_data, helper, unifier, defs, primitives)?
                        .unwrap_or(primitives.none);
                    let field_ty = unifier.subst(field.1, &var_map).unwrap_or(field.1);
                    if unifier.unify(ty, field_ty).is_err() {
                        // field type mismatch
                        return Ok(None);
                    }
                    fields_ty.insert(field.0, ty);
                }
                for (_, ty) in var_map.iter() {
                    // must be concrete type
                    if !unifier.is_concrete(*ty, &[]) {
                        return Ok(None)
                    }
                }
                Ok(Some(unifier.add_ty(TypeEnum::TObj {
                    obj_id: *object_id,
                    fields: RefCell::new(fields_ty),
                    params: RefCell::new(var_map),
                })))
            } else {
                // only object is supported, functions are not supported
                Ok(None)
            }
        } else {
            Ok(None)
        }
    }

    fn get_obj_value<'ctx, 'a>(
        &self,
        obj: &PyAny,
        helper: &PythonHelper,
        ctx: &mut CodeGenContext<'ctx, 'a>,
    ) -> PyResult<Option<BasicValueEnum<'ctx>>> {
        let ty_id: u64 = helper
            .id_fn
            .call1((helper.type_fn.call1((obj,))?,))?
            .extract()?;
        if ty_id == self.primitive_ids.int || ty_id == self.primitive_ids.int32 {
            let val: i32 = obj.extract()?;
            Ok(Some(ctx.ctx.i32_type().const_int(val as u64, false).into()))
        } else if ty_id == self.primitive_ids.int64 {
            let val: i64 = obj.extract()?;
            Ok(Some(ctx.ctx.i64_type().const_int(val as u64, false).into()))
        } else if ty_id == self.primitive_ids.bool {
            let val: bool = obj.extract()?;
            Ok(Some(
                ctx.ctx.bool_type().const_int(val as u64, false).into(),
            ))
        } else if ty_id == self.primitive_ids.float {
            let val: f64 = obj.extract()?;
            Ok(Some(ctx.ctx.f64_type().const_float(val).into()))
        } else if ty_id == self.primitive_ids.list {
            let id: u64 = helper.id_fn.call1((obj,))?.extract()?;
            let id_str = id.to_string();
            let len: usize = helper.len_fn.call1((obj,))?.extract()?;
            let ty = if len == 0 {
                ctx.primitives.int32
            } else {
                self.get_list_elem_type(
                    obj,
                    len,
                    helper,
                    &mut ctx.unifier,
                    &ctx.top_level.definitions.read(),
                    &ctx.primitives,
                )?
                .unwrap()
            };
            let ty = ctx.get_llvm_type(ty);
            let arr_ty = ctx.ctx.struct_type(
                &[
                    ctx.ctx.i32_type().into(),
                    ty.ptr_type(AddressSpace::Generic).into(),
                ],
                false,
            );

            {
                let mut global_value_ids = self.global_value_ids.lock();
                if global_value_ids.contains(&id) {
                    let global = ctx.module.get_global(&id_str).unwrap_or_else(|| {
                        ctx.module
                            .add_global(arr_ty, Some(AddressSpace::Generic), &id_str)
                    });
                    return Ok(Some(global.as_pointer_value().into()));
                } else {
                    global_value_ids.insert(id);
                }
            }

            let arr: Result<Option<Vec<_>>, _> = (0..len)
                .map(|i| {
                    obj.get_item(i)
                        .and_then(|elem| self.get_obj_value(elem, helper, ctx))
                })
                .collect();
            let arr = arr?.unwrap();

            let arr_global = ctx.module.add_global(
                ty.array_type(len as u32),
                Some(AddressSpace::Generic),
                &(id_str.clone() + "_"),
            );
            let arr: BasicValueEnum = if ty.is_int_type() {
                let arr: Vec<_> = arr
                    .into_iter()
                    .map(BasicValueEnum::into_int_value)
                    .collect();
                ty.into_int_type().const_array(&arr)
            } else if ty.is_float_type() {
                let arr: Vec<_> = arr
                    .into_iter()
                    .map(BasicValueEnum::into_float_value)
                    .collect();
                ty.into_float_type().const_array(&arr)
            } else if ty.is_array_type() {
                let arr: Vec<_> = arr
                    .into_iter()
                    .map(BasicValueEnum::into_array_value)
                    .collect();
                ty.into_array_type().const_array(&arr)
            } else if ty.is_struct_type() {
                let arr: Vec<_> = arr
                    .into_iter()
                    .map(BasicValueEnum::into_struct_value)
                    .collect();
                ty.into_struct_type().const_array(&arr)
            } else if ty.is_pointer_type() {
                let arr: Vec<_> = arr
                    .into_iter()
                    .map(BasicValueEnum::into_pointer_value)
                    .collect();
                ty.into_pointer_type().const_array(&arr)
            } else {
                unreachable!()
            }
            .into();
            arr_global.set_initializer(&arr);

            let val = arr_ty.const_named_struct(&[
                ctx.ctx.i32_type().const_int(len as u64, false).into(),
                arr_global
                    .as_pointer_value()
                    .const_cast(ty.ptr_type(AddressSpace::Generic))
                    .into(),
            ]);

            let global = ctx
                .module
                .add_global(arr_ty, Some(AddressSpace::Generic), &id_str);
            global.set_initializer(&val);

            Ok(Some(global.as_pointer_value().into()))
        } else if ty_id == self.primitive_ids.tuple {
            let id: u64 = helper.id_fn.call1((obj,))?.extract()?;
            let id_str = id.to_string();
            let elements: &PyTuple = obj.cast_as()?;
            let types: Result<Option<Vec<_>>, _> = elements
                .iter()
                .map(|elem| {
                    self.get_obj_type(
                        elem,
                        helper,
                        &mut ctx.unifier,
                        &ctx.top_level.definitions.read(),
                        &ctx.primitives,
                    )
                    .map(|ty| ty.map(|ty| ctx.get_llvm_type(ty)))
                })
                .collect();
            let types = types?.unwrap();
            let ty = ctx.ctx.struct_type(&types, false);

            {
                let mut global_value_ids = self.global_value_ids.lock();
                if global_value_ids.contains(&id) {
                    let global = ctx.module.get_global(&id_str).unwrap_or_else(|| {
                        ctx.module
                            .add_global(ty, Some(AddressSpace::Generic), &id_str)
                    });
                    return Ok(Some(global.as_pointer_value().into()));
                } else {
                    global_value_ids.insert(id);
                }
            }

            let val: Result<Option<Vec<_>>, _> = elements
                .iter()
                .map(|elem| self.get_obj_value(elem, helper, ctx))
                .collect();
            let val = val?.unwrap();
            let val = ctx.ctx.const_struct(&val, false);
            let global = ctx
                .module
                .add_global(ty, Some(AddressSpace::Generic), &id_str);
            global.set_initializer(&val);
            Ok(Some(global.as_pointer_value().into()))
        } else {
            let id: u64 = helper.id_fn.call1((obj,))?.extract()?;
            let id_str = id.to_string();
            let top_level_defs = ctx.top_level.definitions.read();
            let ty = self
                .get_obj_type(
                    obj,
                    helper,
                    &mut ctx.unifier,
                    &top_level_defs,
                    &ctx.primitives,
                )?
                .unwrap();
            let ty = ctx
                .get_llvm_type(ty)
                .into_pointer_type()
                .get_element_type()
                .into_struct_type()
                .as_basic_type_enum();
            {
                let mut global_value_ids = self.global_value_ids.lock();
                if global_value_ids.contains(&id) {
                    let global = ctx.module.get_global(&id_str).unwrap_or_else(|| {
                        ctx.module
                            .add_global(ty, Some(AddressSpace::Generic), &id_str)
                    });
                    return Ok(Some(global.as_pointer_value().into()));
                } else {
                    global_value_ids.insert(id);
                }
            }
            // should be classes
            let definition = top_level_defs
                .get(self.pyid_to_def.read().get(&ty_id).unwrap().0)
                .unwrap()
                .read();
            if let TopLevelDef::Class { fields, .. } = &*definition {
                let values: Result<Option<Vec<_>>, _> = fields
                    .iter()
                    .map(|(name, _)| {
                        self.get_obj_value(obj.getattr(&name.to_string())?, helper, ctx)
                    })
                    .collect();
                let values = values?;
                if let Some(values) = values {
                    let val = ctx.ctx.const_struct(&values, false);
                    let global = ctx
                        .module
                        .add_global(ty, Some(AddressSpace::Generic), &id_str);
                    global.set_initializer(&val);
                    Ok(Some(global.as_pointer_value().into()))
                } else {
                    Ok(None)
                }
            } else {
                unreachable!()
            }
        }
    }
}

impl SymbolResolver for Resolver {
    fn get_symbol_type(
        &self,
        unifier: &mut Unifier,
        defs: &[Arc<RwLock<TopLevelDef>>],
        primitives: &PrimitiveStore,
        str: StrRef,
    ) -> Option<Type> {
        let mut id_to_type = self.id_to_type.lock();
        id_to_type.get(&str).cloned().or_else(|| {
            let py_id = self.name_to_pyid.get(&str);
            let result = py_id.and_then(|id| {
                self.pyid_to_type.read().get(&id).copied().or_else(|| {
                    Python::with_gil(|py| -> PyResult<Option<Type>> {
                        let obj: &PyAny = self.module.extract(py)?;
                        let members: &PyList = PyModule::import(py, "inspect")?
                            .getattr("getmembers")?
                            .call1((obj,))?
                            .cast_as()?;
                        let mut sym_ty = None;
                        for member in members.iter() {
                            let key: &str = member.get_item(0)?.extract()?;
                            if key == str.to_string() {
                                let builtins = PyModule::import(py, "builtins")?;
                                let helper = PythonHelper {
                                    id_fn: builtins.getattr("id").unwrap(),
                                    len_fn: builtins.getattr("len").unwrap(),
                                    type_fn: builtins.getattr("type").unwrap(),
                                };
                                sym_ty = self.get_obj_type(
                                    member.get_item(1)?,
                                    &helper,
                                    unifier,
                                    defs,
                                    primitives,
                                )?;
                                break;
                            }
                        }
                        Ok(sym_ty)
                    })
                    .unwrap()
                })
            });
            if let Some(result) = &result {
                id_to_type.insert(str, *result);
            }
            result
        })
    }

    fn get_symbol_value<'ctx, 'a>(
        &self,
        id: StrRef,
        ctx: &mut CodeGenContext<'ctx, 'a>,
    ) -> Option<BasicValueEnum<'ctx>> {
        Python::with_gil(|py| -> PyResult<Option<BasicValueEnum<'ctx>>> {
            let obj: &PyAny = self.module.extract(py)?;
            let members: &PyList = PyModule::import(py, "inspect")?
                .getattr("getmembers")?
                .call1((obj,))?
                .cast_as()?;
            let mut sym_value = None;
            for member in members.iter() {
                let key: &str = member.get_item(0)?.extract()?;
                let val = member.get_item(1)?;
                if key == id.to_string() {
                    let builtins = PyModule::import(py, "builtins")?;
                    let helper = PythonHelper {
                        id_fn: builtins.getattr("id").unwrap(),
                        len_fn: builtins.getattr("len").unwrap(),
                        type_fn: builtins.getattr("type").unwrap(),
                    };
                    sym_value = self.get_obj_value(val, &helper, ctx)?;
                    break;
                }
            }
            Ok(sym_value)
        })
        .unwrap()
    }

    fn get_symbol_location(&self, _: StrRef) -> Option<Location> {
        unimplemented!()
    }

    fn get_identifier_def(&self, id: StrRef) -> Option<DefinitionId> {
        let mut id_to_def = self.id_to_def.lock();
        id_to_def.get(&id).cloned().or_else(|| {
            let py_id = self.name_to_pyid.get(&id);
            let result = py_id.and_then(|id| self.pyid_to_def.read().get(&id).copied());
            if let Some(result) = &result {
                id_to_def.insert(id, *result);
            }
            result
        })
    }
}
