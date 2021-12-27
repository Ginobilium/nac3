use inkwell::{types::BasicType, values::BasicValueEnum, AddressSpace};
use nac3core::{
    codegen::{CodeGenContext, CodeGenerator},
    location::Location,
    symbol_resolver::{StaticValue, SymbolResolver, SymbolValue, ValueEnum},
    toplevel::{DefinitionId, TopLevelDef},
    typecheck::{
        type_inferencer::PrimitiveStore,
        typedef::{Type, TypeEnum, Unifier},
    },
};
use nac3parser::ast::{self, StrRef};
use parking_lot::{Mutex, RwLock};
use pyo3::{
    types::{PyDict, PyTuple},
    PyAny, PyObject, PyResult, Python,
};
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    sync::Arc,
};

use crate::PrimitivePythonId;

pub enum PrimitiveValue {
    I32(i32),
    I64(i64),
    F64(f64),
    Bool(bool),
}

pub struct InnerResolver {
    pub id_to_type: RwLock<HashMap<StrRef, Type>>,
    pub id_to_def: RwLock<HashMap<StrRef, DefinitionId>>,
    pub id_to_pyval: RwLock<HashMap<StrRef, (u64, PyObject)>>,
    pub id_to_primitive: RwLock<HashMap<u64, PrimitiveValue>>,
    pub field_to_val: RwLock<HashMap<(u64, StrRef), Option<(u64, PyObject)>>>,
    pub global_value_ids: Arc<RwLock<HashSet<u64>>>,
    pub class_names: Mutex<HashMap<StrRef, Type>>,
    pub pyid_to_def: Arc<RwLock<HashMap<u64, DefinitionId>>>,
    pub pyid_to_type: Arc<RwLock<HashMap<u64, Type>>>,
    pub primitive_ids: PrimitivePythonId,
    pub helper: PythonHelper,
    // module specific
    pub name_to_pyid: HashMap<StrRef, u64>,
    pub module: PyObject,
}

pub struct Resolver(pub Arc<InnerResolver>);

#[derive(Clone)]
pub struct PythonHelper {
    pub type_fn: PyObject,
    pub len_fn: PyObject,
    pub id_fn: PyObject,
    pub origin_ty_fn: PyObject,
    pub args_ty_fn: PyObject,
}

struct PythonValue {
    id: u64,
    value: PyObject,
    resolver: Arc<InnerResolver>,
}

impl StaticValue for PythonValue {
    fn get_unique_identifier(&self) -> u64 {
        self.id
    }

    fn to_basic_value_enum<'ctx, 'a>(
        &self,
        ctx: &mut CodeGenContext<'ctx, 'a>,
        generator: &mut dyn CodeGenerator,
    ) -> BasicValueEnum<'ctx> {
        if let Some(val) = self.resolver.id_to_primitive.read().get(&self.id) {
            return match val {
                PrimitiveValue::I32(val) => ctx.ctx.i32_type().const_int(*val as u64, false).into(),
                PrimitiveValue::I64(val) => ctx.ctx.i64_type().const_int(*val as u64, false).into(),
                PrimitiveValue::F64(val) => ctx.ctx.f64_type().const_float(*val).into(),
                PrimitiveValue::Bool(val) => {
                    ctx.ctx.bool_type().const_int(*val as u64, false).into()
                }
            };
        }
        if let Some(global) = ctx.module.get_global(&self.id.to_string()) {
            return global.as_pointer_value().into();
        }

        Python::with_gil(|py| -> PyResult<BasicValueEnum<'ctx>> {
            self.resolver
                .get_obj_value(py, self.value.as_ref(py), ctx, generator)
                .map(Option::unwrap)
        })
        .unwrap()
    }

    fn get_field<'ctx, 'a>(
        &self,
        name: StrRef,
        ctx: &mut CodeGenContext<'ctx, 'a>,
    ) -> Option<ValueEnum<'ctx>> {
        {
            let field_to_val = self.resolver.field_to_val.read();
            field_to_val.get(&(self.id, name)).cloned()
        }
        .unwrap_or_else(|| {
            Python::with_gil(|py| -> PyResult<Option<(u64, PyObject)>> {
                let helper = &self.resolver.helper;
                let ty = helper.type_fn.call1(py, (&self.value,))?;
                let ty_id: u64 = helper.id_fn.call1(py, (ty,))?.extract(py)?;
                let def_id = { *self.resolver.pyid_to_def.read().get(&ty_id).unwrap() };
                let mut mutable = true;
                let defs = ctx.top_level.definitions.read();
                if let TopLevelDef::Class { fields, .. } = &*defs[def_id.0].read() {
                    for (field_name, _, is_mutable) in fields.iter() {
                        if field_name == &name {
                            mutable = *is_mutable;
                            break;
                        }
                    }
                }
                let result = if mutable {
                    None
                } else {
                    let obj = self.value.getattr(py, &name.to_string())?;
                    let id = self.resolver.helper.id_fn.call1(py, (&obj,))?.extract(py)?;
                    Some((id, obj))
                };
                self.resolver
                    .field_to_val
                    .write()
                    .insert((self.id, name), result.clone());
                Ok(result)
            })
            .unwrap()
        })
        .map(|(id, obj)| {
            ValueEnum::Static(Arc::new(PythonValue {
                id,
                value: obj,
                resolver: self.resolver.clone(),
            }))
        })
    }
}

impl InnerResolver {
    fn get_list_elem_type(
        &self,
        py: Python,
        list: &PyAny,
        len: usize,
        unifier: &mut Unifier,
        defs: &[Arc<RwLock<TopLevelDef>>],
        primitives: &PrimitiveStore,
    ) -> PyResult<Option<Type>> {
        let first = self.get_obj_type(py, list.get_item(0)?, unifier, defs, primitives)?;
        Ok((1..len).fold(first, |a, i| {
            let b = list
                .get_item(i)
                .map(|elem| self.get_obj_type(py, elem, unifier, defs, primitives));
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

    // handle python objects that represent types themselves
    // primitives and class types should be themselves, use `ty_id` to check,
    // TypeVars and GenericAlias(`A[int, bool]`) should use `ty_ty_id` to check
    // the `bool` value returned indicates whether they are instantiated or not
    fn get_pyty_obj_type(
        &self,
        py: Python,
        pyty: &PyAny,
        unifier: &mut Unifier,
        defs: &[Arc<RwLock<TopLevelDef>>],
        primitives: &PrimitiveStore,
    ) -> PyResult<Result<(Type, bool), String>> {
        let ty_id: u64 = self.helper.id_fn.call1(py, (pyty,))?.extract(py)?;
        let ty_ty_id: u64 = self
            .helper
            .id_fn
            .call1(py, (self.helper.type_fn.call1(py, (pyty,))?,))?
            .extract(py)?;

        if ty_id == self.primitive_ids.int || ty_id == self.primitive_ids.int32 {
            Ok(Ok((primitives.int32, true)))
        } else if ty_id == self.primitive_ids.int64 {
            Ok(Ok((primitives.int64, true)))
        } else if ty_id == self.primitive_ids.bool {
            Ok(Ok((primitives.bool, true)))
        } else if ty_id == self.primitive_ids.float {
            Ok(Ok((primitives.float, true)))
        } else if ty_id == self.primitive_ids.list {
            // do not handle type var param and concrete check here
            let var = unifier.get_fresh_var().0;
            let list = unifier.add_ty(TypeEnum::TList { ty: var });
            Ok(Ok((list, false)))
        } else if ty_id == self.primitive_ids.tuple {
            // do not handle type var param and concrete check here
            Ok(Ok((unifier.add_ty(TypeEnum::TTuple { ty: vec![] }), false)))
        } else if let Some(def_id) = self.pyid_to_def.read().get(&ty_id).cloned() {
            let def = defs[def_id.0].read();
            if let TopLevelDef::Class {
                object_id,
                type_vars,
                fields,
                methods,
                ..
            } = &*def
            {
                // do not handle type var param and concrete check here, and no subst
                Ok(Ok({
                    let ty = TypeEnum::TObj {
                        obj_id: *object_id,
                        params: RefCell::new({
                            type_vars
                                .iter()
                                .map(|x| {
                                    if let TypeEnum::TVar { id, .. } = &*unifier.get_ty(*x) {
                                        (*id, *x)
                                    } else {
                                        unreachable!()
                                    }
                                })
                                .collect()
                        }),
                        fields: RefCell::new({
                            let mut res = methods
                                .iter()
                                .map(|(iden, ty, _)| (*iden, (*ty, false)))
                                .collect::<HashMap<_, _>>();
                            res.extend(fields.clone().into_iter().map(|x| (x.0, (x.1, x.2))));
                            res
                        }),
                    };
                    // here also false, later instantiation use python object to check compatible
                    (unifier.add_ty(ty), false)
                }))
            } else {
                // only object is supported, functions are not supported
                unreachable!("function type is not supported, should not be queried")
            }
        } else if ty_ty_id == self.primitive_ids.typevar {
            let constraint_types = {
                let constraints = pyty.getattr("__constraints__").unwrap();
                let mut result: Vec<Type> = vec![];
                for i in 0.. {
                    if let Ok(constr) = constraints.get_item(i) {
                        result.push({
                            match self.get_pyty_obj_type(py, constr, unifier, defs, primitives)? {
                                Ok((ty, _)) => {
                                    if unifier.is_concrete(ty, &[]) {
                                        ty
                                    } else {
                                        return Ok(Err(format!(
                                            "the {}th constraint of TypeVar `{}` is not concrete",
                                            i + 1,
                                            pyty.getattr("__name__")?.extract::<String>()?
                                        )));
                                    }
                                }
                                Err(err) => return Ok(Err(err)),
                            }
                        })
                    } else {
                        break;
                    }
                }
                result
            };
            let res = unifier.get_fresh_var_with_range(&constraint_types).0;
            Ok(Ok((res, true)))
        } else if ty_ty_id == self.primitive_ids.generic_alias.0
            || ty_ty_id == self.primitive_ids.generic_alias.1
        {
            let origin = self.helper.origin_ty_fn.call1(py, (pyty,))?;
            let args = self.helper.args_ty_fn.call1(py, (pyty,))?;
            let args: &PyTuple = args.cast_as(py)?;
            let origin_ty =
                match self.get_pyty_obj_type(py, origin.as_ref(py), unifier, defs, primitives)? {
                    Ok((ty, false)) => ty,
                    Ok((_, true)) => {
                        return Ok(Err("instantiated type does not take type parameters".into()))
                    }
                    Err(err) => return Ok(Err(err)),
                };

            match &*unifier.get_ty(origin_ty) {
                TypeEnum::TList { .. } => {
                    if args.len() == 1 {
                        let ty = match self.get_pyty_obj_type(
                            py,
                            args.get_item(0),
                            unifier,
                            defs,
                            primitives,
                        )? {
                            Ok(ty) => ty,
                            Err(err) => return Ok(Err(err)),
                        };
                        if !unifier.is_concrete(ty.0, &[]) && !ty.1 {
                            panic!("type list should take concrete parameters in type var ranges")
                        }
                        Ok(Ok((unifier.add_ty(TypeEnum::TList { ty: ty.0 }), true)))
                    } else {
                        return Ok(Err(format!(
                            "type list needs exactly 1 type parameters, found {}",
                            args.len()
                        )));
                    }
                }
                TypeEnum::TTuple { .. } => {
                    let args = match args
                        .iter()
                        .map(|x| self.get_pyty_obj_type(py, x, unifier, defs, primitives))
                        .collect::<Result<Vec<_>, _>>()?
                        .into_iter()
                        .collect::<Result<Vec<_>, _>>() {
                            Ok(args) if !args.is_empty() => args
                                .into_iter()
                                .map(|(x, check)| if !unifier.is_concrete(x, &[]) && !check {
                                        panic!("type tuple should take concrete parameters in type var ranges")
                                    } else {
                                        x
                                    }
                                )
                                .collect::<Vec<_>>(),
                            Err(err) => return Ok(Err(err)),
                            _ => return Ok(Err("tuple type needs at least 1 type parameters".to_string()))
                        };
                    Ok(Ok((unifier.add_ty(TypeEnum::TTuple { ty: args }), true)))
                }
                TypeEnum::TObj { params, obj_id, .. } => {
                    let subst = {
                        let params = &*params.borrow();
                        if params.len() != args.len() {
                            return Ok(Err(format!(
                                "for class #{}, expect {} type parameters, got {}.",
                                obj_id.0,
                                params.len(),
                                args.len(),
                            )));
                        }
                        let args = match args
                            .iter()
                            .map(|x| self.get_pyty_obj_type(py, x, unifier, defs, primitives))
                            .collect::<Result<Vec<_>, _>>()?
                            .into_iter()
                            .collect::<Result<Vec<_>, _>>() {
                                Ok(args) => args
                                .into_iter()
                                .map(|(x, check)| if !unifier.is_concrete(x, &[]) && !check {
                                        panic!("type class should take concrete parameters in type var ranges")
                                    } else {
                                        x
                                    }
                                )
                                .collect::<Vec<_>>(),
                                Err(err) => return Ok(Err(err)),
                            };
                        params
                            .iter()
                            .zip(args.iter())
                            .map(|((id, _), ty)| (*id, *ty))
                            .collect::<HashMap<_, _>>()
                    };
                    Ok(Ok((
                        unifier.subst(origin_ty, &subst).unwrap_or(origin_ty),
                        true,
                    )))
                }
                TypeEnum::TVirtual { .. } => {
                    if args.len() == 1 {
                        let ty = match self.get_pyty_obj_type(
                            py,
                            args.get_item(0),
                            unifier,
                            defs,
                            primitives,
                        )? {
                            Ok(ty) => ty,
                            Err(err) => return Ok(Err(err)),
                        };
                        if !unifier.is_concrete(ty.0, &[]) && !ty.1 {
                            panic!(
                                "virtual class should take concrete parameters in type var ranges"
                            )
                        }
                        Ok(Ok((unifier.add_ty(TypeEnum::TVirtual { ty: ty.0 }), true)))
                    } else {
                        return Ok(Err(format!(
                            "virtual class needs exactly 1 type parameters, found {}",
                            args.len()
                        )));
                    }
                }
                _ => unimplemented!(),
            }
        } else if ty_id == self.primitive_ids.virtual_id {
            Ok(Ok((
                {
                    let ty = TypeEnum::TVirtual {
                        ty: unifier.get_fresh_var().0,
                    };
                    unifier.add_ty(ty)
                },
                false,
            )))
        } else {
            Ok(Err("unknown type".into()))
        }
    }

    fn get_obj_type(
        &self,
        py: Python,
        obj: &PyAny,
        unifier: &mut Unifier,
        defs: &[Arc<RwLock<TopLevelDef>>],
        primitives: &PrimitiveStore,
    ) -> PyResult<Option<Type>> {
        let ty = self.helper.type_fn.call1(py, (obj,)).unwrap();
        let (extracted_ty, inst_check) = match self.get_pyty_obj_type(
            py,
            {
                if [
                    self.primitive_ids.typevar,
                    self.primitive_ids.generic_alias.0,
                    self.primitive_ids.generic_alias.1,
                ]
                .contains(
                    &self
                        .helper
                        .id_fn
                        .call1(py, (ty.clone(),))?
                        .extract::<u64>(py)?,
                ) {
                    obj
                } else {
                    ty.as_ref(py)
                }
            },
            unifier,
            defs,
            primitives,
        )? {
            Ok(s) => s,
            Err(_) => return Ok(None),
        };
        return match (&*unifier.get_ty(extracted_ty), inst_check) {
            // do the instantiation for these three types
            (TypeEnum::TList { ty }, false) => {
                let len: usize = self.helper.len_fn.call1(py, (obj,))?.extract(py)?;
                if len == 0 {
                    assert!(matches!(
                        &*unifier.get_ty(extracted_ty),
                        TypeEnum::TVar { meta: nac3core::typecheck::typedef::TypeVarMeta::Generic, range, .. }
                            if range.borrow().is_empty()
                    ));
                    Ok(Some(extracted_ty))
                } else {
                    let actual_ty =
                        self.get_list_elem_type(py, obj, len, unifier, defs, primitives)?;
                    if let Some(actual_ty) = actual_ty {
                        unifier.unify(*ty, actual_ty).unwrap();
                        Ok(Some(extracted_ty))
                    } else {
                        Ok(None)
                    }
                }
            }
            (TypeEnum::TTuple { .. }, false) => {
                let elements: &PyTuple = obj.cast_as()?;
                let types: Result<Option<Vec<_>>, _> = elements
                    .iter()
                    .map(|elem| self.get_obj_type(py, elem, unifier, defs, primitives))
                    .collect();
                let types = types?;
                Ok(types.map(|types| unifier.add_ty(TypeEnum::TTuple { ty: types })))
            }
            (TypeEnum::TObj { params, fields, .. }, false) => {
                let var_map = params
                    .borrow()
                    .iter()
                    .map(|(id_var, ty)| {
                        if let TypeEnum::TVar { id, range, .. } = &*unifier.get_ty(*ty) {
                            assert_eq!(*id, *id_var);
                            (*id, unifier.get_fresh_var_with_range(&range.borrow()).0)
                        } else {
                            unreachable!()
                        }
                    })
                    .collect::<HashMap<_, _>>();
                // loop through non-function fields of the class to get the instantiated value
                for field in fields.borrow().iter() {
                    let name: String = (*field.0).into();
                    if let TypeEnum::TFunc(..) = &*unifier.get_ty(field.1 .0) {
                        continue;
                    } else {
                        let field_data = obj.getattr(&name)?;
                        let ty = self
                            .get_obj_type(py, field_data, unifier, defs, primitives)?
                            .unwrap_or(primitives.none);
                        let field_ty = unifier.subst(field.1 .0, &var_map).unwrap_or(field.1 .0);
                        if unifier.unify(ty, field_ty).is_err() {
                            // field type mismatch
                            return Ok(None);
                        }
                    }
                }
                for (_, ty) in var_map.iter() {
                    // must be concrete type
                    if !unifier.is_concrete(*ty, &[]) {
                        return Ok(None);
                    }
                }
                return Ok(Some(
                    unifier
                        .subst(extracted_ty, &var_map)
                        .unwrap_or(extracted_ty),
                ));
            }
            _ => Ok(Some(extracted_ty)),
        };
    }

    fn get_obj_value<'ctx, 'a>(
        &self,
        py: Python,
        obj: &PyAny,
        ctx: &mut CodeGenContext<'ctx, 'a>,
        generator: &mut dyn CodeGenerator,
    ) -> PyResult<Option<BasicValueEnum<'ctx>>> {
        let ty_id: u64 = self
            .helper
            .id_fn
            .call1(py, (self.helper.type_fn.call1(py, (obj,))?,))?
            .extract(py)?;
        let id: u64 = self.helper.id_fn.call1(py, (obj,))?.extract(py)?;
        if ty_id == self.primitive_ids.int || ty_id == self.primitive_ids.int32 {
            let val: i32 = obj.extract()?;
            self.id_to_primitive
                .write()
                .insert(id, PrimitiveValue::I32(val));
            Ok(Some(ctx.ctx.i32_type().const_int(val as u64, false).into()))
        } else if ty_id == self.primitive_ids.int64 {
            let val: i64 = obj.extract()?;
            self.id_to_primitive
                .write()
                .insert(id, PrimitiveValue::I64(val));
            Ok(Some(ctx.ctx.i64_type().const_int(val as u64, false).into()))
        } else if ty_id == self.primitive_ids.bool {
            let val: bool = obj.extract()?;
            self.id_to_primitive
                .write()
                .insert(id, PrimitiveValue::Bool(val));
            Ok(Some(
                ctx.ctx.bool_type().const_int(val as u64, false).into(),
            ))
        } else if ty_id == self.primitive_ids.float {
            let val: f64 = obj.extract()?;
            self.id_to_primitive
                .write()
                .insert(id, PrimitiveValue::F64(val));
            Ok(Some(ctx.ctx.f64_type().const_float(val).into()))
        } else if ty_id == self.primitive_ids.list {
            let id_str = id.to_string();

            if let Some(global) = ctx.module.get_global(&id_str) {
                return Ok(Some(global.as_pointer_value().into()));
            }

            let len: usize = self.helper.len_fn.call1(py, (obj,))?.extract(py)?;
            let ty = if len == 0 {
                ctx.primitives.int32
            } else {
                self.get_list_elem_type(
                    py,
                    obj,
                    len,
                    &mut ctx.unifier,
                    &ctx.top_level.definitions.read(),
                    &ctx.primitives,
                )?
                .unwrap()
            };
            let ty = ctx.get_llvm_type(generator, ty);
            let size_t = generator.get_size_type(ctx.ctx);
            let arr_ty = ctx.ctx.struct_type(
                &[ty.ptr_type(AddressSpace::Generic).into(), size_t.into()],
                false,
            );

            {
                if self.global_value_ids.read().contains(&id) {
                    let global = ctx.module.get_global(&id_str).unwrap_or_else(|| {
                        ctx.module
                            .add_global(arr_ty, Some(AddressSpace::Generic), &id_str)
                    });
                    return Ok(Some(global.as_pointer_value().into()));
                } else {
                    self.global_value_ids.write().insert(id);
                }
            }

            let arr: Result<Option<Vec<_>>, _> = (0..len)
                .map(|i| {
                    obj.get_item(i)
                        .and_then(|elem| self.get_obj_value(py, elem, ctx, generator))
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
                arr_global
                    .as_pointer_value()
                    .const_cast(ty.ptr_type(AddressSpace::Generic))
                    .into(),
                size_t.const_int(len as u64, false).into(),
            ]);

            let global = ctx
                .module
                .add_global(arr_ty, Some(AddressSpace::Generic), &id_str);
            global.set_initializer(&val);

            Ok(Some(global.as_pointer_value().into()))
        } else if ty_id == self.primitive_ids.tuple {
            let id_str = id.to_string();

            if let Some(global) = ctx.module.get_global(&id_str) {
                return Ok(Some(global.as_pointer_value().into()));
            }

            let elements: &PyTuple = obj.cast_as()?;
            let types: Result<Option<Vec<_>>, _> = elements
                .iter()
                .map(|elem| {
                    self.get_obj_type(
                        py,
                        elem,
                        &mut ctx.unifier,
                        &ctx.top_level.definitions.read(),
                        &ctx.primitives,
                    )
                    .map(|ty| ty.map(|ty| ctx.get_llvm_type(generator, ty)))
                })
                .collect();
            let types = types?.unwrap();
            let ty = ctx.ctx.struct_type(&types, false);

            {
                if self.global_value_ids.read().contains(&id) {
                    let global = ctx.module.get_global(&id_str).unwrap_or_else(|| {
                        ctx.module
                            .add_global(ty, Some(AddressSpace::Generic), &id_str)
                    });
                    return Ok(Some(global.as_pointer_value().into()));
                } else {
                    self.global_value_ids.write().insert(id);
                }
            }

            let val: Result<Option<Vec<_>>, _> = elements
                .iter()
                .map(|elem| self.get_obj_value(py, elem, ctx, generator))
                .collect();
            let val = val?.unwrap();
            let val = ctx.ctx.const_struct(&val, false);
            let global = ctx
                .module
                .add_global(ty, Some(AddressSpace::Generic), &id_str);
            global.set_initializer(&val);
            Ok(Some(global.as_pointer_value().into()))
        } else {
            let id_str = id.to_string();

            if let Some(global) = ctx.module.get_global(&id_str) {
                return Ok(Some(global.as_pointer_value().into()));
            }

            let top_level_defs = ctx.top_level.definitions.read();
            let ty = self
                .get_obj_type(py, obj, &mut ctx.unifier, &top_level_defs, &ctx.primitives)?
                .unwrap();
            let ty = ctx
                .get_llvm_type(generator, ty)
                .into_pointer_type()
                .get_element_type()
                .into_struct_type()
                .as_basic_type_enum();

            {
                if self.global_value_ids.read().contains(&id) {
                    let global = ctx.module.get_global(&id_str).unwrap_or_else(|| {
                        ctx.module
                            .add_global(ty, Some(AddressSpace::Generic), &id_str)
                    });
                    return Ok(Some(global.as_pointer_value().into()));
                } else {
                    self.global_value_ids.write().insert(id);
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
                    .map(|(name, _, _)| {
                        self.get_obj_value(py, obj.getattr(&name.to_string())?, ctx, generator)
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

    fn get_default_param_obj_value(
        &self,
        py: Python,
        obj: &PyAny,
    ) -> PyResult<Result<SymbolValue, String>> {
        let ty_id: u64 = self
            .helper
            .id_fn
            .call1(py, (self.helper.type_fn.call1(py, (obj,))?,))?
            .extract(py)?;
        Ok(
            if ty_id == self.primitive_ids.int || ty_id == self.primitive_ids.int32 {
                let val: i32 = obj.extract()?;
                Ok(SymbolValue::I32(val))
            } else if ty_id == self.primitive_ids.int64 {
                let val: i64 = obj.extract()?;
                Ok(SymbolValue::I64(val))
            } else if ty_id == self.primitive_ids.bool {
                let val: bool = obj.extract()?;
                Ok(SymbolValue::Bool(val))
            } else if ty_id == self.primitive_ids.float {
                let val: f64 = obj.extract()?;
                Ok(SymbolValue::Double(val))
            } else if ty_id == self.primitive_ids.tuple {
                let elements: &PyTuple = obj.cast_as()?;
                let elements: Result<Result<Vec<_>, String>, _> = elements
                    .iter()
                    .map(|elem| self.get_default_param_obj_value(py, elem))
                    .collect();
                let elements = match elements? {
                    Ok(el) => el,
                    Err(err) => return Ok(Err(err)),
                };
                Ok(SymbolValue::Tuple(elements))
            } else {
                Err("only primitives values and tuple can be default parameter value".into())
            },
        )
    }
}

impl SymbolResolver for Resolver {
    fn get_default_param_value(&self, expr: &ast::Expr) -> Option<SymbolValue> {
        match &expr.node {
            ast::ExprKind::Name { id, .. } => {
                Python::with_gil(|py| -> PyResult<Option<SymbolValue>> {
                    let obj: &PyAny = self.0.module.extract(py)?;
                    let members: &PyDict = obj.getattr("__dict__").unwrap().cast_as().unwrap();
                    let mut sym_value = None;
                    for (key, val) in members.iter() {
                        let key: &str = key.extract()?;
                        if key == id.to_string() {
                            sym_value = Some(
                                self.0
                                    .get_default_param_obj_value(py, val)
                                    .unwrap()
                                    .unwrap(),
                            );
                            break;
                        }
                    }
                    Ok(sym_value)
                })
                .unwrap()
            }
            _ => unimplemented!("other type of expr not supported at {}", expr.location),
        }
    }

    fn get_symbol_type(
        &self,
        unifier: &mut Unifier,
        defs: &[Arc<RwLock<TopLevelDef>>],
        primitives: &PrimitiveStore,
        str: StrRef,
    ) -> Option<Type> {
        {
            let id_to_type = self.0.id_to_type.read();
            id_to_type.get(&str).cloned()
        }
        .or_else(|| {
            let py_id = self.0.name_to_pyid.get(&str);
            let result = py_id.and_then(|id| {
                {
                    let pyid_to_type = self.0.pyid_to_type.read();
                    pyid_to_type.get(id).copied()
                }
                .or_else(|| {
                    let result = Python::with_gil(|py| -> PyResult<Option<Type>> {
                        let obj: &PyAny = self.0.module.extract(py)?;
                        let mut sym_ty = None;
                        let members: &PyDict = obj.getattr("__dict__").unwrap().cast_as().unwrap();
                        for (key, val) in members.iter() {
                            let key: &str = key.extract()?;
                            if key == str.to_string() {
                                sym_ty = self.0.get_obj_type(py, val, unifier, defs, primitives)?;
                                break;
                            }
                        }
                        Ok(sym_ty)
                    })
                    .unwrap();
                    if let Some(result) = result {
                        self.0.pyid_to_type.write().insert(*id, result);
                    }
                    result
                })
            });
            if let Some(result) = &result {
                self.0.id_to_type.write().insert(str, *result);
            }
            result
        })
    }

    fn get_symbol_value<'ctx, 'a>(
        &self,
        id: StrRef,
        _: &mut CodeGenContext<'ctx, 'a>,
    ) -> Option<ValueEnum<'ctx>> {
        let sym_value = {
            let id_to_val = self.0.id_to_pyval.read();
            id_to_val.get(&id).cloned()
        }
        .or_else(|| {
            Python::with_gil(|py| -> PyResult<Option<(u64, PyObject)>> {
                let obj: &PyAny = self.0.module.extract(py)?;
                let mut sym_value: Option<(u64, PyObject)> = None;
                let members: &PyDict = obj.getattr("__dict__").unwrap().cast_as().unwrap();
                for (key, val) in members.iter() {
                    let key: &str = key.extract()?;
                    if key == id.to_string() {
                        let id = self.0.helper.id_fn.call1(py, (val,))?.extract(py)?;
                        sym_value = Some((id, val.extract()?));
                        break;
                    }
                }
                if let Some((pyid, val)) = &sym_value {
                    self.0.id_to_pyval.write().insert(id, (*pyid, val.clone()));
                }
                Ok(sym_value)
            })
            .unwrap()
        });
        sym_value.map(|(id, v)| {
            ValueEnum::Static(Arc::new(PythonValue {
                id,
                value: v,
                resolver: self.0.clone(),
            }))
        })
    }

    fn get_symbol_location(&self, _: StrRef) -> Option<Location> {
        unimplemented!()
    }

    fn get_identifier_def(&self, id: StrRef) -> Option<DefinitionId> {
        {
            let id_to_def = self.0.id_to_def.read();
            id_to_def.get(&id).cloned()
        }
        .or_else(|| {
            let py_id = self.0.name_to_pyid.get(&id);
            let result = py_id.and_then(|id| self.0.pyid_to_def.read().get(id).copied());
            if let Some(result) = &result {
                self.0.id_to_def.write().insert(id, *result);
            }
            result
        })
    }
}
