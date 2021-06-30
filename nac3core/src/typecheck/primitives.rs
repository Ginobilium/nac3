use super::context::*;
use super::typedef::{TypeEnum::*, *};
use std::collections::HashMap;
use std::rc::Rc;

pub const FUNC_TYPE: TypeId = TypeId(0);
pub const TUPLE_TYPE: TypeId = TypeId(1);
pub const LIST_TYPE: TypeId = TypeId(2);
pub const VIRTUAL_TYPE: TypeId = TypeId(3);
pub const NONE_TYPE: TypeId = TypeId(4);

pub const BOOL_TYPE: TypeId = TypeId(5);
pub const INT32_TYPE: TypeId = TypeId(6);
pub const INT64_TYPE: TypeId = TypeId(7);
pub const FLOAT_TYPE: TypeId = TypeId(8);

fn primitive(base: BaseDef) -> TypeDef {
    TypeDef {
        base,
        parents: vec![],
        params: vec![],
    }
}

pub fn get_fn(from: Type, to: Type) -> Type {
    Rc::new(ClassType(FUNC_TYPE, vec![from, to]))
}

pub fn get_tuple(types: &[Type]) -> Type {
    Rc::new(ClassType(TUPLE_TYPE, types.to_vec()))
}

pub fn get_list(t: Type) -> Type {
    Rc::new(ClassType(LIST_TYPE, vec![t]))
}

pub fn get_virtual(t: Type) -> Type {
    Rc::new(ClassType(VIRTUAL_TYPE, vec![t]))
}

pub fn get_none() -> Type {
    Rc::new(ClassType(NONE_TYPE, Vec::new()))
}

pub fn get_bool() -> Type {
    Rc::new(ClassType(BOOL_TYPE, Vec::new()))
}
pub fn get_int32() -> Type {
    Rc::new(ClassType(INT32_TYPE, Vec::new()))
}

pub fn get_int64() -> Type {
    Rc::new(ClassType(INT64_TYPE, Vec::new()))
}

pub fn get_float() -> Type {
    Rc::new(ClassType(FLOAT_TYPE, Vec::new()))
}

pub fn get_var(id: VariableId) -> Type {
    Rc::new(TypeVariable(id))
}

fn impl_math(def: &mut BaseDef, ty: &Type) {
    let fun = get_fn(ty.clone(), ty.clone());
    def.fields.insert("__add__", fun.clone());
    def.fields.insert("__sub__", fun.clone());
    def.fields.insert("__mul__", fun.clone());
    def.fields.insert("__neg__", get_fn(get_none(), ty.clone()));
    def.fields
        .insert("__truediv__", get_fn(ty.clone(), get_float()));
    def.fields.insert("__floordiv__", fun.clone());
    def.fields.insert("__mod__", fun.clone());
    def.fields.insert("__pow__", fun);
}

fn impl_bits(def: &mut BaseDef, ty: &Type) {
    let fun = get_fn(get_int32(), ty.clone());

    def.fields.insert("__lshift__", fun.clone());
    def.fields.insert("__rshift__", fun);
    def.fields.insert("__xor__", get_fn(ty.clone(), ty.clone()));
}

fn impl_eq(def: &mut BaseDef, ty: &Type) {
    let fun = get_fn(ty.clone(), get_bool());

    def.fields.insert("__eq__", fun.clone());
    def.fields.insert("__ne__", fun);
}

fn impl_order(def: &mut BaseDef, ty: &Type) {
    let fun = get_fn(ty.clone(), get_bool());

    def.fields.insert("__lt__", fun.clone());
    def.fields.insert("__gt__", fun.clone());
    def.fields.insert("__le__", fun.clone());
    def.fields.insert("__ge__", fun);
}

pub fn basic_ctx() -> GlobalContext<'static> {
    let mut ctx = GlobalContext::new(vec![
        primitive(BaseDef {
            name: "function",
            fields: HashMap::new(),
        }),
        primitive(BaseDef {
            name: "tuple",
            fields: HashMap::new(),
        }),
        primitive(BaseDef {
            name: "list",
            fields: HashMap::new(),
        }),
        primitive(BaseDef {
            name: "virtual",
            fields: HashMap::new(),
        }),
        primitive(BaseDef {
            name: "None",
            fields: HashMap::new(),
        }),
        primitive(BaseDef {
            name: "bool",
            fields: HashMap::new(),
        }),
        primitive(BaseDef {
            name: "int32",
            fields: HashMap::new(),
        }),
        primitive(BaseDef {
            name: "int64",
            fields: HashMap::new(),
        }),
        primitive(BaseDef {
            name: "float",
            fields: HashMap::new(),
        }),
    ]);

    let t = ctx.add_variable(VarDef {
        name: Some("T"),
        bound: vec![],
    });
    ctx.get_type_def_mut(LIST_TYPE).params.push(t);

    let b_def = ctx.get_type_def_mut(BOOL_TYPE);
    impl_eq(&mut b_def.base, &get_bool());
    let int32 = get_int32();
    let int32_def = &mut ctx.get_type_def_mut(INT32_TYPE).base;
    impl_math(int32_def, &int32);
    impl_bits(int32_def, &int32);
    impl_order(int32_def, &int32);
    impl_eq(int32_def, &int32);
    let int64 = get_int64();
    let int64_def = &mut ctx.get_type_def_mut(INT64_TYPE).base;
    impl_math(int64_def, &int64);
    impl_bits(int64_def, &int64);
    impl_order(int64_def, &int64);
    impl_eq(int64_def, &int64);
    let float = get_float();
    let float_def = &mut ctx.get_type_def_mut(FLOAT_TYPE).base;
    impl_math(float_def, &float);
    impl_order(float_def, &float);
    impl_eq(float_def, &float);

    ctx
}
