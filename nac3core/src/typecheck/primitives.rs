use super::typedef::{TypeEnum::*, *};
use super::context::*;
use std::collections::HashMap;

pub const TUPLE_TYPE: ParamId = ParamId(0);
pub const LIST_TYPE: ParamId = ParamId(1);

pub const BOOL_TYPE: PrimitiveId = PrimitiveId(0);
pub const INT32_TYPE: PrimitiveId = PrimitiveId(1);
pub const INT64_TYPE: PrimitiveId = PrimitiveId(2);
pub const FLOAT_TYPE: PrimitiveId = PrimitiveId(3);

fn impl_math(def: &mut TypeDef, ty: &Type) {
    let result = Some(ty.clone());
    let fun = FnDef {
        args: vec![ty.clone()],
        result: result.clone(),
    };
    def.methods.insert("__add__", fun.clone());
    def.methods.insert("__sub__", fun.clone());
    def.methods.insert("__mul__", fun.clone());
    def.methods.insert(
        "__neg__",
        FnDef {
            args: vec![],
            result,
        },
    );
    def.methods.insert(
        "__truediv__",
        FnDef {
            args: vec![ty.clone()],
            result: Some(PrimitiveType(FLOAT_TYPE).into()),
        },
    );
    def.methods.insert("__floordiv__", fun.clone());
    def.methods.insert("__mod__", fun.clone());
    def.methods.insert("__pow__", fun);
}

fn impl_bits(def: &mut TypeDef, ty: &Type) {
    let result = Some(ty.clone());
    let fun = FnDef {
        args: vec![PrimitiveType(INT32_TYPE).into()],
        result,
    };

    def.methods.insert("__lshift__", fun.clone());
    def.methods.insert("__rshift__", fun);
    def.methods.insert(
        "__xor__",
        FnDef {
            args: vec![ty.clone()],
            result: Some(ty.clone()),
        },
    );
}

fn impl_eq(def: &mut TypeDef, ty: &Type) {
    let fun = FnDef {
        args: vec![ty.clone()],
        result: Some(PrimitiveType(BOOL_TYPE).into()),
    };

    def.methods.insert("__eq__", fun.clone());
    def.methods.insert("__ne__", fun);
}

fn impl_order(def: &mut TypeDef, ty: &Type) {
    let fun = FnDef {
        args: vec![ty.clone()],
        result: Some(PrimitiveType(BOOL_TYPE).into()),
    };

    def.methods.insert("__lt__", fun.clone());
    def.methods.insert("__gt__", fun.clone());
    def.methods.insert("__le__", fun.clone());
    def.methods.insert("__ge__", fun);
}

pub fn basic_ctx() -> TopLevelContext<'static> {
    let primitives = [
        TypeDef {
            name: "bool",
            fields: HashMap::new(),
            methods: HashMap::new(),
        },
        TypeDef {
            name: "int32",
            fields: HashMap::new(),
            methods: HashMap::new(),
        },
        TypeDef {
            name: "int64",
            fields: HashMap::new(),
            methods: HashMap::new(),
        },
        TypeDef {
            name: "float",
            fields: HashMap::new(),
            methods: HashMap::new(),
        },
    ]
    .to_vec();
    let mut ctx = TopLevelContext::new(primitives);

    let b = ctx.get_primitive(BOOL_TYPE);
    let b_def = ctx.get_primitive_def_mut(BOOL_TYPE);
    impl_eq(b_def, &b);
    let int32 = ctx.get_primitive(INT32_TYPE);
    let int32_def = ctx.get_primitive_def_mut(INT32_TYPE);
    impl_math(int32_def, &int32);
    impl_bits(int32_def, &int32);
    impl_order(int32_def, &int32);
    impl_eq(int32_def, &int32);
    let int64 = ctx.get_primitive(INT64_TYPE);
    let int64_def = ctx.get_primitive_def_mut(INT64_TYPE);
    impl_math(int64_def, &int64);
    impl_bits(int64_def, &int64);
    impl_order(int64_def, &int64);
    impl_eq(int64_def, &int64);
    let float = ctx.get_primitive(FLOAT_TYPE);
    let float_def = ctx.get_primitive_def_mut(FLOAT_TYPE);
    impl_math(float_def, &float);
    impl_order(float_def, &float);
    impl_eq(float_def, &float);

    let t = ctx.add_variable_private(VarDef {
        name: "T",
        bound: vec![],
    });

    ctx.add_parametric(ParametricDef {
        base: TypeDef {
            name: "tuple",
            fields: HashMap::new(),
            methods: HashMap::new(),
        },
        // we have nothing for tuple, so no param def
        params: vec![],
    });

    ctx.add_parametric(ParametricDef {
        base: TypeDef {
            name: "list",
            fields: HashMap::new(),
            methods: HashMap::new(),
        },
        params: vec![t],
    });

    let i = ctx.add_variable_private(VarDef {
        name: "I",
        bound: vec![
            PrimitiveType(INT32_TYPE).into(),
            PrimitiveType(INT64_TYPE).into(),
            PrimitiveType(FLOAT_TYPE).into(),
        ],
    });
    let args = vec![TypeVariable(i).into()];
    ctx.add_fn(
        "int32",
        FnDef {
            args: args.clone(),
            result: Some(PrimitiveType(INT32_TYPE).into()),
        },
    );
    ctx.add_fn(
        "int64",
        FnDef {
            args: args.clone(),
            result: Some(PrimitiveType(INT64_TYPE).into()),
        },
    );
    ctx.add_fn(
        "float",
        FnDef {
            args,
            result: Some(PrimitiveType(FLOAT_TYPE).into()),
        },
    );

    ctx
}
