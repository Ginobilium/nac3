use std::collections::HashMap;
use std::rc::Rc;

#[derive(PartialEq, Eq, Copy, Clone, Hash, Debug)]
pub struct PrimitiveId(pub(crate) usize);

#[derive(PartialEq, Eq, Copy, Clone, Hash, Debug)]
pub struct ClassId(pub(crate) usize);

#[derive(PartialEq, Eq, Copy, Clone, Hash, Debug)]
pub struct ParamId(pub(crate) usize);

#[derive(PartialEq, Eq, Copy, Clone, Hash, Debug)]
pub struct VariableId(pub(crate) usize);

#[derive(PartialEq, Eq, Clone, Hash, Debug)]
pub enum TypeEnum {
    BotType,
    SelfType,
    PrimitiveType(PrimitiveId),
    ClassType(ClassId),
    VirtualClassType(ClassId),
    ParametricType(ParamId, Vec<Rc<TypeEnum>>),
    TypeVariable(VariableId),
}

pub type Type = Rc<TypeEnum>;

#[derive(Clone)]
pub struct FnDef {
    // we assume methods first argument to be SelfType,
    // so the first argument is not contained here
    pub args: Vec<Type>,
    pub result: Option<Type>,
}

#[derive(Clone)]
pub struct TypeDef<'a> {
    pub name: &'a str,
    pub fields: HashMap<&'a str, Type>,
    pub methods: HashMap<&'a str, FnDef>,
}

#[derive(Clone)]
pub struct ClassDef<'a> {
    pub base: TypeDef<'a>,
    pub parents: Vec<ClassId>,
}

#[derive(Clone)]
pub struct ParametricDef<'a> {
    pub base: TypeDef<'a>,
    pub params: Vec<VariableId>,
}

#[derive(Clone)]
pub struct VarDef<'a> {
    pub name: &'a str,
    pub bound: Vec<Type>,
}
