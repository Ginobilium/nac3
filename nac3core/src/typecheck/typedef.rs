use std::collections::HashMap;
use std::collections::HashSet;
use std::rc::Rc;

#[derive(PartialEq, Eq, Copy, Clone, Hash, Debug)]
pub struct VariableId(pub(crate) usize);

#[derive(PartialEq, Eq, Copy, Clone, Hash, Debug)]
pub struct TypeId(pub(crate) usize);

#[derive(PartialEq, Eq, Clone, Hash, Debug)]
pub enum TypeEnum {
    ClassType(TypeId, Vec<Rc<TypeEnum>>),
    TypeVariable(VariableId),
}

pub type Type = Rc<TypeEnum>;

#[derive(Clone)]
pub struct BaseDef<'a> {
    pub name: &'a str,
    pub fields: HashMap<&'a str, Type>,
}

#[derive(Clone)]
pub struct TypeDef<'a> {
    pub base: BaseDef<'a>,
    pub parents: Vec<TypeId>,
    pub params: Vec<VariableId>,
}

#[derive(Clone)]
pub struct VarDef<'a> {
    pub name: Option<&'a str>,
    pub bound: Vec<Type>,
}

impl TypeEnum {
    pub fn get_vars(&self, vars: &mut HashSet<VariableId>) {
        match self {
            TypeEnum::TypeVariable(id) => {
                vars.insert(*id);
            }
            TypeEnum::ClassType(_, params) => {
                for t in params.iter() {
                    t.get_vars(vars)
                }
            }
        }
    }

    pub fn subst(&self, map: &HashMap<VariableId, Type>) -> Type {
        match self {
            TypeEnum::TypeVariable(id) => map
                .get(id)
                .cloned()
                .unwrap_or_else(|| Rc::new(self.clone())),
            TypeEnum::ClassType(id, params) => Rc::new(TypeEnum::ClassType(
                *id,
                params.iter().map(|t| t.subst(map)).collect(),
            )),
        }
    }
}
