use ena::unify::{InPlaceUnificationTable, NoError, UnifyKey, UnifyValue};
use generational_arena::{Arena, Index};
use std::cell::RefCell;
use std::collections::HashMap;

// Order:
// TVar
// |--> TSeq
// |   |--> TTuple
// |   `--> TList
// |--> TRecord
// |   |--> TObj
// |   `--> TVirtual
// `--> TCall
//     `--> TFunc

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
struct Type(u32);

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct TypeIndex(Index);

impl UnifyValue for TypeIndex {
    type Error = NoError;
    fn unify_values(_: &Self, value2: &Self) -> Result<Self, Self::Error> {
        // WARN: depends on the implementation details of ena.
        // We do not use this to do unification, instead we perform unification and assign the type
        // by `union_value(key, new_value)`, which set the value as `unify_values(key.value, new_value)`.
        // So, we need to return the right one.
        Ok(*value2)
    }
}

impl UnifyKey for Type {
    type Value = TypeIndex;
    fn index(&self) -> u32 {
        self.0
    }
    fn from_index(u: u32) -> Self {
        Type(u)
    }
    fn tag() -> &'static str {
        "TypeKey"
    }
}

type VarMapping = HashMap<u32, Type>;

struct Call {
    posargs: Vec<Type>,
    kwargs: HashMap<String, Type>,
    ret: Type,
    fn_id: usize,
}

struct FuncArg {
    name: String,
    ty: Type,
    is_optional: bool,
}

enum TypeEnum {
    TVar {
        // TODO: upper/lower bound
        id: u32,
    },
    TSeq {
        index: HashMap<u32, Type>,
    },
    TTuple {
        index: HashMap<u32, Type>,
    },
    TList {
        ty: Type,
    },
    TRecord {
        fields: HashMap<String, Type>,
    },
    TObj {
        obj_id: usize,
        instantiation: VarMapping,
    },
    TVirtual {
        obj_id: usize,
        instantiation: VarMapping,
    },
    TCall {
        calls: Vec<Call>,
    },
    TFunc {
        args: Vec<FuncArg>,
        ret: Type,
        instantiation: VarMapping,
    },
}

impl TypeEnum {
    fn get_int(&self) -> i32 {
        match self {
            TypeEnum::TVar { .. } => 1,
            TypeEnum::TSeq { .. } => 5,
            TypeEnum::TTuple { .. } => 10,
            TypeEnum::TList { .. } => 15,
            TypeEnum::TRecord { .. } => 7,
            TypeEnum::TObj { .. } => 14,
            TypeEnum::TVirtual { .. } => 21,
            TypeEnum::TCall { .. } => 11,
            TypeEnum::TFunc { .. } => 22,
        }
    }

    // e.g. List <: Var
    pub fn kind_le(&self, other: &TypeEnum) -> bool {
        let a = self.get_int();
        let b = other.get_int();
        (a % b) == 0
    }
}

struct ObjDef {
    name: String,
    fields: HashMap<String, Type>,
}

struct Unifier {
    unification_table: RefCell<InPlaceUnificationTable<Type>>,
    type_arena: RefCell<Arena<TypeEnum>>,
    obj_def_table: Vec<ObjDef>,
}

impl Unifier {
    fn unify(&self, a: Type, b: Type) {
        let (i_a, i_b) = {
            let mut table = self.unification_table.borrow_mut();
            (table.probe_value(a), table.probe_value(b))
        };

        if i_a == i_b {
            return;
        }

        let arena = self.type_arena.borrow();
        let mut ty_a = arena.get(i_a.0).unwrap();
        let mut ty_b = arena.get(i_b.0).unwrap();

        // simplify our pattern matching...
        if ty_a.kind_le(ty_b) {
            std::mem::swap(&mut ty_a, &mut ty_b);
        }

        // TODO: type variables bound check
        match (ty_a, ty_b) {
            (TypeEnum::TVar { .. }, TypeEnum::TVar { .. }) => {
                self.unification_table.borrow_mut().union(a, b);
                let old = if self.unification_table.borrow_mut().find(a) == a {
                    i_b
                } else {
                    i_a
                }
                .0;
                self.type_arena.borrow_mut().remove(old);
            }
            (TypeEnum::TVar { .. }, _) => {
                let mut table = self.unification_table.borrow_mut();
                table.union(a, b);
                table.union_value(a, i_b);
                // TODO: occur check...
                self.type_arena.borrow_mut().remove(i_a.0);
            }
            (TypeEnum::TSeq { .. }, TypeEnum::TSeq { .. }) => {
                let is_a = {
                    let mut table = self.unification_table.borrow_mut();
                    table.union(a, b);
                    table.find(a) == a
                };
                // fighting with the borrow checker...
                // we have to manually drop this before we call borrow_mut
                std::mem::drop(arena);
                let (mut new, old) = {
                    // the mutable arena would be dropped before calling unify later
                    let mut arena = self.type_arena.borrow_mut();
                    let (ty_a, ty_b) = arena.get2_mut(i_a.0, i_b.0);
                    let index1 = if let Some(TypeEnum::TSeq { index }) = ty_a {
                        std::mem::take(index)
                    } else {
                        unreachable!()
                    };
                    let index2 = if let Some(TypeEnum::TSeq { index }) = ty_b {
                        std::mem::take(index)
                    } else {
                        unreachable!()
                    };
                    if is_a {
                        arena.remove(i_b.0);
                        (index1, index2)
                    } else {
                        arena.remove(i_a.0);
                        (index2, index1)
                    }
                };
                for (key, value) in old.iter() {
                    if let Some(ty) = new.get(key) {
                        self.unify(*ty, *value);
                    } else {
                        new.insert(*key, *value);
                    }
                }
                // put it back
                let index = if is_a { i_a } else { i_b }.0;
                if let Some(TypeEnum::TSeq { index }) = self.type_arena.borrow_mut().get_mut(index) {
                    *index = new;
                } else {
                    unreachable!()
                }
            }
            _ => unimplemented!(),
        }
    }
}
