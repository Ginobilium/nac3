use ena::unify::{InPlaceUnificationTable, NoError, UnifyKey, UnifyValue};
use generational_arena::{Arena, Index};
use std::borrow::{BorrowMut, Cow};
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::mem::swap;

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
        // We do not use this to do unification, instead we perform unification
        // and assign the type by `union_value(key, new_value)`, which set the
        // value as `unify_values(key.value, new_value)`. So, we need to return
        // the right one.
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
        "TypeID"
    }
}

type Mapping<K, V = Type> = BTreeMap<K, V>;
type VarMap = Mapping<u32>;

struct Call {
    posargs: Vec<Type>,
    kwargs: BTreeMap<String, Type>,
    ret: Type,
    fn_id: usize,
}

struct FuncArg {
    name: String,
    ty: Type,
    is_optional: bool,
}

// We use a lot of `RefCell`s here as we want to simplify our code.
// Pattern:
// 1. Take the complex data structure out
// 2. Drop the arena (required before unification)
// 3. Do unification for each type in the data structure
// 4. Put the complex data structure back...
enum TypeEnum {
    TVar {
        // TODO: upper/lower bound
        id: u32,
    },
    TSeq {
        map: RefCell<VarMap>,
    },
    TTuple {
        ty: RefCell<Vec<Type>>,
    },
    TList {
        ty: Type,
    },
    TRecord {
        fields: RefCell<Mapping<String>>,
    },
    TObj {
        obj_id: usize,
        fields: RefCell<Mapping<String>>,
        params: RefCell<VarMap>,
    },
    TVirtual {
        ty: Type,
    },
    TCall {
        calls: RefCell<Vec<Call>>,
    },
    TFunc {
        args: RefCell<Vec<FuncArg>>,
        ret: Type,
        params: RefCell<VarMap>,
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

    pub fn get_kind_name(&self) -> &'static str {
        // this function is for debugging only...
        // a proper to_str implementation requires the context
        match self {
            TypeEnum::TVar { .. } => "TVar",
            TypeEnum::TSeq { .. } => "TSeq",
            TypeEnum::TTuple { .. } => "TTuple",
            TypeEnum::TList { .. } => "TList",
            TypeEnum::TRecord { .. } => "TRecord",
            TypeEnum::TObj { .. } => "TObj",
            TypeEnum::TVirtual { .. } => "TVirtual",
            TypeEnum::TCall { .. } => "TCall",
            TypeEnum::TFunc { .. } => "TFunc",
        }
    }
}

struct ObjDef {
    name: String,
    fields: Mapping<String>,
}

struct Unifier {
    unification_table: RefCell<InPlaceUnificationTable<Type>>,
    type_arena: RefCell<Arena<TypeEnum>>,
    obj_def_table: Vec<ObjDef>,
}

impl Unifier {
    fn unify(&self, a: Type, b: Type) -> Result<(), String> {
        let (mut i_a, mut i_b) = {
            let mut table = self.unification_table.borrow_mut();
            (table.probe_value(a), table.probe_value(b))
        };

        if i_a == i_b {
            return Ok(());
        }

        let arena = self.type_arena.borrow();
        let mut ty_a = arena.get(i_a.0).unwrap();
        let mut ty_b = arena.get(i_b.0).unwrap();

        // simplify our pattern matching...
        if ty_a.kind_le(ty_b) {
            swap(&mut i_a, &mut i_b);
            swap(&mut ty_a, &mut ty_b);
        }

        match ty_a {
            TypeEnum::TVar { .. } => {
                match ty_b {
                    TypeEnum::TVar { .. } => {
                        // TODO: type variables bound check
                        let old = {
                            let mut table = self.unification_table.borrow_mut();
                            table.union(a, b);
                            if table.find(a) == a {
                                i_b
                            } else {
                                i_a
                            }
                        }
                        .0;
                        drop(arena);
                        self.type_arena.borrow_mut().remove(old);
                    }
                    _ => {
                        // TODO: type variables bound check and occur check
                        drop(arena);
                        self.set_a_to_b(a, b);
                    }
                }
            }
            TypeEnum::TSeq { map: map1 } => {
                match ty_b {
                    TypeEnum::TSeq { map: map2 } => {
                        // we get the tables out first.
                        // unification requires mutable access to the underlying
                        // structs, so we have to manaully drop the arena first,
                        // do the unification, and then get a mutable reference
                        // and put them back...
                        let mut map1 = map1.take();
                        let map2 = map2.take();
                        drop(arena);
                        self.set_a_to_b(a, b);
                        // unify them to map1
                        for (key, value) in map2.iter() {
                            if let Some(ty) = map1.get(key) {
                                self.unify(*ty, *value)?;
                            } else {
                                map1.insert(*key, *value);
                            }
                        }
                        if let Some(TypeEnum::TSeq { map: mapping }) =
                            self.type_arena.borrow().get(i_b.0)
                        {
                            *mapping.borrow_mut() = map1;
                        } else {
                            unreachable!()
                        }
                    }
                    TypeEnum::TTuple { ty: types } => {
                        let map = map1.take();
                        let types = types.take();
                        drop(arena);
                        self.set_a_to_b(a, b);
                        let len = types.len() as u32;
                        for (k, v) in map.iter() {
                            if *k >= len {
                                return Err(format!(
                                    "Tuple index out of range. (Length: {}, Index: {})",
                                    types.len(),
                                    k
                                ));
                            }
                            self.unify(*v, types[*k as usize])?;
                        }

                        if let Some(TypeEnum::TTuple { ty }) = self.type_arena.borrow().get(i_b.0) {
                            *ty.borrow_mut() = types;
                        } else {
                            unreachable!()
                        }
                    }
                    TypeEnum::TList { ty } => {
                        let map = map1.take();
                        let ty = *ty;
                        drop(arena);
                        self.set_a_to_b(a, b);
                        for v in map.values() {
                            self.unify(*v, ty)?;
                        }
                    }
                    _ => {
                        return self.report_kind_error(ty_a, ty_b);
                    }
                }
            }
            TypeEnum::TTuple { ty: ty1 } => {
                if let TypeEnum::TTuple { ty: ty2 } = ty_b {
                    let ty1 = ty1.take();
                    let ty2 = ty2.take();
                    if ty1.len() != ty2.len() {
                        return Err(format!(
                            "Cannot unify tuples with length {} and {}",
                            ty1.len(),
                            ty2.len()
                        ));
                    }
                    drop(arena);
                    self.set_a_to_b(a, b);
                    for (a, b) in ty1.iter().zip(ty2.iter()) {
                        self.unify(*a, *b)?;
                    }
                    if let Some(TypeEnum::TTuple { ty }) =
                        self.type_arena.borrow_mut().get_mut(i_b.0)
                    {
                        *ty.borrow_mut().get_mut() = ty1;
                    } else {
                        unreachable!()
                    }
                } else {
                    return self.report_kind_error(ty_a, ty_b);
                }
            }
            TypeEnum::TList { ty: ty1 } => {
                if let TypeEnum::TList { ty: ty2 } = ty_b {
                    let ty1 = *ty1;
                    let ty2 = *ty2;
                    drop(arena);
                    self.set_a_to_b(a, b);
                    self.unify(ty1, ty2)?;
                } else {
                    return self.report_kind_error(ty_a, ty_b);
                }
            }
            TypeEnum::TRecord { fields: fields1 } => {
                match ty_b {
                    TypeEnum::TRecord { fields: fields2 } => {
                        let mut fields1 = fields1.take();
                        let fields2 = fields2.take();
                        drop(arena);
                        self.set_a_to_b(a, b);
                        for (key, value) in fields2.iter() {
                            if let Some(ty) = fields1.get(key) {
                                self.unify(*ty, *value)?;
                            } else {
                                fields1.insert(key.clone(), *value);
                            }
                        }
                        if let Some(TypeEnum::TRecord { fields }) =
                            self.type_arena.borrow().get(i_b.0)
                        {
                            *fields.borrow_mut() = fields1;
                        } else {
                            unreachable!()
                        }
                    }
                    // obj...
                    _ => {
                        return self.report_kind_error(ty_a, ty_b);
                    }
                }
            }
            _ => unimplemented!(),
        }
        Ok(())
    }

    fn set_a_to_b(&self, a: Type, b: Type) {
        // unify a and b together, and set the value to b's value this would
        // also deallocate a's previous value in the arena to save space...
        let mut table = self.unification_table.borrow_mut();
        let i_a = table.probe_value(a);
        let i_b = table.probe_value(b);
        table.union(a, b);
        table.union_value(a, i_b);
        self.type_arena.borrow_mut().remove(i_a.0);
    }

    fn report_kind_error(&self, a: &TypeEnum, b: &TypeEnum) -> Result<(), String> {
        Err(format!(
            "Cannot unify {} with {}",
            a.get_kind_name(),
            b.get_kind_name()
        ))
    }

    fn subst(&self, a: Type, mapping: &VarMap) -> Option<Type> {
        let index = self.unification_table.borrow_mut().probe_value(a);
        let arena = self.type_arena.borrow();
        let ty = arena.get(index.0).unwrap();
        // this function would only be called when we instantiate functions.
        // function type signature should ONLY contain concrete types and type
        // variables, i.e. things like TRecord, TCall should not occur, and we
        // should be safe to not implement the substitution for those variants.
        match ty {
            TypeEnum::TVar { id } => mapping.get(&id).cloned(),
            TypeEnum::TSeq { map } => {
                let map = map.take();
                drop(arena);
                let new_map = self.subst_map(&map, mapping);
                if let Some(TypeEnum::TSeq { map: m }) = self.type_arena.borrow().get(index.0) {
                    *m.borrow_mut() = map;
                } else {
                    unreachable!();
                };
                new_map.map(|m| {
                    let index = self
                        .type_arena
                        .borrow_mut()
                        .insert(TypeEnum::TSeq { map: m.into() });
                    self.unification_table
                        .borrow_mut()
                        .new_key(TypeIndex(index))
                })
            }
            TypeEnum::TTuple { ty } => {
                let ty = ty.take();
                drop(arena);
                let mut new_ty = None;
                for (i, t) in ty.iter().enumerate() {
                    if let Some(t1) = self.subst(*t, mapping) {
                        if new_ty.is_none() {
                            new_ty = Some(ty.clone());
                        }
                        new_ty.as_mut().unwrap()[i] = t1;
                    }
                }
                if let Some(TypeEnum::TTuple { ty: t }) = self.type_arena.borrow().get(index.0) {
                    *t.borrow_mut() = ty;
                } else {
                    unreachable!();
                };
                new_ty.map(|t| {
                    let index = self
                        .type_arena
                        .borrow_mut()
                        .insert(TypeEnum::TTuple { ty: t.into() });
                    self.unification_table
                        .borrow_mut()
                        .new_key(TypeIndex(index))
                })
            }
            TypeEnum::TList { ty } => {
                let ty = *ty;
                drop(arena);
                self.subst(ty, mapping).map(|t| {
                    let index = self
                        .type_arena
                        .borrow_mut()
                        .insert(TypeEnum::TList { ty: t });
                    self.unification_table
                        .borrow_mut()
                        .new_key(TypeIndex(index))
                })
            }
            TypeEnum::TVirtual { ty } => {
                let ty = *ty;
                drop(arena);
                self.subst(ty, mapping).map(|t| {
                    let index = self
                        .type_arena
                        .borrow_mut()
                        .insert(TypeEnum::TVirtual { ty: t });
                    self.unification_table
                        .borrow_mut()
                        .new_key(TypeIndex(index))
                })
            }
            TypeEnum::TObj {
                obj_id,
                fields,
                params,
            } => {
                let obj_id = *obj_id;
                let params = params.take();
                let fields = fields.take();
                drop(arena);
                let mut new_params = None;
                let mut new_fields = None;
                // Type variables in field types must be present in the type parameter.
                // If the mapping does not contain any type variables in the
                // parameter list, we don't need to substitute the fields.
                // This is also used to prevent infinite substitution...
                let need_subst = params.values().any(|v| {
                    let index = self.unification_table.borrow_mut().probe_value(*v);
                    let arena = self.type_arena.borrow();
                    let ty = arena.get(index.0).unwrap();
                    if let TypeEnum::TVar { id } = ty {
                        mapping.contains_key(id)
                    } else {
                        false
                    }
                });
                if need_subst {
                    new_params = self
                        .subst_map(&params, mapping)
                        .or_else(|| Some(params.clone()));
                    new_fields = self
                        .subst_map(&fields, mapping)
                        .or_else(|| Some(fields.clone()));
                }
                if let Some(TypeEnum::TObj {
                    params: p,
                    fields: f,
                    ..
                }) = self.type_arena.borrow().get(index.0)
                {
                    *p.borrow_mut() = params;
                    *f.borrow_mut() = fields;
                } else {
                    unreachable!();
                };
                if need_subst {
                    let index = self.type_arena.borrow_mut().insert(TypeEnum::TObj {
                        obj_id,
                        params: new_params.unwrap().into(),
                        fields: new_fields.unwrap().into(),
                    });
                    Some(
                        self.unification_table
                            .borrow_mut()
                            .new_key(TypeIndex(index)),
                    )
                } else {
                    None
                }
            }
            _ => unimplemented!(),
        }
    }

    fn subst_map<K>(&self, map: &Mapping<K>, mapping: &VarMap) -> Option<Mapping<K>>
    where
        K: std::cmp::Ord + std::clone::Clone,
    {
        let mut map2 = None;
        for (k, v) in map.iter() {
            if let Some(v1) = self.subst(*v, mapping) {
                if map2.is_none() {
                    map2 = Some(map.clone());
                }
                *map2.as_mut().unwrap().get_mut(k).unwrap() = v1;
            }
        }
        map2
    }
}
