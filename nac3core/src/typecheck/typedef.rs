use ena::unify::{InPlaceUnificationTable, NoError, UnifyKey, UnifyValue};
use generational_arena::{Arena, Index};
use std::cell::RefCell;
use std::collections::HashMap;
use std::iter::once;
use std::mem::swap;
use std::rc::Rc;

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
pub struct Type(u32);

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct TypeIndex(Index);

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

pub type Mapping<K, V = Type> = HashMap<K, V>;
pub type VarMap = Mapping<u32>;

#[derive(Clone)]
pub struct Call {
    posargs: Vec<Type>,
    kwargs: HashMap<String, Type>,
    ret: Type,
    fun: RefCell<Option<Type>>,
}

#[derive(Clone)]
pub struct FuncArg {
    name: String,
    ty: Type,
    is_optional: bool,
}

// We use a lot of `Rc`/`RefCell`s here as we want to simplify our code.
// We may not really need so much `Rc`s, but we would have to do complicated
// stuffs otherwise.
pub enum TypeEnum {
    TVar {
        // TODO: upper/lower bound
        id: u32,
    },
    TSeq {
        map: VarMap,
    },
    TTuple {
        ty: Vec<Type>,
    },
    TList {
        ty: Type,
    },
    TRecord {
        fields: Mapping<String>,
    },
    TObj {
        obj_id: usize,
        fields: Mapping<String>,
        params: VarMap,
    },
    TVirtual {
        ty: Type,
    },
    TCall {
        calls: Vec<Rc<Call>>,
    },
    TFunc {
        args: Vec<FuncArg>,
        ret: Type,
        params: VarMap,
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

pub struct ObjDef {
    name: String,
    fields: Mapping<String>,
}

pub struct Unifier {
    unification_table: RefCell<InPlaceUnificationTable<Type>>,
    type_arena: RefCell<Arena<Rc<RefCell<TypeEnum>>>>,
    obj_def_table: Vec<ObjDef>,
}

impl Unifier {
    pub fn new() -> Unifier {
        Unifier {
            unification_table: RefCell::new(InPlaceUnificationTable::new()),
            type_arena: RefCell::new(Arena::new()),
            obj_def_table: Vec::new(),
        }
    }

    pub fn add_ty(&self, a: TypeEnum) -> Type {
        let index = self.type_arena.borrow_mut().insert(Rc::new(a.into()));
        self.unification_table
            .borrow_mut()
            .new_key(TypeIndex(index))
    }

    pub fn get_ty(&self, a: Type) -> Rc<RefCell<TypeEnum>> {
        let mut table = self.unification_table.borrow_mut();
        let arena = self.type_arena.borrow();
        arena.get(table.probe_value(a).0).unwrap().clone()
    }

    pub fn unify(&self, mut a: Type, mut b: Type) -> Result<(), String> {
        let (mut i_a, mut i_b) = {
            let mut table = self.unification_table.borrow_mut();
            (table.probe_value(a), table.probe_value(b))
        };

        if i_a == i_b {
            return Ok(());
        }

        let (mut ty_a_cell, mut ty_b_cell) = {
            let arena = self.type_arena.borrow();
            (
                arena.get(i_a.0).unwrap().clone(),
                arena.get(i_b.0).unwrap().clone(),
            )
        };

        let (ty_a, ty_b) = {
            // simplify our pattern matching...
            if ty_a_cell.borrow().kind_le(&ty_b_cell.borrow()) {
                swap(&mut a, &mut b);
                swap(&mut i_a, &mut i_b);
                swap(&mut ty_a_cell, &mut ty_b_cell);
            }
            (ty_a_cell.borrow(), ty_b_cell.borrow())
        };

        self.occur_check(i_a, b)?;
        match &*ty_a {
            TypeEnum::TVar { .. } => {
                // TODO: type variables bound check...
                self.set_a_to_b(a, b);
            }
            TypeEnum::TSeq { map: map1 } => {
                match &*ty_b {
                    TypeEnum::TSeq { .. } => {
                        drop(ty_b);
                        if let TypeEnum::TSeq { map: map2 } = &mut *ty_b_cell.as_ref().borrow_mut()
                        {
                            // unify them to map2
                            for (key, value) in map1.iter() {
                                if let Some(ty) = map2.get(key) {
                                    self.unify(*ty, *value)?;
                                } else {
                                    map2.insert(*key, *value);
                                }
                            }
                        } else {
                            unreachable!()
                        }
                        self.set_a_to_b(a, b);
                    }
                    TypeEnum::TTuple { ty: types } => {
                        let len = types.len() as u32;
                        for (k, v) in map1.iter() {
                            if *k >= len {
                                return Err(format!(
                                    "Tuple index out of range. (Length: {}, Index: {})",
                                    types.len(),
                                    k
                                ));
                            }
                            self.unify(*v, types[*k as usize])?;
                        }
                        self.set_a_to_b(a, b);
                    }
                    TypeEnum::TList { ty } => {
                        for v in map1.values() {
                            self.unify(*v, *ty)?;
                        }
                        self.set_a_to_b(a, b);
                    }
                    _ => {
                        return self.report_kind_error(&*ty_a, &*ty_b);
                    }
                }
            }
            TypeEnum::TTuple { ty: ty1 } => {
                if let TypeEnum::TTuple { ty: ty2 } = &*ty_b {
                    if ty1.len() != ty2.len() {
                        return Err(format!(
                            "Cannot unify tuples with length {} and {}",
                            ty1.len(),
                            ty2.len()
                        ));
                    }
                    for (x, y) in ty1.iter().zip(ty2.iter()) {
                        self.unify(*x, *y)?;
                    }
                    self.set_a_to_b(a, b);
                } else {
                    return self.report_kind_error(&*ty_a, &*ty_b);
                }
            }
            TypeEnum::TList { ty: ty1 } => {
                if let TypeEnum::TList { ty: ty2 } = *ty_b {
                    self.unify(*ty1, ty2)?;
                    self.set_a_to_b(a, b);
                } else {
                    return self.report_kind_error(&*ty_a, &*ty_b);
                }
            }
            TypeEnum::TRecord { fields: fields1 } => {
                match &*ty_b {
                    TypeEnum::TRecord { .. } => {
                        drop(ty_b);
                        if let TypeEnum::TRecord { fields: fields2 } =
                            &mut *ty_b_cell.as_ref().borrow_mut()
                        {
                            for (key, value) in fields1.iter() {
                                if let Some(ty) = fields2.get(key) {
                                    self.unify(*ty, *value)?;
                                } else {
                                    fields2.insert(key.clone(), *value);
                                }
                            }
                        } else {
                            unreachable!()
                        }
                        self.set_a_to_b(a, b);
                    }
                    TypeEnum::TObj {
                        fields: fields2, ..
                    } => {
                        for (key, value) in fields1.iter() {
                            if let Some(ty) = fields2.get(key) {
                                self.unify(*ty, *value)?;
                            } else {
                                return Err(format!("No such attribute {}", key));
                            }
                        }
                        self.set_a_to_b(a, b);
                    }
                    TypeEnum::TVirtual { ty } => {
                        // not sure if this is correct...
                        self.unify(a, *ty)?;
                        self.set_a_to_b(a, b);
                    }
                    _ => {
                        return self.report_kind_error(&*ty_a, &*ty_b);
                    }
                }
            }
            TypeEnum::TObj {
                obj_id: id1,
                params: params1,
                ..
            } => {
                if let TypeEnum::TObj {
                    obj_id: id2,
                    params: params2,
                    ..
                } = &*ty_b
                {
                    if id1 != id2 {
                        return Err(format!("Cannot unify objects with ID {} and {}", id1, id2));
                    }
                    for (x, y) in params1.values().zip(params2.values()) {
                        self.unify(*x, *y)?;
                    }
                    self.set_a_to_b(a, b);
                } else {
                    return self.report_kind_error(&*ty_a, &*ty_b);
                }
            }
            TypeEnum::TVirtual { ty: ty1 } => {
                if let TypeEnum::TVirtual { ty: ty2 } = &*ty_b {
                    self.unify(*ty1, *ty2)?;
                    self.set_a_to_b(a, b);
                } else {
                    return self.report_kind_error(&*ty_a, &*ty_b);
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

    fn occur_check(&self, a: TypeIndex, b: Type) -> Result<(), String> {
        let i_b = self.unification_table.borrow_mut().probe_value(b);
        if a == i_b {
            return Err("Recursive type is prohibited.".to_owned());
        }
        let ty = self.type_arena.borrow().get(i_b.0).unwrap().clone();
        let ty = ty.borrow();

        match &*ty {
            TypeEnum::TVar { .. } => {
                // TODO: occur check for bounds...
            }
            TypeEnum::TSeq { map } | TypeEnum::TObj { params: map, .. } => {
                for t in map.values() {
                    self.occur_check(a, *t)?;
                }
            }
            TypeEnum::TTuple { ty } => {
                for t in ty.iter() {
                    self.occur_check(a, *t)?;
                }
            }
            TypeEnum::TList { ty } | TypeEnum::TVirtual { ty } => {
                self.occur_check(a, *ty)?;
            }
            TypeEnum::TRecord { fields } => {
                for t in fields.values() {
                    self.occur_check(a, *t)?;
                }
            }
            TypeEnum::TCall { calls } => {
                for t in calls
                    .iter()
                    .map(|call| {
                        call.posargs
                            .iter()
                            .chain(call.kwargs.values())
                            .chain(once(&call.ret))
                    })
                    .flatten()
                {
                    self.occur_check(a, *t)?;
                }
            }
            TypeEnum::TFunc { args, ret, params } => {
                for t in args
                    .iter()
                    .map(|v| &v.ty)
                    .chain(params.values())
                    .chain(once(ret))
                {
                    self.occur_check(a, *t)?;
                }
            }
        };
        Ok(())
    }

    pub fn subst(&self, a: Type, mapping: &VarMap) -> Option<Type> {
        let index = self.unification_table.borrow_mut().probe_value(a);
        let ty_cell = {
            let arena = self.type_arena.borrow();
            arena.get(index.0).unwrap().clone()
        };
        let ty = ty_cell.borrow();
        // this function would only be called when we instantiate functions.
        // function type signature should ONLY contain concrete types and type
        // variables, i.e. things like TRecord, TCall should not occur, and we
        // should be safe to not implement the substitution for those variants.
        match &*ty {
            TypeEnum::TVar { id } => mapping.get(&id).cloned(),
            TypeEnum::TSeq { map } => self.subst_map(map, mapping).map(|m| {
                let index = self
                    .type_arena
                    .borrow_mut()
                    .insert(Rc::new(TypeEnum::TSeq { map: m }.into()));
                self.unification_table
                    .borrow_mut()
                    .new_key(TypeIndex(index))
            }),
            TypeEnum::TTuple { ty } => {
                let mut new_ty = None;
                for (i, t) in ty.iter().enumerate() {
                    if let Some(t1) = self.subst(*t, mapping) {
                        if new_ty.is_none() {
                            new_ty = Some(ty.clone());
                        }
                        new_ty.as_mut().unwrap()[i] = t1;
                    }
                }
                new_ty.map(|t| self.add_ty(TypeEnum::TTuple { ty: t }))
            }
            TypeEnum::TList { ty } => self
                .subst(*ty, mapping)
                .map(|t| self.add_ty(TypeEnum::TList { ty: t })),
            TypeEnum::TVirtual { ty } => self
                .subst(*ty, mapping)
                .map(|t| self.add_ty(TypeEnum::TVirtual { ty: t })),
            TypeEnum::TObj {
                obj_id,
                fields,
                params,
            } => {
                // Type variables in field types must be present in the type parameter.
                // If the mapping does not contain any type variables in the
                // parameter list, we don't need to substitute the fields.
                // This is also used to prevent infinite substitution...
                let need_subst = params.values().any(|v| {
                    let index = self.unification_table.borrow_mut().probe_value(*v);
                    let arena = self.type_arena.borrow();
                    let ty_cell = arena.get(index.0).unwrap();
                    let ty = ty_cell.borrow();
                    if let TypeEnum::TVar { id } = &*ty {
                        mapping.contains_key(id)
                    } else {
                        false
                    }
                });
                if need_subst {
                    let obj_id = *obj_id;
                    let params = self
                        .subst_map(&params, mapping)
                        .unwrap_or_else(|| params.clone());
                    let fields = self
                        .subst_map(&fields, mapping)
                        .unwrap_or_else(|| fields.clone());
                    Some(self.add_ty(TypeEnum::TObj {
                        obj_id,
                        params,
                        fields,
                    }))
                } else {
                    None
                }
            }
            TypeEnum::TFunc { args, ret, params } => {
                let new_params = self.subst_map(params, mapping);
                let new_ret = self.subst(*ret, mapping);
                let mut new_args = None;
                for (i, t) in args.iter().enumerate() {
                    if let Some(t1) = self.subst(t.ty, mapping) {
                        if new_args.is_none() {
                            new_args = Some(args.clone());
                        }
                        new_args.as_mut().unwrap()[i] = FuncArg {
                            name: t.name.clone(),
                            ty: t1,
                            is_optional: t.is_optional,
                        };
                    }
                }
                if new_params.is_some() || new_ret.is_some() || new_args.is_some() {
                    let params = new_params.unwrap_or_else(|| params.clone());
                    let ret = new_ret.unwrap_or_else(|| *ret);
                    let args = new_args.unwrap_or_else(|| args.clone());
                    Some(self.add_ty(TypeEnum::TFunc { params, ret, args }))
                } else {
                    None
                }
            }
            _ => unimplemented!(),
        }
    }

    fn subst_map<K>(&self, map: &Mapping<K>, mapping: &VarMap) -> Option<Mapping<K>>
    where
        K: std::hash::Hash + std::cmp::Eq + std::clone::Clone,
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

    pub fn eq(&self, a: Type, b: Type) -> bool {
        if a == b {
            return true;
        }
        let (i_a, i_b) = {
            let mut table = self.unification_table.borrow_mut();
            (table.probe_value(a), table.probe_value(b))
        };

        if i_a == i_b {
            return true;
        }

        let (ty_a, ty_b) = {
            let arena = self.type_arena.borrow();
            (
                arena.get(i_a.0).unwrap().clone(),
                arena.get(i_b.0).unwrap().clone(),
            )
        };

        let ty_a = ty_a.borrow();
        let ty_b = ty_b.borrow();

        match (&*ty_a, &*ty_b) {
            (TypeEnum::TVar { id: id1 }, TypeEnum::TVar { id: id2 }) => id1 == id2,
            (TypeEnum::TSeq { map: map1 }, TypeEnum::TSeq { map: map2 }) => self.map_eq(map1, map2),
            (TypeEnum::TTuple { ty: ty1 }, TypeEnum::TTuple { ty: ty2 }) => {
                ty1.len() == ty2.len()
                    && ty1.iter().zip(ty2.iter()).all(|(t1, t2)| self.eq(*t1, *t2))
            }
            (TypeEnum::TList { ty: ty1 }, TypeEnum::TList { ty: ty2 })
            | (TypeEnum::TVirtual { ty: ty1 }, TypeEnum::TVirtual { ty: ty2 }) => {
                self.eq(*ty1, *ty2)
            }
            (TypeEnum::TRecord { fields: fields1 }, TypeEnum::TRecord { fields: fields2 }) => {
                self.map_eq(fields1, fields2)
            }
            (
                TypeEnum::TObj {
                    obj_id: id1,
                    params: params1,
                    ..
                },
                TypeEnum::TObj {
                    obj_id: id2,
                    params: params2,
                    ..
                },
            ) => id1 == id2 && self.map_eq(params1, params2),
            // TCall and TFunc are not yet implemented
            _ => false,
        }
    }

    fn map_eq<K>(&self, map1: &Mapping<K>, map2: &Mapping<K>) -> bool
    where
        K: std::hash::Hash + std::cmp::Eq + std::clone::Clone,
    {
        if map1.len() != map2.len() {
            return false;
        }
        for (k, v) in map1.iter() {
            if !map2.get(k).map(|v1| self.eq(*v, *v1)).unwrap_or(false) {
                return false;
            }
        }
        true
    }
}
