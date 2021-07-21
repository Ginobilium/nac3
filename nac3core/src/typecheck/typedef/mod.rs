use ena::unify::{InPlaceUnificationTable, NoError, UnifyKey, UnifyValue};
use itertools::Itertools;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Debug;
use std::iter::once;
use std::ops::Deref;
use std::rc::Rc;

#[cfg(test)]
mod test_typedef;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
/// Handle for a type, implementated as a key in the unification table.
pub struct Type(u32);

#[derive(Clone)]
pub struct TypeCell(Rc<RefCell<TypeEnum>>);

impl UnifyValue for TypeCell {
    type Error = NoError;
    fn unify_values(_: &Self, value2: &Self) -> Result<Self, Self::Error> {
        // WARN: depends on the implementation details of ena.
        // We do not use this to do unification, instead we perform unification
        // and assign the type by `union_value(key, new_value)`, which set the
        // value as `unify_values(key.value, new_value)`. So, we need to return
        // the right one.
        Ok(value2.clone())
    }
}

impl UnifyKey for Type {
    type Value = TypeCell;
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

impl Deref for TypeCell {
    type Target = Rc<RefCell<TypeEnum>>;

    fn deref(&self) -> &<Self as Deref>::Target {
        &self.0
    }
}

pub type Mapping<K, V = Type> = HashMap<K, V>;
type VarMap = Mapping<u32>;

#[derive(Clone)]
pub struct Call {
    pub posargs: Vec<Type>,
    pub kwargs: HashMap<String, Type>,
    pub ret: Type,
    pub fun: RefCell<Option<Type>>,
}

#[derive(Clone)]
pub struct FuncArg {
    pub name: String,
    pub ty: Type,
    pub is_optional: bool,
}

#[derive(Clone)]
pub struct FunSignature {
    pub args: Vec<FuncArg>,
    pub ret: Type,
    pub vars: VarMap,
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
        map: Mapping<i32>,
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
    TFunc(FunSignature),
}

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

impl TypeEnum {
    pub fn get_type_name(&self) -> &'static str {
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

impl Debug for TypeCell {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.borrow().get_type_name())
    }
}

pub struct Unifier {
    unification_table: InPlaceUnificationTable<Type>,
    var_id: u32,
}

impl Unifier {
    /// Get an empty unifier
    pub fn new() -> Unifier {
        Unifier {
            unification_table: InPlaceUnificationTable::new(),
            var_id: 0,
        }
    }

    /// Register a type to the unifier.
    /// Returns a key in the unification_table.
    pub fn add_ty(&mut self, a: TypeEnum) -> Type {
        self.unification_table.new_key(TypeCell(Rc::new(a.into())))
    }

    /// Get the TypeEnum of a type.
    pub fn get_ty(&mut self, a: Type) -> Rc<RefCell<TypeEnum>> {
        self.unification_table.probe_value(a).0
    }

    /// Unify two types, i.e. a = b.
    pub fn unify(&mut self, a: Type, b: Type) -> Result<(), String> {
        self.unify_impl(a, b, false)
    }

    /// Get a fresh type variable.
    pub fn get_fresh_var(&mut self) -> (Type, u32) {
        let id = self.var_id + 1;
        self.var_id += 1;
        (self.add_ty(TypeEnum::TVar { id }), id)
    }

    /// Get string representation of the type
    pub fn stringify<F, G>(&mut self, ty: Type, obj_to_name: &mut F, var_to_name: &mut G) -> String
    where
        F: FnMut(usize) -> String,
        G: FnMut(u32) -> String,
    {
        let ty = self.unification_table.probe_value(ty).0;
        let ty = ty.as_ref().borrow();
        match &*ty {
            TypeEnum::TVar { id } => var_to_name(*id),
            TypeEnum::TSeq { map } => {
                let mut fields = map.iter().map(|(k, v)| {
                    format!("{}={}", k, self.stringify(*v, obj_to_name, var_to_name))
                });
                format!("seq[{}]", fields.join(", "))
            }
            TypeEnum::TTuple { ty } => {
                let mut fields = ty
                    .iter()
                    .map(|v| self.stringify(*v, obj_to_name, var_to_name));
                format!("tuple[{}]", fields.join(", "))
            }
            TypeEnum::TList { ty } => {
                format!("list[{}]", self.stringify(*ty, obj_to_name, var_to_name))
            }
            TypeEnum::TVirtual { ty } => {
                format!("virtual[{}]", self.stringify(*ty, obj_to_name, var_to_name))
            }
            TypeEnum::TRecord { fields } => {
                let mut fields = fields.iter().map(|(k, v)| {
                    format!("{}={}", k, self.stringify(*v, obj_to_name, var_to_name))
                });
                format!("record[{}]", fields.join(", "))
            }
            TypeEnum::TObj { obj_id, params, .. } => {
                let name = obj_to_name(*obj_id);
                let mut params = params
                    .values()
                    .map(|v| self.stringify(*v, obj_to_name, var_to_name));
                format!("{}[{}]", name, params.join(", "))
            }
            TypeEnum::TCall { .. } => "call".to_owned(),
            TypeEnum::TFunc(signature) => {
                let params = signature
                    .args
                    .iter()
                    .map(|arg| {
                        format!(
                            "{}={}",
                            arg.name,
                            self.stringify(arg.ty, obj_to_name, var_to_name)
                        )
                    })
                    .join(", ");
                let ret = self.stringify(signature.ret, obj_to_name, var_to_name);
                format!("fn[[{}], {}]", params, ret)
            }
        }
    }

    fn unify_impl(&mut self, a: Type, b: Type, swapped: bool) -> Result<(), String> {
        use TypeEnum::*;
        let (ty_a_cell, ty_b_cell) = {
            if self.unification_table.unioned(a, b) {
                return Ok(());
            }
            (
                self.unification_table.probe_value(a),
                self.unification_table.probe_value(b),
            )
        };

        let (ty_a, ty_b) = { (ty_a_cell.borrow(), ty_b_cell.borrow()) };

        self.occur_check(a, b)?;
        match (&*ty_a, &*ty_b) {
            (TypeEnum::TVar { .. }, _) => {
                self.set_a_to_b(a, b);
            }
            (TSeq { map: map1 }, TSeq { .. }) => {
                drop(ty_b);
                if let TypeEnum::TSeq { map: map2 } = &mut *ty_b_cell.as_ref().borrow_mut() {
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
            (TSeq { map: map1 }, TTuple { ty: types }) => {
                let len = types.len() as i32;
                for (k, v) in map1.iter() {
                    // handle negative index
                    let ind = if *k < 0 { len + *k } else { *k };
                    if ind >= len || ind < 0 {
                        return Err(format!(
                            "Tuple index out of range. (Length: {}, Index: {})",
                            types.len(),
                            k
                        ));
                    }
                    self.unify(*v, types[ind as usize])?;
                }
                self.set_a_to_b(a, b);
            }
            (TSeq { map: map1 }, TList { ty }) => {
                for v in map1.values() {
                    self.unify(*v, *ty)?;
                }
                self.set_a_to_b(a, b);
            }
            (TTuple { ty: ty1 }, TTuple { ty: ty2 }) => {
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
            }
            (TList { ty: ty1 }, TList { ty: ty2 }) => {
                self.unify(*ty1, *ty2)?;
                self.set_a_to_b(a, b);
            }
            (TRecord { fields: fields1 }, TRecord { .. }) => {
                drop(ty_b);
                if let TypeEnum::TRecord { fields: fields2 } = &mut *ty_b_cell.as_ref().borrow_mut()
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
            (
                TRecord { fields: fields1 },
                TObj {
                    fields: fields2, ..
                },
            ) => {
                for (key, value) in fields1.iter() {
                    if let Some(ty) = fields2.get(key) {
                        self.unify(*ty, *value)?;
                    } else {
                        return Err(format!("No such attribute {}", key));
                    }
                }
                self.set_a_to_b(a, b);
            }
            (TRecord { .. }, TVirtual { ty }) => {
                self.unify(a, *ty)?;
            }
            (
                TObj {
                    obj_id: id1,
                    params: params1,
                    ..
                },
                TObj {
                    obj_id: id2,
                    params: params2,
                    ..
                },
            ) => {
                if id1 != id2 {
                    return Err(format!("Cannot unify objects with ID {} and {}", id1, id2));
                }
                for (x, y) in params1.values().zip(params2.values()) {
                    self.unify(*x, *y)?;
                }
                self.set_a_to_b(a, b);
            }
            (TVirtual { ty: ty1 }, TVirtual { ty: ty2 }) => {
                self.unify(*ty1, *ty2)?;
                self.set_a_to_b(a, b);
            }
            (TCall { calls: c1 }, TCall { .. }) => {
                drop(ty_b);
                if let TypeEnum::TCall { calls: c2 } = &mut *ty_b_cell.as_ref().borrow_mut() {
                    c2.extend(c1.iter().cloned());
                } else {
                    unreachable!()
                }
                self.set_a_to_b(a, b);
            }
            (TCall { calls }, TFunc(signature)) => {
                let required: Vec<String> = signature
                    .args
                    .iter()
                    .filter(|v| !v.is_optional)
                    .map(|v| v.name.clone())
                    .rev()
                    .collect();
                for c in calls {
                    let Call {
                        posargs,
                        kwargs,
                        ret,
                        fun,
                    } = c.as_ref();
                    let instantiated = self.instantiate_fun(b, signature);
                    let signature;
                    let r = self.get_ty(instantiated);
                    let r = r.as_ref().borrow();
                    if let TypeEnum::TFunc(s) = &*r {
                        signature = s;
                    } else {
                        unreachable!();
                    }
                    let mut required = required.clone();
                    let mut all_names: Vec<_> = signature
                        .args
                        .iter()
                        .map(|v| (v.name.clone(), v.ty))
                        .rev()
                        .collect();
                    for (i, t) in posargs.iter().enumerate() {
                        if signature.args.len() <= i {
                            return Err("Too many arguments.".to_string());
                        }
                        if !required.is_empty() {
                            required.pop();
                        }
                        self.unify(all_names.pop().unwrap().1, *t)?;
                    }
                    for (k, t) in kwargs.iter() {
                        if let Some(i) = required.iter().position(|v| v == k) {
                            required.remove(i);
                        }
                        if let Some(i) = all_names.iter().position(|v| &v.0 == k) {
                            self.unify(all_names.remove(i).1, *t)?;
                        } else {
                            return Err(format!("Unknown keyword argument {}", k));
                        }
                    }
                    self.unify(*ret, signature.ret)?;
                    *fun.borrow_mut() = Some(instantiated);
                }
                self.set_a_to_b(a, b);
            }
            (TFunc(sign1), TFunc(sign2)) => {
                if !sign1.vars.is_empty() || !sign2.vars.is_empty() {
                    return Err("Polymorphic function pointer is prohibited.".to_string());
                }
                if sign1.args.len() != sign2.args.len() {
                    return Err("Functions differ in number of parameters.".to_string());
                }
                for (x, y) in sign1.args.iter().zip(sign2.args.iter()) {
                    if x.name != y.name {
                        return Err("Functions differ in parameter names.".to_string());
                    }
                    if x.is_optional != y.is_optional {
                        return Err("Functions differ in optional parameters.".to_string());
                    }
                    self.unify(x.ty, y.ty)?;
                }
                self.unify(sign1.ret, sign2.ret)?;
                self.set_a_to_b(a, b);
            }
            _ => {
                if swapped {
                    return self.incompatible_types(&*ty_a, &*ty_b);
                } else {
                    self.unify_impl(b, a, true)?;
                }
            }
        }
        Ok(())
    }

    fn set_a_to_b(&mut self, a: Type, b: Type) {
        // unify a and b together, and set the value to b's value.
        let table = &mut self.unification_table;
        let ty_b = table.probe_value(b);
        table.union(a, b);
        table.union_value(a, ty_b);
    }

    fn incompatible_types(&self, a: &TypeEnum, b: &TypeEnum) -> Result<(), String> {
        Err(format!(
            "Cannot unify {} with {}",
            a.get_type_name(),
            b.get_type_name()
        ))
    }

    fn occur_check(&mut self, a: Type, b: Type) -> Result<(), String> {
        if self.unification_table.unioned(a, b) {
            return Err("Recursive type is prohibited.".to_owned());
        }
        let ty = self.unification_table.probe_value(b);
        let ty = ty.borrow();

        match &*ty {
            TypeEnum::TVar { .. } => {
                // TODO: occur check for bounds...
            }
            TypeEnum::TSeq { map } => {
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
            TypeEnum::TObj { params: map, .. } => {
                for t in map.values() {
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
            TypeEnum::TFunc(FunSignature {
                args,
                ret,
                vars: params,
            }) => {
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

    /// Substitute type variables within a type into other types.
    /// If this returns Some(T), T would be the substituted type.
    /// If this returns None, the result type would be the original type
    /// (no substitution has to be done).
    fn subst(&mut self, a: Type, mapping: &VarMap) -> Option<Type> {
        let ty_cell = self.unification_table.probe_value(a);
        let ty = ty_cell.borrow();
        // this function would only be called when we instantiate functions.
        // function type signature should ONLY contain concrete types and type
        // variables, i.e. things like TRecord, TCall should not occur, and we
        // should be safe to not implement the substitution for those variants.
        match &*ty {
            TypeEnum::TVar { id } => mapping.get(&id).cloned(),
            TypeEnum::TSeq { map } => self
                .subst_map(map, mapping)
                .map(|m| self.add_ty(TypeEnum::TSeq { map: m })),
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
                    let ty_cell = self.unification_table.probe_value(*v);
                    let ty = ty_cell.borrow();
                    if let TypeEnum::TVar { id } = &*ty {
                        mapping.contains_key(&id)
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
            TypeEnum::TFunc(FunSignature {
                args,
                ret,
                vars: params,
            }) => {
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
                    Some(self.add_ty(TypeEnum::TFunc(FunSignature {
                        args,
                        ret,
                        vars: params,
                    })))
                } else {
                    None
                }
            }
            _ => unimplemented!(),
        }
    }

    fn subst_map<K>(&mut self, map: &Mapping<K>, mapping: &VarMap) -> Option<Mapping<K>>
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

    /// Instantiate a function if it hasn't been instntiated.
    /// Returns Some(T) where T is the instantiated type.
    /// Returns None if the function is already instantiated.
    fn instantiate_fun(&mut self, ty: Type, fun: &FunSignature) -> Type {
        let mut instantiated = false;
        for (k, v) in fun.vars.iter() {
            if let TypeEnum::TVar { id } =
                &*self.unification_table.probe_value(*v).as_ref().borrow()
            {
                if k != id {
                    instantiated = true;
                    break;
                }
            } else {
                instantiated = true;
                break;
            }
        }
        if instantiated {
            ty
        } else {
            let mapping = fun
                .vars
                .iter()
                .map(|(k, _)| (*k, self.get_fresh_var().0))
                .collect();
            self.subst(ty, &mapping).unwrap_or(ty)
        }
    }

    /// Check whether two types are equal.
    fn eq(&mut self, a: Type, b: Type) -> bool {
        if a == b {
            return true;
        }
        let (ty_a, ty_b) = {
            let table = &mut self.unification_table;
            if table.unioned(a, b) {
                return true;
            }
            (table.probe_value(a), table.probe_value(b))
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

    fn map_eq<K>(&mut self, map1: &Mapping<K>, map2: &Mapping<K>) -> bool
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
