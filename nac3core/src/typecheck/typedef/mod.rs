use itertools::{chain, zip, Itertools};
use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::iter::once;
use std::rc::Rc;

use super::unification_table::{UnificationKey, UnificationTable};

#[cfg(test)]
mod test;

/// Handle for a type, implementated as a key in the unification table.
pub type Type = UnificationKey;

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

#[derive(Clone)]
pub enum TypeVarMeta {
    Generic,
    Sequence(RefCell<Mapping<i32>>),
    Record(RefCell<Mapping<String>>),
}

#[derive(Clone)]
pub enum TypeEnum {
    TVar {
        id: u32,
        meta: TypeVarMeta,
        // empty indicates no restriction
        range: RefCell<Vec<Type>>,
    },
    TTuple {
        ty: Vec<Type>,
    },
    TList {
        ty: Type,
    },
    TObj {
        obj_id: usize,
        fields: Mapping<String>,
        params: VarMap,
    },
    TVirtual {
        ty: Type,
    },
    TCall(RefCell<Vec<Rc<Call>>>),
    TFunc(FunSignature),
}

impl TypeEnum {
    pub fn get_type_name(&self) -> &'static str {
        match self {
            TypeEnum::TVar { .. } => "TVar",
            TypeEnum::TTuple { .. } => "TTuple",
            TypeEnum::TList { .. } => "TList",
            TypeEnum::TObj { .. } => "TObj",
            TypeEnum::TVirtual { .. } => "TVirtual",
            TypeEnum::TCall { .. } => "TCall",
            TypeEnum::TFunc { .. } => "TFunc",
        }
    }

    pub fn is_concrete(&self) -> bool {
        !matches!(self, TypeEnum::TVar { .. })
    }
}

pub struct Unifier {
    unification_table: UnificationTable<Rc<TypeEnum>>,
    var_id: u32,
}

impl Unifier {
    /// Get an empty unifier
    pub fn new() -> Unifier {
        Unifier { unification_table: UnificationTable::new(), var_id: 0 }
    }

    /// Register a type to the unifier.
    /// Returns a key in the unification_table.
    pub fn add_ty(&mut self, a: TypeEnum) -> Type {
        self.unification_table.new_key(Rc::new(a))
    }

    pub fn add_record(&mut self, fields: Mapping<String>) -> Type {
        let id = self.var_id + 1;
        self.var_id += 1;
        self.add_ty(TypeEnum::TVar {
            id,
            range: vec![].into(),
            meta: TypeVarMeta::Record(fields.into()),
        })
    }

    pub fn add_sequence(&mut self, sequence: Mapping<i32>) -> Type {
        let id = self.var_id + 1;
        self.var_id += 1;
        self.add_ty(TypeEnum::TVar {
            id,
            range: vec![].into(),
            meta: TypeVarMeta::Sequence(sequence.into()),
        })
    }

    /// Get the TypeEnum of a type.
    pub fn get_ty(&mut self, a: Type) -> Rc<TypeEnum> {
        self.unification_table.probe_value(a).clone()
    }

    pub fn get_fresh_var(&mut self) -> (Type, u32) {
        self.get_fresh_var_with_range(&[])
    }

    /// Get a fresh type variable.
    pub fn get_fresh_var_with_range(&mut self, range: &[Type]) -> (Type, u32) {
        let id = self.var_id + 1;
        self.var_id += 1;
        let range = range.to_vec().into();
        (self.add_ty(TypeEnum::TVar { id, range, meta: TypeVarMeta::Generic }), id)
    }

    pub fn unify(&mut self, a: Type, b: Type) -> Result<(), String> {
        if self.unification_table.unioned(a, b) {
            Ok(())
        } else {
            self.unify_impl(a, b, false)
        }
    }

    fn unify_impl(&mut self, a: Type, b: Type, swapped: bool) -> Result<(), String> {
        use TypeEnum::*;
        use TypeVarMeta::*;
        let (ty_a, ty_b) = {
            (
                self.unification_table.probe_value(a).clone(),
                self.unification_table.probe_value(b).clone(),
            )
        };
        match (&*ty_a, &*ty_b) {
            (TVar { meta: meta1, range: range1, .. }, TVar { meta: meta2, range: range2, .. }) => {
                self.occur_check(a, b)?;
                self.occur_check(b, a)?;
                match (meta1, meta2) {
                    (Generic, _) => {}
                    (_, Generic) => {
                        return self.unify_impl(b, a, true);
                    }
                    (Record(fields1), Record(fields2)) => {
                        let mut fields2 = fields2.borrow_mut();
                        for (key, value) in fields1.borrow().iter() {
                            if let Some(ty) = fields2.get(key) {
                                self.unify(*ty, *value)?;
                            } else {
                                fields2.insert(key.clone(), *value);
                            }
                        }
                    }
                    (Sequence(map1), Sequence(map2)) => {
                        let mut map2 = map2.borrow_mut();
                        for (key, value) in map1.borrow().iter() {
                            if let Some(ty) = map2.get(key) {
                                self.unify(*ty, *value)?;
                            } else {
                                map2.insert(*key, *value);
                            }
                        }
                    }
                    _ => {
                        return Err("Incompatible".to_string());
                    }
                }
                let range1 = range1.borrow();
                // new range is the intersection of them
                // empty range indicates no constraint
                if !range1.is_empty() {
                    let old_range2 = range2.take();
                    let mut range2 = range2.borrow_mut();
                    if old_range2.is_empty() {
                        range2.extend_from_slice(&range1);
                    }
                    for v1 in old_range2.iter() {
                        for v2 in range1.iter() {
                            if let Ok(result) = self.get_intersection(*v1, *v2){
                                range2.push(result.unwrap_or(*v2));
                            }
                        }
                    }
                    if range2.is_empty() {
                        return Err(
                            "cannot unify type variables with incompatible value range".to_string()
                        );
                    }
                }
                self.set_a_to_b(a, b);
            }
            (TVar { meta: Generic, id, range, .. }, _) => {
                self.occur_check(a, b)?;
                let x = self.check_var_compatibility(*id, b, &range.borrow())?.unwrap_or(b);
                self.unify(x, b)?;
                self.set_a_to_b(a, x);
            }
            (TVar { meta: Sequence(map), id, range, .. }, TTuple { ty }) => {
                self.occur_check(a, b)?;
                let len = ty.len() as i32;
                for (k, v) in map.borrow().iter() {
                    // handle negative index
                    let ind = if *k < 0 { len + *k } else { *k };
                    if ind >= len || ind < 0 {
                        return Err(format!(
                            "Tuple index out of range. (Length: {}, Index: {})",
                            len, k
                        ));
                    }
                    self.unify(*v, ty[ind as usize])?;
                }
                let x = self.check_var_compatibility(*id, b, &range.borrow())?.unwrap_or(b);
                self.unify(x, b)?;
                self.set_a_to_b(a, x);
            }
            (TVar { meta: Sequence(map), id, range, .. }, TList { ty }) => {
                self.occur_check(a, b)?;
                for v in map.borrow().values() {
                    self.unify(*v, *ty)?;
                }
                let x = self.check_var_compatibility(*id, b, &range.borrow())?.unwrap_or(b);
                self.unify(x, b)?;
                self.set_a_to_b(a, x);
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
            (TVar { meta: Record(map), id, range, .. }, TObj { fields, .. }) => {
                self.occur_check(a, b)?;
                for (k, v) in map.borrow().iter() {
                    if let Some(ty) = fields.get(k) {
                        self.unify(*ty, *v)?;
                    } else {
                        return Err(format!("No such attribute {}", k));
                    }
                }
                let x = self.check_var_compatibility(*id, b, &range.borrow())?.unwrap_or(b);
                self.unify(x, b)?;
                self.set_a_to_b(a, x);
            }
            (TVar { meta: Record(map), id, range, .. }, TVirtual { ty }) => {
                self.occur_check(a, b)?;
                let ty = self.get_ty(*ty);
                if let TObj { fields, .. } = ty.as_ref() {
                    for (k, v) in map.borrow().iter() {
                        if let Some(ty) = fields.get(k) {
                            if !matches!(self.get_ty(*ty).as_ref(), TFunc { .. }) {
                                return Err(format!("Cannot access field {} for virtual type", k));
                            }
                            self.unify(*v, *ty)?;
                        } else {
                            return Err(format!("No such attribute {}", k));
                        }
                    }
                } else {
                    // require annotation...
                    return Err("Requires type annotation for virtual".to_string());
                }
                let x = self.check_var_compatibility(*id, b, &range.borrow())?.unwrap_or(b);
                self.unify(x, b)?;
                self.set_a_to_b(a, x);
            }
            (
                TObj { obj_id: id1, params: params1, .. },
                TObj { obj_id: id2, params: params2, .. },
            ) => {
                if id1 != id2 {
                    return Err(format!("Cannot unify objects with ID {} and {}", id1, id2));
                }
                for (x, y) in zip(params1.values(), params2.values()) {
                    self.unify(*x, *y)?;
                }
                self.set_a_to_b(a, b);
            }
            (TVirtual { ty: ty1 }, TVirtual { ty: ty2 }) => {
                self.unify(*ty1, *ty2)?;
                self.set_a_to_b(a, b);
            }
            (TCall(calls1), TCall(calls2)) => {
                calls2.borrow_mut().extend_from_slice(&calls1.borrow());
            }
            (TCall(calls), TFunc(signature)) => {
                self.occur_check(a, b)?;
                let required: Vec<String> = signature
                    .args
                    .iter()
                    .filter(|v| !v.is_optional)
                    .map(|v| v.name.clone())
                    .rev()
                    .collect();
                for c in calls.borrow().iter() {
                    let Call { posargs, kwargs, ret, fun } = c.as_ref();
                    let instantiated = self.instantiate_fun(b, signature);
                    let signature;
                    let r = self.get_ty(instantiated);
                    let r = r.as_ref();
                    if let TypeEnum::TFunc(s) = &*r {
                        signature = s;
                    } else {
                        unreachable!();
                    }
                    let mut required = required.clone();
                    let mut all_names: Vec<_> =
                        signature.args.iter().map(|v| (v.name.clone(), v.ty)).rev().collect();
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
                    if !required.is_empty() {
                        return Err("Expected more arguments".to_string());
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

    /// Get string representation of the type
    pub fn stringify<F, G>(&mut self, ty: Type, obj_to_name: &mut F, var_to_name: &mut G) -> String
    where
        F: FnMut(usize) -> String,
        G: FnMut(u32) -> String,
    {
        use TypeVarMeta::*;
        let ty = self.unification_table.probe_value(ty).clone();
        match ty.as_ref() {
            TypeEnum::TVar { id, meta: Generic, .. } => var_to_name(*id),
            TypeEnum::TVar { meta: Sequence(map), .. } => {
                let fields = map
                    .borrow()
                    .iter()
                    .map(|(k, v)| format!("{}={}", k, self.stringify(*v, obj_to_name, var_to_name)))
                    .join(", ");
                format!("seq[{}]", fields)
            }
            TypeEnum::TVar { meta: Record(fields), .. } => {
                let fields = fields
                    .borrow()
                    .iter()
                    .map(|(k, v)| format!("{}={}", k, self.stringify(*v, obj_to_name, var_to_name)))
                    .join(", ");
                format!("record[{}]", fields)
            }
            TypeEnum::TTuple { ty } => {
                let mut fields = ty.iter().map(|v| self.stringify(*v, obj_to_name, var_to_name));
                format!("tuple[{}]", fields.join(", "))
            }
            TypeEnum::TList { ty } => {
                format!("list[{}]", self.stringify(*ty, obj_to_name, var_to_name))
            }
            TypeEnum::TVirtual { ty } => {
                format!("virtual[{}]", self.stringify(*ty, obj_to_name, var_to_name))
            }
            TypeEnum::TObj { obj_id, params, .. } => {
                let name = obj_to_name(*obj_id);
                if !params.is_empty() {
                    let mut params =
                        params.values().map(|v| self.stringify(*v, obj_to_name, var_to_name));
                    format!("{}[{}]", name, params.join(", "))
                } else {
                    name
                }
            }
            TypeEnum::TCall { .. } => "call".to_owned(),
            TypeEnum::TFunc(signature) => {
                let params = signature
                    .args
                    .iter()
                    .map(|arg| {
                        format!("{}={}", arg.name, self.stringify(arg.ty, obj_to_name, var_to_name))
                    })
                    .join(", ");
                let ret = self.stringify(signature.ret, obj_to_name, var_to_name);
                format!("fn[[{}], {}]", params, ret)
            }
        }
    }

    fn set_a_to_b(&mut self, a: Type, b: Type) {
        // unify a and b together, and set the value to b's value.
        let table = &mut self.unification_table;
        let ty_b = table.probe_value(b).clone();
        table.unify(a, b);
        table.set_value(a, ty_b)
    }

    fn incompatible_types(&self, a: &TypeEnum, b: &TypeEnum) -> Result<(), String> {
        Err(format!("Cannot unify {} with {}", a.get_type_name(), b.get_type_name()))
    }

    /// Instantiate a function if it hasn't been instntiated.
    /// Returns Some(T) where T is the instantiated type.
    /// Returns None if the function is already instantiated.
    fn instantiate_fun(&mut self, ty: Type, fun: &FunSignature) -> Type {
        let mut instantiated = false;
        let mut vars = Vec::new();
        for (k, v) in fun.vars.iter() {
            if let TypeEnum::TVar { id, range, .. } =
                self.unification_table.probe_value(*v).as_ref()
            {
                if k != id {
                    instantiated = true;
                    break;
                }
                // actually, if the first check succeeded, the function should be uninstatiated.
                // The cloned values must be used and would not be wasted.
                vars.push((*k, range.clone()));
            } else {
                instantiated = true;
                break;
            }
        }
        if instantiated {
            ty
        } else {
            let mapping = vars
                .into_iter()
                .map(|(k, range)| (k, self.get_fresh_var_with_range(range.borrow().as_ref()).0))
                .collect();
            self.subst(ty, &mapping).unwrap_or(ty)
        }
    }

    /// Substitute type variables within a type into other types.
    /// If this returns Some(T), T would be the substituted type.
    /// If this returns None, the result type would be the original type
    /// (no substitution has to be done).
    fn subst(&mut self, a: Type, mapping: &VarMap) -> Option<Type> {
        use TypeVarMeta::*;
        let ty = self.unification_table.probe_value(a).clone();
        // this function would only be called when we instantiate functions.
        // function type signature should ONLY contain concrete types and type
        // variables, i.e. things like TRecord, TCall should not occur, and we
        // should be safe to not implement the substitution for those variants.
        match &*ty {
            TypeEnum::TVar { id, meta: Generic, .. } => mapping.get(&id).cloned(),
            TypeEnum::TTuple { ty } => {
                let mut new_ty = Cow::from(ty);
                for (i, t) in ty.iter().enumerate() {
                    if let Some(t1) = self.subst(*t, mapping) {
                        new_ty.to_mut()[i] = t1;
                    }
                }
                if matches!(new_ty, Cow::Owned(_)) {
                    Some(self.add_ty(TypeEnum::TTuple { ty: new_ty.into_owned() }))
                } else {
                    None
                }
            }
            TypeEnum::TList { ty } => {
                self.subst(*ty, mapping).map(|t| self.add_ty(TypeEnum::TList { ty: t }))
            }
            TypeEnum::TVirtual { ty } => {
                self.subst(*ty, mapping).map(|t| self.add_ty(TypeEnum::TVirtual { ty: t }))
            }
            TypeEnum::TObj { obj_id, fields, params } => {
                // Type variables in field types must be present in the type parameter.
                // If the mapping does not contain any type variables in the
                // parameter list, we don't need to substitute the fields.
                // This is also used to prevent infinite substitution...
                let need_subst = params.values().any(|v| {
                    let ty = self.unification_table.probe_value(*v);
                    if let TypeEnum::TVar { id, .. } = ty.as_ref() {
                        mapping.contains_key(&id)
                    } else {
                        false
                    }
                });
                if need_subst {
                    let obj_id = *obj_id;
                    let params = self.subst_map(&params, mapping).unwrap_or_else(|| params.clone());
                    let fields = self.subst_map(&fields, mapping).unwrap_or_else(|| fields.clone());
                    Some(self.add_ty(TypeEnum::TObj { obj_id, params, fields }))
                } else {
                    None
                }
            }
            TypeEnum::TFunc(FunSignature { args, ret, vars: params }) => {
                let new_params = self.subst_map(params, mapping);
                let new_ret = self.subst(*ret, mapping);
                let mut new_args = Cow::from(args);
                for (i, t) in args.iter().enumerate() {
                    if let Some(t1) = self.subst(t.ty, mapping) {
                        let mut t = t.clone();
                        t.ty = t1;
                        new_args.to_mut()[i] = t;
                    }
                }
                if new_params.is_some() || new_ret.is_some() || matches!(new_args, Cow::Owned(..)) {
                    let params = new_params.unwrap_or_else(|| params.clone());
                    let ret = new_ret.unwrap_or_else(|| *ret);
                    let args = new_args.into_owned();
                    Some(self.add_ty(TypeEnum::TFunc(FunSignature { args, ret, vars: params })))
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

    fn occur_check(&mut self, a: Type, b: Type) -> Result<(), String> {
        use TypeVarMeta::*;
        if self.unification_table.unioned(a, b) {
            return Err("Recursive type is prohibited.".to_owned());
        }
        let ty = self.unification_table.probe_value(b).clone();

        match ty.as_ref() {
            TypeEnum::TVar { meta: Generic, .. } => {}
            TypeEnum::TVar { meta: Sequence(map), .. } => {
                for t in map.borrow().values() {
                    self.occur_check(a, *t)?;
                }
            }
            TypeEnum::TVar { meta: Record(map), .. } => {
                for t in map.borrow().values() {
                    self.occur_check(a, *t)?;
                }
            }
            TypeEnum::TCall(calls) => {
                for t in calls
                    .borrow()
                    .iter()
                    .map(|call| chain!(call.posargs.iter(), call.kwargs.values(), once(&call.ret)))
                    .flatten()
                {
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
            TypeEnum::TObj { params: map, .. } => {
                for t in map.values() {
                    self.occur_check(a, *t)?;
                }
            }
            TypeEnum::TFunc(FunSignature { args, ret, vars: params }) => {
                for t in chain!(args.iter().map(|v| &v.ty), params.values(), once(ret)) {
                    self.occur_check(a, *t)?;
                }
            }
        }
        Ok(())
    }

    fn get_intersection(&mut self, a: Type, b: Type) -> Result<Option<Type>, ()> {
        use TypeEnum::*;
        let x = self.get_ty(a);
        let y = self.get_ty(b);
        match (x.as_ref(), y.as_ref()) {
            (TVar { range: range1, .. }, TVar { meta, range: range2, .. }) => {
                // we should restrict range2
                let range1 = range1.borrow();
                // new range is the intersection of them
                // empty range indicates no constraint
                if !range1.is_empty() {
                    let range2 = range2.borrow();
                    let mut range = Vec::new();
                    if range2.is_empty() {
                        range.extend_from_slice(&range1);
                    }
                    for v1 in range2.iter() {
                        for v2 in range1.iter() {
                            let result = self.get_intersection(*v1, *v2);
                            if let Ok(result) = result {
                                range.push(result.unwrap_or(*v2));
                            }
                        }
                    }
                    if range.is_empty() {
                        Err(())
                    } else {
                        let id = self.var_id + 1;
                        self.var_id += 1;
                        let ty = TVar { id, meta: meta.clone(), range: range.into() };
                        Ok(Some(self.unification_table.new_key(ty.into())))
                    }
                } else {
                    Ok(Some(b))
                }
            }
            (_, TVar { range, .. }) => {
                // range should be restricted to the left hand side
                let range = range.borrow();
                if range.is_empty() {
                    Ok(Some(a))
                } else {
                    for v in range.iter() {
                        let result = self.get_intersection(a, *v);
                        if let Ok(result) = result {
                            return Ok(result.or(Some(a)));
                        }
                    }
                    Err(())
                }
            }
            (TVar { id, range, .. }, _) => {
                self.check_var_compatibility(*id, b, &range.borrow()).or(Err(()))
            }
            (TTuple { ty: ty1 }, TTuple { ty: ty2 }) => {
                if ty1.len() != ty2.len() {
                    return Err(());
                }
                let mut need_new = false;
                let mut ty = ty1.clone();
                for (a, b) in zip(ty1.iter(), ty2.iter()) {
                    let result = self.get_intersection(*a, *b)?;
                    ty.push(result.unwrap_or(*a));
                    if result.is_some() {
                        need_new = true;
                    }
                }
                if need_new {
                    Ok(Some(self.add_ty(TTuple { ty })))
                } else {
                    Ok(None)
                }
            }
            (TList { ty: ty1 }, TList { ty: ty2 }) => {
                Ok(self.get_intersection(*ty1, *ty2)?.map(|ty| self.add_ty(TList { ty })))
            }
            (TVirtual { ty: ty1 }, TVirtual { ty: ty2 }) => {
                Ok(self.get_intersection(*ty1, *ty2)?.map(|ty| self.add_ty(TVirtual { ty })))
            }
            (TObj { obj_id: id1, .. }, TObj { obj_id: id2, .. }) => {
                if id1 == id2 {
                    Ok(None)
                } else {
                    Err(())
                }
            }
            // don't deal with function shape for now
            _ => Err(()),
        }
    }

    fn check_var_compatibility(
        &mut self,
        id: u32,
        b: Type,
        range: &[Type],
    ) -> Result<Option<Type>, String> {
        if range.is_empty() {
            return Ok(None);
        }
        for t in range.iter() {
            let result = self.get_intersection(*t, b);
            if let Ok(result) = result {
                return Ok(result);
            }
        }
        return Err(format!(
            "Cannot unify type variable {} with {} due to incompatible value range",
            id,
            self.get_ty(b).get_type_name()
        ));
    }
}
