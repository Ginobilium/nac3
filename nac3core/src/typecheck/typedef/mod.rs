use itertools::{zip, Itertools};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::{Arc, Mutex};
use std::{borrow::Cow, collections::HashSet};

use nac3parser::ast::StrRef;

use super::unification_table::{UnificationKey, UnificationTable};
use crate::symbol_resolver::SymbolValue;
use crate::toplevel::{DefinitionId, TopLevelContext, TopLevelDef};

#[cfg(test)]
mod test;

/// Handle for a type, implementated as a key in the unification table.
pub type Type = UnificationKey;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct CallId(usize);

pub type Mapping<K, V = Type> = HashMap<K, V>;
type VarMap = Mapping<u32>;

#[derive(Clone)]
pub struct Call {
    pub posargs: Vec<Type>,
    pub kwargs: HashMap<StrRef, Type>,
    pub ret: Type,
    pub fun: RefCell<Option<Type>>,
}

#[derive(Clone)]
pub struct FuncArg {
    pub name: StrRef,
    pub ty: Type,
    pub default_value: Option<SymbolValue>,
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
    Record(RefCell<Mapping<StrRef>>),
}

#[derive(Clone)]
pub enum TypeEnum {
    TRigidVar {
        id: u32,
    },
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
        obj_id: DefinitionId,
        fields: RefCell<Mapping<StrRef>>,
        params: RefCell<VarMap>,
    },
    TVirtual {
        ty: Type,
    },
    TCall(RefCell<Vec<CallId>>),
    TFunc(RefCell<FunSignature>),
}

impl TypeEnum {
    pub fn get_type_name(&self) -> &'static str {
        match self {
            TypeEnum::TRigidVar { .. } => "TRigidVar",
            TypeEnum::TVar { .. } => "TVar",
            TypeEnum::TTuple { .. } => "TTuple",
            TypeEnum::TList { .. } => "TList",
            TypeEnum::TObj { .. } => "TObj",
            TypeEnum::TVirtual { .. } => "TVirtual",
            TypeEnum::TCall { .. } => "TCall",
            TypeEnum::TFunc { .. } => "TFunc",
        }
    }
}

pub type SharedUnifier = Arc<Mutex<(UnificationTable<TypeEnum>, u32, Vec<Call>)>>;

#[derive(Clone)]
pub struct Unifier {
    pub top_level: Option<Arc<TopLevelContext>>,
    unification_table: UnificationTable<Rc<TypeEnum>>,
    calls: Vec<Rc<Call>>,
    var_id: u32,
    unify_cache: HashSet<(Type, Type)>,
}

impl Default for Unifier {
    fn default() -> Self {
        Unifier::new()
    }
}

impl Unifier {
    /// Get an empty unifier
    pub fn new() -> Unifier {
        Unifier {
            unification_table: UnificationTable::new(),
            var_id: 0,
            calls: Vec::new(),
            unify_cache: HashSet::new(),
            top_level: None,
        }
    }

    /// Determine if the two types are the same
    pub fn unioned(&mut self, a: Type, b: Type) -> bool {
        self.unification_table.unioned(a, b)
    }

    pub fn from_shared_unifier(unifier: &SharedUnifier) -> Unifier {
        let lock = unifier.lock().unwrap();
        Unifier {
            unification_table: UnificationTable::from_send(&lock.0),
            var_id: lock.1,
            calls: lock.2.iter().map(|v| Rc::new(v.clone())).collect_vec(),
            top_level: None,
            unify_cache: HashSet::new(),
        }
    }

    pub fn get_shared_unifier(&self) -> SharedUnifier {
        Arc::new(Mutex::new((
            self.unification_table.get_send(),
            self.var_id,
            self.calls.iter().map(|v| v.as_ref().clone()).collect_vec(),
        )))
    }

    /// Register a type to the unifier.
    /// Returns a key in the unification_table.
    pub fn add_ty(&mut self, a: TypeEnum) -> Type {
        self.unification_table.new_key(Rc::new(a))
    }

    pub fn add_record(&mut self, fields: Mapping<StrRef>) -> Type {
        let id = self.var_id + 1;
        self.var_id += 1;
        self.add_ty(TypeEnum::TVar {
            id,
            range: vec![].into(),
            meta: TypeVarMeta::Record(fields.into()),
        })
    }

    pub fn add_call(&mut self, call: Call) -> CallId {
        let id = CallId(self.calls.len());
        self.calls.push(Rc::new(call));
        id
    }

    pub fn get_call_signature(&mut self, id: CallId) -> Option<FunSignature> {
        let fun = self.calls.get(id.0).unwrap().fun.borrow().unwrap();
        if let TypeEnum::TFunc(sign) = &*self.get_ty(fun) {
            Some(sign.borrow().clone())
        } else {
            None
        }
    }

    pub fn get_representative(&mut self, ty: Type) -> Type {
        self.unification_table.get_representative(ty)
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

    pub fn get_fresh_rigid_var(&mut self) -> (Type, u32) {
        let id = self.var_id + 1;
        self.var_id += 1;
        (self.add_ty(TypeEnum::TRigidVar { id }), id)
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

    /// Unification would not unify rigid variables with other types, but we want to do this for
    /// function instantiations, so we make it explicit.
    pub fn replace_rigid_var(&mut self, rigid: Type, b: Type) {
        assert!(matches!(&*self.get_ty(rigid), TypeEnum::TRigidVar { .. }));
        self.set_a_to_b(rigid, b);
    }

    pub fn get_instantiations(&mut self, ty: Type) -> Option<Vec<Type>> {
        match &*self.get_ty(ty) {
            TypeEnum::TVar { range, .. } => {
                let range = range.borrow();
                if range.is_empty() {
                    None
                } else {
                    Some(
                        range
                            .iter()
                            .map(|ty| self.get_instantiations(*ty).unwrap_or_else(|| vec![*ty]))
                            .flatten()
                            .collect_vec(),
                    )
                }
            }
            TypeEnum::TList { ty } => self
                .get_instantiations(*ty)
                .map(|ty| ty.iter().map(|&ty| self.add_ty(TypeEnum::TList { ty })).collect_vec()),
            TypeEnum::TVirtual { ty } => self.get_instantiations(*ty).map(|ty| {
                ty.iter().map(|&ty| self.add_ty(TypeEnum::TVirtual { ty })).collect_vec()
            }),
            TypeEnum::TTuple { ty } => {
                let tuples = ty
                    .iter()
                    .map(|ty| self.get_instantiations(*ty).unwrap_or_else(|| vec![*ty]))
                    .multi_cartesian_product()
                    .collect_vec();
                if tuples.len() == 1 {
                    None
                } else {
                    Some(
                        tuples.into_iter().map(|ty| self.add_ty(TypeEnum::TTuple { ty })).collect(),
                    )
                }
            }
            TypeEnum::TObj { params, .. } => {
                let params = params.borrow();
                let (keys, params): (Vec<&u32>, Vec<&Type>) = params.iter().unzip();
                let params = params
                    .into_iter()
                    .map(|ty| self.get_instantiations(*ty).unwrap_or_else(|| vec![*ty]))
                    .multi_cartesian_product()
                    .collect_vec();
                if params.len() <= 1 {
                    None
                } else {
                    Some(
                        params
                            .into_iter()
                            .map(|params| {
                                self.subst(
                                    ty,
                                    &zip(keys.iter().cloned().cloned(), params.iter().cloned())
                                        .collect(),
                                )
                                .unwrap_or(ty)
                            })
                            .collect(),
                    )
                }
            }
            _ => None,
        }
    }

    pub fn is_concrete(&mut self, a: Type, allowed_typevars: &[Type]) -> bool {
        use TypeEnum::*;
        match &*self.get_ty(a) {
            TRigidVar { .. } => true,
            TVar { .. } => allowed_typevars.iter().any(|b| self.unification_table.unioned(a, *b)),
            TCall { .. } => false,
            TList { ty } => self.is_concrete(*ty, allowed_typevars),
            TTuple { ty } => ty.iter().all(|ty| self.is_concrete(*ty, allowed_typevars)),
            TObj { params: vars, .. } => {
                vars.borrow().values().all(|ty| self.is_concrete(*ty, allowed_typevars))
            }
            // functions are instantiated for each call sites, so the function type can contain
            // type variables.
            TFunc { .. } => true,
            TVirtual { ty } => self.is_concrete(*ty, allowed_typevars),
        }
    }

    pub fn unify_call(
        &mut self,
        call: &Call,
        b: Type,
        signature: &FunSignature,
        required: &[StrRef],
    ) -> Result<(), String> {
        let Call { posargs, kwargs, ret, fun } = call;
        let instantiated = self.instantiate_fun(b, &*signature);
        let r = self.get_ty(instantiated);
        let r = r.as_ref();
        let signature;
        if let TypeEnum::TFunc(s) = &*r {
            signature = s;
        } else {
            unreachable!();
        }
        // we check to make sure that all required arguments (those without default
        // arguments) are provided, and do not provide the same argument twice.
        let mut required = required.to_vec();
        let mut all_names: Vec<_> =
            signature.borrow().args.iter().map(|v| (v.name, v.ty)).rev().collect();
        for (i, t) in posargs.iter().enumerate() {
            if signature.borrow().args.len() <= i {
                return Err("Too many arguments.".to_string());
            }
            if !required.is_empty() {
                required.pop();
            }
            self.unify_impl(all_names.pop().unwrap().1, *t, false)?;
        }
        for (k, t) in kwargs.iter() {
            if let Some(i) = required.iter().position(|v| v == k) {
                required.remove(i);
            }
            let i = all_names
                .iter()
                .position(|v| &v.0 == k)
                .ok_or_else(|| format!("Unknown keyword argument {}", k))?;
            self.unify_impl(all_names.remove(i).1, *t, false)?;
        }
        if !required.is_empty() {
            return Err("Expected more arguments".to_string());
        }
        self.unify_impl(*ret, signature.borrow().ret, false)?;
        *fun.borrow_mut() = Some(instantiated);
        Ok(())
    }

    pub fn unify(&mut self, a: Type, b: Type) -> Result<(), String> {
        self.unify_cache.clear();
        if self.unification_table.unioned(a, b) {
            Ok(())
        } else {
            self.unify_impl(a, b, false)
        }
    }

    fn unify_impl(&mut self, a: Type, b: Type, swapped: bool) -> Result<(), String> {
        use TypeEnum::*;
        use TypeVarMeta::*;

        if !swapped {
            let rep_a = self.unification_table.get_representative(a);
            let rep_b = self.unification_table.get_representative(b);
            if rep_a == rep_b || self.unify_cache.contains(&(rep_a, rep_b)) {
                return Ok(());
            }
            self.unify_cache.insert((rep_a, rep_b));
        }

        let (ty_a, ty_b) = {
            (
                self.unification_table.probe_value(a).clone(),
                self.unification_table.probe_value(b).clone(),
            )
        };
        match (&*ty_a, &*ty_b) {
            (TVar { meta: meta1, range: range1, .. }, TVar { meta: meta2, range: range2, .. }) => {
                match (meta1, meta2) {
                    (Generic, _) => {}
                    (_, Generic) => {
                        return self.unify_impl(b, a, true);
                    }
                    (Record(fields1), Record(fields2)) => {
                        let mut fields2 = fields2.borrow_mut();
                        for (key, value) in fields1.borrow().iter() {
                            if let Some(ty) = fields2.get(key) {
                                self.unify_impl(*ty, *value, false)?;
                            } else {
                                fields2.insert(*key, *value);
                            }
                        }
                    }
                    (Sequence(map1), Sequence(map2)) => {
                        let mut map2 = map2.borrow_mut();
                        for (key, value) in map1.borrow().iter() {
                            if let Some(ty) = map2.get(key) {
                                self.unify_impl(*ty, *value, false)?;
                            } else {
                                map2.insert(*key, *value);
                            }
                        }
                    }
                    _ => {
                        return Err("Incompatible type variables".to_string());
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
                            if let Ok(result) = self.get_intersection(*v1, *v2) {
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
                // We check for the range of the type variable to see if unification is allowed.
                // Note that although b may be compatible with a, we may have to constrain type
                // variables in b to make sure that instantiations of b would always be compatible
                // with a.
                // The return value x of check_var_compatibility would be a new type that is
                // guaranteed to be compatible with a under all possible instantiations. So we
                // unify x with b to recursively apply the constrains, and then set a to x.
                let x = self.check_var_compatibility(*id, b, &range.borrow())?.unwrap_or(b);
                self.unify_impl(x, b, false)?;
                self.set_a_to_b(a, x);
            }
            (TVar { meta: Sequence(map), id, range, .. }, TTuple { ty }) => {
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
                    self.unify_impl(*v, ty[ind as usize], false)?;
                }
                let x = self.check_var_compatibility(*id, b, &range.borrow())?.unwrap_or(b);
                self.unify_impl(x, b, false)?;
                self.set_a_to_b(a, x);
            }
            (TVar { meta: Sequence(map), id, range, .. }, TList { ty }) => {
                for v in map.borrow().values() {
                    self.unify_impl(*v, *ty, false)?;
                }
                let x = self.check_var_compatibility(*id, b, &range.borrow())?.unwrap_or(b);
                self.unify_impl(x, b, false)?;
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
                    self.unify_impl(*x, *y, false)?;
                }
                self.set_a_to_b(a, b);
            }
            (TList { ty: ty1 }, TList { ty: ty2 }) => {
                self.unify_impl(*ty1, *ty2, false)?;
                self.set_a_to_b(a, b);
            }
            (TVar { meta: Record(map), id, range, .. }, TObj { fields, .. }) => {
                for (k, v) in map.borrow().iter() {
                    let ty = fields
                        .borrow()
                        .get(k)
                        .copied()
                        .ok_or_else(|| format!("No such attribute {}", k))?;
                    self.unify_impl(ty, *v, false)?;
                }
                let x = self.check_var_compatibility(*id, b, &range.borrow())?.unwrap_or(b);
                self.unify_impl(x, b, false)?;
                self.set_a_to_b(a, x);
            }
            (TVar { meta: Record(map), id, range, .. }, TVirtual { ty }) => {
                let ty = self.get_ty(*ty);
                if let TObj { fields, .. } = ty.as_ref() {
                    for (k, v) in map.borrow().iter() {
                        let ty = fields
                            .borrow()
                            .get(k)
                            .copied()
                            .ok_or_else(|| format!("No such attribute {}", k))?;
                        if !matches!(self.get_ty(ty).as_ref(), TFunc { .. }) {
                            return Err(format!("Cannot access field {} for virtual type", k));
                        }
                        self.unify_impl(*v, ty, false)?;
                    }
                } else {
                    // require annotation...
                    return Err("Requires type annotation for virtual".to_string());
                }
                let x = self.check_var_compatibility(*id, b, &range.borrow())?.unwrap_or(b);
                self.unify_impl(x, b, false)?;
                self.set_a_to_b(a, x);
            }
            (
                TObj { obj_id: id1, params: params1, .. },
                TObj { obj_id: id2, params: params2, .. },
            ) => {
                if id1 != id2 {
                    self.incompatible_types(a, b)?;
                }
                for (x, y) in zip(params1.borrow().values(), params2.borrow().values()) {
                    self.unify_impl(*x, *y, false)?;
                }
                self.set_a_to_b(a, b);
            }
            (TVirtual { ty: ty1 }, TVirtual { ty: ty2 }) => {
                self.unify_impl(*ty1, *ty2, false)?;
                self.set_a_to_b(a, b);
            }
            (TCall(calls1), TCall(calls2)) => {
                // we do not unify individual calls, instead we defer until the unification wtih a
                // function definition.
                calls2.borrow_mut().extend_from_slice(&calls1.borrow());
            }
            (TCall(calls), TFunc(signature)) => {
                let required: Vec<StrRef> = signature
                    .borrow()
                    .args
                    .iter()
                    .filter(|v| v.default_value.is_none())
                    .map(|v| v.name)
                    .rev()
                    .collect();
                // we unify every calls to the function signature.
                let signature = signature.borrow();
                for c in calls.borrow().iter() {
                    let call = self.calls[c.0].clone();
                    self.unify_call(&call, b, &signature, &required)?;
                }
                self.set_a_to_b(a, b);
            }
            (TFunc(sign1), TFunc(sign2)) => {
                let (sign1, sign2) = (&*sign1.borrow(), &*sign2.borrow());
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
                    if x.default_value != y.default_value {
                        return Err("Functions differ in optional parameters value".to_string());
                    }
                    self.unify_impl(x.ty, y.ty, false)?;
                }
                self.unify_impl(sign1.ret, sign2.ret, false)?;
                self.set_a_to_b(a, b);
            }
            _ => {
                if swapped {
                    return self.incompatible_types(a, b);
                } else {
                    self.unify_impl(b, a, true)?;
                }
            }
        }
        Ok(())
    }

    pub fn default_stringify(&mut self, ty: Type) -> String {
        let top_level = self.top_level.clone();
        self.stringify(
            ty,
            &mut |id| {
                top_level.as_ref().map_or_else(
                    || format!("{}", id),
                    |top_level| {
                        if let TopLevelDef::Class { name, .. } =
                            &*top_level.definitions.read()[id].read()
                        {
                            name.to_string()
                        } else {
                            unreachable!("expected class definition")
                        }
                    },
                )
            },
            &mut |id| format!("var{}", id),
        )
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
            TypeEnum::TRigidVar { id } => var_to_name(*id),
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
                let name = obj_to_name(obj_id.0);
                let params = params.borrow();
                if !params.is_empty() {
                    let params = params.iter().map(|(id, v)| {
                        format!("{}->{}", *id, self.stringify(*v, obj_to_name, var_to_name))
                    });
                    // sort to preserve order
                    let mut params = params.sorted();
                    format!("{}[{}]", name, params.join(", "))
                } else {
                    name
                }
            }
            TypeEnum::TCall { .. } => "call".to_owned(),
            TypeEnum::TFunc(signature) => {
                let params = signature
                    .borrow()
                    .args
                    .iter()
                    .map(|arg| {
                        format!("{}={}", arg.name, self.stringify(arg.ty, obj_to_name, var_to_name))
                    })
                    .join(", ");
                let ret = self.stringify(signature.borrow().ret, obj_to_name, var_to_name);
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

    fn incompatible_types(&mut self, a: Type, b: Type) -> Result<(), String> {
        Err(format!(
            "Cannot unify {} with {}",
            self.default_stringify(a),
            self.default_stringify(b)
        ))
    }

    /// Instantiate a function if it hasn't been instantiated.
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
    pub fn subst(&mut self, a: Type, mapping: &VarMap) -> Option<Type> {
        self.subst_impl(a, mapping, &mut HashMap::new())
    }

    fn subst_impl(
        &mut self,
        a: Type,
        mapping: &VarMap,
        cache: &mut HashMap<Type, Option<Type>>,
    ) -> Option<Type> {
        let cached = cache.get_mut(&a);
        if let Some(cached) = cached {
            if cached.is_none() {
                *cached = Some(self.get_fresh_var().0);
            }
            return *cached;
        }

        let ty = self.unification_table.probe_value(a).clone();
        // this function would only be called when we instantiate functions.
        // function type signature should ONLY contain concrete types and type
        // variables, i.e. things like TRecord, TCall should not occur, and we
        // should be safe to not implement the substitution for those variants.
        match &*ty {
            TypeEnum::TRigidVar { .. } => None,
            TypeEnum::TVar { id, .. } => mapping.get(id).cloned(),
            TypeEnum::TTuple { ty } => {
                let mut new_ty = Cow::from(ty);
                for (i, t) in ty.iter().enumerate() {
                    if let Some(t1) = self.subst_impl(*t, mapping, cache) {
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
                self.subst_impl(*ty, mapping, cache).map(|t| self.add_ty(TypeEnum::TList { ty: t }))
            }
            TypeEnum::TVirtual { ty } => self
                .subst_impl(*ty, mapping, cache)
                .map(|t| self.add_ty(TypeEnum::TVirtual { ty: t })),
            TypeEnum::TObj { obj_id, fields, params } => {
                // Type variables in field types must be present in the type parameter.
                // If the mapping does not contain any type variables in the
                // parameter list, we don't need to substitute the fields.
                // This is also used to prevent infinite substitution...
                let params = params.borrow();
                let need_subst = params.values().any(|v| {
                    let ty = self.unification_table.probe_value(*v);
                    if let TypeEnum::TVar { id, .. } = ty.as_ref() {
                        mapping.contains_key(id)
                    } else {
                        false
                    }
                });
                if need_subst {
                    cache.insert(a, None);
                    let obj_id = *obj_id;
                    let params =
                        self.subst_map(&params, mapping, cache).unwrap_or_else(|| params.clone());
                    let fields = self
                        .subst_map(&fields.borrow(), mapping, cache)
                        .unwrap_or_else(|| fields.borrow().clone());
                    let new_ty = self.add_ty(TypeEnum::TObj {
                        obj_id,
                        params: params.into(),
                        fields: fields.into(),
                    });
                    if let Some(var) = cache.get(&a).unwrap() {
                        self.unify_impl(new_ty, *var, false).unwrap();
                    }
                    Some(new_ty)
                } else {
                    None
                }
            }
            TypeEnum::TFunc(sig) => {
                let FunSignature { args, ret, vars: params } = &*sig.borrow();
                let new_params = self.subst_map(params, mapping, cache);
                let new_ret = self.subst_impl(*ret, mapping, cache);
                let mut new_args = Cow::from(args);
                for (i, t) in args.iter().enumerate() {
                    if let Some(t1) = self.subst_impl(t.ty, mapping, cache) {
                        let mut t = t.clone();
                        t.ty = t1;
                        new_args.to_mut()[i] = t;
                    }
                }
                if new_params.is_some() || new_ret.is_some() || matches!(new_args, Cow::Owned(..)) {
                    let params = new_params.unwrap_or_else(|| params.clone());
                    let ret = new_ret.unwrap_or_else(|| *ret);
                    let args = new_args.into_owned();
                    Some(
                        self.add_ty(TypeEnum::TFunc(
                            FunSignature { args, ret, vars: params }.into(),
                        )),
                    )
                } else {
                    None
                }
            }
            _ => {
                println!("{}", ty.get_type_name());
                unreachable!("{} not expected", ty.get_type_name())
            }
        }
    }

    fn subst_map<K>(
        &mut self,
        map: &Mapping<K>,
        mapping: &VarMap,
        cache: &mut HashMap<Type, Option<Type>>,
    ) -> Option<Mapping<K>>
    where
        K: std::hash::Hash + std::cmp::Eq + std::clone::Clone,
    {
        let mut map2 = None;
        for (k, v) in map.iter() {
            if let Some(v1) = self.subst_impl(*v, mapping, cache) {
                if map2.is_none() {
                    map2 = Some(map.clone());
                }
                *map2.as_mut().unwrap().get_mut(k).unwrap() = v1;
            }
        }
        map2
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
            "Cannot unify variable {} with {} due to incompatible value range",
            id,
            self.default_stringify(b)
        ));
    }
}
