use itertools::{zip, Itertools};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Display;
use std::rc::Rc;
use std::sync::{Arc, Mutex};
use std::{borrow::Cow, collections::HashSet};

use nac3parser::ast::{Location, StrRef};

use super::type_error::{TypeError, TypeErrorKind};
use super::unification_table::{UnificationKey, UnificationTable};
use crate::symbol_resolver::SymbolValue;
use crate::toplevel::{DefinitionId, TopLevelContext, TopLevelDef};

#[cfg(test)]
mod test;

/// Handle for a type, implementated as a key in the unification table.
pub type Type = UnificationKey;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct CallId(pub(super) usize);

pub type Mapping<K, V = Type> = HashMap<K, V>;
type VarMap = Mapping<u32>;

#[derive(Clone)]
pub struct Call {
    pub posargs: Vec<Type>,
    pub kwargs: HashMap<StrRef, Type>,
    pub ret: Type,
    pub fun: RefCell<Option<Type>>,
    pub loc: Option<Location>,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RecordKey {
    Str(StrRef),
    Int(i32),
}

impl Type {
    // a wrapper function for cleaner code so that we don't need to
    // write this long pattern matching just to get the field `obj_id`
    pub fn get_obj_id(self, unifier: &Unifier) -> DefinitionId {
        if let TypeEnum::TObj { obj_id, .. } = unifier.get_ty_immutable(self).as_ref() {
            *obj_id
        } else {
            unreachable!("expect a object type")
        }
    }
}

impl From<&RecordKey> for StrRef {
    fn from(r: &RecordKey) -> Self {
        match r {
            RecordKey::Str(s) => *s,
            RecordKey::Int(i) => StrRef::from(i.to_string()),
        }
    }
}

impl From<StrRef> for RecordKey {
    fn from(s: StrRef) -> Self {
        RecordKey::Str(s)
    }
}

impl From<&str> for RecordKey {
    fn from(s: &str) -> Self {
        RecordKey::Str(s.into())
    }
}

impl From<i32> for RecordKey {
    fn from(i: i32) -> Self {
        RecordKey::Int(i)
    }
}

impl Display for RecordKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RecordKey::Str(s) => write!(f, "{}", s),
            RecordKey::Int(i) => write!(f, "{}", i),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct RecordField {
    ty: Type,
    mutable: bool,
    loc: Option<Location>,
}

impl RecordField {
    pub fn new(ty: Type, mutable: bool, loc: Option<Location>) -> RecordField {
        RecordField { ty, mutable, loc }
    }
}

#[derive(Clone)]
pub enum TypeEnum {
    TRigidVar {
        id: u32,
        name: Option<StrRef>,
        loc: Option<Location>,
    },
    TVar {
        id: u32,
        // empty indicates this is not a struct/tuple/list
        fields: Option<Mapping<RecordKey, RecordField>>,
        // empty indicates no restriction
        range: Vec<Type>,
        name: Option<StrRef>,
        loc: Option<Location>,
    },
    TTuple {
        ty: Vec<Type>,
    },
    TList {
        ty: Type,
    },
    TObj {
        obj_id: DefinitionId,
        fields: Mapping<StrRef, (Type, bool)>,
        params: VarMap,
    },
    TVirtual {
        ty: Type,
    },
    TCall(Vec<CallId>),
    TFunc(FunSignature),
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
    pub(crate) top_level: Option<Arc<TopLevelContext>>,
    pub(crate) unification_table: UnificationTable<Rc<TypeEnum>>,
    pub(crate) calls: Vec<Rc<Call>>,
    var_id: u32,
    unify_cache: HashSet<(Type, Type)>,
    snapshot: Option<(usize, u32)>
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
            snapshot: None,
        }
    }

    pub unsafe fn get_unification_table(&mut self) -> &mut UnificationTable<Rc<TypeEnum>> {
        &mut self.unification_table
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
            snapshot: None,
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

    pub fn add_record(&mut self, fields: Mapping<RecordKey, RecordField>) -> Type {
        let id = self.var_id + 1;
        self.var_id += 1;
        self.add_ty(TypeEnum::TVar {
            id,
            range: vec![],
            fields: Some(fields),
            name: None,
            loc: None,
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
            Some(sign.clone())
        } else {
            None
        }
    }

    pub fn get_call_signature_immutable(&self, id: CallId) -> Option<FunSignature> {
        let fun = self.calls.get(id.0).unwrap().fun.borrow().unwrap();
        if let TypeEnum::TFunc(sign) = &*self.get_ty_immutable(fun) {
            Some(sign.clone())
        } else {
            None
        }
    }

    pub fn get_representative(&mut self, ty: Type) -> Type {
        self.unification_table.get_representative(ty)
    }

    /// Get the TypeEnum of a type.
    pub fn get_ty(&mut self, a: Type) -> Rc<TypeEnum> {
        self.unification_table.probe_value(a).clone()
    }

    pub fn get_ty_immutable(&self, a: Type) -> Rc<TypeEnum> {
        self.unification_table.probe_value_immutable(a).clone()
    }

    pub fn get_fresh_rigid_var(
        &mut self,
        name: Option<StrRef>,
        loc: Option<Location>,
    ) -> (Type, u32) {
        let id = self.var_id + 1;
        self.var_id += 1;
        (self.add_ty(TypeEnum::TRigidVar { id, name, loc }), id)
    }

    pub fn get_dummy_var(&mut self) -> (Type, u32) {
        self.get_fresh_var_with_range(&[], None, None)
    }

    pub fn get_fresh_var(&mut self, name: Option<StrRef>, loc: Option<Location>) -> (Type, u32) {
        self.get_fresh_var_with_range(&[], name, loc)
    }

    /// Get a fresh type variable.
    pub fn get_fresh_var_with_range(
        &mut self,
        range: &[Type],
        name: Option<StrRef>,
        loc: Option<Location>,
    ) -> (Type, u32) {
        let id = self.var_id + 1;
        self.var_id += 1;
        let range = range.to_vec();
        (self.add_ty(TypeEnum::TVar { id, range, fields: None, name, loc }), id)
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
                let (keys, params): (Vec<u32>, Vec<Type>) = params.iter().unzip();
                let params = params
                    .into_iter()
                    .map(|ty| self.get_instantiations(ty).unwrap_or_else(|| vec![ty]))
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
                                    &zip(keys.iter().cloned(), params.iter().cloned()).collect(),
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
                vars.values().all(|ty| self.is_concrete(*ty, allowed_typevars))
            }
            // functions are instantiated for each call sites, so the function type can contain
            // type variables.
            TFunc { .. } => true,
            TVirtual { ty } => self.is_concrete(*ty, allowed_typevars),
        }
    }

    fn restore_snapshot(&mut self) {
        if let Some(snapshot) = self.snapshot.take() {
            self.unification_table.restore_snapshot(snapshot);
        }
    }

    fn discard_snapshot(&mut self, snapshot: (usize, u32)) {
        if self.snapshot == Some(snapshot) {
            self.unification_table.discard_snapshot(snapshot);
            self.snapshot = None;
        }
    }

    pub fn unify_call(
        &mut self,
        call: &Call,
        b: Type,
        signature: &FunSignature,
        required: &[StrRef],
    ) -> Result<(), TypeError> {
        let snapshot = self.unification_table.get_snapshot();
        if self.snapshot.is_none() {
            self.snapshot = Some(snapshot);
        }

        let Call { posargs, kwargs, ret, fun, loc } = call;
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
        let mut all_names: Vec<_> = signature.args.iter().map(|v| (v.name, v.ty)).rev().collect();
        for (i, t) in posargs.iter().enumerate() {
            if signature.args.len() <= i {
                self.restore_snapshot();
                return Err(TypeError::new(
                    TypeErrorKind::TooManyArguments {
                        expected: signature.args.len(),
                        got: posargs.len() + kwargs.len(),
                    },
                    *loc,
                ));
            }
            required.pop();
            let (name, expected) = all_names.pop().unwrap();
            self.unify_impl(expected, *t, false).map_err(|_| {
                self.restore_snapshot();
                TypeError::new(TypeErrorKind::IncorrectArgType { name, expected, got: *t }, *loc)
            })?;
        }
        for (k, t) in kwargs.iter() {
            if let Some(i) = required.iter().position(|v| v == k) {
                required.remove(i);
            }
            let i = all_names
                .iter()
                .position(|v| &v.0 == k)
                .ok_or_else(|| {
                    self.restore_snapshot();
                    TypeError::new(TypeErrorKind::UnknownArgName(*k), *loc)
                })?;
            let (name, expected) = all_names.remove(i);
            self.unify_impl(expected, *t, false).map_err(|_| {
                self.restore_snapshot();
                TypeError::new(TypeErrorKind::IncorrectArgType { name, expected, got: *t }, *loc)
            })?;
        }
        if !required.is_empty() {
            self.restore_snapshot();
            return Err(TypeError::new(
                TypeErrorKind::MissingArgs(required.iter().join(", ")),
                *loc,
            ));
        }
        self.unify_impl(*ret, signature.ret, false).map_err(|mut err| {
            self.restore_snapshot();
            if err.loc.is_none() {
                err.loc = *loc;
            }
            err
        })?;
        *fun.borrow_mut() = Some(instantiated);

        self.discard_snapshot(snapshot);
        Ok(())
    }

    pub fn unify(&mut self, a: Type, b: Type) -> Result<(), TypeError> {
        let snapshot = self.unification_table.get_snapshot();
        if self.snapshot.is_none() {
            self.snapshot = Some(snapshot);
        }
        self.unify_cache.clear();
        if self.unification_table.unioned(a, b) {
            self.discard_snapshot(snapshot);
            Ok(())
        } else {
            let result = self.unify_impl(a, b, false);
            if result.is_err() {
                self.restore_snapshot();
            }
            self.discard_snapshot(snapshot);
            result
        }
    }

    fn unify_impl(&mut self, a: Type, b: Type, swapped: bool) -> Result<(), TypeError> {
        use TypeEnum::*;

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
            (
                TVar { fields: fields1, id, name: name1, loc: loc1, .. },
                TVar { fields: fields2, id: id2, name: name2, loc: loc2, .. },
            ) => {
                let new_fields = match (fields1, fields2) {
                    (None, None) => None,
                    (None, Some(fields)) => Some(fields.clone()),
                    (_, None) => {
                        return self.unify_impl(b, a, true);
                    }
                    (Some(fields1), Some(fields2)) => {
                        let mut new_fields: Mapping<_, _> = fields2.clone();
                        for (key, val1) in fields1.iter() {
                            if let Some(val2) = fields2.get(key) {
                                self.unify_impl(val1.ty, val2.ty, false).map_err(|_| {
                                    TypeError::new(
                                        TypeErrorKind::FieldUnificationError {
                                            field: *key,
                                            types: (val1.ty, val2.ty),
                                            loc: (*loc1, *loc2),
                                        },
                                        None,
                                    )
                                })?;
                                new_fields.insert(
                                    *key,
                                    RecordField::new(
                                        val1.ty,
                                        val1.mutable || val2.mutable,
                                        val1.loc.or(val2.loc),
                                    ),
                                );
                            } else {
                                new_fields.insert(*key, *val1);
                            }
                        }
                        Some(new_fields)
                    }
                };
                let intersection = self
                    .get_intersection(a, b)
                    .map_err(|_| TypeError::new(TypeErrorKind::IncompatibleTypes(a, b), None))?
                    .unwrap();
                let range = if let TypeEnum::TVar { range, .. } = &*self.get_ty(intersection) {
                    range.clone()
                } else {
                    unreachable!()
                };
                self.unification_table.unify(a, b);
                self.unification_table.set_value(
                    a,
                    Rc::new(TypeEnum::TVar {
                        id: name1.map_or(*id2, |_| *id),
                        fields: new_fields,
                        range,
                        name: name1.or(*name2),
                        loc: loc1.or(*loc2),
                    }),
                );
            }
            (TVar { fields: None, range, .. }, _) => {
                // We check for the range of the type variable to see if unification is allowed.
                // Note that although b may be compatible with a, we may have to constrain type
                // variables in b to make sure that instantiations of b would always be compatible
                // with a.
                // The return value x of check_var_compatibility would be a new type that is
                // guaranteed to be compatible with a under all possible instantiations. So we
                // unify x with b to recursively apply the constrains, and then set a to x.
                let x = self
                    .check_var_compatibility(b, range)
                    .map_err(|_| {
                        TypeError::new(TypeErrorKind::IncompatibleRange(b, range.clone()), None)
                    })?
                    .unwrap_or(b);
                self.unify_impl(x, b, false)?;
                self.set_a_to_b(a, x);
            }
            (TVar { fields: Some(fields), range, .. }, TTuple { ty }) => {
                let len = ty.len() as i32;
                for (k, v) in fields.iter() {
                    match *k {
                        RecordKey::Int(i) => {
                            if v.mutable {
                                return Err(TypeError::new(
                                    TypeErrorKind::MutationError(*k, b),
                                    v.loc,
                                ));
                            }
                            let ind = if i < 0 { len + i } else { i };
                            if ind >= len || ind < 0 {
                                return Err(TypeError::new(
                                    TypeErrorKind::TupleIndexOutOfBounds { index: i, len },
                                    v.loc,
                                ));
                            }
                            self.unify_impl(v.ty, ty[ind as usize], false)
                                .map_err(|e| e.at(v.loc))?;
                        }
                        RecordKey::Str(_) => {
                            return Err(TypeError::new(TypeErrorKind::NoSuchField(*k, b), v.loc))
                        }
                    }
                }
                let x = self.check_var_compatibility(b, range)?.unwrap_or(b);
                self.unify_impl(x, b, false)?;
                self.set_a_to_b(a, x);
            }
            (TVar { fields: Some(fields), range, .. }, TList { ty }) => {
                for (k, v) in fields.iter() {
                    match *k {
                        RecordKey::Int(_) => {
                            self.unify_impl(v.ty, *ty, false).map_err(|e| e.at(v.loc))?
                        }
                        RecordKey::Str(_) => {
                            return Err(TypeError::new(TypeErrorKind::NoSuchField(*k, b), v.loc))
                        }
                    }
                }
                let x = self.check_var_compatibility(b, range)?.unwrap_or(b);
                self.unify_impl(x, b, false)?;
                self.set_a_to_b(a, x);
            }
            (TTuple { ty: ty1 }, TTuple { ty: ty2 }) => {
                if ty1.len() != ty2.len() {
                    return Err(TypeError::new(TypeErrorKind::IncompatibleTypes(a, b), None));
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
            (TVar { fields: Some(map), range, .. }, TObj { fields, .. }) => {
                for (k, field) in map.iter() {
                    match *k {
                        RecordKey::Str(s) => {
                            let (ty, mutable) = fields.get(&s).copied().ok_or_else(|| {
                                TypeError::new(TypeErrorKind::NoSuchField(*k, b), field.loc)
                            })?;
                            // typevar represents the usage of the variable
                            // it is OK to have immutable usage for mutable fields
                            // but cannot have mutable usage for immutable fields
                            if field.mutable && !mutable {
                                return Err(TypeError::new(
                                    TypeErrorKind::MutationError(*k, b),
                                    field.loc,
                                ));
                            }
                            self.unify_impl(field.ty, ty, false).map_err(|v| v.at(field.loc))?;
                        }
                        RecordKey::Int(_) => {
                            return Err(TypeError::new(
                                TypeErrorKind::NoSuchField(*k, b),
                                field.loc,
                            ))
                        }
                    }
                }
                let x = self.check_var_compatibility(b, range)?.unwrap_or(b);
                self.unify_impl(x, b, false)?;
                self.set_a_to_b(a, x);
            }
            (TVar { fields: Some(map), range, .. }, TVirtual { ty }) => {
                let ty = self.get_ty(*ty);
                if let TObj { fields, .. } = ty.as_ref() {
                    for (k, field) in map.iter() {
                        match *k {
                            RecordKey::Str(s) => {
                                let (ty, _) = fields.get(&s).copied().ok_or_else(|| {
                                    TypeError::new(TypeErrorKind::NoSuchField(*k, b), field.loc)
                                })?;
                                if !matches!(self.get_ty(ty).as_ref(), TFunc { .. }) {
                                    return Err(TypeError::new(
                                        TypeErrorKind::NoSuchField(*k, b),
                                        field.loc,
                                    ));
                                }
                                if field.mutable {
                                    return Err(TypeError::new(
                                        TypeErrorKind::MutationError(*k, b),
                                        field.loc,
                                    ));
                                }
                                self.unify_impl(field.ty, ty, false)
                                    .map_err(|v| v.at(field.loc))?;
                            }
                            RecordKey::Int(_) => {
                                return Err(TypeError::new(
                                    TypeErrorKind::NoSuchField(*k, b),
                                    field.loc,
                                ))
                            }
                        }
                    }
                } else {
                    // require annotation...
                    return Err(TypeError::new(TypeErrorKind::RequiresTypeAnn, None));
                }
                let x = self.check_var_compatibility(b, range)?.unwrap_or(b);
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
                for (x, y) in zip(params1.values(), params2.values()) {
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
                let calls = calls1.iter().chain(calls2.iter()).cloned().collect();
                self.set_a_to_b(a, b);
                self.unification_table.set_value(b, Rc::new(TCall(calls)));
            }
            (TCall(calls), TFunc(signature)) => {
                let required: Vec<StrRef> = signature
                    .args
                    .iter()
                    .filter(|v| v.default_value.is_none())
                    .map(|v| v.name)
                    .rev()
                    .collect();
                // we unify every calls to the function signature.
                for c in calls.iter() {
                    let call = self.calls[c.0].clone();
                    self.unify_call(&call, b, signature, &required)?;
                }
                self.set_a_to_b(a, b);
            }
            (TFunc(sign1), TFunc(sign2)) => {
                if !sign1.vars.is_empty() || !sign2.vars.is_empty() {
                    return Err(TypeError::new(TypeErrorKind::PolymorphicFunctionPointer, None));
                }
                if sign1.args.len() != sign2.args.len() {
                    return Err(TypeError::new(TypeErrorKind::IncompatibleTypes(a, b), None));
                }
                for (x, y) in sign1.args.iter().zip(sign2.args.iter()) {
                    if x.name != y.name || x.default_value != y.default_value {
                        return Err(TypeError::new(TypeErrorKind::IncompatibleTypes(a, b), None));
                    }
                    self.unify_impl(x.ty, y.ty, false)?;
                }
                self.unify_impl(sign1.ret, sign2.ret, false)?;
                self.set_a_to_b(a, b);
            }
            (TVar { fields: Some(fields), .. }, _) => {
                let (k, v) = fields.iter().next().unwrap();
                return Err(TypeError::new(TypeErrorKind::NoSuchField(*k, b), v.loc));
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

    pub fn stringify(&self, ty: Type) -> String {
        self.stringify_with_notes(ty, &mut None)
    }

    pub fn stringify_with_notes(
        &self,
        ty: Type,
        notes: &mut Option<HashMap<u32, String>>,
    ) -> String {
        let top_level = self.top_level.clone();
        self.internal_stringify(
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
            &mut |id| format!("typevar{}", id),
            notes,
        )
    }

    /// Get string representation of the type
    pub fn internal_stringify<F, G>(
        &self,
        ty: Type,
        obj_to_name: &mut F,
        var_to_name: &mut G,
        notes: &mut Option<HashMap<u32, String>>,
    ) -> String
    where
        F: FnMut(usize) -> String,
        G: FnMut(u32) -> String,
    {
        let ty = self.unification_table.probe_value_immutable(ty).clone();
        match ty.as_ref() {
            TypeEnum::TRigidVar { id, name, .. } => {
                name.map(|v| v.to_string()).unwrap_or_else(|| var_to_name(*id))
            }
            TypeEnum::TVar { id, name, fields, range, .. } => {
                let n = if let Some(fields) = fields {
                    let mut fields = fields.iter().map(|(k, f)| {
                        format!(
                            "{}={}",
                            k,
                            self.internal_stringify(f.ty, obj_to_name, var_to_name, notes)
                        )
                    });
                    let fields = fields.join(", ");
                    format!(
                        "{}[{}]",
                        name.map(|v| v.to_string()).unwrap_or_else(|| var_to_name(*id)),
                        fields
                    )
                } else {
                    name.map(|v| v.to_string()).unwrap_or_else(|| var_to_name(*id))
                };
                if !range.is_empty() && notes.is_some() && !notes.as_ref().unwrap().contains_key(id)
                {
                    // just in case if there is any cyclic dependency
                    notes.as_mut().unwrap().insert(*id, "".into());
                    let body = format!(
                        "{} ∈ {{{}}}",
                        n,
                        range
                            .iter()
                            .map(|v| self.internal_stringify(*v, obj_to_name, var_to_name, notes))
                            .collect::<Vec<_>>()
                            .join(", ")
                    );
                    notes.as_mut().unwrap().insert(*id, body);
                };
                n
            }
            TypeEnum::TTuple { ty } => {
                let mut fields =
                    ty.iter().map(|v| self.internal_stringify(*v, obj_to_name, var_to_name, notes));
                format!("tuple[{}]", fields.join(", "))
            }
            TypeEnum::TList { ty } => {
                format!("list[{}]", self.internal_stringify(*ty, obj_to_name, var_to_name, notes))
            }
            TypeEnum::TVirtual { ty } => {
                format!(
                    "virtual[{}]",
                    self.internal_stringify(*ty, obj_to_name, var_to_name, notes)
                )
            }
            TypeEnum::TObj { obj_id, params, .. } => {
                let name = obj_to_name(obj_id.0);
                if !params.is_empty() {
                    let params = params
                        .iter()
                        .map(|(_, v)| self.internal_stringify(*v, obj_to_name, var_to_name, notes));
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
                    .args
                    .iter()
                    .map(|arg| {
                        if let Some(dv) = &arg.default_value {
                            format!(
                                "{}:{}={}",
                                arg.name,
                                self.internal_stringify(arg.ty, obj_to_name, var_to_name, notes),
                                dv
                            )
                        } else {
                            format!(
                                "{}:{}",
                                arg.name,
                                self.internal_stringify(arg.ty, obj_to_name, var_to_name, notes)
                            )
                        }
                    })
                    .join(", ");
                let ret = self.internal_stringify(signature.ret, obj_to_name, var_to_name, notes);
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

    fn incompatible_types(&mut self, a: Type, b: Type) -> Result<(), TypeError> {
        Err(TypeError::new(TypeErrorKind::IncompatibleTypes(a, b), None))
    }

    /// Instantiate a function if it hasn't been instantiated.
    /// Returns Some(T) where T is the instantiated type.
    /// Returns None if the function is already instantiated.
    fn instantiate_fun(&mut self, ty: Type, fun: &FunSignature) -> Type {
        let mut instantiated = true;
        let mut vars = Vec::new();
        for (k, v) in fun.vars.iter() {
            if let TypeEnum::TVar { id, name, loc, range, .. } =
                self.unification_table.probe_value(*v).as_ref()
            {
                // for class methods that contain type vars not in class declaration,
                // as long as there exits one uninstantiated type var, the function is not instantiated,
                // and need to do substitution on those type vars
                if k == id {
                    instantiated = false;
                    vars.push((*k, range.clone(), *name, *loc));
                }
            }
        }
        if instantiated {
            ty
        } else {
            let mapping = vars
                .into_iter()
                .map(|(k, range, name, loc)| {
                    (k, self.get_fresh_var_with_range(range.as_ref(), name, loc).0)
                })
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
                *cached = Some(self.get_fresh_var(None, None).0);
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
                        self.subst_map(params, mapping, cache).unwrap_or_else(|| params.clone());
                    let fields =
                        self.subst_map2(fields, mapping, cache).unwrap_or_else(|| fields.clone());
                    let new_ty = self.add_ty(TypeEnum::TObj { obj_id, params, fields });
                    if let Some(var) = cache.get(&a).unwrap() {
                        self.unify_impl(new_ty, *var, false).unwrap();
                    }
                    Some(new_ty)
                } else {
                    None
                }
            }
            TypeEnum::TFunc(FunSignature { args, ret, vars: params }) => {
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
                    Some(self.add_ty(TypeEnum::TFunc(FunSignature { args, ret, vars: params })))
                } else {
                    None
                }
            }
            _ => {
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

    fn subst_map2<K>(
        &mut self,
        map: &Mapping<K, (Type, bool)>,
        mapping: &VarMap,
        cache: &mut HashMap<Type, Option<Type>>,
    ) -> Option<Mapping<K, (Type, bool)>>
    where
        K: std::hash::Hash + std::cmp::Eq + std::clone::Clone,
    {
        let mut map2 = None;
        for (k, (v, mutability)) in map.iter() {
            if let Some(v1) = self.subst_impl(*v, mapping, cache) {
                if map2.is_none() {
                    map2 = Some(map.clone());
                }
                *map2.as_mut().unwrap().get_mut(k).unwrap() = (v1, *mutability);
            }
        }
        map2
    }

    fn get_intersection(&mut self, a: Type, b: Type) -> Result<Option<Type>, ()> {
        use TypeEnum::*;
        let x = self.get_ty(a);
        let y = self.get_ty(b);
        match (x.as_ref(), y.as_ref()) {
            (
                TVar { range: range1, name, loc, .. },
                TVar { fields, range: range2, name: name2, loc: loc2, .. },
            ) => {
                // new range is the intersection of them
                // empty range indicates no constraint
                if range1.is_empty() {
                    Ok(Some(b))
                } else if range2.is_empty() {
                    Ok(Some(a))
                } else {
                    let range = range2
                        .iter()
                        .cartesian_product(range1.iter())
                        .filter_map(|(v1, v2)| {
                            self.get_intersection(*v1, *v2).map(|v| v.unwrap_or(*v1)).ok()
                        })
                        .collect_vec();
                    if range.is_empty() {
                        Err(())
                    } else {
                        let id = self.var_id + 1;
                        self.var_id += 1;
                        let ty = TVar {
                            id,
                            fields: fields.clone(),
                            range,
                            name: name2.or(*name),
                            loc: loc2.or(*loc),
                        };
                        Ok(Some(self.unification_table.new_key(ty.into())))
                    }
                }
            }
            (_, TVar { range, .. }) => {
                // range should be restricted to the left hand side
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
            (TVar { range, .. }, _) => self.check_var_compatibility(b, range).or(Err(())),
            (TTuple { ty: ty1 }, TTuple { ty: ty2 }) if ty1.len() == ty2.len() => {
                let ty: Vec<_> = zip(ty1.iter(), ty2.iter())
                    .map(|(a, b)| self.get_intersection(*a, *b))
                    .try_collect()?;
                if ty.iter().any(Option::is_some) {
                    Ok(Some(self.add_ty(TTuple {
                        ty: zip(ty.into_iter(), ty1.iter()).map(|(a, b)| a.unwrap_or(*b)).collect(),
                    })))
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
            (TObj { obj_id: id1, .. }, TObj { obj_id: id2, .. }) if id1 == id2 => Ok(None),
            // don't deal with function shape for now
            _ => Err(()),
        }
    }

    fn check_var_compatibility(
        &mut self,
        b: Type,
        range: &[Type],
    ) -> Result<Option<Type>, TypeError> {
        if range.is_empty() {
            return Ok(None);
        }
        for t in range.iter() {
            let result = self.get_intersection(*t, b);
            if let Ok(result) = result {
                return Ok(result);
            }
        }
        Err(TypeError::new(TypeErrorKind::IncompatibleRange(b, range.to_vec()), None))
    }
}
