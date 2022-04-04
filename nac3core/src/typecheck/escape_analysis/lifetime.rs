use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use crate::typecheck::unification_table::{UnificationKey, UnificationTable};

use itertools::Itertools;
use nac3parser::ast::StrRef;

// change this to enum, only local needs unification key
pub type Lifetime = UnificationKey;

#[derive(Copy, Debug, Clone, PartialEq, Eq, Hash)]
pub enum LifetimeKind {
    // can be assigned to fields of anything
    // can be returned
    // lifetime of static values
    Global,
    // can only be assigned to fields of objects with local lifetime
    // can be returned
    // lifetime of parameters
    NonLocal,
    // can only be assigned to fields of objects with local lifetime
    // cannot be returned
    // lifetime of constructor return values
    Local,
    // can only be assigned to fields of objects with local lifetime
    // cannot be returned
    // lifetime of function return values
    Unknown,
}

impl std::ops::BitAnd for LifetimeKind {
    type Output = Self;

    fn bitand(self, other: Self) -> Self {
        use LifetimeKind::*;
        match (self, other) {
            (x, y) if x == y => x,
            (Global, NonLocal) | (NonLocal, Global) => NonLocal,
            _ => Unknown,
        }
    }
}

impl std::cmp::PartialOrd for LifetimeKind {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        use LifetimeKind::*;
        match (*self, *other) {
            (x, y) if x == y => Some(std::cmp::Ordering::Equal),
            (Local, _) | (_, Global) => Some(std::cmp::Ordering::Less),
            (_, Local) | (Global, _) => Some(std::cmp::Ordering::Greater),
            _ => None,
        }
    }
}

pub struct BlockLifetimeContext {
    mapping: Vec<(Option<Lifetime>, Lifetime)>,
}

impl BlockLifetimeContext {
    pub fn new() -> Self {
        BlockLifetimeContext { mapping: Vec::new() }
    }

    pub fn add_fresh(&mut self, lifetime: Lifetime) {
        self.mapping.push((None, lifetime));
    }
}

struct LifetimeEntry {
    kind: LifetimeKind,
    fields: RefCell<HashMap<StrRef, Lifetime>>,
}

pub struct LifetimeTable {
    table: UnificationTable<Rc<LifetimeEntry>>,
    cache: HashSet<(Lifetime, Lifetime)>,
}

impl LifetimeTable {
    pub fn new() -> Self {
        let mut zelf = Self { table: UnificationTable::new(), cache: Default::default() };
        zelf.table.new_key(Rc::new(LifetimeEntry {
            kind: LifetimeKind::Unknown,
            fields: Default::default(),
        }));
        zelf
    }

    pub fn add_lifetime(&mut self, kind: LifetimeKind) -> Lifetime {
        self.table.new_key(Rc::new(LifetimeEntry { kind, fields: Default::default() }))
    }

    pub fn unify(&mut self, a: Lifetime, b: Lifetime, ctx: &mut BlockLifetimeContext) {
        self.cache.clear();
        self.unify_impl(a, b, ctx);
    }

    fn get_scoped<const N: usize>(
        &mut self,
        mut lifetimes: [Lifetime; N],
        ctx: &mut BlockLifetimeContext,
    ) -> [Lifetime; N] {
        for l in lifetimes.iter_mut() {
            let mut result = None;
            for (k, v) in ctx.mapping.iter() {
                if self.table.unioned(*v, *l) || k.map_or(false, |k| self.table.unioned(k, *l)) {
                    // already fresh
                    result = Some(*v);
                    break;
                }
            }
            if let Some(result) = result {
                *l = result;
            } else {
                let lifetime = self.table.probe_value(*l).clone();
                *l = if lifetime.kind == LifetimeKind::Unknown {
                    UnificationKey(0)
                } else {
                    let k = self.table.new_key(lifetime);
                    ctx.mapping.push((Some(*l), k));
                    k
                }
            }
        }
        lifetimes
    }

    fn unify_impl(&mut self, a: Lifetime, b: Lifetime, ctx: &mut BlockLifetimeContext) {
        use LifetimeKind::*;

        let [a, b] = self.get_scoped([a, b], ctx);
        let a = self.table.get_representative(a);
        let b = self.table.get_representative(b);
        if a == b || self.cache.contains(&(a, b)) || self.cache.contains(&(b, a)) {
            return;
        }
        self.cache.insert((a, b));

        let v_a = self.table.probe_value(a).clone();
        let v_b = self.table.probe_value(b).clone();

        let result_kind = v_a.kind & v_b.kind;

        let fields = if result_kind == Local {
            // we only need to track fields lifetime for objects with local lifetime
            let fields = v_a.fields.clone();
            {
                let mut fields_ref = fields.borrow_mut();
                for (k, v) in v_b.fields.borrow().iter() {
                    if let Some(old) = fields_ref.insert(k.clone(), *v) {
                        self.unify_impl(old, *v, ctx);
                    }
                }
            }
            fields
        } else {
            Default::default()
        };

        self.table.unify(a, b);
        self.table.set_value(a, Rc::new(LifetimeEntry { kind: result_kind, fields }));
    }

    pub fn get_field_lifetime(
        &mut self,
        lifetime: Lifetime,
        field: StrRef,
        ctx: &mut BlockLifetimeContext,
    ) -> Lifetime {
        use LifetimeKind::*;
        let [lifetime] = self.get_scoped([lifetime], ctx);
        if let LifetimeEntry { kind: Local, fields } = &*self.table.probe_value(lifetime).clone() {
            if let Some(lifetime) = fields.borrow().get(&field) {
                *lifetime
            } else {
                // unknown lifetime
                // we can reuse this lifetime because it will never be unified to something else
                UnificationKey(0)
            }
        } else {
            lifetime
        }
    }

    pub fn set_field_lifetime(
        &mut self,
        obj: Lifetime,
        field: StrRef,
        lifetime: Lifetime,
        is_strong_update: bool,
        ctx: &mut BlockLifetimeContext,
    ) -> Result<(), String> {
        let [obj, lifetime] = self.get_scoped([obj, lifetime], ctx);
        let obj_lifetime = self.table.probe_value(obj).clone();
        let field_lifetime = self.table.probe_value(lifetime).clone();
        if !(obj_lifetime.kind <= field_lifetime.kind) {
            return Err("lifetime error".to_string());
        }
        let mut fields = obj_lifetime.fields.borrow_mut();
        if is_strong_update {
            fields.insert(field, lifetime);
        } else {
            if let Some(old) = fields.insert(field, lifetime) {
                self.unify(old, lifetime, ctx);
            }
        }
        Ok(())
    }

    pub fn get_lifetime_kind(
        &mut self,
        lifetime: Lifetime,
        ctx: &mut BlockLifetimeContext,
    ) -> LifetimeKind {
        let [lifetime] = self.get_scoped([lifetime], ctx);
        self.table.probe_value(lifetime).kind
    }

    pub fn set_function_params(&mut self, lifetime: Lifetime, ctx: &mut BlockLifetimeContext) {
        use LifetimeKind::*;
        // unify each field with global
        let [lifetime] = self.get_scoped([lifetime], ctx);
        let lifetime = self.table.probe_value(lifetime).clone();
        let mut worklist = lifetime.fields.borrow().values().copied().collect_vec();
        while let Some(item) = worklist.pop() {
            let [item] = self.get_scoped([item], ctx);
            let lifetime = self.table.probe_value(item).clone();
            if lifetime.kind == Unknown || lifetime.kind == Global {
                continue;
            }
            let fields = lifetime.fields.borrow().clone();
            for (_, v) in fields.iter() {
                worklist.push(*v);
            }
            self.table.set_value(
                item,
                Rc::new(LifetimeEntry {
                    kind: lifetime.kind & Global,
                    fields: RefCell::new(fields),
                }),
            );
        }
    }

    pub fn get_unknown_lifetime(&self) -> Lifetime {
        UnificationKey(0)
    }

    pub fn equiv(&mut self, a: Lifetime, b: Lifetime, ctx: &mut BlockLifetimeContext) -> bool {
        use LifetimeKind::Local;
        let [a, b] = self.get_scoped([a, b], ctx);
        if self.table.unioned(a, b) {
            return true;
        }
        let lifetime_a = self.table.probe_value(a).clone();
        let lifetime_b = self.table.probe_value(b).clone();
        if lifetime_a.kind == Local && lifetime_b.kind == Local {
            let fields_a = lifetime_a.fields.borrow();
            let fields_b = lifetime_b.fields.borrow();
            for (k, v) in fields_a.iter() {
                if fields_b.get(k).map(|v1| self.equiv(*v, *v1, ctx)) != Some(true) {
                    return false;
                }
            }
            // they are just equivalent
            // this can avoid infinite recursion
            self.table.unify(a, b);
            true
        } else {
            lifetime_a.kind == lifetime_b.kind
        }
    }
}
