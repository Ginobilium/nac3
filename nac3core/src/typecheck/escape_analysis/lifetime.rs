use slab::Slab;
use std::borrow::Cow;
use std::collections::{HashMap, HashSet};

use nac3parser::ast::{Location, StrRef};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LifetimeKind {
    Static,
    NonLocal,
    Unknown,
    PreciseLocal,
    ImpreciseLocal,
}

impl std::ops::BitAnd for LifetimeKind {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        use LifetimeKind::*;
        match (self, rhs) {
            (x, y) if x == y => x,
            (PreciseLocal, ImpreciseLocal) | (ImpreciseLocal, PreciseLocal) => ImpreciseLocal,
            (Static, NonLocal) | (NonLocal, Static) => NonLocal,
            _ => Unknown,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LifetimeId(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BasicBlockId(usize);

#[derive(Debug, Clone)]
pub enum LifetimeIR {
    VarAssign { var: StrRef, lifetime: LifetimeId },
    VarAccess { var: StrRef },
    FieldAssign { obj: LifetimeId, field: StrRef, new: LifetimeId, is_init: bool },
    FieldAccess { obj: LifetimeId, field: StrRef },
    CreateLifetime { kind: LifetimeKind },
    PassedToFunc { param_lifetimes: Vec<LifetimeId> },
    UnifyLifetimes { lifetimes: Vec<LifetimeId> },
    Branch { targets: Vec<BasicBlockId> },
    Return { val: Option<LifetimeId> },
}

pub struct LifetimeIRBuilder {
    irs: Vec<Option<(LifetimeIR, Location)>>,
    basic_blocks: Vec<Vec<usize>>,
    current_block: BasicBlockId,
}

impl LifetimeIRBuilder {
    pub fn new() -> Self {
        LifetimeIRBuilder {
            irs: vec![None],
            basic_blocks: vec![vec![]],
            current_block: BasicBlockId(0),
        }
    }

    pub fn print_ir(&self) -> String {
        let mut lines = vec![];
        for (i, bb) in self.basic_blocks.iter().enumerate() {
            if bb.is_empty() {
                continue;
            }
            lines.push(format!("{}:", i));
            for ir in bb.iter() {
                if let Some((inst, loc)) = &self.irs[*ir] {
                    lines.push(format!("  {}: {:?} ({})", *ir, inst, loc));
                }
            }
        }
        lines.join("\n")
    }

    pub fn append_ir(&mut self, inst: LifetimeIR, loc: Location) -> LifetimeId {
        let id = self.irs.len();
        self.irs.push(Some((inst, loc)));
        self.basic_blocks[self.current_block.0].push(id);
        LifetimeId(id)
    }

    pub fn append_block(&mut self) -> BasicBlockId {
        let id = self.basic_blocks.len();
        self.basic_blocks.push(vec![]);
        BasicBlockId(id)
    }

    pub fn get_current_block(&self) -> BasicBlockId {
        self.current_block
    }

    pub fn position_at_end(&mut self, id: BasicBlockId) {
        self.current_block = id;
    }

    pub fn is_terminated(&self, id: BasicBlockId) -> bool {
        let bb = &self.basic_blocks[id.0];
        if bb.is_empty() {
            false
        } else {
            matches!(
                self.irs[*bb.last().unwrap()],
                Some((LifetimeIR::Return { .. }, _)) | Some((LifetimeIR::Branch { .. }, _))
            )
        }
    }

    pub fn remove_empty_bb(&mut self) {
        let mut destination_mapping = HashMap::new();
        let basic_blocks = &mut self.basic_blocks;
        let irs = &mut self.irs;
        for (i, bb) in basic_blocks.iter_mut().enumerate() {
            bb.retain(|&id| irs[id].is_some());
            if bb.len() == 1 {
                let id = bb.pop().unwrap();
                let ir = irs[id].take().unwrap();
                match ir.0 {
                    LifetimeIR::Branch { targets } => {
                        destination_mapping.insert(i, targets);
                    }
                    _ => (),
                }
            }
        }
        let mut buffer = HashSet::new();
        for bb in basic_blocks.iter_mut() {
            if bb.is_empty() {
                continue;
            }
            if let LifetimeIR::Branch { targets } =
                &mut irs[*bb.last().unwrap()].as_mut().unwrap().0
            {
                buffer.clear();
                let mut updated = false;
                for target in targets.iter() {
                    if let Some(dest) = destination_mapping.get(&target.0) {
                        buffer.extend(dest.iter().cloned());
                        updated = true;
                    } else {
                        buffer.insert(*target);
                    }
                }
                if updated {
                    targets.clear();
                    targets.extend(buffer.iter().cloned());
                }
            }
        }
    }

    pub fn analyze(&self) -> Result<(), String> {
        let mut analyzers = HashMap::new();
        analyzers.insert(0, (0, true, LifetimeAnalyzer::new()));
        let mut worklist = vec![0];
        while let Some(bb) = worklist.pop() {
            let (counter, updated, analyzer) = analyzers.get_mut(&bb).unwrap();
            *counter += 1;
            if *counter > 100 {
                return Err(format!("infinite loop detected at basic block {}", bb));
            }
            *updated = false;
            let mut analyzer = analyzer.clone();
            let block = &self.basic_blocks[bb];
            let ir_iter = block.iter().filter_map(|&id| {
                self.irs[id].as_ref().map(|(ir, loc)| (LifetimeId(id), ir, *loc))
            });
            if let Some(branch) = analyzer.analyze_basic_block(ir_iter)? {
                for &target in branch.iter() {
                    if let Some((_, updated, successor)) = analyzers.get_mut(&target.0) {
                        if successor.merge(&analyzer) && !*updated {
                            // changed
                            worklist.push(target.0);
                            *updated = true;
                        }
                    } else {
                        analyzers.insert(target.0, (0, true, analyzer.clone()));
                        worklist.push(target.0);
                    }
                }
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct LifetimeStore {
    kind: LifetimeKind,
    fields: HashMap<StrRef, LifetimeId>,
    lifetimes: HashSet<LifetimeId>,
}

#[derive(Debug, Clone)]
pub struct LifetimeAnalyzer<'a> {
    lifetime_to_id: HashMap<LifetimeId, usize>,
    lifetime_stores: Slab<Cow<'a, LifetimeStore>>,
    variable_assignment: HashMap<StrRef, LifetimeId>,
}

impl<'a> LifetimeAnalyzer<'a> {
    pub fn new() -> Self {
        let mut zelf = LifetimeAnalyzer {
            lifetime_to_id: HashMap::new(),
            lifetime_stores: Default::default(),
            variable_assignment: HashMap::new(),
        };
        zelf.add_lifetime(LifetimeId(0), LifetimeKind::Unknown);
        zelf
    }

    pub fn merge(&mut self, other: &LifetimeAnalyzer) -> bool {
        let mut to_be_merged = other.lifetime_to_id.keys().cloned().collect::<Vec<_>>();
        let mut updated = false;

        let mut lifetime_merge_list = vec![];
        for (&var_name, &lifetime) in other.variable_assignment.iter() {
            if let Some(&our_lifetime) = self.variable_assignment.get(&var_name) {
                if our_lifetime != lifetime {
                    lifetime_merge_list.push((our_lifetime, lifetime));
                }
            } else {
                self.variable_assignment.insert(var_name, lifetime);
                updated = true;
            }
        }

        while let Some(lifetime) = to_be_merged.pop() {
            let other_store_id = *other.lifetime_to_id.get(&lifetime).unwrap();
            if let Some(&self_store_id) = self.lifetime_to_id.get(&lifetime) {
                let self_store = self.lifetime_stores.get_mut(self_store_id).unwrap();
                let other_store = other.lifetime_stores.get(other_store_id).unwrap();
                let self_store = self_store.to_mut();
                // merge them
                for (&field, &other_lifetime) in other_store.fields.iter() {
                    if let Some(&self_lifetime) = self_store.fields.get(&field) {
                        if self_lifetime != other_lifetime {
                            lifetime_merge_list.push((self_lifetime, other_lifetime));
                        }
                    } else {
                        self_store.fields.insert(field, other_lifetime);
                        updated = true;
                    }
                }
                let zelf_lifetimes = &mut self_store.lifetimes;
                for &other_lifetime in other_store.lifetimes.iter() {
                    if zelf_lifetimes.insert(other_lifetime) {
                        lifetime_merge_list.push((lifetime, other_lifetime));
                    }
                }
                let result_kind = self_store.kind & other_store.kind;
                if self_store.kind != result_kind {
                    self_store.kind = result_kind;
                }
            } else {
                let store = other.lifetime_stores.get(other_store_id).unwrap().as_ref().clone();
                let store = self.lifetime_stores.insert(Cow::Owned(store));
                self.lifetime_to_id.insert(lifetime, store);
                updated = true;
            }
        }

        for (a, b) in lifetime_merge_list.into_iter() {
            self.unify(a, b);
        }

        updated
    }

    pub fn add_lifetime(&mut self, lifetime: LifetimeId, kind: LifetimeKind) {
        let id = self.lifetime_stores.insert(Cow::Owned(LifetimeStore {
            kind,
            fields: HashMap::new(),
            lifetimes: [lifetime].iter().cloned().collect(),
        }));
        let old_store_id = self.lifetime_to_id.insert(lifetime, id);
        if let Some(old_store_id) = old_store_id {
            let old_lifetime_store = self.lifetime_stores.get_mut(old_store_id).unwrap().to_mut();
            old_lifetime_store.lifetimes.remove(&lifetime);
            if old_lifetime_store.lifetimes.is_empty() {
                self.lifetime_stores.remove(old_store_id);
            }
        }
    }

    pub fn set_lifetime(&mut self, lifetime: LifetimeId, to: LifetimeId) {
        let id = *self.lifetime_to_id.get(&to).unwrap();
        let store = self.lifetime_stores.get_mut(id).unwrap();
        store.to_mut().lifetimes.insert(lifetime);
        let old_store_id = self.lifetime_to_id.insert(lifetime, id);
        if let Some(old_store_id) = old_store_id {
            let old_lifetime_store = self.lifetime_stores.get_mut(old_store_id).unwrap().to_mut();
            old_lifetime_store.lifetimes.remove(&lifetime);
            if old_lifetime_store.lifetimes.is_empty() {
                self.lifetime_stores.remove(old_store_id);
            }
        }
    }

    fn unify(&mut self, lhs: LifetimeId, rhs: LifetimeId) {
        use LifetimeKind::{ImpreciseLocal, PreciseLocal};
        let lhs_id = *self.lifetime_to_id.get(&lhs).unwrap();
        let rhs_id = *self.lifetime_to_id.get(&rhs).unwrap();
        if lhs_id == rhs_id {
            return;
        }
        let lhs_store = self.lifetime_stores.get(lhs_id).unwrap();
        let rhs_store = self.lifetime_stores.get(rhs_id).unwrap();
        let all_lifetimes: HashSet<_> =
            lhs_store.lifetimes.union(&rhs_store.lifetimes).cloned().collect();
        let result_kind = lhs_store.kind & rhs_store.kind;
        let fields = if matches!(result_kind, PreciseLocal | ImpreciseLocal) {
            let mut need_union = vec![];
            let mut fields = lhs_store.fields.clone();
            for (k, v) in rhs_store.fields.iter() {
                if let Some(old) = fields.insert(*k, *v) {
                    need_union.push((old, *v));
                }
            }
            drop(lhs_store);
            drop(rhs_store);
            for (lhs, rhs) in need_union {
                self.unify(lhs, rhs);
            }
            fields
        } else {
            Default::default()
        };
        // unify them, slow
        for lifetime in all_lifetimes.iter() {
            self.lifetime_to_id.insert(*lifetime, lhs_id);
        }
        *self.lifetime_stores.get_mut(lhs_id).unwrap() =
            Cow::Owned(LifetimeStore { kind: result_kind, fields, lifetimes: all_lifetimes });
        self.lifetime_stores.remove(rhs_id);
    }

    fn get_field_lifetime(&mut self, obj: LifetimeId, field: StrRef) -> LifetimeId {
        use LifetimeKind::*;
        let id = *self.lifetime_to_id.get(&obj).unwrap();
        let store = self.lifetime_stores.get(id).unwrap();
        if matches!(store.kind, PreciseLocal | ImpreciseLocal) {
            if let Some(&lifetime) = store.fields.get(&field) {
                let field_lifetime_kind = self.get_lifetime_kind(lifetime);
                if field_lifetime_kind == PreciseLocal
                    && (store.kind == ImpreciseLocal || field == "$elem".into())
                {
                    let id = *self.lifetime_to_id.get(&lifetime).unwrap();
                    self.lifetime_stores.get_mut(id).unwrap().to_mut().kind = ImpreciseLocal;
                }
                lifetime
            } else {
                LifetimeId(0)
            }
        } else {
            obj
        }
    }

    fn set_field_lifetime(
        &mut self,
        obj: LifetimeId,
        field: StrRef,
        field_lifetime: LifetimeId,
        is_init: bool,
    ) -> Result<(), String> {
        use LifetimeKind::*;
        let obj_id = *self.lifetime_to_id.get(&obj).unwrap();
        let field_id = *self.lifetime_to_id.get(&field_lifetime).unwrap();
        let field_lifetime_kind = self.lifetime_stores.get(field_id).unwrap().kind;
        let obj_store = self.lifetime_stores.get_mut(obj_id).unwrap();
        if !matches!(
            (obj_store.kind, field_lifetime_kind),
            (PreciseLocal, _) | (ImpreciseLocal, _) | (_, Static)
        ) {
            return Err("field lifetime error".into());
        }
        match obj_store.kind {
            // $elem means list elements
            PreciseLocal if field != "$elem".into() => {
                // strong update
                obj_store.to_mut().fields.insert(field, field_lifetime);
            }
            PreciseLocal | ImpreciseLocal => {
                // weak update
                let old_lifetime = obj_store.to_mut().fields.get(&field).copied();
                if let Some(old_lifetime) = old_lifetime {
                    self.unify(old_lifetime, field_lifetime);
                } else {
                    obj_store.to_mut().fields.insert(field, field_lifetime);
                    if !is_init {
                        // unify with unknown lifetime
                        self.unify(LifetimeId(0), field_lifetime);
                    }
                }
            }
            _ => (),
        }
        Ok(())
    }

    fn get_lifetime_kind(&self, lifetime: LifetimeId) -> LifetimeKind {
        self.lifetime_stores.get(*self.lifetime_to_id.get(&lifetime).unwrap()).unwrap().kind
    }

    fn pass_function_params(&mut self, lifetimes: &[LifetimeId]) {
        use LifetimeKind::*;
        let mut visited = HashSet::new();
        let mut worklist = vec![];

        fn add_fields_to_worklist(
            visited: &mut HashSet<LifetimeId>,
            worklist: &mut Vec<(LifetimeId, bool)>,
            fields: &HashMap<StrRef, LifetimeId>,
        ) {
            for (&name, &field) in fields.iter() {
                if visited.insert(field) {
                    // not visited previously
                    let name = name.to_string();
                    let mutable = !(name.starts_with("$elem") && name.len() != "$elem".len());
                    worklist.push((field, mutable));
                }
            }
        }

        for lifetime in lifetimes.iter() {
            let lifetime =
                self.lifetime_stores.get_mut(*self.lifetime_to_id.get(lifetime).unwrap()).unwrap();
            add_fields_to_worklist(&mut visited, &mut worklist, &lifetime.fields);
        }
        while let Some((item, mutable)) = worklist.pop() {
            let lifetime =
                self.lifetime_stores.get_mut(*self.lifetime_to_id.get(&item).unwrap()).unwrap();
            if matches!(lifetime.kind, Unknown | Static) {
                continue;
            }
            add_fields_to_worklist(&mut visited, &mut worklist, &lifetime.fields);
            if mutable {
                // we may assign values with static lifetime to function params
                lifetime.to_mut().kind = lifetime.kind & Static;
            }
        }
    }

    pub fn analyze_basic_block<'b, I: Iterator<Item = (LifetimeId, &'b LifetimeIR, Location)>>(
        &mut self,
        instructions: I,
    ) -> Result<Option<&'b [BasicBlockId]>, String> {
        use LifetimeIR::*;
        for (id, inst, loc) in instructions {
            match inst {
                VarAssign { var, lifetime } => {
                    self.variable_assignment.insert(*var, *lifetime);
                }
                VarAccess { var } => {
                    let lifetime = self.variable_assignment.get(var).cloned();
                    if let Some(lifetime) = lifetime {
                        self.set_lifetime(id, lifetime);
                    } else {
                        // should be static lifetime
                        self.add_lifetime(id, LifetimeKind::Static)
                    }
                }
                FieldAssign { obj, field, new, is_init } => {
                    self.set_field_lifetime(*obj, *field, *new, *is_init)
                        .map_err(|e| format!("{} in {}", e, loc))?;
                }
                FieldAccess { obj, field } => {
                    let lifetime = self.get_field_lifetime(*obj, *field);
                    self.set_lifetime(id, lifetime);
                }
                CreateLifetime { kind } => {
                    if *kind == LifetimeKind::Unknown {
                        self.set_lifetime(id, LifetimeId(0));
                    } else {
                        self.add_lifetime(id, *kind);
                    }
                }
                PassedToFunc { param_lifetimes } => {
                    self.pass_function_params(param_lifetimes);
                }
                UnifyLifetimes { lifetimes } => {
                    assert!(!lifetimes.is_empty());
                    let lhs = lifetimes[0];
                    for rhs in lifetimes[1..].iter() {
                        self.unify(lhs, *rhs);
                    }
                    self.set_lifetime(id, lhs);
                }
                Return { val } => {
                    if let Some(val) = val {
                        let kind = self.get_lifetime_kind(*val);
                        if !matches!(kind, LifetimeKind::Static | LifetimeKind::NonLocal) {
                            return Err(format!("return value lifetime error in {}", loc));
                        }
                    }
                    return Ok(None);
                }
                Branch { targets } => return Ok(Some(targets)),
            }
        }
        Ok(None)
    }
}
