use std::rc::Rc;

use itertools::izip;

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub struct UnificationKey(usize);

#[derive(Clone)]
pub struct UnificationTable<V> {
    parents: Vec<usize>,
    ranks: Vec<u32>,
    values: Vec<Option<V>>,
    log: Vec<Action<V>>,
    generation: u32,
}

#[derive(Clone, Debug)]
enum Action<V> {
    Parent {
        key: usize,
        original_parent: usize,
    },
    Value {
        key: usize,
        original_value: Option<V>,
    },
    Rank {
        key: usize,
        original_rank: u32,
    },
    Marker {
        generation: u32,
    }
}

impl<V> Default for UnificationTable<V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<V> UnificationTable<V> {
    pub fn new() -> UnificationTable<V> {
        UnificationTable { parents: Vec::new(), ranks: Vec::new(), values: Vec::new(), log: Vec::new(), generation: 0 }
    }

    pub fn new_key(&mut self, v: V) -> UnificationKey {
        let index = self.parents.len();
        self.parents.push(index);
        self.ranks.push(0);
        self.values.push(Some(v));
        UnificationKey(index)
    }

    pub fn unify(&mut self, a: UnificationKey, b: UnificationKey) {
        let mut a = self.find(a);
        let mut b = self.find(b);
        if a == b {
            return;
        }
        if self.ranks[a] < self.ranks[b] {
            std::mem::swap(&mut a, &mut b);
        }
        self.log.push(Action::Parent { key: b, original_parent: self.parents[b] });
        self.parents[b] = a;
        if self.ranks[a] == self.ranks[b] {
            self.log.push(Action::Rank { key: a, original_rank: self.ranks[a] });
            self.ranks[a] += 1;
        }
    }

    pub fn probe_value_immutable(&self, key: UnificationKey) -> &V {
        let mut root = key.0;
        let mut parent = self.parents[root];
        while root != parent {
            root = parent;
            // parent = root.parent
            parent = self.parents[parent];
        }
        self.values[parent].as_ref().unwrap()
    }

    pub fn probe_value(&mut self, a: UnificationKey) -> &V {
        let index = self.find(a);
        self.values[index].as_ref().unwrap()
    }

    pub fn set_value(&mut self, a: UnificationKey, v: V) {
        let index = self.find(a);
        let original_value = self.values[index].replace(v);
        self.log.push(Action::Value { key: index, original_value });
    }

    pub fn unioned(&mut self, a: UnificationKey, b: UnificationKey) -> bool {
        self.find(a) == self.find(b)
    }

    pub fn get_representative(&mut self, key: UnificationKey) -> UnificationKey {
        UnificationKey(self.find(key))
    }

    fn find(&mut self, key: UnificationKey) -> usize {
        let mut root = key.0;
        let mut parent = self.parents[root];
        while root != parent {
            // a = parent.parent
            let a = self.parents[parent];
            // root.parent = parent.parent
            self.log.push(Action::Parent { key: root, original_parent: self.parents[root] });
            self.parents[root] = a;
            root = parent;
            // parent = root.parent
            parent = a;
        }
        parent
    }

    pub fn get_snapshot(&mut self) -> (usize, u32) {
        let generation = self.generation;
        self.log.push(Action::Marker { generation });
        self.generation += 1;
        (self.log.len(), generation)
    }

    pub fn restore_snapshot(&mut self, snapshot: (usize, u32)) {
        let (log_len, generation) = snapshot;
        assert!(self.log.len() >= log_len, "snapshot restoration error");
        assert!(matches!(self.log[log_len - 1], Action::Marker { generation: gen } if gen == generation), "snapshot restoration error");
        for action in self.log.drain(log_len - 1..).rev() {
            match action {
                Action::Parent { key, original_parent } => {
                    self.parents[key] = original_parent;
                }
                Action::Value { key, original_value } => {
                    self.values[key] = original_value;
                }
                Action::Rank { key, original_rank } => {
                    self.ranks[key] = original_rank;
                }
                Action::Marker { .. } => {}
            }
        }
    }

    pub fn discard_snapshot(&mut self, snapshot: (usize, u32)) {
        let (log_len, generation) = snapshot;
        assert!(self.log.len() >= log_len, "snapshot discard error");
        assert!(matches!(self.log[log_len - 1], Action::Marker { generation: gen } if gen == generation), "snapshot discard error");
        self.log.clear();
    }
}

impl<V> UnificationTable<Rc<V>>
where
    V: Clone,
{
    pub fn get_send(&self) -> UnificationTable<V> {
        let values = izip!(self.values.iter(), self.parents.iter())
            .enumerate()
            .map(|(i, (v, p))| if *p == i { v.as_ref().map(|v| v.as_ref().clone()) } else { None })
            .collect();
        UnificationTable { parents: self.parents.clone(), ranks: self.ranks.clone(), values, log: Vec::new(), generation: 0 }
    }

    pub fn from_send(table: &UnificationTable<V>) -> UnificationTable<Rc<V>> {
        let values = table.values.iter().cloned().map(|v| v.map(Rc::new)).collect();
        UnificationTable { parents: table.parents.clone(), ranks: table.ranks.clone(), values, log: Vec::new(), generation: 0 }
    }
}
