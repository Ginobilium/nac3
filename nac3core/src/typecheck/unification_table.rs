use std::rc::Rc;

use itertools::izip;

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub struct UnificationKey(usize);

#[derive(Clone)]
pub struct UnificationTable<V> {
    parents: Vec<usize>,
    ranks: Vec<u32>,
    values: Vec<Option<V>>,
}

impl<V> Default for UnificationTable<V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<V> UnificationTable<V> {
    pub fn new() -> UnificationTable<V> {
        UnificationTable { parents: Vec::new(), ranks: Vec::new(), values: Vec::new() }
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
        self.parents[b] = a;
        if self.ranks[a] == self.ranks[b] {
            self.ranks[a] += 1;
        }
    }

    pub fn probe_value(&mut self, a: UnificationKey) -> &V {
        let index = self.find(a);
        self.values[index].as_ref().unwrap()
    }

    pub fn set_value(&mut self, a: UnificationKey, v: V) {
        let index = self.find(a);
        self.values[index] = Some(v);
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
            self.parents[root] = a;
            root = parent;
            // parent = root.parent
            parent = a;
        }
        parent
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
        UnificationTable { parents: self.parents.clone(), ranks: self.ranks.clone(), values }
    }

    pub fn from_send(table: &UnificationTable<V>) -> UnificationTable<Rc<V>> {
        let values = table.values.iter().cloned().map(|v| v.map(Rc::new)).collect();
        UnificationTable { parents: table.parents.clone(), ranks: table.ranks.clone(), values }
    }
}
