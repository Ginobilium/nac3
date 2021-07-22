use std::cell::RefCell;
use std::rc::Rc;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct UnificationKey(usize);

pub struct UnificationTable<V> {
    parents: Vec<usize>,
    ranks: Vec<u32>,
    values: Vec<V>,
}

impl<V> UnificationTable<V> {
    pub fn new() -> UnificationTable<V> {
        UnificationTable {
            parents: Vec::new(),
            ranks: Vec::new(),
            values: Vec::new(),
        }
    }

    pub fn new_key(&mut self, v: V) -> UnificationKey {
        let index = self.parents.len();
        self.parents.push(index);
        self.ranks.push(0);
        self.values.push(v);
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
        &self.values[index]
    }

    pub fn set_value(&mut self, a: UnificationKey, v: V) {
        let index = self.find(a);
        self.values[index] = v;
    }

    pub fn unioned(&mut self, a: UnificationKey, b: UnificationKey) -> bool {
        self.find(a) == self.find(b)
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

impl<V> UnificationTable<Rc<RefCell<V>>>
where
    V: Clone,
{
    pub fn into_send(self) -> UnificationTable<V> {
        let values = self
            .values
            .iter()
            .map(|v| v.as_ref().borrow().clone())
            .collect();
        UnificationTable {
            parents: self.parents,
            ranks: self.ranks,
            values,
        }
    }

    pub fn from_send(table: UnificationTable<V>) -> UnificationTable<Rc<RefCell<V>>> {
        let values = table
            .values
            .into_iter()
            .map(|v| Rc::new(RefCell::new(v)))
            .collect();
        UnificationTable {
            parents: table.parents,
            ranks: table.ranks,
            values,
        }
    }
}
