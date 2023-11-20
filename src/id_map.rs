use std::marker::PhantomData;

use id_arena::Id;

#[derive(Clone, Debug)]
pub struct IdMap<K, V> {
    values: Vec<Option<V>>,
    _phantom: PhantomData<K>
}

impl<K, V> Default for IdMap<K, V> {
    fn default() -> Self {
        Self { values: Default::default(), _phantom: Default::default() }
    }
}

impl<K, V> IdMap<K, V> {
    pub fn insert(&mut self, key: Id<K>, value: V) {
        let index = key.index();
        if index >= self.values.len() {
            self.values.resize_with(index + 1, || None);
        }
        self.values[index] = Some(value);
    }

    pub fn get(&self, key: Id<K>) -> Option<&V> {
        match self.values.get(key.index()) {
            Some(opt) => opt.as_ref(),
            None => None,
        }
    }
}