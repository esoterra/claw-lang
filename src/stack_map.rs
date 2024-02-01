use std::cmp;
use std::collections::HashMap;
use std::hash::Hash;

#[derive(Debug, Clone, Default)]
pub struct StackMap<K, V> {
    mapping: HashMap<K, V>,
    history: Vec<(K, Option<V>)>,
}

pub struct StackMapCheckpoint(usize);

impl<K, V> From<HashMap<K, V>> for StackMap<K, V> {
    fn from(value: HashMap<K, V>) -> Self {
        Self {
            mapping: value,
            history: Default::default(),
        }
    }
}

impl<K, V> StackMap<K, V>
where
    K: Clone + cmp::Eq + Hash,
    V: Clone,
{
    pub fn insert(&mut self, key: K, value: V) {
        let previous = self.mapping.insert(key.clone(), value);
        self.history.push((key, previous));
    }

    pub fn checkpoint(&self) -> StackMapCheckpoint {
        StackMapCheckpoint(self.history.len())
    }

    pub fn restore(&mut self, checkpoint: StackMapCheckpoint) {
        while self.history.len() > checkpoint.0 {
            self.pop();
        }
    }

    fn pop(&mut self) {
        if let Some((key, value)) = self.history.pop() {
            if let Some(value) = value {
                self.mapping.insert(key, value);
            } else {
                self.mapping.remove(&key);
            }
        } else {
            panic!("pop called more than push");
        }
    }

    pub fn lookup(&self, key: &K) -> Option<&V> {
        self.mapping.get(key)
    }
}

#[cfg(test)]
mod tests {
    use super::StackMap;

    #[test]
    fn test_checkpoint_restore() {
        let mut map = StackMap::default();
        map.insert("a", 1);
        map.insert("b", 2);
        map.insert("c", 3);
        assert_eq!(map.lookup(&"a"), Some(&1));
        assert_eq!(map.lookup(&"b"), Some(&2));
        assert_eq!(map.lookup(&"c"), Some(&3));
        let checkpoint1 = map.checkpoint();

        map.insert("a", 4);
        map.insert("b", 5);
        map.insert("c", 6);
        assert_eq!(map.lookup(&"a"), Some(&4));
        assert_eq!(map.lookup(&"b"), Some(&5));
        assert_eq!(map.lookup(&"c"), Some(&6));
        let checkpoint2 = map.checkpoint();

        map.insert("a", 7);
        map.insert("b", 8);
        map.insert("c", 9);
        assert_eq!(map.lookup(&"a"), Some(&7));
        assert_eq!(map.lookup(&"b"), Some(&8));
        assert_eq!(map.lookup(&"c"), Some(&9));

        map.restore(checkpoint2);
        assert_eq!(map.lookup(&"a"), Some(&4));
        assert_eq!(map.lookup(&"b"), Some(&5));
        assert_eq!(map.lookup(&"c"), Some(&6));

        map.restore(checkpoint1);
        assert_eq!(map.lookup(&"a"), Some(&1));
        assert_eq!(map.lookup(&"b"), Some(&2));
        assert_eq!(map.lookup(&"c"), Some(&3));
    }
}
