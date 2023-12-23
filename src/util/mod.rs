use std::{
    cell::{Ref, RefCell, RefMut},
    collections::HashMap,
    hash::Hash,
    ops::{Add, AddAssign, Deref, DerefMut},
    rc::{Rc, Weak},
};

use id_arena::{Arena, Id};
pub(crate) type RcRef<T> = Rc<RefCell<T>>;
pub(crate) type WeakRef<T> = Weak<RefCell<T>>;
pub(crate) trait FromInner<T> {
    fn from_inner(value: T) -> RcRef<T>;
}
impl<T> FromInner<T> for RcRef<T> {
    fn from_inner(value: T) -> RcRef<T> {
        Rc::new(RefCell::new(value))
    }
}
pub(crate) trait RefExt<'b, T> {
    fn filter_map<U, F>(&'b self, f: F) -> Result<Ref<'b, U>, Ref<'b, T>>
    where
        U: ?Sized,
        F: FnOnce(&T) -> Option<&U>,
        Self: Sized;
    fn filter_map_mut<U, F>(&'b self, f: F) -> Result<RefMut<'b, U>, RefMut<'b, T>>
    where
        U: ?Sized,
        F: FnOnce(&mut T) -> Option<&mut U>,
        Self: std::marker::Sized;
    fn map<U, F>(&'b self, f: F) -> Ref<'b, U>
    where
        U: ?Sized,
        F: FnOnce(&T) -> &U;
    fn map_mut<U, F>(&'b self, f: F) -> RefMut<'b, U>
    where
        U: ?Sized,
        F: FnOnce(&mut T) -> &mut U;
    fn filter_map2<U: Sized, F>(&'b self, f: F) -> Result<U, Ref<'b, T>>
    where
        F: FnOnce(&T) -> Option<U>,
        Self: Sized;
    fn filter_map_mut2<U: Sized, F>(&'b self, f: F) -> Result<U, RefMut<'b, T>>
    where
        F: FnOnce(&mut T) -> Option<U>,
        Self: Sized;
}
impl<'b, T> RefExt<'b, T> for RcRef<T> {
    fn filter_map<U, F>(&'b self, f: F) -> Result<Ref<'b, U>, Ref<'b, T>>
    where
        U: ?Sized,
        F: FnOnce(&T) -> Option<&U>,
        Self: std::marker::Sized,
    {
        Ref::filter_map(self.borrow(), f)
    }
    fn filter_map_mut<U, F>(&'b self, f: F) -> Result<RefMut<'b, U>, RefMut<'b, T>>
    where
        U: ?Sized,
        F: FnOnce(&mut T) -> Option<&mut U>,
        Self: std::marker::Sized,
    {
        RefMut::filter_map(self.borrow_mut(), f)
    }

    fn map<U, F>(&'b self, f: F) -> Ref<'b, U>
    where
        U: ?Sized,
        F: FnOnce(&T) -> &U,
    {
        Ref::map(self.borrow(), f)
    }
    fn map_mut<U, F>(&'b self, f: F) -> RefMut<'b, U>
    where
        U: ?Sized,
        F: FnOnce(&mut T) -> &mut U,
    {
        RefMut::map(self.borrow_mut(), f)
    }

    fn filter_map2<U: Sized, F>(&'b self, f: F) -> Result<U, Ref<'b, T>>
    where
        F: FnOnce(&T) -> Option<U>,
        Self: Sized,
    {
        let borrowed = self.borrow();
        match f(&*borrowed) {
            Some(value) => Ok(value),
            None => Err(borrowed),
        }
    }
    fn filter_map_mut2<U: Sized, F>(&'b self, f: F) -> Result<U, RefMut<'b, T>>
    where
        F: FnOnce(&mut T) -> Option<U>,
        Self: Sized,
    {
        let mut borrowed = self.borrow_mut();
        match f(&mut *borrowed) {
            Some(value) => Ok(value),
            None => Err(borrowed),
        }
    }
}

pub(crate) trait WeakFromInner<T> {
    fn from_inner(value: T) -> WeakRef<T>;
}
impl<T> WeakFromInner<T> for WeakRef<T> {
    fn from_inner(value: T) -> WeakRef<T> {
        Rc::downgrade(&RcRef::from_inner(value))
    }
}
pub fn include<T: Clone, U, R>(t: T, f: impl Fn(T, U) -> R) -> impl Fn(U) -> R {
    move |u| f(t.clone(), u)
}

pub struct MonotonicIdGenerator<T> {
    next_id: T,
    increment: T,
}
impl<T: Default> MonotonicIdGenerator<T> {
    pub fn new(increment: T) -> Self {
        Self {
            next_id: T::default(),
            increment,
        }
    }
}

impl<T: Default + Clone + AddAssign> MonotonicIdGenerator<T> {
    /// Increments the `next_id` value by the `increment` value and returns the
    /// updated `next_id`.
    ///
    /// Returns:
    /// Next unique ID
    pub fn generate(&mut self) -> T {
        let generated_id = self.next_id.clone();
        self.next_id += self.increment.clone();
        generated_id
    }
    pub fn peek_next_id(&mut self) -> T {
        self.next_id.clone()
    }
}

/// Combines a hashmap and an arena to store values
/// with multiple keys.
pub struct MultiKeyArenaHashMap<K: Hash + Eq + Copy, V> {
    map: HashMap<K, Id<V>>,
    arena: RcRef<Arena<V>>,
}

impl<K: Hash + Eq + Copy, V> MultiKeyArenaHashMap<K, V> {
    pub fn new(arena: RcRef<Arena<V>>) -> Self {
        Self {
            map: HashMap::new(),
            arena,
        }
    }
    pub fn get_id(&self, key: &K) -> Option<&Id<V>> {
        self.map.get(key)
    }
    pub fn get(&self, key: &K) -> Option<Ref<V>> {
        self.arena
            .filter_map(|arena| {
                let id = self.map.get(key)?;
                arena.get(*id)
            })
            .ok()
    }
    pub fn get_mut(&mut self, key: &K) -> Option<RefMut<V>> {
        self.arena
            .filter_map_mut(|arena| {
                let id = self.map.get_mut(key)?;
                arena.get_mut(*id)
            })
            .ok()
    }
    pub fn insert(&mut self, key: K, value: V) -> Id<V> {
        let id = self.arena.borrow_mut().alloc(value);
        self.map.insert(key, id);
        id
    }
    pub fn remove(&mut self, key: &K) -> Option<Id<V>> {
        self.map.remove(key)
    }
    pub fn len(&self) -> usize {
        self.map.len()
    }
    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }
    /// Removes a key-value pair from a map and inserts a new key-value
    /// pair with the same value as the redirected key.
    ///
    /// Arguments:
    ///
    /// * `key`: A reference to the key that needs to be redirected.
    /// * `redirect_to`: The `redirect_to` parameter is a reference to the key that the `key` parameter
    /// should be redirected to.
    ///
    /// Returns:
    ///
    /// The old value of the key that was redirected.
    /// If the key was not found in the map, `None` is returned.
    pub fn redirect(&mut self, key: &K, redirect_to: &K) -> Option<Id<V>> {
        let removed = self.map.remove(key);
        self.map.insert(*key, self.map[redirect_to]);
        removed
    }
    pub fn iter(&self) -> impl Iterator<Item = (&K, Ref<V>)> {
        self.map
            .iter()
            .map(|(k, v)| (k, Ref::map(self.arena.borrow(), |x| x.get(*v).unwrap())))
    }
    pub fn iter_mut(&mut self) -> impl Iterator<Item = (&K, RefMut<V>)> {
        self.map.iter_mut().map(|(k, v)| {
            (
                k,
                RefMut::map(self.arena.borrow_mut(), |x| x.get_mut(*v).unwrap()),
            )
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_monotonic_id_generator() {
        let mut generator = MonotonicIdGenerator::new(2);
        assert_eq!(generator.generate(), 0);
        assert_eq!(generator.peek_next_id(), 2);
        assert_eq!(generator.generate(), 2);
        assert_eq!(generator.peek_next_id(), 4);
    }
    #[test]
    fn test_multi_key_arena_hash_map() {
        let arena = RcRef::from_inner(Arena::new());
        let mut map = MultiKeyArenaHashMap::new(arena.clone());
        let id0 = map.insert(0, 0);
        assert_eq!(map.get_id(&0), Some(&id0));
        assert!(matches!(map.get(&0), Some(x) if *x == 0));
        assert!(matches!(map.get_mut(&0), Some(x) if *x == 0));

        let id1 = map.insert(1, 1);
        let id2 = map.insert(2, 2);
        assert!(matches!(map.get(&1), Some(x) if *x == 1));
        assert!(matches!(map.get(&2), Some(x) if *x == 2));

        map.redirect(&1, &2);
        assert!(matches!(map.get(&1), Some(x) if *x == 2));

        map.remove(&2);
        // Key not found
        assert!(map.get(&2).is_none());
        // Redirection still exists
        assert!(matches!(map.get(&1), Some(x) if *x == 2));
    }
}
