use std::{
    cell::RefCell,
    rc::{Rc, Weak},
};

pub(crate) type Ref<T> = Rc<RefCell<T>>;
pub(crate) type WeakRef<T> = Weak<RefCell<T>>;
pub(crate) trait FromInner<T> {
    fn from_inner(value: T) -> Ref<T>;
}
impl<T> FromInner<T> for Ref<T> {
    fn from_inner(value: T) -> Ref<T> {
        Rc::new(RefCell::new(value))
    }
}

pub(crate) trait WeakFromInner<T> {
    fn from_inner(value: T) -> WeakRef<T>;
}
impl<T> WeakFromInner<T> for WeakRef<T> {
    fn from_inner(value: T) -> WeakRef<T> {
        Rc::downgrade(&Ref::from_inner(value))
    }
}
