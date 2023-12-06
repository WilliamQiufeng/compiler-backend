use std::ops;

pub trait SemiLattice: PartialEq {
    fn meet(&self, other: &Self) -> Self;
    fn top() -> Self;
    fn bottom() -> Self;
}
pub trait Lattice: SemiLattice + Ord {
    fn join(&self, other: &Self) -> Self;
}
pub trait SemiLatticeLTE {
    fn lte(&self, other: &Self) -> bool;
}

#[derive(Debug)]
pub struct SemiLatticeWrapper<T: SemiLattice>(pub T);
impl<T: SemiLattice> SemiLattice for SemiLatticeWrapper<T> {
    fn meet(&self, other: &Self) -> Self {
        Self(self.0.meet(&other.0))
    }

    fn top() -> Self {
        Self(T::top())
    }

    fn bottom() -> Self {
        Self(T::bottom())
    }
}
impl<T: SemiLattice> PartialEq for SemiLatticeWrapper<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}
impl<T: SemiLattice> PartialOrd for SemiLatticeWrapper<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self.0.meet(&other.0) == self.0 {
            Some(std::cmp::Ordering::Less)
        } else if self.0 == other.0 {
            Some(std::cmp::Ordering::Equal)
        } else {
            None
        }
    }
}
impl<T: SemiLattice> From<T> for SemiLatticeWrapper<T> {
    fn from(t: T) -> Self {
        Self(t)
    }
}
