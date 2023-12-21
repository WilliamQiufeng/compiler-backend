pub trait SemiLattice: PartialEq {
    fn meet(&self, other: &Self) -> Self;
    fn meet_with(&mut self, other: &Self) -> bool;
}
pub trait Lattice: SemiLattice + Ord {
    fn join(&self, other: &Self) -> Self;
}
pub trait SemiLatticeOrd {
    fn lte(&self, other: &Self) -> bool;
    fn lt(&self, other: &Self) -> bool;
}

impl<T: SemiLattice + PartialEq> SemiLatticeOrd for T {
    fn lte(&self, other: &Self) -> bool {
        self.meet(other) == *self
    }

    fn lt(&self, other: &Self) -> bool {
        self.lte(other) && self != other
    }
}

pub trait ProductLattice<SubLattice: SemiLattice, Ix = usize>: SemiLattice {
    fn get(&self, index: Ix) -> Option<&SubLattice>;
}

#[derive(Clone)]
pub struct VecProductLattice<SubLattice: SemiLattice + Clone> {
    storage: Vec<SubLattice>,
}

impl<SubLattice: SemiLattice + Clone> PartialEq for VecProductLattice<SubLattice> {
    fn eq(&self, other: &Self) -> bool {
        self.storage == other.storage
    }
}
impl<SubLattice: SemiLattice + Clone> SemiLattice for VecProductLattice<SubLattice> {
    fn meet(&self, other: &Self) -> Self {
        let mut res = self.clone();
        res.meet_with(other);
        res
    }

    fn meet_with(&mut self, other: &Self) -> bool {
        self.storage.iter_mut().enumerate().fold(false, |cur, (i, e)| {
            cur || e.meet_with(other.get(i).unwrap())
        })
    }
}
impl<SubLattice: SemiLattice + Clone> ProductLattice<SubLattice, usize> for VecProductLattice<SubLattice> {
    fn get(&self, index: usize) -> Option<&SubLattice> {
        self.storage.get(index)
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum FlatLattice<T: PartialEq + Clone> {
    Top,
    Bottom,
    Value(T),
}

impl<T: PartialEq + Clone> SemiLattice for FlatLattice<T> {
    fn meet(&self, other: &Self) -> Self {
        let mut copy = self.clone();
        copy.meet_with(other);
        copy
    }

    fn meet_with(&mut self, other: &Self) -> bool {
        let res = match (&*self, other) {
            (FlatLattice::Value(ref a), FlatLattice::Value(ref b)) if a == b => return false,
            (FlatLattice::Bottom, _) | (_, FlatLattice::Top) => return false,
            (FlatLattice::Top, FlatLattice::Value(b)) => Self::Value(b.clone()),
            _ => FlatLattice::Bottom
        };
        *self = res;
        true
    }
}
