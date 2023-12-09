
pub trait SemiLattice: PartialEq {
    fn meet(&self, other: &Self) -> Self;
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

pub trait ProductLattice<SubLattice: SemiLattice, Ix = usize> : SemiLattice {
    fn get(&self, index: Ix) -> Option<&SubLattice>;
}

pub struct VecProductLattice<SubLattice: SemiLattice> {
    storage: Vec<SubLattice>
}

impl<SubLattice: SemiLattice> PartialEq for VecProductLattice<SubLattice> {
    fn eq(&self, other: &Self) -> bool {
        self.storage == other.storage
    }
}
impl<SubLattice: SemiLattice> SemiLattice for VecProductLattice<SubLattice> {
    fn meet(&self, other: &Self) -> Self {
        Self {
            storage: self.storage.iter().zip(other.storage.iter()).map(|(a, b)| a.meet(b)).collect()
        }
    }
}
impl<SubLattice: SemiLattice> ProductLattice<SubLattice, usize> for VecProductLattice<SubLattice> {
    fn get(&self, index: usize) -> Option<&SubLattice> {
        self.storage.get(index)
    }
}
