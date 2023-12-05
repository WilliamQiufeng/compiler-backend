use std::ops;

pub trait SemiLattice : PartialEq + PartialOrd {
    fn meet(&self, other: &Self) -> Self;
    fn top() -> Self;
    fn bottom() -> Self;
}
pub trait SemiLatticeLTE {
    fn lte(&self, other: &Self) -> bool;
}
impl <T: SemiLattice> SemiLatticeLTE for T {
    fn lte(&self, other: &Self) -> bool {
        self.meet(other) == *self
    }
}
