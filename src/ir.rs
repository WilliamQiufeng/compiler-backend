use std::collections::HashSet;

use fixedbitset::FixedBitSet;

use crate::{block::Block, semilattice::SemiLattice};

#[derive(Debug)]
pub enum Value {
    Const(u32),
    Variable(Variable)
}
#[derive(Debug)]
pub struct Variable {
    name: String
}
#[derive(Debug)]
pub enum IR {
    Add(Variable, Value, Value),
    Sub(Variable, Value, Value),
    Mul(Variable, Value, Value),
    Div(Variable, Value, Value),
    Assign(Variable, Value),
    Jump(Value),
    LessThan(Variable, Value, Value),
    MoreThan(Variable, Value, Value),
    Equal(Variable, Value, Value),
    ArrayAccess(Variable, Value, Value),
}
pub struct ReachLattice {
    value: FixedBitSet
}
impl ReachLattice {
    pub fn new(capacity: usize) -> Self {
        Self {
            value: FixedBitSet::with_capacity(capacity)
        }
    }
}
impl PartialEq for ReachLattice {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}
impl PartialOrd for ReachLattice {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.value.partial_cmp(&other.value)
    }
}
impl SemiLattice for ReachLattice {
    fn meet(&self, other: &Self) -> Self {
        let mut res_value = FixedBitSet::with_capacity(self.value.len());
        res_value.union_with(&other.value);
        Self {
            value: res_value
        }
    }

    fn top() -> Self {
        todo!()
    }

    fn bottom() -> Self {
        todo!()
    }
}
pub struct CodeBlock {

}
