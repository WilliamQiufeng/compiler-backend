use crate::{block::Block, reach_lattice::ReachLattice};

#[derive(Debug)]
pub enum Value {
    Const(i32),
    Variable(u32),
}
type Variable = u32;
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
pub struct CodeBlock {
    pub irs: Vec<IR>,
}
impl CodeBlock {
    pub fn new(irs: Vec<IR>) -> Self {
        Self { irs }
    }
}
impl Block for CodeBlock {
    fn entry() -> Self {
        Self { irs: vec![] }
    }

    fn exit() -> Self {
        Self { irs: vec![] }
    }
}
