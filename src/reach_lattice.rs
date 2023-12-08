use fixedbitset::FixedBitSet;

use crate::{
    block::{BlockLattice, BlockTransfer},
    ir::CodeBlock,
    semilattice::SemiLattice,
};
use crate::ir::{CodeBlockGraphWeight, IR, IRInformation};

pub struct ReachLattice {
    value: FixedBitSet,
}
impl ReachLattice {
    pub fn new(capacity: usize) -> Self {
        Self {
            value: FixedBitSet::with_capacity(capacity),
        }
    }
    pub fn gen_var(ir_info: &IRInformation, code_block_graph_weight: CodeBlockGraphWeight) -> Self {
        let mut set = FixedBitSet::with_capacity(code_block_graph_weight.assignment_count);
        set.set(ir_info.declaration_number.expect("No declaration number"), true);
        Self {
            value: set
        }
    }
    pub fn kill_var(ir_info: &IRInformation, code_block_graph_weight: CodeBlockGraphWeight) -> Self {
        let mut set = FixedBitSet::with_capacity(code_block_graph_weight.assignment_count);
        set.set_range(.., true);
        set.set(ir_info.declaration_number.expect("No declaration number"), false);
        Self {
            value: set
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
        Self { value: res_value }
    }

    fn top() -> Self {
        todo!()
    }

    fn bottom() -> Self {
        todo!()
    }
}
impl BlockLattice<ReachLattice> for CodeBlock {
    fn get_in(&self) -> &ReachLattice {
        todo!()
    }

    fn set_in(&mut self, value: ReachLattice) {
        todo!()
    }

    fn get_out(&self) -> &ReachLattice {
        todo!()
    }

    fn set_out(&mut self, value: ReachLattice) {
        todo!()
    }
}
impl BlockTransfer<ReachLattice, CodeBlock, CodeBlockGraphWeight> for CodeBlock {
    fn transfer_forward(
        &self,
        graph: &crate::block::DataFlowGraph<CodeBlock, CodeBlockGraphWeight>,
        self_index: petgraph::prelude::NodeIndex<u32>,
    ) -> ReachLattice {
        todo!()
    }

    fn transfer_backward(
        &self,
        graph: &crate::block::DataFlowGraph<CodeBlock, CodeBlockGraphWeight>,
        self_index: petgraph::prelude::NodeIndex<u32>,
    ) -> ReachLattice {
        todo!()
    }
}
