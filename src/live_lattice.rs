use fixedbitset::FixedBitSet;

use crate::block::DataFlowGraph;
use crate::ir::CodeBlockGraphWeight;
use crate::{
    block::{BlockLattice, BlockTransfer},
    ir::CodeBlock,
    semilattice::SemiLattice,
};

pub struct LiveLattice {
    value: FixedBitSet,
}
impl LiveLattice {
    pub fn new(capacity: usize) -> Self {
        Self {
            value: FixedBitSet::with_capacity(capacity),
        }
    }
}
impl PartialEq for LiveLattice {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}
impl PartialOrd for LiveLattice {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.value.partial_cmp(&other.value)
    }
}
impl SemiLattice for LiveLattice {
    fn meet(&self, other: &Self) -> Self {
        let mut res_value = FixedBitSet::with_capacity(self.value.len());
        res_value.union_with(&other.value);
        Self { value: res_value }
    }
}
impl BlockLattice<LiveLattice> for CodeBlock {
    fn get_in(&self) -> &LiveLattice {
        todo!()
    }

    fn set_in(&mut self, value: LiveLattice) {
        todo!()
    }

    fn get_out(&self) -> &LiveLattice {
        todo!()
    }

    fn set_out(&mut self, value: LiveLattice) {
        todo!()
    }
}
impl BlockTransfer<LiveLattice, CodeBlock, CodeBlockGraphWeight> for CodeBlock {
    fn transfer_forward(
        &self,
        in_value: &LiveLattice,
        graph: &crate::block::DataFlowGraph<CodeBlock, CodeBlockGraphWeight>,
        self_index: petgraph::prelude::NodeIndex<u32>,
    ) -> LiveLattice {
        todo!()
    }

    fn transfer_backward(
        &self,
        out_value: &LiveLattice,
        graph: &crate::block::DataFlowGraph<CodeBlock, CodeBlockGraphWeight>,
        self_index: petgraph::prelude::NodeIndex<u32>,
    ) -> LiveLattice {
        todo!()
    }

    fn entry_out(data_flow_graph: &DataFlowGraph<CodeBlock, CodeBlockGraphWeight>) -> LiveLattice {
        todo!()
    }

    fn exit_in(data_flow_graph: &DataFlowGraph<CodeBlock, CodeBlockGraphWeight>) -> LiveLattice {
        todo!()
    }

    fn top(data_flow_graph: &DataFlowGraph<CodeBlock, CodeBlockGraphWeight>) -> LiveLattice {
        todo!()
    }

    fn bottom(data_flow_graph: &DataFlowGraph<CodeBlock, CodeBlockGraphWeight>) -> LiveLattice {
        todo!()
    }
}
