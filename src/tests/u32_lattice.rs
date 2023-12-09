use petgraph::stable_graph::NodeIndex;

use crate::block::DataFlowGraph;

use crate::block::BlockTransfer;

use crate::block::Block;

use crate::block::BlockLattice;

use crate::semilattice::SemiLattice;

impl SemiLattice for u32 {
    fn meet(&self, other: &Self) -> Self {
        self | other
    }
}

pub(crate) type U32SemiLattice = u32;

pub(crate) struct U32Block {
    pub(crate) block_number: NodeIndex,
    pub(crate) in_value: U32SemiLattice,
    pub(crate) out_value: U32SemiLattice,
}

impl U32Block {
    pub(crate) fn new(
        block_number: NodeIndex,
        in_value: U32SemiLattice,
        out_value: U32SemiLattice,
    ) -> Self {
        Self {
            block_number,
            in_value,
            out_value,
        }
    }
}

impl BlockLattice<U32SemiLattice> for U32Block {
    fn get_in(&self) -> &U32SemiLattice {
        &self.in_value
    }

    fn set_in(&mut self, value: U32SemiLattice) {
        self.in_value = value
    }

    fn get_out(&self) -> &U32SemiLattice {
        &self.out_value
    }

    fn set_out(&mut self, value: U32SemiLattice) {
        self.out_value = value
    }
}

impl Block for U32Block {
    fn entry() -> Self {
        Self::new(0.into(), 0, 0)
    }

    fn exit() -> Self {
        Self::new(u32::MAX.into(), 0u32, 0u32)
    }

    fn set_node_index(&mut self, index: NodeIndex<u32>) {
        self.block_number = index;
    }
}

impl BlockTransfer<U32SemiLattice, U32Block, ()> for U32Block {
    fn transfer_forward(
        &self,
        _: &U32SemiLattice,
        graph: &crate::block::DataFlowGraph<U32Block>,
        _: petgraph::prelude::NodeIndex<u32>,
    ) -> U32SemiLattice {
        graph.graph.node_weights().fold(0, |cur, w| {
            if w.block_number == self.block_number {
                cur
            } else {
                cur | w.in_value
            }
        })
    }

    fn transfer_backward(
        &self,
        _: &U32SemiLattice,
        _: &crate::block::DataFlowGraph<U32Block>,
        _: petgraph::prelude::NodeIndex<u32>,
    ) -> U32SemiLattice {
        todo!()
    }

    fn entry_out(_: &DataFlowGraph<U32Block, ()>) -> U32SemiLattice {
        0
    }

    fn exit_in(_: &DataFlowGraph<U32Block, ()>) -> U32SemiLattice {
        0
    }

    fn top(_: &DataFlowGraph<U32Block, ()>) -> U32SemiLattice {
        0
    }

    fn bottom(_: &DataFlowGraph<U32Block, ()>) -> U32SemiLattice {
        u32::MAX
    }
}
