mod semilattice;
mod block;
mod ir;
mod reach_lattice;
mod live_lattice;
fn main() {
    println!("Hello, world!");
}

#[cfg(test)]
mod tests {

    use crate::{semilattice::{SemiLattice, SemiLatticeLTE, SemiLatticeWrapper}, block::{Block, BlockTransfer, DataFlowGraph, BlockUpdate, BlockLattice}};

    impl SemiLattice for u32 {
        fn meet(&self, other: &Self) -> Self {
            self | other
        }

        fn top() -> Self {
            0
        }

        fn bottom() -> Self {
            u32::MAX
        }
    }
    type U32SemiLattice = SemiLatticeWrapper<u32>;
    struct U32Block {
        block_number : u32,
        in_value : U32SemiLattice,
        out_value : U32SemiLattice
    }
    impl U32Block {
        fn new(block_number: u32, in_value: U32SemiLattice, out_value: U32SemiLattice) -> Self {
            Self {
                block_number, in_value, out_value
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
            Self::new(0, U32SemiLattice::top(), U32SemiLattice::top())
        }
        
        fn exit() -> Self {
            Self::new(u32::MAX, U32SemiLattice::top(), U32SemiLattice::top())
        }
    }

    impl BlockTransfer<U32SemiLattice, U32Block> for U32Block {
        fn transfer_forward(
            &self,
            graph: &crate::block::DataFlowGraph<U32Block>,
            self_index: petgraph::prelude::NodeIndex<u32>,
        ) -> U32SemiLattice {
            graph.graph.node_weights().fold(0.into(), |cur, w| {
                if w.block_number == self.block_number{
                    cur
                } else {
                    (cur.0 | w.in_value.0).into()
                }
            })
        }

        fn transfer_backward(
            &self,
            graph: &crate::block::DataFlowGraph<U32Block>,
            self_index: petgraph::prelude::NodeIndex<u32>,
        ) -> U32SemiLattice {
            todo!()
        }
    }
    #[test]
    fn it_works() {
        let x = U32SemiLattice::from(0b101101u32);
        let y = U32SemiLattice::from(0b110011u32);
        let res = x.meet(&y);
        assert_eq!(res, 0b111111u32.into());
        assert_eq!(x.meet(&U32SemiLattice::top()), x);
        assert_eq!(x.meet(&U32SemiLattice::bottom()), U32SemiLattice::bottom());
        assert!(x < U32SemiLattice::top())
    }
    #[test]
    fn graph() {
        let mut graph = DataFlowGraph::<U32Block>::new();
        let b1 = graph.graph.add_node(U32Block::new(1, 0b10010.into(), 0b01101.into()));
        let b2 = graph.graph.add_node(U32Block::new(2, 0b01110.into(), 0b01110.into()));
        graph.graph.add_edge(graph.root, b1, ());
        graph.graph.add_edge(b1, b2, ());
        graph.converge(crate::block::Direction::Forward);
        graph.graph.node_indices().for_each(|i| {
            let node = graph.graph.node_weight(i).unwrap();
            println!("{}, {}", node.get_in().0, node.get_out().0);
        });
    }
}