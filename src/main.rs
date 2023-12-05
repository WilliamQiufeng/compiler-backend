mod semilattice;
mod block;
mod ir;
fn main() {
    println!("Hello, world!");
}

#[cfg(test)]
mod tests {

    use crate::{semilattice::{SemiLattice, SemiLatticeLTE}, block::{Block, BlockTransfer, DataFlowGraph, BlockUpdate, BlockLattice}};

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
    struct U32Block {
        block_number : u32,
        in_value : u32,
        out_value : u32
    }
    impl U32Block {
        fn new(block_number: u32, in_value: u32, out_value: u32) -> Self {
            Self {
                block_number, in_value, out_value
            }
        }
    }
    impl BlockLattice<u32> for U32Block {
        fn get_in(&self) -> &u32 {
            &self.in_value
        }

        fn set_in(&mut self, value: &u32) {
            self.in_value = *value
        }

        fn get_out(&self) -> &u32 {
            &self.out_value
        }

        fn set_out(&mut self, value: &u32) {
            self.out_value = *value
        }
    }
    impl Block for U32Block {

        fn entry() -> Self {
            Self::new(0, u32::top(), u32::top())
        }
    }
    impl BlockTransfer<u32, U32Block> for U32Block {
        fn transfer_forward(
            &self,
            graph: &crate::block::DataFlowGraph<u32, U32Block>,
            self_index: petgraph::prelude::NodeIndex<u32>,
        ) -> u32 {
            graph.graph.node_weights().fold(0, |cur, w| {
                if w.block_number == self.block_number{
                    cur
                } else {
                    cur | w.in_value
                }
            })
        }

        fn transfer_backward(
            &self,
            graph: &crate::block::DataFlowGraph<u32, U32Block>,
            self_index: petgraph::prelude::NodeIndex<u32>,
        ) -> u32 {
            todo!()
        }
    }
    #[test]
    fn it_works() {
        let x = 0b101101u32;
        let y = 0b110011u32;
        let res = x.meet(&y);
        assert_eq!(res, 0b111111u32);
        assert_eq!(x.meet(&u32::top()), x);
        assert_eq!(x.meet(&u32::bottom()), u32::bottom());
        assert!(x.lte(&u32::top()))
    }
    #[test]
    fn graph() {
        let mut graph = DataFlowGraph::<u32, U32Block>::new();
        let b1 = graph.graph.add_node(U32Block::new(1, 0b10010, 0b01101));
        let b2 = graph.graph.add_node(U32Block::new(2, 0b01110, 0b01110));
        graph.graph.add_edge(graph.root, b1, ());
        graph.graph.add_edge(b1, b2, ());
        graph.converge(crate::block::Direction::Forward);
        graph.graph.node_indices().for_each(|i| {
            let node = graph.graph.node_weight(i).unwrap();
            println!("{}, {}", node.get_in(), node.get_out());
        });
    }
}