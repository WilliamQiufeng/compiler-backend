use petgraph::{
    stable_graph::{NodeIndex, StableDiGraph},
    visit::Bfs,
};
use crate::ir::BlockType;

use crate::semilattice::{SemiLattice};

pub trait BlockLattice<SemiLatticeType: SemiLattice>: Block {
    fn get_in(&self) -> &SemiLatticeType;
    fn set_in(&mut self, value: SemiLatticeType);
    fn get_out(&self) -> &SemiLatticeType;
    fn set_out(&mut self, value: SemiLatticeType);
}

pub trait Block<Ix = u32> {
    fn entry() -> Self;
    fn exit() -> Self;
    fn set_node_index(&mut self, index: NodeIndex<Ix>);
}


#[derive(Clone, Copy, Debug)]
pub enum Direction {
    Forward = 0,
    Backward = 1,
}

pub trait BlockUpdate<T: SemiLattice> {
    fn update(&mut self, direction: Direction) -> bool;
    fn converge(&mut self, direction: Direction);
}

pub trait BlockTransfer<SemiLatticeType: SemiLattice, BlockType: Block>: Block {
    fn transfer_forward(
        &self,
        graph: &DataFlowGraph<BlockType>,
        self_index: NodeIndex<u32>,
    ) -> SemiLatticeType;
    fn transfer_backward(
        &self,
        graph: &DataFlowGraph<BlockType>,
        self_index: NodeIndex<u32>,
    ) -> SemiLatticeType;
}

pub trait FullBlock<SemiLatticeType: SemiLattice, T: Block>:
Block + BlockLattice<SemiLatticeType> + BlockTransfer<SemiLatticeType, T>
{}

impl<
    SemiLatticeType: SemiLattice,
    T: Block + BlockLattice<SemiLatticeType> + BlockTransfer<SemiLatticeType, T>,
> FullBlock<SemiLatticeType, T> for T
{}

#[derive(Debug)]
pub struct DataFlowGraph<BlockType: Block> {
    pub graph: StableDiGraph<BlockType, ()>,
    pub entry: NodeIndex<u32>,
    pub exit: NodeIndex<u32>,
}

impl<BlockType: Block> DataFlowGraph<BlockType> {
    pub fn new() -> Self {
        let mut graph = StableDiGraph::new();
        let entry = graph.add_node(BlockType::entry());
        let exit = graph.add_node(BlockType::exit());
        graph.node_weight_mut(entry).unwrap().set_node_index(entry);
        graph.node_weight_mut(exit).unwrap().set_node_index(exit);
        Self { graph, entry, exit }
    }
}

impl<SemiLatticeType, BlockType> BlockUpdate<SemiLatticeType> for DataFlowGraph<BlockType>
    where
        SemiLatticeType: SemiLattice,
        BlockType: FullBlock<SemiLatticeType, BlockType>,
{
    fn update(&mut self, direction: Direction) -> bool {
        let mut bfs = Bfs::new(&self.graph, self.entry);
        let mut changed = false;
        while let Some(nx) = bfs.next(&self.graph) {
            let (res_in, res_out) = match direction {
                Direction::Forward => {
                    let res_in = self
                        .graph
                        .neighbors_directed(nx, petgraph::Direction::Incoming)
                        .fold(SemiLatticeType::top(), |cur, neighbor_index| {
                            let block = &self.graph[neighbor_index];
                            cur.meet(block.get_out())
                        });
                    let res_out = self.graph[nx].transfer_forward(self, nx);
                    (res_in, res_out)
                }
                Direction::Backward => {
                    let res_out = self
                        .graph
                        .neighbors_directed(nx, petgraph::Direction::Outgoing)
                        .fold(SemiLatticeType::top(), |cur, neighbor_index| {
                            let block = &self.graph[neighbor_index];
                            cur.meet(block.get_in())
                        });
                    let res_in = self.graph[nx].transfer_backward(self, nx);
                    (res_in, res_out)
                }
            };

            changed = changed
                || res_in != *self.graph[nx].get_in()
                || res_out != *self.graph[nx].get_out();
            self.graph[nx].set_in(res_in);
            self.graph[nx].set_out(res_out);
        }
        changed
    }

    fn converge(&mut self, direction: Direction) {
        let mut changed = true;
        while changed {
            changed = self.update(direction);
        }
    }
}
