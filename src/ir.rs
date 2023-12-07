use std::fmt::Display;
use std::process::id;
use fixedbitset::FixedBitSet;
use petgraph::graph::NodeIndex;
use petgraph::visit::NodeRef;
use crate::block::{Block, DataFlowGraph};
use crate::ir::BlockType::{Entry, Normal};

type GraphBlockID = NodeIndex<u32>;

#[derive(Debug, Clone, Copy)]
pub enum BlockType {
    Entry,
    Exit,
    Normal,
}

pub trait BlockIDGenerator {
    fn next_normal_id(&mut self) -> BlockType;
}

pub struct DefaultBlockIDGenerator {
    current_id: GraphBlockID,
}

type Variable = u32;

#[derive(Debug, Copy, Clone)]
pub struct AddressMarker {
    ir_index: u32,
    pub block_id: Option<GraphBlockID>,
}

impl AddressMarker {
    pub fn new(ir_index: u32) -> Self {
        Self { ir_index, block_id: None }
    }
}


#[derive(Debug, Copy, Clone)]
pub enum Value {
    Const(i32),
    Variable(u32),
    None,
}

#[derive(Debug, Copy, Clone)]
pub enum JumpType {
    Unconditional,
    LT,
    GT,
    E,
    NE,
    LTE,
    GTE,
    End,
}

#[derive(Debug, Copy, Clone)]
pub enum IR {
    Add(Variable, Value, Value),
    Sub(Variable, Value, Value),
    Mul(Variable, Value, Value),
    Div(Variable, Value, Value),
    Assign(Variable, Value),
    Jump(JumpType, AddressMarker, Value, Value),
    Equal(Variable, Value, Value),
    ArrayAccess(Variable, Value, Value),
    Exit,
}

pub struct BlockPartitioner {}

impl BlockPartitioner {
    pub fn generate_graph(src: Vec<IR>) -> DataFlowGraph<CodeBlock> {
        if src.is_empty() { return DataFlowGraph::new(); }
        let mut res = DataFlowGraph::new();
        let mut is_head = FixedBitSet::with_capacity(src.len());
        let mut assigned_block = vec![];
        assigned_block.resize(src.len(), NodeIndex::new(0));
        is_head.set(0, true);
        // Mark block head
        let mut set = |ir_index: usize| {
            is_head.set(ir_index, true);
            if ir_index + 1 < is_head.len() {
                is_head.set(ir_index + 1, true);
            }
        };
        let add_fallthrough = |i: usize, &current_index: &NodeIndex, assigned_block: &Vec<NodeIndex>, is_head: &FixedBitSet, res: &mut DataFlowGraph<CodeBlock>| {
            if i + 1 == assigned_block.len() || is_head[i + 1] {
                let fallthrough_index = if i + 1 == assigned_block.len() { res.exit } else { assigned_block[i + 1] };
                res.graph.add_edge(current_index, fallthrough_index, ());
            }
        };
        for ir in &src {
            match ir {
                IR::Jump(_, addr, _, _) => set(addr.ir_index as usize),
                _ => {}
            }
        }
        // Generate blocks
        let mut current_id = NodeIndex::new(0);
        for i in 0..src.len()
        {
            if is_head[i] {
                current_id = res.graph.add_node(CodeBlock::new(current_id, Normal, vec![]));
                res.graph.node_weight_mut(current_id).unwrap().id = current_id;
            }
            assigned_block[i] = current_id;
        }
        // Build graph
        res.graph.add_edge(res.entry, *assigned_block.first().unwrap(), ());
        let mut current_index = assigned_block[0];

        for (i, ir) in src.into_iter().enumerate()
        {
            if is_head[i] {
                current_index = assigned_block[i];
            }
            let mut new_ir = ir;
            match new_ir {
                IR::Jump(_, ref mut addr, _, _) => addr.block_id = Some(assigned_block[addr.ir_index as usize]),
                _ => {}
            }
            res.graph.node_weight_mut(current_index).unwrap().irs.push(new_ir);
            if let IR::Jump(jump_type, addr, _, _) = new_ir {
                if let Some(target_block_index) = addr.block_id {
                    res.graph.add_edge(current_index, target_block_index, ());
                }
                match jump_type {
                    JumpType::Unconditional => {}
                    JumpType::End => {
                        res.graph.add_edge(current_index, res.exit, ());
                    }
                    _ => {
                        add_fallthrough(i, &current_index, &assigned_block, &is_head, &mut res);
                    }
                }
            } else { add_fallthrough(i, &current_index, &assigned_block, &is_head, &mut res); }
        }
        res
    }
}

#[derive(Debug)]
pub struct CodeBlock {
    pub id: GraphBlockID,
    pub block_type: BlockType,
    pub irs: Vec<IR>,
}

impl CodeBlock {
    pub fn new(id: GraphBlockID, block_type: BlockType, irs: Vec<IR>) -> Self {
        Self { id, block_type, irs }
    }
}

impl Display for CodeBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let res = format!("Block {:?}: \n{:?}", self.id, self.irs);
        write!(f, "{}", res)
    }
}

impl Block for CodeBlock {
    fn entry() -> Self {
        Self { id: NodeIndex::default(), block_type: BlockType::Entry, irs: vec![] }
    }

    fn exit() -> Self {
        Self { id: NodeIndex::default(), block_type: BlockType::Exit, irs: vec![] }
    }

    fn set_node_index(&mut self, index: NodeIndex<u32>) {
        self.id = index
    }
}
