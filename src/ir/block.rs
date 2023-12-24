use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{write, Debug, Display, Formatter};
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

use fixedbitset::FixedBitSet;
use id_arena::{Id, DefaultArenaBehavior, ArenaBehavior};
use petgraph::graph::NodeIndex;

type GraphBlockID = NodeIndex<u32>;

use crate::block::{Block, DataFlowGraph};
use crate::reach_lattice::ReachLattice;

use super::{AddressMarker, BlockType, IRInformation, JumpOperation, SpaceSignature, IR, SpaceId, BlockNameId};

pub type CodeBlockId = Id<CodeBlock>;


#[derive(Default)]
pub struct CodeBlockGraphWeight {
    pub assignment_count: usize,
    pub variable_assignment_map: HashMap<SpaceSignature, Vec<usize>>,
}



impl Display for CodeBlockGraphWeight {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "IRs:").expect("");
        Ok(())
    }
}


impl From<Vec<IR>> for DataFlowGraph<CodeBlockAnalysisNode, CodeBlockGraphWeight> {
    fn from(src: Vec<IR>) -> Self {
        let mut res = Self::new(CodeBlockGraphWeight::default());
        // res.weight.irs = src
        //     .into_iter()
        //     .map(|ir| Rc::new(RefCell::new(ir)))
        //     .collect();
        // if res.weight.irs.is_empty() {
        //     return res;
        // }
        // let mut is_head = FixedBitSet::with_capacity(res.weight.irs.len());
        // let mut assigned_block = vec![];
        // assigned_block.resize(res.weight.irs.len(), NodeIndex::new(0));
        // is_head.set(0, true);
        // // Mark block head
        // for (i, ir) in res.weight.irs.iter().enumerate() {
        //     if let IR::Jump(JumpOperation::Branch(_, addr1, addr2), _) = ir.borrow().deref() {
        //         set_head(addr1, &mut is_head, i);
        //         set_head(addr2, &mut is_head, i);
        //     }
        // }
        // // Generate blocks
        // let mut current_id = NodeIndex::new(0);
        // for i in 0..res.weight.irs.len() {
        //     if is_head[i] {
        //         current_id = res
        //             .graph
        //             .add_node(CodeBlock::new(current_id, BlockType::Normal, vec![]));
        //         res.graph.node_weight_mut(current_id).unwrap().id = current_id;
        //     }
        //     res.graph
        //         .node_weight_mut(current_id)
        //         .unwrap()
        //         .irs_range
        //         .push(res.weight.irs[i].clone());
        //     assigned_block[i] = current_id;
        // }
        // // Build graph
        // res.graph
        //     .add_edge(res.entry, *assigned_block.first().unwrap(), ());
        // let mut peek_iter = res.weight.irs.iter_mut().enumerate().peekable();
        // while let Some((i, ir)) = peek_iter.next() {
        //     let current_index = assigned_block[i];
        //     // Assign declaration number to assignment statements
        //     if let IR::Assignment(var, _, ref mut info) = ir.borrow_mut().deref_mut() {
        //         info.declaration_number = Some(res.weight.assignment_count);
        //         res.weight
        //             .variable_assignment_map
        //             .entry(*var)
        //             .or_default()
        //             .push(info.declaration_number.unwrap());
        //         res.weight.assignment_count += 1
        //     }
        //     // Decide fallthrough
        //     let fallthrough = match ir.borrow_mut().deref_mut() {
        //         // Jump to another address -> Fallthrough depends on unconditional jump or end
        //         IR::Jump(jump_type, _) => {
        //             // Update the desired block id accordingly
        //             let fallthrough = match jump_type {
        //                 JumpOperation::End => {
        //                     res.graph.add_edge(current_index, res.exit, ());
        //                     false
        //                 }
        //                 JumpOperation::Unconditional(addr) => {
        //                     add_edge(&mut res, current_index, &assigned_block, addr);
        //                     false
        //                 }
        //                 JumpOperation::Branch(_, addr1, addr2) => {
        //                     add_edge(&mut res, current_index, &assigned_block, addr1);
        //                     add_edge(&mut res, current_index, &assigned_block, addr2);
        //                     false
        //                 }
        //                 JumpOperation::Next => true,
        //             };
        //             fallthrough
        //         }
        //         // Other instructions, fall through
        //         _ => true,
        //     };
        //     // Add fallthrough instructions
        //     if !fallthrough {
        //         continue;
        //     }
        //     let fallthrough_block_index = match peek_iter.peek() {
        //         Some((peek_i, _)) => {
        //             // Next IR is not head of a block -> skip
        //             if !is_head[*peek_i] {
        //                 continue;
        //             };
        //             assigned_block[*peek_i]
        //         }
        //         // End of instructions
        //         _ => res.exit,
        //     };
        //     res.graph
        //         .add_edge(current_index, fallthrough_block_index, ());
        // }

        res
    }
}

#[derive(Debug)]
pub struct CodeBlock {
    pub id: CodeBlockId,
    pub block_type: BlockType,
    pub irs_range: Vec<IR>,
    pub terminator: IR,
}

pub struct CodeBlockAnalysisNode {
    pub block: CodeBlockId,
    pub reach_in: ReachLattice,
    pub reach_out: ReachLattice,
    pub node_index: NodeIndex
}

impl CodeBlock {
    pub fn new(
        id: CodeBlockId,
        block_type: BlockType,
        irs: Vec<IR>,
        terminator: IR,
    ) -> Self {
        Self {
            id,
            block_type,
            irs_range: irs,
            terminator,
        }
    }
}

impl Display for CodeBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Block {:?}:", self.id)?;
        self.irs_range
            .iter()
            .for_each(|ir| writeln!(f, "    {}", ir).expect(""));
        writeln!(f, "=> {}", self.terminator)?;
        Ok(())
    }
}
impl Display for CodeBlockAnalysisNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", self.block.index())?;
        
        writeln!(f, "IN = {}, OUT = {}", self.reach_in, self.reach_out)?;
        Ok(())
    }
}

impl Block for CodeBlockAnalysisNode {
    fn entry() -> Self {
        Self {
            block: DefaultArenaBehavior::new_id(0, 0),
            reach_in: ReachLattice::new(0),
            reach_out: ReachLattice::new(0),
            node_index: NodeIndex::new(0),
        }
    }

    fn exit() -> Self {
        Self {
            block: DefaultArenaBehavior::new_id(0, 0),
            reach_in: ReachLattice::new(0),
            reach_out: ReachLattice::new(0),
            node_index: NodeIndex::new(1),
        }
    }

    fn set_node_index(&mut self, index: NodeIndex<u32>) {
        self.node_index = index
    }
}
