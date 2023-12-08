use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::ops::{Deref, DerefMut};
use std::rc::Rc;
use std::slice::SliceIndex;

use fixedbitset::FixedBitSet;
use petgraph::graph::NodeIndex;

use crate::block::{Block, DataFlowGraph};
use crate::ir::BlockType::Normal;

type GraphBlockID = NodeIndex<u32>;

#[derive(Debug, Clone, Copy)]
pub enum BlockType {
    Entry,
    Exit,
    Normal,
}

type Variable = u32;

#[derive(Debug, Copy, Clone)]
pub struct AddressMarker {
    ir_index: u32,
    pub block_id: Option<GraphBlockID>,
}

impl AddressMarker {
    pub fn new(ir_index: u32) -> Self {
        Self {
            ir_index,
            block_id: None,
        }
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
    Bool,
    End,
}

#[derive(Debug, Copy, Clone)]
pub enum QuadType {
    Add,
    Sub,
    Mul,
    Div,
    LT,
    GT,
    E,
    NE,
    LTE,
    GTE,
    Not,
    And,
    Or,
    Xor,
    Assign,
}

#[derive(Debug, Copy, Clone, Default)]
pub struct IRInformation {
    pub declaration_number: Option<usize>,
}

impl Display for IRInformation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{").expect("Cannot write");
        if let Some(declaration_number) = self.declaration_number {
            write!(f, "DECL = {}", declaration_number).expect("Cannot write");
        }
        write!(f, "}}").expect("Cannot write");
        Ok(())
    }
}

#[derive(Debug, Copy, Clone)]
pub enum IR {
    Quad(QuadType, Variable, Value, Value, IRInformation),
    Jump(JumpType, AddressMarker, Value, Value, IRInformation),
}

impl Display for IR {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            IR::Quad(quad_type, var, v1, Value::None, _) => {
                write!(f, "{:?} <- {:?} {:?}", var, quad_type, v1)
            }
            IR::Quad(quad_type, var, v1, v2, _) => {
                write!(f, "{:?} <- {:?} {:?} {:?}", var, v1, quad_type, v2)
            }
            IR::Jump(JumpType::End, _, _, _, _) => {
                write!(f, "End")
            }
            IR::Jump(JumpType::Unconditional, addr, _, _, _) => {
                write!(f, "goto {:?}", addr)
            }
            IR::Jump(JumpType::Bool, addr, value, _, _) => {
                write!(f, "if {:?} goto {:?}", value, addr)
            }
            IR::Jump(jump_type, addr, v1, v2, _) => {
                write!(f, "if {:?} {:?} {:?} goto {:?}", v1, jump_type, v2, addr)
            }
        }
    }
}

pub struct CodeBlockGraphWeight {
    pub assignment_count: usize,
    pub variable_assignment_map: HashMap<Variable, Vec<usize>>,
    pub irs: Vec<Rc<RefCell<IR>>>,
}

impl CodeBlockGraphWeight {
    pub fn new() -> Self {
        Self {
            assignment_count: 0,
            variable_assignment_map: HashMap::new(),
            irs: vec![],
        }
    }
}

impl Display for CodeBlockGraphWeight {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "IRs:").expect("");
        for ir in &self.irs {
            writeln!(f, "{}", ir.borrow().deref()).expect("IR Err");
        }
        Ok(())
    }
}

impl DataFlowGraph<CodeBlock, CodeBlockGraphWeight> {}

impl From<Vec<IR>> for DataFlowGraph<CodeBlock, CodeBlockGraphWeight> {
    fn from(src: Vec<IR>) -> Self {
        let mut res = Self::new(CodeBlockGraphWeight::new());
        res.weight.irs = src
            .into_iter()
            .map(|ir| Rc::new(RefCell::new(ir)))
            .collect();
        if res.weight.irs.is_empty() {
            return res;
        }
        let mut is_head = FixedBitSet::with_capacity(res.weight.irs.len());
        let mut assigned_block = vec![];
        assigned_block.resize(res.weight.irs.len(), NodeIndex::new(0));
        is_head.set(0, true);
        // Mark block head
        for ir in &res.weight.irs {
            match ir.borrow().deref() {
                IR::Jump(_, addr, _, _, _) => {
                    let ir_index = addr.ir_index as usize;
                    is_head.set(ir_index, true);
                    if ir_index + 1 < is_head.len() {
                        is_head.set(ir_index + 1, true);
                    }
                }
                _ => {}
            }
        }
        // Generate blocks
        let mut current_id = NodeIndex::new(0);
        for i in 0..res.weight.irs.len() {
            if is_head[i] {
                current_id = res
                    .graph
                    .add_node(CodeBlock::new(current_id, Normal, vec![]));
                res.graph.node_weight_mut(current_id).unwrap().id = current_id;
            }
            res.graph
                .node_weight_mut(current_id)
                .unwrap()
                .irs_range
                .push(res.weight.irs[i].clone());
            assigned_block[i] = current_id;
        }
        // Build graph
        res.graph
            .add_edge(res.entry, *assigned_block.first().unwrap(), ());
        let mut peek_iter = res.weight.irs.iter_mut().enumerate().peekable();
        while let Some((i, ir)) = peek_iter.next() {
            let current_index = assigned_block[i];
            match ir.borrow_mut().deref_mut() {
                IR::Quad(_, var, _, _, ref mut info) => {
                    info.declaration_number = Some(res.weight.assignment_count);
                    res.weight
                        .variable_assignment_map
                        .entry(*var)
                        .or_default()
                        .push(info.declaration_number.unwrap());
                    res.weight.assignment_count += 1
                }
                _ => {}
            }
            // Decide fallthrough
            let fallthrough = match ir.borrow_mut().deref_mut() {
                // Jump to another address -> Fallthrough depends on unconditional jump or end
                IR::Jump(jump_type, ref mut addr, _, _, _) => {
                    // Update the desired block id accordingly
                    let mut target_block_index = assigned_block[addr.ir_index as usize];
                    let fallthrough = match jump_type {
                        JumpType::End => {
                            target_block_index = res.exit;
                            false
                        }
                        JumpType::Unconditional => false,
                        _ => true,
                    };
                    // Definitely having an edge to the target block
                    res.graph.add_edge(current_index, target_block_index, ());
                    // Fill target block index
                    addr.block_id = Some(target_block_index);
                    fallthrough
                }
                // Other instructions, fall through
                _ => true,
            };
            // Add fallthrough instructions
            if !fallthrough {
                continue;
            }
            let fallthrough_block_index = match peek_iter.peek() {
                Some((peek_i, _)) => {
                    // Next IR is not head of a block -> skip
                    if !is_head[*peek_i] {
                        continue;
                    };
                    assigned_block[*peek_i]
                }
                // End of instructions
                _ => res.exit,
            };
            res.graph
                .add_edge(current_index, fallthrough_block_index, ());
        }

        res
    }
}

#[derive(Debug)]
pub struct CodeBlock {
    pub id: GraphBlockID,
    pub block_type: BlockType,
    pub irs_range: Vec<Rc<RefCell<IR>>>,
}

impl CodeBlock {
    pub fn new(id: GraphBlockID, block_type: BlockType, irs: Vec<Rc<RefCell<IR>>>) -> Self {
        Self {
            id,
            block_type,
            irs_range: irs,
        }
    }
}

impl Display for CodeBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Block {:?}:", self.id.index());
        self.irs_range
            .iter()
            .for_each(|ir| writeln!(f, "  {}", ir.borrow().deref()).expect(""));
        Ok(())
    }
}

impl Block for CodeBlock {
    fn entry() -> Self {
        Self {
            id: NodeIndex::default(),
            block_type: BlockType::Entry,
            irs_range: vec![],
        }
    }

    fn exit() -> Self {
        Self {
            id: NodeIndex::default(),
            block_type: BlockType::Exit,
            irs_range: vec![],
        }
    }

    fn set_node_index(&mut self, index: NodeIndex<u32>) {
        self.id = index
    }
}
