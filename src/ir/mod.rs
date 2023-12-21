use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::{Debug, Display, Formatter},
    ops::Deref,
    rc::{Rc, Weak},
};

use id_arena::{Arena, Id};
use petgraph::graph::NodeIndex;
mod lexer;
pub(crate) mod ops;
mod parser;
use ops::*;

use crate::{
    block::DataFlowGraph,
    util::{FromInner, MonotonicIdGenerator, RcRef, WeakRef, MultiKeyArenaHashMap}, semilattice::FlatLattice,
};

use self::block::{CodeBlock, CodeBlockAnalysisNode, CodeBlockGraphWeight, CodeBlockId};
pub mod block;

#[cfg(test)]
mod tests;

type GraphBlockID = NodeIndex<u32>;
type SpaceId = Id<Space>;
type SpaceNameId = usize;
type BlockNameId = usize;
type WeakSpaceRef = WeakRef<SpaceKind>;
type AddressMarkerRef = RcRef<AddressMarker>;

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum Scope {
    Global,
    Local { fn_name_id: SpaceNameId },
}

impl Display for Scope {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Scope::Global => write!(f, "global"),
            Scope::Local { fn_name_id } => write!(f, "{}", fn_name_id),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BlockType {
    Entry,
    Exit,
    Normal,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SpaceKind {
    Normal(Option<DataType>, Vec<SpaceNameId>),
    Offset(SpaceNameId, usize, Option<DataType>),
}
pub struct Space {
    pub kind: SpaceKind,
    pub scope: Scope,
    pub value: BoxedValue,
}
impl Display for SpaceKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SpaceKind::Normal(Some(ty), _) => write!(f, "{}", ty),
            SpaceKind::Offset(space, offset, _) => write!(f, "{}.{}", space, offset),
            SpaceKind::Normal(None, _) => write!(f, "Unknown"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct AddressMarker {
    pub block_id: GraphBlockID,
}
impl Display for AddressMarker {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.block_id.index())
    }
}

impl AddressMarker {
    pub fn new(block_id: GraphBlockID) -> Self {
        Self { block_id }
    }
}

pub trait Value: Debug {
    fn get_type(&self) -> DataType;
    fn apply(&mut self, op: Operation, other: Option<&impl Value>)
    where
        Self: Sized;
    fn static_cmp(&self, cmp: CompareType, other: Option<&impl Value>) -> bool
    where
        Self: Sized;
}
pub trait ValueCloneExt<'a>: Value + Clone {
    fn then(&'a self, op: Operation, other: Option<&impl Value>) -> Box<dyn Value + 'a>
    where
        Self: Sized;
}
impl<'a, T: 'a + Value + Clone> ValueCloneExt<'a> for T {
    fn then(&'_ self, op: Operation, other: Option<&impl Value>) -> Box<dyn Value + 'a>
    where
        Self: Sized,
    {
        let mut clone = self.clone();
        clone.apply(op, other);
        Box::new(clone)
    }
}
type BoxedValue = Box<dyn Value>;

#[derive(Debug, Clone)]
pub struct IntValue {
    pub value: FlatLattice<i64>,
}

impl Value for IntValue {
    fn get_type(&self) -> DataType {
        DataType::I64
    }
    fn apply(&mut self, op: Operation, other: Option<&impl Value>)
    where
        Self: Sized,
    {
        todo!()
    }

    fn static_cmp(&self, cmp: CompareType, other: Option<&impl Value>) -> bool
    where
        Self: Sized,
    {
        todo!()
    }
}
#[derive(Debug, Clone)]
pub struct VoidValue;

impl Value for VoidValue {
    fn get_type(&self) -> DataType {
        DataType::I64
    }

    fn apply(&mut self, op: Operation, other: Option<&impl Value>)
    where
        Self: Sized,
    {
        unimplemented!("Not supposed to use this value")
    }

    fn static_cmp(&self, cmp: CompareType, other: Option<&impl Value>) -> bool
    where
        Self: Sized,
    {
        unimplemented!("Not supposed to use this value")
    }
}

#[derive(Debug)]
#[allow(dead_code)]
pub enum Operation {
    Binary(BinaryOp, BoxedValue, BoxedValue),
    Unary(UnaryOp, BoxedValue),
}

#[derive(Debug)]
#[allow(dead_code)]
pub enum JumpOperation {
    Unconditional(AddressMarker),
    Branch(BoxedValue, AddressMarker, AddressMarker),
    Next,
    End,
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

#[derive(Debug)]
#[allow(dead_code)]
pub enum IR {
    Assignment(SpaceId, Operation, IRInformation),
    Jump(JumpOperation, IRInformation),
}

impl Display for IR {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            IR::Assignment(var, op, _) => match op {
                Operation::Binary(op, v1, v2) => {
                    write!(f, "{:?} <- {:?} {:?} {:?}", var, v1, op, v2)
                }
                Operation::Unary(op, v1) => {
                    write!(f, "{:?} <- {:?} {:?}", var, op, v1)
                }
            },
            IR::Jump(JumpOperation::Branch(v, true_br, false_br), info) => {
                write!(f, "=> {:?} ? {} : {}", v, true_br, false_br)
            }
            IR::Jump(JumpOperation::Unconditional(m), _) => write!(f, "=> {}", m),
            IR::Jump(JumpOperation::Next, _) => write!(f, "=> next"),
            IR::Jump(JumpOperation::End, _) => write!(f, "=> end"),
        }
    }
}

pub struct Function {
    pub name: String,
    pub params: Vec<SpaceNameId>,
    pub locals: MultiKeyArenaHashMap<SpaceNameId, Space>,
    pub local_names: HashMap<String, SpaceNameId>,
    pub blocks: MultiKeyArenaHashMap<BlockNameId, CodeBlock>,
    pub block_names: HashMap<String, BlockNameId>,
    pub graph: DataFlowGraph<CodeBlockAnalysisNode, CodeBlockGraphWeight>,
    program: ProgramRef,
}
pub type FunctionRef = RcRef<Function>;
pub struct Program {
    pub functions: Vec<Function>,
    pub globals: MultiKeyArenaHashMap<SpaceNameId, Space>,
    pub global_names: HashMap<String, SpaceNameId>,
    pub space_arena: RcRef<Arena<Space>>,
    pub block_arena: RcRef<Arena<CodeBlock>>,
    pub ir_arena: RcRef<Arena<IR>>,
    space_id_generator: MonotonicIdGenerator<SpaceNameId>,
    block_id_generator: MonotonicIdGenerator<BlockNameId>,
}
pub type ProgramRef = RcRef<Program>;
