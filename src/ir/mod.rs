use std::{fmt::{Debug, Display, Formatter}, cell::RefCell, rc::{Rc, Weak}, collections::HashMap};

use petgraph::graph::NodeIndex;
pub(crate) mod ops;
mod parser;
use ops::*;

use crate::{block::DataFlowGraph, util::{Ref, WeakRef}};

use self::block::{CodeBlock, CodeBlockRef, CodeBlockGraphWeight, CodeBlockAnalysisNode};
pub mod block;

#[cfg(test)]
mod tests;

type GraphBlockID = NodeIndex<u32>;
type SpaceRef = Ref<Space>;
type WeakSpaceRef = WeakRef<Space>;
type AddressMarkerRef = Ref<AddressMarker>;

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum Scope {
    Global,
    Local {fn_name: String},
}

impl Display for Scope {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Scope::Global => write!(f, "global"),
            Scope::Local { fn_name } => write!(f, "{}", fn_name),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BlockType {
    Entry,
    Exit,
    Normal,
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum Space {
    Normal(String, Option<DataType>, Scope),
    Offset(Box<Space>, u32)
}
impl Display for Space {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Space::Normal(name, ty, scope) => write!(f, "{}::{}", scope, name),
            Space::Offset(space, offset) => write!(f, "{}.{}", *space, offset),
        }
    }
}

#[derive(Debug, Clone)]
pub struct AddressMarker {
    pub block_name: String,
    pub block_id: Option<GraphBlockID>,
}
impl Display for AddressMarker {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.block_id {
            Some(id) => write!(f, "{:?}", id),
            None => write!(f, "{}", self.block_name),
        }
    }
}

impl AddressMarker {
    pub fn new(ir_index: u32) -> Self {
        Self {
            block_id: None,
            block_name: "".to_string()
        }
    }
}

pub trait Value: Debug {
    fn get_type(&self) -> DataType;
    fn apply(&self, op: Operation, other: Option<&impl Value>) -> BoxedValue
    where
        Self: Sized;
    fn get_storage_type(&self) -> StorageType;
}
type BoxedValue = Box<dyn Value>;

#[derive(Debug)]
pub struct IntValue {
    pub value: i64,
    pub storage_type: StorageType,
}

impl Value for IntValue {
    fn get_type(&self) -> DataType {
        DataType::I64
    }

    fn apply(&self, op: Operation, other: Option<&impl Value>) -> BoxedValue
    where
        Self: Sized,
    {
        todo!()
    }

    fn get_storage_type(&self) -> StorageType {
        self.storage_type
    }
}
#[derive(Debug, Clone)]
pub struct VoidValue;

impl Value for VoidValue {
    fn get_type(&self) -> DataType {
        DataType::I64
    }

    fn apply(&self, _: Operation, _: Option<&impl Value>) -> BoxedValue
    where
        Self: Sized,
    {
        unimplemented!("Not supposed to use this value")
    }

    fn get_storage_type(&self) -> StorageType {
        StorageType::Const
    }
}

#[derive(Debug, Copy, Clone)]
#[allow(dead_code)]
pub enum StorageType {
    Const,
    Variable(u32),
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
    Assignment(Space, Operation, IRInformation),
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

struct Function {
    name: String,
    blocks: Vec<CodeBlockRef>,
    graph: DataFlowGraph<CodeBlockAnalysisNode, CodeBlockGraphWeight>
}
struct Program {
    functions: Vec<Function>,
    globals: HashMap<String, SpaceRef>
}