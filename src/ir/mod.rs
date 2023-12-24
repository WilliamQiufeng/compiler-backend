use std::{
    cell::{Ref, RefCell, RefMut},
    collections::HashMap,
    fmt::{Debug, Display, Formatter},
    ops::Deref,
    rc::{Rc, Weak},
};

use id_arena::{Arena, Id};
use petgraph::graph::NodeIndex;
pub mod lexer;
pub(crate) mod ops;
pub mod parser;
use ops::*;

use crate::{
    block::DataFlowGraph,
    semilattice::{FlatLattice, SemiLattice},
    util::{
        FromInner, MonotonicIdGenerator, MonotonicNameMap, MonotonicNamedPool,
        MultiKeyArenaHashMap, RcRef, RefExt, WeakRef,
    },
};

use self::block::{CodeBlock, CodeBlockAnalysisNode, CodeBlockGraphWeight, CodeBlockId};
pub mod block;

#[cfg(test)]
mod tests;

type GraphBlockID = NodeIndex<u32>;
type SpaceId = Id<Space>;
type SpaceNameId = usize;
type FunctionNameId = usize;
type FunctionId = Id<Function>;
type BlockNameId = usize;
type WeakSpaceRef = WeakRef<SpaceSignature>;
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
pub enum SpaceSignature {
    Normal(Option<DataType>, Vec<SpaceNameId>),
    Offset(SpaceNameId, usize, Option<DataType>, Vec<SpaceNameId>),
}
pub struct Space {
    pub signature: SpaceSignature,
    pub scope: Scope,
    pub value: FlatLattice<Value>,
}
impl Display for SpaceSignature {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SpaceSignature::Normal(Some(ty), _) => write!(f, "{}", ty),
            SpaceSignature::Offset(space, offset, _, _) => write!(f, "{}.{}", space, offset),
            SpaceSignature::Normal(None, _) => write!(f, "Unknown"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct AddressMarker {
    pub block_id: BlockNameId,
}
impl Display for AddressMarker {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.block_id)
    }
}

impl AddressMarker {
    pub fn new(block_id: BlockNameId) -> Self {
        Self { block_id }
    }
}

pub trait Literal: Debug {
    fn get_type(&self) -> DataType;
    fn binary(&mut self, op: BinaryOp, other: Option<Self>) -> Self
    where
        Self: Sized;
    fn unary(&mut self, op: UnaryOp, other: Option<Self>) -> Self
    where
        Self: Sized;
    fn static_cmp(&self, cmp: CompareType, other: Option<Self>) -> bool
    where
        Self: Sized;
}
#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Int(IntValue),
    Void,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IntValue {
    pub value: i64,
}

impl Literal for IntValue {
    fn get_type(&self) -> DataType {
        DataType::I64
    }

    fn binary(&mut self, op: BinaryOp, other: Option<Self>) -> Self
    where
        Self: Sized,
    {
        match op {
            BinaryOp::Add => IntValue {
                value: self.value + other.unwrap().value,
            },
            BinaryOp::Sub => todo!(),
            BinaryOp::Mul => todo!(),
            BinaryOp::Div => todo!(),
            BinaryOp::And => todo!(),
            BinaryOp::Or => todo!(),
            BinaryOp::Xor => todo!(),
        }
    }

    fn static_cmp(&self, cmp: CompareType, other: Option<Self>) -> bool
    where
        Self: Sized,
    {
        todo!()
    }

    fn unary(&mut self, op: UnaryOp, other: Option<Self>) -> Self
    where
        Self: Sized,
    {
        todo!()
    }
}

#[derive(Debug)]
#[allow(dead_code)]
pub enum Operation {
    Binary(BinaryOp, SpaceNameId, SpaceNameId),
    Unary(UnaryOp, SpaceNameId),
    Compare(CompareType, SpaceNameId, SpaceNameId),
}

#[derive(Debug)]
pub enum CommandOperation {}

#[derive(Debug)]
#[allow(dead_code)]
pub enum JumpOperation {
    Unconditional(AddressMarker),
    Branch(SpaceNameId, AddressMarker, AddressMarker),
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
    Command(CommandOperation, IRInformation),
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
                Operation::Compare(cmp, v1, v2) => {
                    write!(f, "{:?} <- {:?} {:?} {:?}", var, v1, cmp, v2)
                }
            },
            IR::Jump(JumpOperation::Branch(v, true_br, false_br), _info) => {
                write!(f, "=> {:?} ? {} : {}", v, true_br, false_br)
            }
            IR::Jump(JumpOperation::Unconditional(m), _) => write!(f, "=> {}", m),
            IR::Jump(JumpOperation::Next, _) => write!(f, "=> next"),
            IR::Jump(JumpOperation::End, _) => write!(f, "=> end"),
            IR::Command(op, _) => write!(f, "{:?}", op),
        }
    }
}

pub struct Function {
    pub name: String,
    pub name_id: FunctionNameId,
    pub params: Vec<SpaceNameId>,
    pub return_type: DataType,
    pub locals: MonotonicNameMap<String, SpaceNameId, Space>,
    pub blocks: MonotonicNameMap<String, BlockNameId, CodeBlock>,
    pub graph: DataFlowGraph<CodeBlockAnalysisNode, CodeBlockGraphWeight>,
    pub is_declared: bool,
    pub is_extern: bool,
    pub is_defined: bool,
    program: ProgramRef,
}
impl Function {
    pub fn new(program: ProgramRef, name: String, name_id: FunctionNameId) -> Self {
        Self {
            name,
            name_id,
            params: vec![],
            return_type: DataType::Void,
            locals: program.borrow().space_pool.borrow().create_map(),
            blocks: program.borrow().block_pool.borrow().create_map(),
            graph: DataFlowGraph::new(CodeBlockGraphWeight::default()),
            is_declared: false,
            is_extern: false,
            is_defined: false,
            program: program.clone(),
        }
    }
    pub fn lookup_space(&mut self, name_id: SpaceNameId) -> Option<SpaceId> {
        self.locals
            .get_id_from_name_id(&name_id)
            .or_else(|| self.program.borrow().lookup_space(name_id))
    }
    pub fn lookup_or_insert_space(&mut self, name: String) -> (SpaceNameId, SpaceId) {
        if let Some(name_id) = self.locals.get_name_id(&name) {
            (
                *name_id,
                self.program.borrow().lookup_space(*name_id).unwrap(),
            )
        } else if let Some(t) = self.program.clone().borrow().lookup_global_by_name(&name) {
            t
        } else {
            self.declare_local(name, None)
        }
    }
    pub fn declare_local(
        &mut self,
        name: String,
        data_type: Option<DataType>,
    ) -> (SpaceNameId, SpaceId) {
        let (name_id, space_id) = self.program.borrow_mut().declare_space(
            data_type,
            Scope::Local {
                fn_name_id: self.name_id,
            },
        );
        self.locals.bind(name, name_id);
        (name_id, space_id)
    }
    pub fn lookup_or_insert_block(&mut self, name: String) -> (BlockNameId, CodeBlockId) {
        self.blocks.get_name_id_and_id(&name).unwrap_or_else(|| {
            self.blocks.get_or_insert(name, |name_id, id| {
                CodeBlock::new(
                    id,
                    BlockType::Normal,
                    vec![],
                    IR::Jump(JumpOperation::Next, IRInformation::default()),
                )
            })
        })
    }
}
pub type FunctionRef = RcRef<Function>;

pub struct Program {
    pub space_pool: RcRef<MonotonicNamedPool<String, SpaceNameId, Space>>,
    pub globals: MonotonicNameMap<String, SpaceNameId, Space>,
    pub block_pool: RcRef<MonotonicNamedPool<String, BlockNameId, CodeBlock>>,
    pub function_pool: RcRef<MonotonicNamedPool<String, FunctionNameId, Function>>,
    pub functions: MonotonicNameMap<String, FunctionNameId, Function>,
    weak_self: WeakRef<Self>,
}
pub type ProgramRef = RcRef<Program>;

impl Program {
    pub fn new() -> RcRef<Self> {
        let space_pool = MonotonicNamedPool::new(1);
        let block_pool = MonotonicNamedPool::new(1);
        let function_pool = MonotonicNamedPool::new(1);

        Rc::new_cyclic(|weak| {
            RefCell::new(Self {
                globals: space_pool.clone().borrow().create_map(),
                functions: function_pool.clone().borrow().create_map(),
                space_pool,
                block_pool,
                function_pool,
                weak_self: weak.clone(),
            })
        })
    }
    pub fn lookup_space(&self, name_id: SpaceNameId) -> Option<SpaceId> {
        self.space_pool.borrow().get_id(&name_id).copied()
    }
    pub fn lookup_global_by_name(&self, name: &String) -> Option<(SpaceNameId, SpaceId)> {
        self.globals.get_name_id_and_id(name)
    }
    pub fn lookup_or_insert_global(&mut self, name: &String) -> (SpaceNameId, SpaceId) {
        self.lookup_global_by_name(name)
            .unwrap_or_else(|| self.declare_global(name.clone(), None))
    }
    pub fn declare_global(
        &mut self,
        name: String,
        data_type: Option<DataType>,
    ) -> (SpaceNameId, SpaceId) {
        let (name_id, id) = self.declare_space(data_type, Scope::Global);
        self.globals.bind(name.clone(), name_id);
        (name_id, id)
    }
    pub fn declare_space(
        &mut self,
        data_type: Option<DataType>,
        scope: Scope,
    ) -> (SpaceNameId, SpaceId) {
        let members = match data_type.clone() {
            Some(DataType::Array(elem_ty, len)) => (0..len)
                .map(|_| {
                    self.declare_space(Some(elem_ty.deref().clone()), scope.clone())
                        .0
                })
                .collect(),
            Some(DataType::Struct(members)) => members
                .iter()
                .map(|dt| self.declare_space(Some(dt.clone()), scope.clone()).0)
                .collect(),
            _ => vec![],
        };
        let space = Space {
            signature: SpaceSignature::Normal(data_type, members),
            scope,
            value: FlatLattice::Top,
        };
        self.space_pool.borrow_mut().insert(space)
    }
    pub fn lookup_or_insert_function(&mut self, name: String) -> (FunctionNameId, FunctionId) {
        self.functions.get_or_insert(name.clone(), |name_id, id| {
            Function::new(self.weak_self.upgrade().unwrap(), name, name_id)
        })
    }
}
