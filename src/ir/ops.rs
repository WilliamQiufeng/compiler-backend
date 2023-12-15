
#[allow(dead_code)]
#[derive(Clone, Debug, PartialEq, Hash, Eq)]
pub enum DataType {
    I64,
    F64,
    Bool,
    Array(Box<DataType>, usize),
    Void,
    Struct(Vec<DataType>),
}

#[derive(Debug, Copy, Clone)]
#[allow(dead_code)]
pub enum CompareType {
    Less,
    Greater,
    Eq,
    NotEq,
    LessEqual,
    GreaterEqual,
}

#[derive(Debug, Copy, Clone)]
#[allow(dead_code)]
pub enum Op {
    Binary(BinaryOp),
    Unary(UnaryOp)
}


#[derive(Debug, Clone, Copy)]
#[allow(dead_code)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Compare(CompareType),
    And,
    Or,
    Xor,
}

#[derive(Debug, Clone, Copy)]
#[allow(dead_code)]
pub enum UnaryOp {
    Not,
    Negative,
    Unit, // no-op
}