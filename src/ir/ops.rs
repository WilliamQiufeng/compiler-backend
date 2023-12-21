use std::fmt::Display;


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


impl Display for DataType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DataType::I64 => write!(f, "i64"),
            DataType::F64 => write!(f, "f64"),
            DataType::Bool => write!(f, "bool"),
            DataType::Array(t, n) => write!(f, "[{}; {}]", t, n),
            DataType::Void => write!(f, "void"),
            DataType::Struct(fields) => {
                write!(f, "{{")?;
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", field)?;
                }
                write!(f, "}}")
            }
        }
    }
}