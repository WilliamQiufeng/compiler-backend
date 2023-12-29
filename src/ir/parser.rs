use std::{
    collections::{HashMap, VecDeque},
    fmt::Display,
    iter::Peekable,
    str::FromStr,
};

use thiserror::Error;

use crate::{
    ir::{
        ops::{BinaryOp, Op, UnaryOp},
        BlockType, IRInformation, Operation, IR,
    },
    util::{MonotonicNameMap, MonotonicNamedPool, RcRef},
};

use super::{
    block::{CodeBlock, CodeBlockId},
    lexer::{Token, TokenKind},
    ops::DataType,
    AddressMarker, ArrayValue, BlockNameId, Function, FunctionId, FunctionNameId, IntValue,
    JumpOperation, ProgramRef, Scope, Space, SpaceId, SpaceNameId, SpaceSignature, StructValue,
    Value, WeakSpaceRef,
};

pub struct Parser<T: Iterator<Item = Token>> {
    pub token_iter: Peekable<T>,
    program: ProgramRef,
    block_pool: RcRef<MonotonicNamedPool<BlockNameId, CodeBlock>>,
    space_pool: RcRef<MonotonicNamedPool<SpaceNameId, Space>>,
    function_pool: RcRef<MonotonicNamedPool<FunctionNameId, Function>>,
    preloaded_tokens: VecDeque<Token>,
    buffer: VecDeque<VecDeque<Token>>,
}

#[derive(Debug, Error)]
pub enum ParseErrorKind {
    #[error("unexpected token: expected one of {expected:?}, found {found:?}")]
    UnexpectedToken {
        expected: Vec<TokenKind>,
        found: TokenKind,
    },
    #[error("out of range: got {got}")]
    OutOfRange { got: i64 },
    #[error("not declared: {name}")]
    NotDeclared { name: String },
    #[error("invalid element: {name}.{index}")]
    InvalidElement { name: String, index: usize },
    #[error("format error")]
    Format,
    #[error("function already declared: {name}")]
    FunctionAlreadyDeclared { name: String },
    #[error("block already declared: {name}")]
    BlockAlreadyDeclared { name: String },
    #[error("data type is not consistent: expected {expected}, found {found}")]
    InconsistentDataType { expected: DataType, found: DataType },
}
#[derive(Error, Debug)]
#[error("parse error at {}: {kind}", .current_token.clone().expect(""))]
pub struct ParseError {
    #[source]
    kind: ParseErrorKind,
    current_token: Option<Token>,
}
impl ParseError {
    pub fn new(kind: ParseErrorKind, current_token: Option<Token>) -> Self {
        Self {
            kind,
            current_token,
        }
    }
}
impl<T: Iterator<Item = Token>> Parser<T> {
    pub fn new(tokens: T) -> Self {
        let program = super::Program::new();
        Parser {
            token_iter: tokens.peekable(),
            preloaded_tokens: VecDeque::new(),
            block_pool: program.clone().borrow().block_pool.clone(),
            space_pool: program.clone().borrow().space_pool.clone(),
            function_pool: program.clone().borrow().function_pool.clone(),
            program,
            buffer: VecDeque::from([VecDeque::new()]),
        }
    }
    fn peek(&mut self) -> &Token {
        self.preloaded_tokens
            .back()
            .or_else(|| self.token_iter.peek())
            .expect("End")
    }
    fn consume(&mut self) -> &Token {
        let token = self
            .preloaded_tokens
            .pop_back()
            .or_else(|| self.token_iter.next());
        match (token.clone(), self.buffer.back_mut()) {
            (Some(t), Some(back)) => {
                back.push_back(t);
                back.back().expect("End")
            }
            _ => panic!(""),
        }
    }

    fn match_one_of(&mut self, expected: Vec<TokenKind>) -> Result<&Token, ParseError> {
        let kind = self.peek().kind.clone();
        if expected.contains(&kind) {
            Ok(self.consume())
        } else {
            Err(ParseError::new(
                ParseErrorKind::UnexpectedToken {
                    expected,
                    found: kind.clone(),
                },
                Some(self.peek().clone()),
            ))
        }
    }
    fn match_token(&mut self, expect: TokenKind) -> Result<&Token, ParseError> {
        let kind = self.peek().kind.clone();
        if kind == expect {
            Ok(self.consume())
        } else {
            Err(ParseError::new(
                ParseErrorKind::UnexpectedToken {
                    expected: vec![expect],
                    found: kind.clone(),
                },
                Some(self.peek().clone()),
            ))
        }
    }
    fn put_back(&mut self, token: Token) {
        self.preloaded_tokens.push_back(token);
    }
    fn put_back_all(&mut self, tokens: VecDeque<Token>) {
        self.preloaded_tokens.extend(tokens.into_iter().rev());
    }
    fn enter_preservation(&mut self) {
        self.buffer.push_back(VecDeque::new());
    }
    /// Pops the last element from a buffer and appends it to the
    /// previous element, if it exists.
    fn leave_preservation(&mut self) {
        let back = &mut self.buffer.pop_back().unwrap();
        if let Some(new_back) = self.buffer.back_mut() {
            new_back.append(back)
        }
    }
    fn backtrack_preservation(&mut self) {
        let tokens = self.buffer.pop_back().unwrap();
        self.put_back_all(tokens);
    }
    fn match_preservative<R, F>(&mut self, transform: F) -> Result<R, ParseError>
    where
        F: FnOnce(&mut Self) -> Result<R, ParseError>,
    {
        self.enter_preservation();
        let result = transform(self);
        match result {
            Ok(_) => self.leave_preservation(),
            Err(_) => self.backtrack_preservation(),
        };
        result
    }
    fn match_parse<ParseType>(&mut self) -> Result<(&Token, ParseType), ParseError>
    where
        ParseType: FromStr,
    {
        let token = self.peek();
        match token.content.parse() {
            Ok(i) => Ok((self.consume(), i)),
            Err(_) => Err(ParseError::new(ParseErrorKind::Format, Some(token.clone()))),
        }
    }

    fn format_error(&mut self) -> ParseError {
        ParseError::new(ParseErrorKind::Format, Some(self.peek().clone()))
    }

    fn match_fn_param(&mut self, function: &mut Function) -> Result<SpaceNameId, ParseError> {
        let data_type = self.match_data_type()?;
        let name_token_content = self.match_token(TokenKind::SpaceId)?.content.clone();
        let (name_id, _) = function.declare_local(name_token_content, Some(data_type));
        Ok(name_id)
    }
    fn match_space(
        &mut self,
        function: Option<&mut Function>,
    ) -> Result<(SpaceNameId, SpaceId), ParseError> {
        let cur = self.match_token(TokenKind::SpaceId)?;
        let cur_content = cur.content.clone();
        let (mut name_id, mut id) = match function {
            Some(f) => f.lookup_or_insert_space(cur_content.clone()),
            None => self
                .program
                .borrow_mut()
                .lookup_or_insert_global(&cur_content),
        };
        while self.match_token(TokenKind::Dot).is_ok() {
            let (index_token, index) = self.match_parse::<usize>()?;
            let index_token = index_token.clone();
            self.space_pool
                .borrow_mut()
                .get_mut_from_id(id)
                .map(|space| {
                    let fields = match space.signature {
                        crate::ir::SpaceSignature::Normal(_, ref fields) => fields,
                        crate::ir::SpaceSignature::Offset(_, _, _, ref fields) => fields,
                    };
                    name_id = fields.get(index).copied().unwrap();

                    Ok(())
                })
                .unwrap()?;
            id = self.program.borrow().lookup_space(name_id).ok_or_else(|| {
                ParseError::new(
                    ParseErrorKind::NotDeclared {
                        name: cur_content.clone(),
                    },
                    Some(index_token),
                )
            })?;
        }
        Ok((name_id, id))
    }
    fn match_int(&mut self) -> Result<(DataType, (SpaceNameId, SpaceId)), ParseError> {
        Ok((
            DataType::I64,
            self.program.clone().borrow_mut().lookup_or_insert_constant(
                DataType::I64,
                Value::Int(match self.peek().kind.clone() {
                    TokenKind::IntLiteral => {
                        let value = self
                            .consume()
                            .content
                            .parse()
                            .map_err(|_| self.format_error())?;
                        IntValue { value }
                    }
                    TokenKind::IntBinLiteral => {
                        let value = i64::from_str_radix(&self.consume().content[1..], 2)
                            .map_err(|_| self.format_error())?;
                        IntValue { value }
                    }
                    TokenKind::IntHexLiteral => {
                        let value = i64::from_str_radix(&self.consume().content[1..], 16)
                            .map_err(|_| self.format_error())?;
                        IntValue { value }
                    }
                    _ => {
                        return Err(ParseError::new(
                            ParseErrorKind::Format,
                            Some(self.peek().clone()),
                        ))
                    }
                }),
            ),
        ))
    }
    fn match_value(
        &mut self,
        function: Option<&mut Function>,
    ) -> Result<(Option<DataType>, (SpaceNameId, SpaceId)), ParseError> {
        match self.peek().kind.clone() {
            TokenKind::SpaceId => {
                let (name_id, id) = self.match_space(function)?;
                Ok((
                    self.program
                        .borrow()
                        .space_pool
                        .borrow()
                        .get_from_id(id)
                        .unwrap()
                        .signature
                        .get_type(),
                    (name_id, id),
                ))
            }
            TokenKind::IntBinLiteral | TokenKind::IntHexLiteral | TokenKind::IntLiteral => {
                self.match_int().map(|(data_type, (name_id, id))| (Some(data_type), (name_id, id)))
            }
            TokenKind::OpenBrace => {
                let mut members_names = Vec::new();
                let mut members = Vec::new();
                let mut function = function;
                while self.match_token(TokenKind::CloseBrace).is_err() {
                    let (data_type, (name_id, _)) = self.match_value(function.as_deref_mut())?;
                    members_names.push(name_id);
                    members.push(data_type.unwrap());
                    let _ = self.match_token(TokenKind::Comma);
                }

                let data_type = DataType::Struct(members);
                let value = StructValue {
                    value: members_names,
                };
                Ok((
                    Some(data_type.clone()),
                    self.program
                        .borrow_mut()
                        .lookup_or_insert_constant(data_type.clone(), Value::Struct(value)),
                ))
            }
            TokenKind::OpenBracket => {
                let mut members_names = Vec::new();
                let mut element_type = None;
                let mut function = function;
                while self.match_token(TokenKind::CloseBracket).is_err() {
                    let (data_type, (name_id, _)) = self.match_value(function.as_deref_mut())?;
                    let data_type = data_type.unwrap();
                    members_names.push(name_id);
                    match element_type {
                        Some(dt) if dt != data_type => {
                            return Err(ParseError::new(
                                ParseErrorKind::InconsistentDataType {
                                    expected: dt,
                                    found: data_type,
                                },
                                Some(self.peek().clone()),
                            ))
                        }
                        None => element_type = Some(data_type),
                        _ => (),
                    }
                    let _ = self.match_token(TokenKind::Comma);
                }

                let data_type = DataType::Array(
                    Box::new(element_type.ok_or(self.format_error())?),
                    members_names.len(),
                );
                let value = ArrayValue {
                    value: members_names,
                };
                Ok((
                    Some(data_type.clone()),
                    self.program
                        .borrow_mut()
                        .lookup_or_insert_constant(data_type.clone(), Value::Array(value)),
                ))
            }
            _ => Err(self.format_error()),
        }
    }
    fn match_data_type(&mut self) -> Result<DataType, ParseError> {
        let first = self.consume();
        match first.kind {
            TokenKind::I64 => Ok(DataType::I64),
            TokenKind::F64 => Ok(DataType::F64),
            TokenKind::Bool => Ok(DataType::Bool),
            TokenKind::Void => Ok(DataType::Void),
            TokenKind::OpenBracket => {
                let inner = self.match_data_type()?;
                self.match_token(TokenKind::Comma)?;
                let (_, len) = self.match_parse()?;
                self.match_token(TokenKind::CloseBracket)?;
                Ok(DataType::Array(Box::new(inner), len))
            }
            TokenKind::OpenBrace => {
                let mut fields = Vec::new();
                while let Ok(field) = self.match_data_type() {
                    fields.push(field);
                    if let TokenKind::CloseBrace = self.peek().kind {
                        break;
                    }
                    self.match_token(TokenKind::Comma)?;
                }
                self.match_token(TokenKind::CloseBrace)?;
                Ok(DataType::Struct(fields))
            }
            _ => Err(ParseError::new(
                ParseErrorKind::UnexpectedToken {
                    expected: vec![
                        TokenKind::I64,
                        TokenKind::F64,
                        TokenKind::OpenBrace,
                        TokenKind::OpenBracket,
                        TokenKind::Bool,
                        TokenKind::Void,
                    ],
                    found: first.kind.clone(),
                },
                Some(first.clone()),
            )),
        }
    }
    fn match_fn_header(&mut self) -> Result<(FunctionNameId, FunctionId, bool), ParseError> {
        let (function_name, fn_name_id, fn_id) = self.match_fn_id()?;
        self.match_token(TokenKind::OpenParen)?;

        self.program
            .clone()
            .borrow_mut()
            .functions
            .get_mut_from_id(fn_id)
            .map(|mut function| {
                if function.is_declared {
                    Err(ParseError::new(
                        ParseErrorKind::FunctionAlreadyDeclared {
                            name: function_name.clone(),
                        },
                        Some(self.peek().clone()),
                    ))
                } else {
                    function.is_declared = true;
                    while self.match_token(TokenKind::CloseParen).is_err() {
                        let name_id = self.match_fn_param(&mut function)?;
                        function.params.push(name_id);
                        let _ = self.match_token(TokenKind::Comma);
                    }
                    self.match_token(TokenKind::Colon)?;
                    function.return_type = self.match_data_type()?;
                    function.is_extern = self.match_token(TokenKind::Extern).is_ok();
                    Ok((
                        fn_name_id,
                        fn_id,
                        !function.is_extern && self.match_token(TokenKind::Stub).is_err(),
                    ))
                }
            })
            .unwrap()
    }
    fn match_instruction(&mut self, function: &mut Function) -> Result<IR, ParseError> {
        // %x
        if let Ok((assign_space_name_id, _)) = self.match_space(Some(function)) {
            // %x =
            self.match_token(TokenKind::Assign)?;
            // %x = %a
            if let Ok((_, (left_space_name_id, _))) = self.match_value(Some(function)) {
                // %x = %a + %b
                // or %x = %a
                let op = match self.peek().kind {
                    TokenKind::Add => Op::Binary(BinaryOp::Add),
                    TokenKind::Sub => Op::Binary(BinaryOp::Sub),
                    TokenKind::Mul => Op::Binary(BinaryOp::Mul),
                    TokenKind::Div => Op::Binary(BinaryOp::Div),
                    _ => Op::Unary(UnaryOp::Unit),
                };
                if let Op::Binary(op) = op {
                    self.consume();
                    let right_space_name_id = self.match_value(Some(function))?.1 .0;
                    Ok(IR::Assignment(
                        assign_space_name_id,
                        Operation::Binary(op, left_space_name_id, right_space_name_id),
                        IRInformation::default(),
                    ))
                } else if let Op::Unary(op) = op {
                    Ok(IR::Assignment(
                        assign_space_name_id,
                        Operation::Unary(op, left_space_name_id),
                        IRInformation::default(),
                    ))
                } else {
                    unreachable!()
                }
            } else {
                // %x = + %a
                let token_kind = self.peek().kind.clone();
                let valued_expr = match token_kind {
                    TokenKind::Not => Some(UnaryOp::Not),
                    TokenKind::Sub => Some(UnaryOp::Negative),
                    TokenKind::Load => Some(UnaryOp::Load),
                    TokenKind::Param => Some(UnaryOp::Param),
                    _ => None,
                };
                if let Some(op) = valued_expr {
                    self.consume();
                    Ok(IR::Assignment(
                        assign_space_name_id,
                        Operation::Unary(op, self.match_value(Some(function))?.1 .0),
                        IRInformation::default(),
                    ))
                } else if let TokenKind::Call = token_kind {
                    self.match_token(TokenKind::Call)?;
                    let (_, fn_name_id, _) = self.match_fn_id()?;
                    Ok(IR::Assignment(
                        assign_space_name_id,
                        Operation::Call(fn_name_id),
                        IRInformation::default(),
                    ))
                } else {
                    Err(ParseError::new(
                        ParseErrorKind::UnexpectedToken {
                            expected: vec![TokenKind::Not, TokenKind::Sub, TokenKind::SpaceId],
                            found: token_kind,
                        },
                        Some(self.peek().clone()),
                    ))
                }
            }
        } else {
            todo!("commands")
        }
    }
    fn match_terminator(&mut self, function: &mut Function) -> Result<IR, ParseError> {
        if let Ok((_, name_id, _)) = self.match_block_id(function) {
            Ok(IR::Jump(
                JumpOperation::Unconditional(AddressMarker::new(name_id)),
                IRInformation::default(),
            ))
        } else if let Ok((_, (name_id, _))) = self.match_value(Some(function)) {
            self.match_token(TokenKind::QuestionMark)?;
            let true_block = self.match_block_id(function)?;
            self.match_token(TokenKind::Colon)?;
            let false_block = self.match_block_id(function)?;
            Ok(IR::Jump(
                JumpOperation::Branch(
                    name_id,
                    AddressMarker::new(true_block.1),
                    AddressMarker::new(false_block.1),
                ),
                IRInformation::default(),
            ))
        } else if self.match_token(TokenKind::Ret).is_ok() {
            let val = self.match_value(Some(function))?;
            Ok(IR::Jump(
                JumpOperation::Ret(val.1 .0),
                IRInformation::default(),
            ))
        } else if self.match_token(TokenKind::Next).is_ok() {
            Ok(IR::Jump(JumpOperation::Next, IRInformation::default()))
        } else {
            self.match_token(TokenKind::End)?;
            Ok(IR::Jump(JumpOperation::End, IRInformation::default()))
        }
    }
    fn match_block(&mut self, function: &mut Function) -> Result<(), ParseError> {
        let id = self.match_block_id(function)?.2;
        self.match_token(TokenKind::OpenBrace)?;
        if let Some(mut block) = self.block_pool.clone().borrow_mut().get_mut_from_id(id) {
            while self.match_token(TokenKind::Terminator).is_err() {
                let instruction = self.match_instruction(function)?;
                block.irs_range.push(instruction);
            }
        }
        let terminator = self.match_terminator(function)?;
        self.block_pool
            .clone()
            .borrow_mut()
            .get_mut_from_id(id)
            .unwrap()
            .terminator = terminator;
        self.match_token(TokenKind::CloseBrace)?;
        Ok(())
    }

    fn match_block_id(
        &mut self,
        function: &mut Function,
    ) -> Result<(String, BlockNameId, CodeBlockId), ParseError> {
        let block_name_token = self.match_token(TokenKind::BlockId)?.clone();
        let block_name = block_name_token.content.clone();
        let (name_id, id) = function.lookup_or_insert_block(block_name.clone());
        Ok((block_name, name_id, id))
    }
    fn match_fn_body(&mut self, function: &mut Function) -> Result<(), ParseError> {
        self.match_token(TokenKind::OpenBrace)?;
        while self.match_token(TokenKind::CloseBrace).is_err() {
            self.match_block(function)?;
        }
        function.is_defined = true;
        Ok(())
    }
    fn match_fn(&mut self) -> Result<(FunctionNameId, FunctionId), ParseError> {
        // impl $fn_name { ... }
        let ((fn_name_id, fn_id), match_body) = if self.match_token(TokenKind::Impl).is_ok() {
            let (_info, fn_name_id, fn_id) = self.match_fn_id()?;
            ((fn_name_id, fn_id), true)
        } else {
            // fn $fn_name (i64 x, i64 y, ...) : bool { ... }
            // fn $fn_name ([i64, 3] a, ...) : i64 stub
            // fn $fn_name ([i64, 3] a, ...) : f64 ext
            self.match_token(TokenKind::Fn)?;
            let (fn_name_id, fn_id, match_body) = self.match_fn_header()?;

            ((fn_name_id, fn_id), match_body)
        };
        if match_body {
            self.function_pool
                .clone()
                .borrow_mut()
                .get_mut_from_id(fn_id)
                .map(|mut function| self.match_fn_body(&mut function))
                .expect("Function is not declared unexpectedly")?;
        }
        Ok((fn_name_id, fn_id))
    }

    fn match_fn_id(&mut self) -> Result<(String, FunctionNameId, FunctionId), ParseError> {
        let function_name_token = self.match_token(TokenKind::FunctionId)?.clone();
        let function_name = function_name_token.content.clone();
        let (fn_name_id, fn_id) = self
            .program
            .borrow_mut()
            .lookup_or_insert_function(function_name.clone());
        Ok((function_name, fn_name_id, fn_id))
    }
    pub fn match_program(&mut self) -> Result<ProgramRef, ParseError> {
        // todo!
        while self.match_token(TokenKind::Eof).is_err() {
            self.match_fn()?;
        }
        Ok(self.program.clone())
    }
}

#[cfg(test)]
mod tests {
    use crate::ir::lexer::Tokenizer;

    use super::*;
    #[test]
    fn test_parser_data_type() {
        let r = Parser::new("[[i64, 4], 6]".chars().tokenize()).match_data_type();
        assert_eq!(
            r.unwrap(),
            DataType::Array(Box::new(DataType::Array(Box::new(DataType::I64), 4)), 6)
        );
        let r = Parser::new("[i64, -1]".chars().tokenize()).match_data_type();
        assert!(
            matches!(
                r,
                Err(ParseError {
                    kind: ParseErrorKind::Format,
                    ..
                })
            ),
            "{:?}",
            r
        );
        println!("{}", r.err().unwrap());
        let r = Parser::new("[err, -1]".chars().tokenize()).match_data_type();
        assert!(
            matches!(
                r,
                Err(ParseError {
                    kind: ParseErrorKind::UnexpectedToken { .. },
                    ..
                })
            ),
            "{:?}",
            r
        );
        println!("{}", r.err().unwrap());
    }
    #[test]
    fn test_parser_space() {
        let mut parser = Parser::new("%abc %abc %def".chars().tokenize());
        let r = parser.match_space(None);
        let r2 = parser.match_space(None);
        let r3 = parser.match_space(None);
        assert_eq!(r.unwrap().0, 0);
        assert_eq!(r2.unwrap().0, 0);
        assert_eq!(r3.unwrap().0, 1);
    }
    #[test]
    fn test_parser_program() {
        let src = include_str!("../../tests/ir/test.ir");
        let mut parser = Parser::new(src.chars().tokenize());
        let program = parser.match_program();
        match program {
            Ok(ref program) => {
                println!("{}", program.borrow());
            }
            Err(ref e) => {
                println!("{}", e);
            }
        }
        assert!(program.is_ok());
        let program = program.unwrap();
        println!("{}", program.borrow())
    }
}
