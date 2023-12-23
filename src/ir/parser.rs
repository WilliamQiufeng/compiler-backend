use std::{
    collections::{HashMap, VecDeque},
    error::Error,
    fmt::Display,
    fs::File,
    iter::Peekable,
    ops::{Deref, DerefMut},
};

use thiserror::Error;

use crate::util::{FromInner, RcRef};

use super::{
    lexer::{Token, TokenKind},
    FunctionRef, ProgramRef, Scope, SpaceId, SpaceNameId, WeakSpaceRef, ops::DataType,
};

pub struct Parser<T: Iterator<Item = Token>> {
    pub token_iter: Peekable<T>,
    program: ProgramRef,
    preloaded_tokens: VecDeque<Token>,
    buffer: VecDeque<VecDeque<Token>>,
}

#[derive(Debug, Error)]
pub enum ParseErrorKind {
    #[error("unexpected token: expected {expected:?}, found {found:?}")]
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
}
#[derive(Debug, Error)]
pub struct ParseError {
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
impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
impl<T: Iterator<Item = Token>> Parser<T> {
    pub fn new(tokens: T) -> Self {
        Parser {
            token_iter: tokens.peekable(),
            preloaded_tokens: VecDeque::new(),
            program: super::Program::new(),
            buffer: VecDeque::new(),
        }
    }
    fn peek(&mut self) -> Option<&Token> {
        self.preloaded_tokens
            .back()
            .or_else(|| self.token_iter.peek())
    }
    fn consume(&mut self) -> Option<Token> {
        let token = self.preloaded_tokens
            .pop_back()
            .or_else(|| self.token_iter.next());
        if let (Some(t), Some(back)) = (token.clone(), self.buffer.back_mut()) {
            back.push_back(t)
        };
        token
    }
    
    fn match_one_of(&mut self, expected: Vec<TokenKind>) -> Result<Token, ParseError> {
        if let Some(Token { kind, .. }) = self.peek() {
            if expected.contains(kind) {
                Ok(self.consume().unwrap())
            } else {
                Err(ParseError::new(
                    ParseErrorKind::UnexpectedToken {
                        expected,
                        found: kind.clone(),
                    },
                    self.peek().cloned(),
                ))
            }
        } else {
            Err(ParseError::new(
                ParseErrorKind::UnexpectedToken {
                    expected,
                    found: TokenKind::Eof,
                },
                None,
            ))
        }
    }
    fn match_token(&mut self, expect: TokenKind) -> Result<Token, ParseError> {
        if let Some(Token { kind, .. }) = self.peek() {
            if *kind == expect {
                Ok(self.consume().unwrap())
            } else {
                Err(ParseError::new(
                    ParseErrorKind::UnexpectedToken {
                        expected: vec![expect],
                        found: kind.clone(),
                    },
                    self.peek().cloned(),
                ))
            }
        } else {
            Err(ParseError::new(
                ParseErrorKind::UnexpectedToken {
                    expected: vec![expect],
                    found: TokenKind::Eof,
                },
                None,
            ))
        }
    }
    fn put_back(&mut self, token: Token) {
        self.preloaded_tokens.push_back(token);
    }
    fn match_preservative<R, F>(&mut self, transform: F) -> Result<R, ParseError> where F: FnOnce(&mut Self) -> Result<R, ParseError> {
        let saved = std::mem::replace(&mut self.preloaded_tokens, VecDeque::new());
        let result = transform(self);
        self.preloaded_tokens = saved;
        result
        
    }
    fn match_space(&mut self, function: Option<FunctionRef>) -> Result<SpaceNameId, ParseError> {
        let cur = self.match_token(TokenKind::SpaceId)?;
        let (mut name_id, mut id) = match function {
            Some(f) => f
                .borrow_mut()
                .lookup_or_insert_space_name(cur.content.clone()),
            None => self
                .program
                .borrow_mut()
                .lookup_or_insert_global(cur.content.clone()),
        };
        while self.match_token(TokenKind::Dot).is_ok() {
            let index_token = self.match_token(TokenKind::IntLiteral)?;
            let index = match index_token.content.parse::<usize>() {
                Ok(i) => i,
                Err(_) => {
                    return Err(ParseError::new(
                        ParseErrorKind::OutOfRange {
                            got: index_token.content.parse().unwrap(),
                        },
                        Some(index_token),
                    ))
                }
            };
            self.program
                .borrow()
                .get_space(id)
                .map(|space| {
                    let fields = match space.signature {
                        crate::ir::SpaceSignature::Normal(_, ref fields) => fields,
                        crate::ir::SpaceSignature::Offset(_, _, _, ref fields) => fields,
                    };
                    name_id = fields.get(index).copied().ok_or_else(|| {
                        ParseError::new(
                            ParseErrorKind::OutOfRange {
                                got: index_token.content.parse().unwrap(),
                            },
                            Some(index_token.clone()),
                        )
                    })?;
                    id = self.program.borrow().lookup_space(name_id).ok_or_else(|| {
                        ParseError::new(
                            ParseErrorKind::NotDeclared {
                                name: cur.content.clone(),
                            },
                            Some(index_token),
                        )
                    })?;

                    Ok(())
                })
                .unwrap()?;
        }
        Ok(name_id)
    }
    fn match_data_type(&mut self) -> Result<DataType, ParseError> {
        let peek = self.peek();
        match peek {
            Some(Token { kind, .. }) => {
                match kind {
                    TokenKind::I64 => Ok(DataType::I64),
                    TokenKind::F64 => Ok(DataType::F64),
                    TokenKind::Bool => Ok(DataType::Bool),
                    TokenKind::Void => Ok(DataType::Void),
                    // TokenKind::OpenBracket => {

                    // }
                    _ => todo!()
                }
            },
            _ => Err(ParseError::new(ParseErrorKind::UnexpectedToken {
                expected: vec![TokenKind::I64],
                found: TokenKind::Eof,
            }, peek.cloned()))
        }
    }
    fn match_fn(&mut self, program: ProgramRef) -> Result<FunctionRef, ParseError> {
        self.match_token(TokenKind::Fn)?;
        let function_name_token = self.match_token(TokenKind::FunctionId)?;
        let function_name = function_name_token.content.clone();
        self.match_token(TokenKind::OpenParen)?;
        let (fn_name_id, fn_id) = program.borrow_mut().lookup_or_insert_function(function_name);
        program.borrow().get_function(fn_id).map(|function| {

        });
        todo!()
    }
}
