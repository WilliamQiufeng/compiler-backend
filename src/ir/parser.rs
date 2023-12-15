use std::{
    borrow::BorrowMut,
    collections::{HashMap, VecDeque},
    error::Error,
    fmt::Display,
    fs::File,
    iter::Peekable,
    ops::{Deref, DerefMut},
};

use thiserror::Error;

use crate::{util::{FromInner, Ref}, ir::{SpaceContextRef, SpaceContext}};

use super::{
    lexer::{Token, TokenKind},
    Scope, ScopeContextRef, SpaceRef, WeakSpaceRef,
};

pub struct Parser<T: Iterator<Item = Token>> {
    pub token_iter: Peekable<T>,
    preloaded_tokens: VecDeque<Token>,
}

#[derive(Debug, Error)]
pub enum ParseErrorKind {
    #[error("unexpected token: expected {expected:?}, found {found:?}")]
    UnexpectedToken {
        expected: Vec<TokenKind>,
        found: TokenKind,
    },
    #[error("out of range: got {got}")]
    OutOfRange {
        got: i64,
    },
    #[error("not declared: {name}")]
    NotDeclared {
        name: String,
    },
    #[error("invalid element: {name}.{index}")]
    InvalidElement {
        name: String,
        index: usize,
    },
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
        }
    }
    fn peek(&mut self) -> Option<&Token> {
        self.preloaded_tokens
            .back()
            .or_else(|| self.token_iter.peek())
    }
    fn consume(&mut self) -> Option<Token> {
        self.preloaded_tokens
            .pop_back()
            .or_else(|| self.token_iter.next())
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
    fn preload_token(&mut self, token: Token) {
        self.preloaded_tokens.push_back(token);
    }
    fn match_space(&mut self, scope: ScopeContextRef) -> Result<SpaceContextRef, ParseError> {
        let mut cur = self.match_token(TokenKind::SpaceId)?;
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
        }
        todo!()
    }
}
