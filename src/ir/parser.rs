use std::{
    collections::{HashMap, VecDeque},
    fmt::Display,
    iter::Peekable,
    str::FromStr,
};

use thiserror::Error;

use super::{
    lexer::{Token, TokenKind},
    ops::DataType,
    FunctionId, FunctionNameId, FunctionRef, ProgramRef, Scope, SpaceId, SpaceNameId,
    SpaceSignature, WeakSpaceRef,
};

pub struct Parser<T: Iterator<Item = Token>> {
    pub token_iter: Peekable<T>,
    program: ProgramRef,
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
        Parser {
            token_iter: tokens.peekable(),
            preloaded_tokens: VecDeque::new(),
            program: super::Program::new(),
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
    fn match_space(&mut self, function: Option<FunctionRef>) -> Result<SpaceNameId, ParseError> {
        let cur = self.match_token(TokenKind::SpaceId)?;
        let cur_content = cur.content.clone();
        let (mut name_id, mut id) = match function {
            Some(f) => f.borrow_mut().lookup_or_insert_space(cur_content.clone()),
            None => self
                .program
                .borrow_mut()
                .lookup_or_insert_global(&cur_content),
        };
        while self.match_token(TokenKind::Dot).is_ok() {
            let (index_token, index) = self.match_parse::<usize>()?;
            let index_token = index_token.clone();
            self.program
                .borrow()
                .get_space(id)
                .map(|space| {
                    let fields = match space.signature {
                        crate::ir::SpaceSignature::Normal(_, ref fields) => fields,
                        crate::ir::SpaceSignature::Offset(_, _, _, ref fields) => fields,
                    };
                    name_id = fields.get(index).copied().unwrap();
                    id = self.program.borrow().lookup_space(name_id).ok_or_else(|| {
                        ParseError::new(
                            ParseErrorKind::NotDeclared {
                                name: cur_content.clone(),
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
    fn match_fn_header(&mut self) -> Result<(FunctionNameId, FunctionId), ParseError> {
        let function_name_token = self.match_token(TokenKind::FunctionId)?.clone();
        let function_name = function_name_token.content.clone();
        self.match_token(TokenKind::OpenParen)?;
        let (fn_name_id, fn_id) = self
            .program
            .borrow_mut()
            .lookup_or_insert_function(function_name.clone());
        self.program
            .clone()
            .borrow()
            .get_function_mut(fn_id)
            .map(|mut function| {
                if function.declared {
                    Err(ParseError::new(
                        ParseErrorKind::FunctionAlreadyDeclared {
                            name: function_name.clone(),
                        },
                        Some(function_name_token.clone()),
                    ))
                } else {
                    function.declared = true;
                    while self.match_token(TokenKind::CloseParen).is_err() {
                        let (_, param_name) = self.match_parse::<String>()?;
                        self.match_token(TokenKind::Colon)?;
                        let param_type = self.match_data_type()?;
                        let (name_id, _) =
                            function.declare_local(param_name.clone(), Some(param_type));
                        function.params.push(name_id);
                        let _ = self.match_token(TokenKind::Comma);
                    }
                    Ok((fn_name_id, fn_id))
                }
            })
            .unwrap()
    }
    fn match_fn_body(&mut self, fn_name_id: FunctionNameId, fn_id: FunctionId) {
        self.program.borrow().get_function_mut(fn_id).map(|mut function| {
            
        });
        todo!()
    }
    fn match_fn(&mut self) -> Result<(FunctionNameId, FunctionId), ParseError> {
        if self.match_token(TokenKind::Impl).is_ok() {
            let function_name_token = self.match_token(TokenKind::FunctionId)?.clone();
            let function_name = function_name_token.content.clone();
            let (fn_name_id, fn_id) = self
                .program
                .borrow_mut()
                .lookup_or_insert_function(function_name.clone());
            self.match_fn_body(fn_name_id, fn_id);
            Ok((fn_name_id, fn_id))
        } else {
            self.match_token(TokenKind::Fn)?;
            let (fn_name_id, fn_id) = self.match_fn_header()?;
            if self.match_token(TokenKind::Stub).is_err()  {
                self.match_fn_body(fn_name_id, fn_id);
            }
            Ok((fn_name_id, fn_id))
        }
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
        assert_eq!(r.unwrap(), 0);
        assert_eq!(r2.unwrap(), 0);
        assert_eq!(r3.unwrap(), 1);
    }
}
