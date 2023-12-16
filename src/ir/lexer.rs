use std::collections::HashMap;
use std::fmt::Display;
use std::iter::Peekable;
use std::ops::RangeBounds;

use crate::util::{FromInner, Ref};

use super::ops::DataType;
use super::{IntValue, Operation, Scope, Space, SpaceRef, Value, VoidValue, WeakSpaceRef};

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    SpaceId,
    BlockId,
    Dot,
    Colon,
    Comma,
    Fn,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,
    Goto,
    Next,
    Ret,
    Add,
    Sub,
    Mul,
    Div,
    Greater,
    Less,
    Eq,
    Ne,
    LessEq,
    GreaterEq,
    And,
    Or,
    Not,
    Assign,
    Store,
    Load,
    I64,
    IntLiteral,
    IntBinLiteral,
    IntHexLiteral,
    RealLiteral,
    F64,
    Bool,
    Void,
    Error,
    Eof,
}

#[derive(Debug, Clone, Copy)]
pub struct Cursor {
    pub line: usize,
    pub column: usize,
}

impl Display for Cursor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

impl Cursor {
    fn step(&mut self) {
        self.column += 1;
    }
    fn newline(&mut self) {
        self.line += 1;
        self.column = 0;
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub start: Cursor,
    pub end: Cursor,
    pub content: String,
}
impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[{:?}({}) {}~{}]",
            self.kind, self.content, self.start, self.end
        )
    }
}

struct Tokenize<Iter: Iterator<Item = char>> {
    iter_source: Peekable<Iter>,
    cursor: Cursor,
    buffer: [char; 256],
    current_buffer_length: usize,
    buffer_start_cursor: Cursor,
    buffer_end_cursor: Cursor,
    finished: bool,
}
impl<Iter: Iterator<Item = char>> Tokenize<Iter> {
    fn new(iter_source: Peekable<Iter>) -> Self {
        Self {
            iter_source,
            cursor: Cursor { line: 0, column: 0 },
            buffer: ['\0'; 256],
            current_buffer_length: 0,
            buffer_start_cursor: Cursor { line: 0, column: 0 },
            buffer_end_cursor: Cursor { line: 0, column: 0 },
            finished: false,
        }
    }
    fn peek_char(&mut self) -> Option<&char> {
        self.iter_source.peek()
    }
    fn consume_char(&mut self) -> Option<char> {
        let next = self.iter_source.next()?;
        self.buffer[self.current_buffer_length] = next;
        self.current_buffer_length += 1;
        self.buffer_end_cursor = self.cursor;
        match next {
            '\n' => self.cursor.newline(),
            _ => self.cursor.step(),
        };
        Some(next)
    }
    fn match_char(&mut self, expect: char) -> Option<char> {
        if self.peek_char() == Some(&expect) {
            self.consume_char()
        } else {
            None
        }
    }
    fn match_char_range(&mut self, range: impl RangeBounds<char>) -> Option<char> {
        self.match_fn(|c| range.contains(c))
    }
    fn match_char_of(&mut self, mut range: impl Iterator<Item = char>) -> Option<char> {
        let c = self.peek_char()?;
        if range.any(|p| p == *c) {
            self.consume_char()
        } else {
            None
        }
    }
    fn match_one_or_more_range(&mut self, range: impl RangeBounds<char>) -> bool {
        self.match_one_or_more_fn(|c| range.contains(c))
    }

    fn match_one_or_more_fn(&mut self, test: impl Fn(&char) -> bool + Clone) -> bool {
        if self.match_fn(test.clone()).is_none() {
            false
        } else {
            while self.match_fn(&test).is_some() {}
            true
        }
    }
    fn match_fn(&mut self, test: impl Fn(&char) -> bool) -> Option<char> {
        if test(self.peek_char()?) {
            self.consume_char()
        } else {
            None
        }
    }

    fn match_string(&mut self, expect: &str) -> Option<String> {
        for c in expect.chars() {
            self.match_char(c)?;
        }
        Some(self.buffer_content())
    }
    fn match_alnum(&mut self) -> Option<char> {
        self.match_char('_')
            .or(self.match_char_range('a'..='z'))
            .or(self.match_char_range('A'..='Z'))
            .or(self.match_char_range('0'..='9'))
    }
    fn buffer_content(&self) -> String {
        self.buffer[..self.current_buffer_length].iter().collect()
    }
    fn create_token(&self, kind: TokenKind) -> Token {
        Token {
            kind,
            start: self.buffer_start_cursor,
            end: self.buffer_end_cursor,
            content: self.buffer_content(),
        }
    }
    fn clear_buffer(&mut self) {
        self.current_buffer_length = 0;
        self.buffer_start_cursor = self.cursor;
    }

    fn skip_ws(&mut self) {
        while self
            .match_char_of([' ', '\t', '\n', '\r'].into_iter())
            .is_some()
        {}
    }
    fn error_token(&self) -> Token {
        self.create_token(TokenKind::Error)
    }
    fn match_num(&mut self) -> TokenKind {
        if self.match_one_or_more_range('0'..='9') {
            if self.match_char('.').is_some() {
                self.match_one_or_more_range('0'..='9');
                TokenKind::RealLiteral
            } else {
                TokenKind::IntLiteral
            }
        } else if self.match_char_of("hH".chars()).is_some() {
            self.match_one_or_more_fn(|c| c.is_ascii_hexdigit());
            TokenKind::IntHexLiteral
        } else if self.match_char_of("bB".chars()).is_some() {
            self.match_one_or_more_fn(|c| *c == '0' || *c == '1');
            TokenKind::IntBinLiteral
        } else {
            TokenKind::Error
        }
    }
}

impl<T: Iterator<Item = char>> Iterator for Tokenize<T> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.finished {
            return None;
        }
        self.skip_ws();
        self.clear_buffer();
        match self.match_num() {
            TokenKind::Error => {}
            kind => return Some(self.create_token(kind)),
        }
        let c = self.consume_char();
        if c.is_none() {
            self.finished = true;
            return Some(self.create_token(TokenKind::Eof));
        }
        let res = match c.unwrap() {
            '(' => self.create_token(TokenKind::OpenParen),
            ')' => self.create_token(TokenKind::CloseParen),
            '[' => self.create_token(TokenKind::OpenBracket),
            ']' => self.create_token(TokenKind::CloseBracket),
            '{' => self.create_token(TokenKind::OpenBrace),
            '}' => self.create_token(TokenKind::CloseBrace),
            '/' => self.create_token(TokenKind::Div),
            '=' => {
                if self.match_char('=').is_some() {
                    self.create_token(TokenKind::Eq)
                } else {
                    self.create_token(TokenKind::Assign)
                }
            }
            '+' => self.create_token(TokenKind::Add),
            '*' => self.create_token(TokenKind::Mul),
            '-' => match self.match_num() {
                TokenKind::Error => {
                    if self.match_char('>').is_some() {
                        self.create_token(TokenKind::Load)
                    } else {
                        self.create_token(TokenKind::Sub)
                    }
                }
                kind => self.create_token(kind),
            },
            '<' => {
                if self.match_char('-').is_some() {
                    self.create_token(TokenKind::Store)
                } else if self.match_char('=').is_some() {
                    self.create_token(TokenKind::LessEq)
                } else {
                    self.create_token(TokenKind::Less)
                }
            }
            '>' => {
                if self.match_char('=').is_some() {
                    self.create_token(TokenKind::GreaterEq)
                } else {
                    self.create_token(TokenKind::Greater)
                }
            }
            '@' | '%' => {
                while self.match_alnum().is_some() {}
                self.create_token(TokenKind::SpaceId)
            }
            '0'..='9' => {
                self.match_one_or_more_range('0'..='9');
                if self.match_char('.').is_some() {
                    self.match_one_or_more_range('0'..='9');
                    self.create_token(TokenKind::RealLiteral)
                } else {
                    self.create_token(TokenKind::IntLiteral)
                }
            }
            '#' => {
                while self.match_alnum().is_some() {}
                self.create_token(TokenKind::BlockId)
            }
            '!' => {
                if self.match_char('=').is_some() {
                    self.create_token(TokenKind::Ne)
                } else {
                    self.create_token(TokenKind::Not)
                }
            }
            'a' => {
                if self.match_string("nd").is_some() {
                    self.create_token(TokenKind::And)
                } else {
                    self.error_token()
                }
            }
            'b' => {
                if self.match_string("ool").is_some() {
                    self.create_token(TokenKind::Bool)
                } else if self.match_one_or_more_range('0'..='1') {
                    self.create_token(TokenKind::IntBinLiteral)
                } else {
                    self.error_token()
                }
            }
            'f' => {
                if self.match_string("n").is_some() {
                    self.create_token(TokenKind::Fn)
                } else if self.match_string("64").is_some() {
                    self.create_token(TokenKind::F64)
                } else {
                    self.error_token()
                }
            }
            'h' => {
                if self.match_one_or_more_fn(|c| c.is_ascii_hexdigit()) {
                    self.create_token(TokenKind::IntHexLiteral)
                } else {
                    self.error_token()
                }
            }
            'i' => {
                if self.match_string("64").is_some() {
                    self.create_token(TokenKind::I64)
                } else {
                    self.error_token()
                }
            }
            'g' => {
                if self.match_string("oto").is_some() {
                    self.create_token(TokenKind::Goto)
                } else {
                    self.error_token()
                }
            }
            'n' => {
                if self.match_string("ext").is_some() {
                    self.create_token(TokenKind::Next)
                } else {
                    self.error_token()
                }
            }
            'o' => {
                if self.match_string("r").is_some() {
                    self.create_token(TokenKind::Or)
                } else {
                    self.error_token()
                }
            }
            'r' => {
                if self.match_string("et").is_some() {
                    self.create_token(TokenKind::Ret)
                } else {
                    self.error_token()
                }
            }
            'v' => {
                if self.match_string("oid").is_some() {
                    self.create_token(TokenKind::Void)
                } else {
                    self.error_token()
                }
            }
            '.' => self.create_token(TokenKind::Dot),
            ',' => self.create_token(TokenKind::Comma),
            ':' => self.create_token(TokenKind::Colon),

            _ => self.error_token(),
        };
        Some(res)
    }
}
trait Tokenizer<T: Iterator<Item = char>> {
    fn tokenize(self) -> Tokenize<T>;
}
impl<T: Iterator<Item = char>> Tokenizer<T> for T {
    fn tokenize(self) -> Tokenize<T> {
        Tokenize::new(self.peekable())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn tokenization() {
        let src =
            "fn @a(@m: i64, @n: i64) -> i64 { %x = @m + @n %1 = -2.34 + h10F %2 = %1 - -b0101 }";
        assert!(src.chars().tokenize().all(|t| {
            println!("{}", t);
            !matches!(t.kind, TokenKind::Error)
        }));
    }
    #[test]
    fn tokenization_err() {
        let src = "fnaesrys75i 9    uhh 9[]((";
        src.chars().tokenize().for_each(|t| println!("{}", t))
    }
}
