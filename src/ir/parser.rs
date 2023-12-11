use std::{iter::Peekable, ops::Range};

pub enum TokenContent {
    Variable(String),
    Assign,
    Comma,
    Function,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,
    Integer(i64),
    Float(f64),
    Colon,
}

pub struct Token {
    content: TokenContent,
    location: Range<(usize, usize)>,
}

pub struct Tokenize<T: Iterator<Item = char>> {
    iter: Peekable<T>,
}
impl<T: Iterator<Item = char>> Iterator for Tokenize<T> {
    type Item = TokenContent;
    fn next(&mut self) -> Option<Self::Item> {
        unimplemented!()
    }
}

pub trait Tokenizer<T>
where
    T: Iterator<Item = char>,
{
    fn tokenize(&self, iter: T) -> Tokenize<T>;
}
impl<T: Iterator<Item = char>> Tokenizer<T> for T {
    fn tokenize(&self, iter: T) -> Tokenize<T> {
        Tokenize {
            iter: iter.peekable(),
        }
    }
}
