use std::io::Cursor;

/// Represents the set of recognizable tokens.
pub enum Token {
    Assign,
    Int,
    Unsigned,
    Ident(String),

    LPar,
    RRar,

    LSq,
    RSq,

    LCb,
    RCb,

    If,

    Plus,
    Minus,
    Star,
    Slash,
    Percent,

    Number(u64),
    Semicolon,
    Goto,
    Colon,
}

pub struct Lexer<'a> {
    cursor: Cursor<&'a [u8]>,
}

/// Represents a lexer error.
pub enum Error {}

type Result<T> = std::result::Result<T, Error>;

impl<'a> Lexer<'a> {
    pub fn new(input: &'a [u8]) -> Self {
        Self {
            cursor: Cursor::new(input),
        }
    }

    pub fn next(&mut self) -> Result<Option<Token>> {
        todo!()
    }
}
