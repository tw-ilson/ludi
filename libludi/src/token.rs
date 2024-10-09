use std::hash::Hash;
use serde::{Serialize, Deserialize};

use crate::{
    err::{ErrorKind, Result},
};

#[derive(derive_more::Display, Debug, Clone, PartialEq, Eq)]
pub enum Token {
    PLUS,
    MINUS,
    STAR,
    SLASH,
    EQUAL,
    DOT,
    COLON,
    SEMICOLON,
    PERCENT,
    COMMA,
    VBAR,
    UNDERSCORE,
    BACKSLASH,
    OPEN_BRACKET,
    CLOSE_BRACKET,
    OPEN_BRACE,
    CLOSE_BRACE,
    OPEN_PAREN,
    CLOSE_PAREN,

    AND,
    OR,
    BANG,
    GREATER,
    LESS,

    ARROW,

    GREATER_EQUAL,
    LESS_EQUAL,
    BANG_EQUAL,
    EQUAL_EQUAL,
    PLUS_EQUAL,
    MINUS_EQUAL,
    STAR_EQUAL,
    SLASH_EQUAL,

    IF,
    ELSEIF, 
    ELSE,
    FN,
    IN,
    LET,
    PRINT,
    TRUE,
    FALSE,

    ARRAY,
    FRAME,

    IDENTIFIER(String),
    STRING_LITERAL(String),
    INTEGER_LITERAL(String),
    FLOAT_LITERAL(String),

    EOF, // for throwing errors
}

#[derive(Serialize, Deserialize, derive_more::Display, Debug, Eq, PartialEq, Copy, Clone)]
pub struct Location {
    pub line: usize,
}

pub fn same_variant(lhs: &Token, rhs: &Token) -> bool {
    std::mem::discriminant(lhs) == std::mem::discriminant(rhs)
}

#[derive(Eq, Debug, Clone, PartialEq)]
pub struct TokenData {
    pub token: Token,
    pub loc: Location,
}

impl Default for Location {
    fn default() -> Self {
        Location { line: 1 }
    }
}
