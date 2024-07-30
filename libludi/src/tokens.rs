use std::hash::Hash;

use crate::{
    err::{LangError, Result},
    err_at_tok, parse_err,
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
    COMMA,
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

    FN,
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

    EOF,
}

pub fn same_variant(lhs: &Token, rhs: &Token) -> bool {
    std::mem::discriminant(lhs) == std::mem::discriminant(rhs)
}

#[derive(Eq, Debug, Clone, PartialEq)]
pub struct TokenData {
    pub token: Token,
    pub line: usize,
}
