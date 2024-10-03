use std::hash::Hash;

use crate::{
    err::{ErrorKind, Result},
    err_at_tok_msg, parse_err,
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

#[derive(derive_more::Display, Debug, Eq, PartialEq, Copy, Clone)]
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

// impl TryFrom<Token> for BinaryOpType {
//     type Error = LangError;
//     fn try_from(value: Token) -> Result<Self> {
//         match value {
//             Token::PLUS => Ok(Self::ADD),
//             Token::MINUS => Ok(Self::SUB),
//             Token::STAR => Ok(Self::MUL),
//             Token::SLASH => Ok(Self::DIV),
//             _ => parse_err!(format!("cannot use {} as binary operator", value)),
//         }
//     }
// }
// impl TryFrom<Token> for UnaryOpType {
//     type Error = LangError;
//     fn try_from(value: Token) -> Result<Self> {
//         match value {
//             Token::MINUS => Ok(Self::NEG),
//             Token::PERCENT => Ok(Self::INV),
//             _ => parse_err!(format!("cannot use {} as unary operator", value)),
//         }
//     }
// }

impl Default for Location {
    fn default() -> Self {
        Location { line: 1 }
    }
}
