use crate::err::{Error, ErrorKind, Result};
use crate::parse_err;
use crate::token::{Location, Token, TokenData};
use std::fmt::{write, Debug, Display};
use std::hash::Hash;
use std::rc::Rc;

#[derive(Debug, Eq, Clone)]
pub enum Literal {
    Int { loc: Location, atom: String },
    Float { loc: Location, atom: String },
    Char { loc: Location, atom: String },
    Bool { loc: Location, atom: bool },
}

impl TryFrom<TokenData> for Literal {
    type Error = Error;
    fn try_from(value: TokenData) -> Result<Self> {
        let loc = value.loc;
        Ok(match value.token {
            Token::INTEGER_LITERAL(atom) => Literal::Int { loc, atom },
            Token::FLOAT_LITERAL(atom) => Literal::Float { loc, atom },
            Token::TRUE => Literal::Bool { loc, atom: true },
            Token::FALSE => Literal::Bool { loc, atom: false },
            _ => Err(parse_err!(format!("Expected atomic value at location: {}", loc)))?,
        })
    }
}

impl PartialEq for Literal {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Int { atom: atom1, .. }, Self::Int { atom: atom2, .. }) => atom1 == atom2,
            (Self::Float { atom: atom1, .. }, Self::Float { atom: atom2, .. }) => atom1 == atom2,
            (Self::Bool { atom: atom1, .. }, Self::Bool { atom: atom2, .. }) => atom1 == atom2,
            (Self::Char { atom: atom1, .. }, Self::Char { atom: atom2, .. }) => atom1 == atom2,
            _ => false,
        }
    }
}
