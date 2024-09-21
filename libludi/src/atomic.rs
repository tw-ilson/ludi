use crate::array::Array;
use crate::ast::CallSignature;
use crate::data::{Data, DataType, ArrayType, AtomicType};
use crate::err::Result;
use crate::ops::*;
use crate::tokens::{Token, TokenData};
use std::fmt::{write, Debug, Display};
use std::hash::Hash;
use std::rc::Rc;

trait Atomic {}


impl AtomicType {
    pub fn descriminant(&self) -> u8 {
        unsafe { *<*const _>::from(self).cast::<u8>() }
    }
    pub fn can_upcast_to(&self, other: &Self) -> bool {
        self.descriminant() < other.descriminant()
    }
    pub fn upcast_to(self, descriminant: u8) -> Self {
        //unimplemented!()
        self
    }
    pub fn upgrade(self) -> ArrayType {
        match self {
            Self::UInt8(x) => ArrayType::UInt8(Array::scaler(x)),
            Self::Int8(x) => ArrayType::Int8(Array::scaler(x)),
            Self::UInt16(x) => ArrayType::UInt16(Array::scaler(x)),
            Self::Int16(x) => ArrayType::Int16(Array::scaler(x)),
            Self::UInt32(x) => ArrayType::UInt32(Array::scaler(x)),
            Self::Int32(x) => ArrayType::Int32(Array::scaler(x)),
            Self::UInt64(x) => ArrayType::UInt64(Array::scaler(x)),
            Self::Int64(x) => ArrayType::Int64(Array::scaler(x)),
            Self::BFloat16(x) => ArrayType::BFloat16(Array::scaler(x)),
            Self::Float16(x) => ArrayType::Float16(Array::scaler(x)),
            Self::Float32(x) => ArrayType::Float32(Array::scaler(x)),
            Self::Float64(x) => ArrayType::Float64(Array::scaler(x)),
            Self::Complex(x) => ArrayType::Complex(Array::scaler(x)),
            Self::Character(c) => ArrayType::Character(Array::scaler(c)),
            Self::Boolean(b) => ArrayType::Boolean(Array::scaler(b)),
            Self::Box(a) => ArrayType::Box(Array::scaler(a.into())),
            Self::Fn(f) => ArrayType::Fn(Array::scaler(f))
        }
    }
}

impl From<AtomicType> for ArrayType {
    fn from(value: AtomicType) -> Self {
        value.upgrade()
    }
}

impl From<TokenData> for AtomicType {
    fn from(value: TokenData) -> Self {
        use AtomicType::*;
        use Token::*;
        match value.token {
            FLOAT_LITERAL(literal) => Float64(
                literal
                    .parse::<f64>()
                    .expect("uncaught error. expected floating point literal"),
            ),
            INTEGER_LITERAL(literal) => Int64(
                literal
                    .parse::<i64>()
                    .expect("uncaught error. expected integer literal"),
            ),
            TRUE => Boolean(true),
            FALSE => Boolean(false),
            _ => {
                dbg!(value.token);
                panic!()
            }
        }
    }
}
