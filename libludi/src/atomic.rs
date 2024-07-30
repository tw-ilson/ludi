use crate::array::{Array, ArrayType};
use crate::data::Data;
use crate::err::Result;
use crate::ops::*;
use crate::tokens::{Token, TokenData};
use std::fmt::{write, Debug, Display};
use std::hash::Hash;
use std::rc::Rc;

trait Atomic {}

#[repr(C, u8)]
#[derive(derive_more::Display, Debug, Copy, Clone, PartialEq)]
pub enum AtomicType {
    // Numberic
    UInt8(u8) = 0,
    Int8(i8) = 1,
    UInt16(u16) = 2,
    Int16(i16) = 3,
    UInt32(u32) = 4,
    Int32(i32) = 5,
    Int64(i64) = 6,
    UInt64(u64) = 7,
    BFloat16(half::bf16) = 8,
    Float16(half::f16) = 9,
    Float32(f32) = 10,
    Float64(f64) = 11,
    Complex(num::Complex<f32>) = 12,

    //Non-Numeric
    Character(char),
    Boolean(bool),
}

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
            Self::UInt8(x) => ArrayType::UInt8(Array::new(&[], &[x])),
            Self::Int8(x) => ArrayType::Int8(Array::new(&[], &[x])),
            Self::UInt16(x) => ArrayType::UInt16(Array::new(&[], &[x])),
            Self::Int16(x) => ArrayType::Int16(Array::new(&[], &[x])),
            Self::UInt32(x) => ArrayType::UInt32(Array::new(&[], &[x])),
            Self::Int32(x) => ArrayType::Int32(Array::new(&[], &[x])),
            Self::UInt64(x) => ArrayType::UInt64(Array::new(&[], &[x])),
            Self::Int64(x) => ArrayType::Int64(Array::new(&[], &[x])),
            Self::BFloat16(x) => ArrayType::BFloat16(Array::new(&[], &[x])),
            Self::Float16(x) => ArrayType::Float16(Array::new(&[], &[x])),
            Self::Float32(x) => ArrayType::Float32(Array::new(&[], &[x])),
            Self::Float64(x) => ArrayType::Float64(Array::new(&[], &[x])),
            Self::Complex(x) => ArrayType::Complex(Array::new(&[], &[x])),
            Self::Character(c) => ArrayType::Character(Array::new(&[], &[c])),
            Self::Boolean(b) => ArrayType::Boolean(Array::new(&[], &[b])),
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
