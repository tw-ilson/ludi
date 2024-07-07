use crate::array::Array;
use crate::data::{Data};
use crate::err::Result;
use crate::ops::*;
use crate::tokens::{Token, TokenData};
use std::fmt::{write, Debug, Display};
use std::hash::Hash;
use std::rc::Rc;

trait Atomic {}

#[repr(u8)]
#[derive(derive_more::Display, Debug, Copy, Clone, PartialEq)]
pub enum AtomicType {
    Number(NumberType),
    Character(char),
    Boolean(bool)
}

#[repr(u8)]
#[derive(derive_more::Display, Debug, Copy, Clone, PartialEq)]
pub enum NumberType {
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
}

impl NumberType {
    pub fn descriminant(&self) -> u8 {
        unsafe { *<*const _>::from(self).cast::<u8>() }
    }
    pub fn can_upcast_to(&self, other: &Self) -> bool {
        self.descriminant() < other.descriminant()
    }
    pub fn upcast_to(self, descriminant: u8) -> NumberType {
        //unimplemented!()
        self
    }
}

impl From<TokenData> for AtomicType {
    fn from(value: TokenData) -> Self {
        use NumberType::*;
        use AtomicType::*;
        use Token::*;
        match value.token {
            NUMBER_LITERAL(literal) => {
                if literal.contains(".") {
                    Number(Float64(
                        literal
                            .parse::<f64>()
                            .expect("uncaught error. expected floating point literal"),
                    ))
                } else {
                    Number(Int64(
                        literal
                            .parse::<i64>()
                            .expect("uncaught error. expected integer literal"),
                    ))
                }
            }
            // TRUE => Bool(true),
            // FALSE => Bool(false),
            _ => {
                dbg!(value.token);
                panic!()
            }
        }
    }
}
