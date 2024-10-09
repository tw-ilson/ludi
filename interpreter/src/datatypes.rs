use crate::array::Array;

use libludi::ast::CallSignature;
use libludi::atomic::Literal;
use libludi::err::{Error, LudiError, Result};
use libludi::shape::ambassador_impl_ArrayProps;
use libludi::shape::ambassador_impl_ShapeOps;
use libludi::shape::{ArrayProps, Shape, ShapeOps};
use libludi::token::Token;

use ambassador::{delegatable_trait, Delegate};
use itertools::Itertools;
use libludi::token::TokenData;
use std::cell::Cell;
use std::hash::Hash;
use std::ops::Deref;
use std::ptr::NonNull;
use std::rc::Rc;
use std::str::FromStr;

// A data is either Array or Atomic
pub trait Data // BinaryOp +
// UnaryOp +
// Hash +
// std::fmt::Display +
// std::fmt::Debug
{
}

#[derive(derive_more::Display, Debug, PartialEq, Clone)]
pub enum DataType {
    Array(ArrayType),
    Atomic(AtomicType),
    Unit
}
impl Data for DataType {}

pub struct BoxType(Box<DataType>);

#[repr(C, u8)]
#[derive(derive_more::Display, Debug, Clone, PartialEq)]
pub enum AtomicType {
    // Numeric
    Int(isize),
    Index(usize),
    Float(f64),
    Complex(num::Complex<f64>),

    //Non-Numeric
    Character(char),
    Boolean(bool),

    //Box
    Box(Box<DataType>),

    //Fn
    Fn(CallSignature),
}

#[repr(C, u8)]
#[derive(derive_more::Display, ambassador::Delegate, Debug, Clone, PartialEq)]
#[delegate(ArrayProps)]
#[delegate(ShapeOps)]
pub enum ArrayType {
    // Numbers
    Int(Array<isize>),
    Index(Array<usize>),
    Float(Array<f64>),
    Complex(Array<num::Complex<f64>>),

    // Non-Numbers
    Character(Array<char>),
    Boolean(Array<bool>),

    Box(Array<Box<DataType>>),
    Fn(Array<CallSignature>),
}

#[repr(u8)]
#[derive(derive_more::Display, Eq, Debug, Copy, Clone, PartialEq)]
pub enum DataTypeTag {
    //Numeric
    UInt8,
    Int8,
    UInt16,
    Int16,
    UInt32,
    Int32,
    UInt64,
    Int64,
    BFloat16,
    Float16,
    Float32,
    Float64,
    Complex,

    //Non-Numeric
    Character,
    Boolean,

    //Box
    Box,

    // Function
    Fn,

    Unit,
}
impl FromIterator<DataType> for Result<DataType> {
    fn from_iter<T: IntoIterator<Item = DataType>>(data_seq: T) -> Result<DataType> {
        Ok(DataType::Array({
            let mut data_seq = data_seq.into_iter().peekable();
            match data_seq.peek() {
                Some(DataType::Array(_)) => data_seq
                    .map(|data| {
                        if let DataType::Array(a) = data {
                            Ok(a)
                        } else {
                            //TODO: add better error information
                            Err(Error::runtime_err(
                                "Frame error: found non conforming value in frame",
                            ))
                        }
                    })
                    .collect::<Result<Result<ArrayType>>>()??,
                Some(DataType::Atomic(_)) => data_seq
                    .map(|data| {
                        if let DataType::Atomic(a) = data {
                            Ok(a.upgrade())
                        } else {
                            Err(Error::runtime_err(
                                "Frame error: found non conforming value in frame",
                            ))
                        }
                    })
                    .collect::<Result<Result<ArrayType>>>()??,
                Some(DataType::Unit) => return Ok(DataType::Unit),
                None => Err(Error::runtime_err("Frame error: empty frame"))?,
            }
        }))
    }
}

impl std::fmt::Display for BoxType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{}>", std::any::type_name_of_val(&self.0))
    }
}

impl FromStr for DataTypeTag {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self> {
        match s {
            "u8" => Ok(DataTypeTag::UInt8),
            "i8" => Ok(DataTypeTag::Int8),
            "u16" => Ok(DataTypeTag::UInt16),
            "i16" => Ok(DataTypeTag::Int16),
            "u32" => Ok(DataTypeTag::UInt32),
            "i32" => Ok(DataTypeTag::Int32),
            "u64" => Ok(DataTypeTag::UInt64),
            "i64" => Ok(DataTypeTag::Int64),
            "bf16" => Ok(DataTypeTag::BFloat16),
            "f16" => Ok(DataTypeTag::Float16),
            "f32" => Ok(DataTypeTag::Float32),
            "f64" => Ok(DataTypeTag::Float64),
            "complex" => Ok(DataTypeTag::Complex),
            "char" => Ok(DataTypeTag::Character),
            "bool" => Ok(DataTypeTag::Boolean),
            "box" => Err(Error::runtime_err(
                "Box type not supported in function signature",
            )),
            "fn" => Err(Error::runtime_err(
                "Fn type not suppored in function signature",
            )),
            "()" => Err(Error::runtime_err(
                "Unit type not supported in function signature",
            )),
            _ => Err(Error::runtime_err("not a known builtin type")),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct TypeSignature(pub DataTypeTag, pub Shape);

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct OptionalTypeSignature(pub Option<DataTypeTag>, pub Shape);

impl TryFrom<OptionalTypeSignature> for TypeSignature {
    type Error = Error;
    fn try_from(value: OptionalTypeSignature) -> Result<Self> {
        Ok(TypeSignature(
            value
                .0
                .ok_or(Error::runtime_err("expected explicit type annotations"))?,
            value.1,
        ))
    }
}

impl std::fmt::Display for TypeSignature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[{}{}]",
            self.0,
            self.1
                .shape_slice()
                .iter()
                .fold("".to_string(), |d, acc| format!("{} {}", acc, d))
        )
    }
}

impl std::fmt::Display for OptionalTypeSignature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[{}{}]",
            if let Some(d) = self.0 {
                d.to_string()
            } else {
                "".to_string()
            },
            self.1
                .shape_slice()
                .iter()
                .fold("".to_string(), |d, acc| format!("{} {}", acc, d))
        )
    }
}

impl ArrayType {
    fn signature(&self) -> TypeSignature {
        use DataTypeTag::*;
        match self {
            Self::Int(_) => TypeSignature(Int64, self.shape().clone()),
            Self::Index(_) => TypeSignature(UInt64, self.shape().clone()),
            Self::Float(_) => TypeSignature(Float64, self.shape().clone()),
            Self::Complex(_) => TypeSignature(Complex, self.shape().clone()),
            Self::Character(_) => TypeSignature(Character, self.shape().clone()),
            Self::Boolean(_) => TypeSignature(Boolean, self.shape().clone()),
            Self::Box(_) => todo!(),
            Self::Fn(_) => todo!(),
        }
    }
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
            Self::Int(x) => ArrayType::Int(Array::scaler(x)),
            Self::Index(x) => ArrayType::Index(Array::scaler(x)),
            Self::Float(x) => ArrayType::Float(Array::scaler(x)),
            Self::Complex(x) => ArrayType::Complex(Array::scaler(x)),
            Self::Character(c) => ArrayType::Character(Array::scaler(c)),
            Self::Boolean(b) => ArrayType::Boolean(Array::scaler(b)),
            Self::Box(a) => ArrayType::Box(Array::scaler(a.into())),
            Self::Fn(f) => ArrayType::Fn(Array::scaler(f)),
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
            FLOAT_LITERAL(literal) => Float(
                literal
                    .parse::<f64>()
                    .expect("uncaught error. expected floating point literal"),
            ),
            INTEGER_LITERAL(literal) => Int(literal
                .parse::<isize>()
                .expect("uncaught error. expected integer literal")),
            TRUE => Boolean(true),
            FALSE => Boolean(false),
            _ => {
                dbg!(value.token);
                panic!()
            }
        }
    }
}

impl From<Literal> for DataType {
    fn from(value: Literal) -> Self {
        Self::Atomic(match value {
            Literal::Int { atom, .. } => AtomicType::Int(atom.parse().unwrap()),
            Literal::Float { atom, .. } => AtomicType::Float(atom.parse().unwrap()),
            Literal::Bool { atom, .. } => AtomicType::Boolean(atom),
            Literal::Char { atom, .. } => AtomicType::Character({
                assert_eq!(atom.len(), 1);
                atom.chars().next().unwrap()
            }),
        })
    }
}
