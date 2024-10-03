use crate::array::ambassador_impl_ArrayProps;
use crate::array::ambassador_impl_ShapeOps;
use crate::array::{Array, ArrayProps, Shape, ShapeOps};
use crate::ast::CallSignature;
use crate::err::{runtime_err, LangError, Result};
use crate::{compile_err, lex_err};

use ambassador::{delegatable_trait, Delegate};
use itertools::Itertools;
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
}
impl Data for DataType {}

pub struct BoxType(Box<DataType>);

#[repr(C, u8)]
#[derive(derive_more::Display, Debug, Clone, PartialEq)]
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
    UInt8(Array<u8>) = 0,
    Int8(Array<i8>) = 1,
    UInt16(Array<u16>) = 2,
    Int16(Array<i16>) = 3,
    UInt32(Array<u32>) = 4,
    Int32(Array<i32>) = 5,
    Int64(Array<i64>) = 6,
    UInt64(Array<u64>) = 7,
    BFloat16(Array<half::bf16>) = 8,
    Float16(Array<half::f16>) = 9,
    Float32(Array<f32>) = 10,
    Float64(Array<f64>) = 11,
    Complex(Array<num::Complex<f32>>) = 12,

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
                            runtime_err!("Frame error: found non conforming value in frame")
                        }
                    })
                    .collect::<Result<Result<ArrayType>>>()??,
                Some(DataType::Atomic(_)) => data_seq
                    .map(|data| {
                        if let DataType::Atomic(a) = data {
                            Ok(a.upgrade())
                        } else {
                            runtime_err!("Frame error: found non conforming value in frame")
                        }
                    })
                    .collect::<Result<Result<ArrayType>>>()??,
                None => runtime_err!("Frame error: empty frame")?,
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
    type Err = LangError;
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
            "box" => lex_err!("Box type not supported in function signature"),
            "fn" => lex_err!("Fn type not suppored in function signature"),
            "()" => lex_err!("Unit type not supported in function signature"),
            _ => lex_err!("not a known builtin type"),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct TypeSignature(pub DataTypeTag, pub Shape);

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct OptionalTypeSignature(pub Option<DataTypeTag>, pub Shape);

impl TryFrom<OptionalTypeSignature> for TypeSignature {
    type Error = LangError;
    fn try_from(value: OptionalTypeSignature) -> Result<Self> {
        Ok(TypeSignature(
            value
                .0
                .unwrap_or(compile_err!("expected explicit type annotations")?),
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
            Self::UInt8(_) => TypeSignature(UInt8, self.shape()),
            Self::Int8(_) => TypeSignature(Int8, self.shape()),
            Self::UInt16(_) => TypeSignature(UInt16, self.shape()),
            Self::Int16(_) => TypeSignature(Int16, self.shape()),
            Self::UInt32(_) => TypeSignature(UInt32, self.shape()),
            Self::Int32(_) => TypeSignature(Int32, self.shape()),
            Self::Int64(_) => TypeSignature(Int64, self.shape()),
            Self::UInt64(_) => TypeSignature(UInt64, self.shape()),
            Self::BFloat16(_) => TypeSignature(BFloat16, self.shape()),
            Self::Float16(_) => TypeSignature(Float16, self.shape()),
            Self::Float32(_) => TypeSignature(Float32, self.shape()),
            Self::Float64(_) => TypeSignature(Float64, self.shape()),
            Self::Complex(_) => TypeSignature(Complex, self.shape()),
            Self::Character(_) => TypeSignature(Character, self.shape()),
            Self::Boolean(_) => TypeSignature(Boolean, self.shape()),
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
