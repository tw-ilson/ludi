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

#[derive(
    derive_more::Display,
    Debug,
    PartialEq,
    Clone,
)]
pub enum DataType {
    Array(ArrayType),
    Atomic(AtomicType),
}
impl Data for DataType {}

pub struct BoxType(Box<DataType>);

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct TypeSignature(pub DataTypeTag, pub Shape);

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct OptionalTypeSignature(pub Option<DataTypeTag>, pub Shape);

#[repr(C, u8)]
#[derive(
    derive_more::Display,
    Debug,
    Clone,
    PartialEq,
)]
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
#[derive(
    derive_more::Display,
    ambassador::Delegate,
    Debug,
    Clone,
    PartialEq,
)]
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

impl DataTypeTag {
    fn into_mlir(self, context: &'static melior::Context) -> melior::ir::Type<'static> {
        use melior::ir::r#type::{IntegerType, Type};
        match self {
            Self::UInt8 => IntegerType::unsigned(context, 8).into(),
            Self::Int8 => IntegerType::signed(context, 8).into(),
            Self::UInt16 => IntegerType::unsigned(context, 16).into(),
            Self::Int16 => IntegerType::signed(context, 16).into(),
            Self::UInt32 => IntegerType::unsigned(context, 32).into(),
            Self::Int32 => IntegerType::signed(context, 32).into(),
            Self::UInt64 => IntegerType::unsigned(context, 64).into(),
            Self::Int64 => IntegerType::signed(context, 64).into(),
            Self::BFloat16 => Type::bfloat16(context),
            Self::Float16 => Type::float16(context),
            Self::Float32 => Type::float32(context),
            Self::Float64 => Type::float64(context),
            Self::Complex => todo!(),
            Self::Character => todo!(),
            Self::Boolean => todo!(),
            Self::Box => todo!(),
            Self::Fn => todo!(),
            Self::Unit => todo!(),
        }
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

impl std::fmt::Display for BoxType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{}>", std::any::type_name_of_val(&self.0))
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
