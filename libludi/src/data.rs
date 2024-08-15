use crate::array::ArrayType;
use crate::atomic::AtomicType;
use crate::err::{runtime_err, LangError, Result};
use crate::ops::*;
use itertools::Itertools;
use std::cell::Cell;
use std::hash::Hash;
use std::ops::Deref;
use std::ptr::NonNull;
use std::rc::Rc;

// A data is either Array or Atomic
pub trait Data:
 BinaryOp +
 // UnaryOp +
 // Hash +
 std::fmt::Display +
 std::fmt::Debug
{}

#[derive(derive_more::Display, Debug, PartialEq, Clone )]
pub enum DataType {
    Array(ArrayType),
    Atomic(AtomicType),
    // Closure(ClosureType)
}
impl Data for DataType {}

#[repr(u8)]
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum DataTypeTag {
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
        use melior::ir::r#type::{Type, IntegerType};
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
        }
    }
}
