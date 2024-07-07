use std::any::TypeId;
use std::fmt::Display;
use std::mem::Discriminant;

use crate::ast::{ArrayNode, FrameNode};
use crate::atomic::{AtomicType, NumberType};
use crate::data::Data;
use crate::err::{LangError, Result};
use crate::ops::*;
use smallvec::{smallvec, SmallVec};

pub type ShapeVec = SmallVec<[u32; 4]>; // TODO: use newtype pattern

// Row-major Array
#[derive(Clone, Hash, Debug, PartialEq, Eq)]
#[repr(C)]
pub struct Array<T> {
    shape: ShapeVec,
    data: Vec<T>,
}

#[repr(u8)]
#[derive(derive_more::Display, Debug, Clone, PartialEq)]
pub enum ArrayType {
    Number(NumberArrayType),
    Character(Array<char>),
    Boolean(Array<bool>),
}

#[repr(u8)]
#[derive(derive_more::Display, Debug, Clone, PartialEq)]
pub enum NumberArrayType {
    UInt8(Array<u8>),
    Int8(Array<i8>),
    UInt16(Array<u16>),
    Int16(Array<i16>),
    UInt32(Array<u32>),
    Int32(Array<i32>),
    Int64(Array<i64>),
    UInt64(Array<u64>),
    BFloat16(Array<half::bf16>),
    Float16(Array<half::f16>),
    Float32(Array<f32>),
    Float64(Array<f64>),
    Complex(Array<num::Complex<f32>>),
}

// How do we make it also look like this?
struct Type(NumberType, ShapeVec);

impl<T> Array<T> {
    pub fn new(shape: &[u32], data: &[T]) -> Self
    where
        T: Clone,
    {
        Array {
            shape: ShapeVec::from_slice(shape),
            data: Vec::from(data),
        }
    }
    // pub fn type(&self) -> Type {
    //
    // }
    pub fn shape(&self) -> &[u32] {
        &self.shape
    }
    pub fn reshape(&mut self, newshape: &[u32]) -> Result<()> {
        self.shape = ShapeVec::from_slice(newshape);
        Ok(())
    }
    pub fn data(&self) -> &[T] {
        &self.data
    }
    pub fn rank(&self) -> usize {
        self.shape.len()
    }
    pub fn get(&self, idxs: &[usize]) -> Option<&T> {
        if idxs.len() != self.rank() {
            None
        } else {
            self.data.get(
                (1..=self.rank())
                    .rev()
                    .scan(1, |acc, i| {
                        let res = *acc * idxs[i - 1];
                        *acc = self.shape[i - 1] as usize;
                        Some(res)
                    })
                    .sum::<usize>(),
            )
        }
    }
}

impl<A> FromIterator<A> for Array<A> {
    fn from_iter<T: IntoIterator<Item = A>>(iter: T) -> Self {
        let data: Vec<A> = iter.into_iter().collect();
        let shape = smallvec![data.len() as u32];
        Self { shape, data }
    }
}

// something like this?
pub trait MapAxis<T> {}

pub trait Iota {
    fn iota(n: u32) -> Self;
}

macro_rules! iota_impl {
    ($($prim:ty),*) => {
        $(
        impl Iota for Array<$prim> {
            fn iota(n: u32) -> Self {
                Array {
                    shape: smallvec![n],
                    data: (0..n as $prim).collect()
                }
            }
        }
    )*
    };
}
iota_impl!(i8, i16, i32, u8, u16, u32);

impl<T: Display> Display for Array<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn fmt_help<T: Display>(data_slice: &[T], shape_remaining: &[u32]) -> String {
            let mut r = String::new();
            for i in 0..shape_remaining[0] {
                r = format!(
                    "{}{}",
                    r,
                    if shape_remaining.len() == 1 {
                        format!("{} ", data_slice[i as usize])
                    } else {
                        format!(
                            "\n{}",
                            fmt_help(
                                &data_slice
                                    [(shape_remaining[1..].iter().product::<u32>() * i) as usize..],
                                &shape_remaining[1..],
                            )
                        )
                    }
                )
            }
            // format!("{}{}", s, r)
            r
        }
        write!(f, "{}", fmt_help(&self.data, &self.shape))
    }
}

// macro_rules! tryfrom_array_impl {
//     ($($variant_name:ident($inner_type:ty),)*) => {
//     impl TryFrom<Vec<NumberType>> for NumberArrayType {
//         type Error = LangError;
//         fn try_from(value: Vec<NumberType>) -> Result<Self> {
//             Ok(match value[0] {
//                 $(NumberType::$variant_name(_) => NumberArrayType::$variant_name(Array {
//                     shape: smallvec![value.len() as u32],
//                     data: value
//                     .iter()
//                     .map(|i| {
//                         if let NumberType::$variant_name(n) = i {
//                             Ok(*n)
//                         } else {
//                             Err(LangError::CompileErr("expected uniform types".to_owned()))
//                         }
//                     })
//                     .collect::<Result<Vec<$inner_type>>>()?,
//                 }),
//                 )*})
//         }
//     }
//     };
// }
// tryfrom_array_impl!(
//     UInt8(u8),
//     Int8(i8),
//     UInt16(u16),
//     Int16(i16),
//     UInt32(u32),
//     Int32(i32),
//     Int64(i64),
//     UInt64(u64),
//     BFloat16(half::bf16),
//     Float16(half::f16),
//     Float32(f32),
//     Float64(f64),
//     Complex(num::Complex<f32>),
// );
