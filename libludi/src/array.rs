use std::any::TypeId;
use std::fmt::Display;
use std::mem::Discriminant;

use crate::atomic::NumberType;
use crate::data::Data;
use crate::err::{LangError, Result};
use crate::ops::*;
use smallvec::{smallvec, SmallVec};

pub type ShapeVec = SmallVec<[u32; 4]>;// TODO: use newtype pattern

// Row-major Array
#[derive(Clone, Hash, Debug, PartialEq, Eq)]
#[repr(C)]
pub struct Array<T> {
    shape: ShapeVec,
    data: Vec<T>,
}

#[repr(u8)]
#[derive(derive_more::Display, Debug, Clone, PartialEq)]
// // #[delegate(Data)]
// #[delegate(crate::ops::Add)]
pub enum ArrayType {
    UInt8(Array<u8>),
    Int8(Array<i8>),
    UInt16(Array<u16>),
    Int16(Array<i16>),
    UInt32(Array<u32>),
    Int32(Array<i32>),
    Int64(Array<i64>),
    UInt64(Array<u64>),
    // BigUInt(Array<num::BigUint>),
    // BigInt(Array<num::BigInt>),
    BFloat16(Array<half::bf16>),
    Float16(Array<half::f16>),
    Float32(Array<f32>),
    Float64(Array<f64>),
    Complex(Array<num::Complex<f32>>),
}

impl<T> Array<T> {
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
        self.shape().len()
    }
    pub fn get(&self, idxs: &[usize]) -> Option<&T> {
        if idxs.len() != self.rank() {
            None
        } else {
            self.data.get(
                self.shape()
                    .iter()
                    .rev()
                    .scan(1usize, |acc, dim| Some((*acc) * (*dim as usize)))
                    .zip(&idxs[..idxs.len() - 1])
                    .fold(0, |acc, (chunk_sz, i)| acc + chunk_sz * i)
                    + idxs[idxs.len() - 1],
            )
        }
    }
}

pub trait MapAxis {}
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

// pub trait MapElem {
//     type Item;
//     fn map_elem<R, F>(&mut self, f: F) -> Array<R>
//     where
//         F: FnMut(&Self::Item) -> R;
// }
// impl<T> MapElem for Array<T> {
//     type Item = T;
//     fn map_elem<R, F>(&mut self, f: F) -> Array<R>
//     where
//         F: FnMut(&Self::Item) -> R,
//     {
//         Array {
//             shape: self.shape.clone(),
//             data: self.data.iter().map(f).collect(),
//         }
//     }
// }

// struct IterableArray<T> {
//     counter: usize,
//     array: Array<T>,
// }
//
// impl<T> Iterator for IterableArray<T>{
//     type Item = T;
//     fn next(&mut self) -> Option<Self::Item> {
//         if let Some(t) = self.array.data().get(self.counter) {
//             self.counter += 1;
//             Some(t)
//         } else {
//             None
//         }
//     }
// }
//
// impl<T> IntoIterator for Array<T> {
//     type Item = T;
//     type IntoIter = IterableArray<T>;
//     fn into_iter(self) -> Self::IntoIter {
//         IterableArray {
//             counter: 0,
//             array: self,
//         }
//     }
// }

impl<T: Display> Display for Array<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn fmt_help<T: Display>(data_slice: &[T], shape_remaining: &[u32]) -> String {
            let mut r = String::new();
            for i in 0..shape_remaining[0] {
                r = format!("{}{}",r,
                    if shape_remaining.len() == 1 {
                        format!("{} ", data_slice[i as usize])
                    } else {
                        format!(
                            "\n{}",
                            fmt_help(
                                &data_slice[
                                    (shape_remaining[1..].iter().product::<u32>()*i) as usize..
                                ],
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

macro_rules! tryfrom_array_impl {
    ($($variant_name:ident($inner_type:ty),)*) => {
    impl TryFrom<Vec<NumberType>> for ArrayType {
        type Error = LangError;
        fn try_from(value: Vec<NumberType>) -> Result<Self> {
            Ok(match value[0] {
                $(NumberType::$variant_name(_) => ArrayType::$variant_name(Array {
                    shape: smallvec![value.len() as u32],
                    data: value
                    .iter()
                    .map(|i| {
                        if let NumberType::$variant_name(n) = i {
                            Ok(*n)
                        } else {
                            Err(LangError::CompileErr("expected uniform types".to_owned()))
                        }
                    })
                    .collect::<Result<Vec<$inner_type>>>()?,
                }),
                )*})
        }
    }
    };
}
tryfrom_array_impl!(
    UInt8(u8),
    Int8(i8),
    UInt16(u16),
    Int16(i16),
    UInt32(u32),
    Int32(i32),
    Int64(i64),
    UInt64(u64),
    // BigUInt(num::BigUint),
    // BigInt(num::BigInt),
    BFloat16(half::bf16),
    Float16(half::f16),
    Float32(f32),
    Float64(f64),
    Complex(num::Complex<f32>),
);

// impl<T> From<Vec<NumberType>> for Array<T> {
//     fn from(values: Vec<NumberType>) -> Self {
//         // we will upcast all values to this type.
//         // TODO: try to make array without checking for cast
//         let mut numtype = values.iter().fold(0u8, |min, i| std::cmp::max(min, i.descriminant()));
//         let data: Vec<T> = values.iter().map(|a| {
//             // do upcasting
//
//         }).collect();
//         Self {shape: smallvec![data.len() as u32], data}
//     }
// }
