use std::any::TypeId;
use std::fmt::Display;
use std::mem::Discriminant;

use crate::ast::{ArrayNode, FrameNode};
use crate::atomic::{AtomicType, };
use crate::data::{Data, DataTypeTag};
use crate::err::{LangError, Result};
use crate::{ops::*, runtime_err};
use smallvec::{smallvec, SmallVec};
use ambassador::{delegatable_trait, Delegate};

pub type ShapeVec = SmallVec<[usize; 4]>; // TODO: use newtype pattern

#[delegatable_trait]
pub trait ArrayProps {
    fn shape(&self) -> &[usize];
    fn rank(&self) -> usize;
    fn cardinality(&self) -> usize;
}

#[delegatable_trait]
pub trait ShapeOps {
    fn reshape(&mut self, newshape: &[usize]) -> Result<()>;
}

// Row-major Array
#[derive(Clone, Hash, Debug, PartialEq, Eq)]
#[repr(C)]
pub struct Array<T> {
    shape: ShapeVec,
    data: Vec<T>,
}

#[repr(C, u8)]
#[derive(derive_more::Display, Delegate, Debug, Clone, PartialEq)]
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
}

pub type Signature<'a> = (DataTypeTag, &'a [usize]);

impl ArrayType {
    fn signature(&self) -> Signature {
        use DataTypeTag::*;
        match self {
            Self::UInt8(_) => (UInt8, self.shape()),
            Self::Int8(_) => (Int8,self.shape()),
            Self::UInt16(_) => (UInt16,self.shape()),
            Self::Int16(_) => (Int16,self.shape()),
            Self::UInt32(_) => (UInt32,self.shape()),
            Self::Int32(_) => (Int32,self.shape()),
            Self::Int64(_) => (Int64,self.shape()),
            Self::UInt64(_) => (UInt64,self.shape()),
            Self::BFloat16(_) => (BFloat16,self.shape()),
            Self::Float16(_) => (Float16,self.shape()),
            Self::Float32(_) => (Float32,self.shape()),
            Self::Float64(_) => (Float64,self.shape()),
            Self::Complex(_) => (Complex,self.shape()),
            Self::Character(_) => (Character,self.shape()),
            Self::Boolean(_) => (Boolean,self.shape()),
        }
    }
}

impl<A> ArrayProps for Array<A> {
    fn shape(&self) -> &[usize] {
        &self.shape 
    }
    fn rank(&self) -> usize {
        self.shape().len()
    }
    fn cardinality(&self) -> usize {
        self.shape().iter().sum()
    }
}

impl<T> ShapeOps for Array<T> {

    fn reshape(&mut self, newshape: &[usize]) -> Result<()> {
        self.shape = ShapeVec::from_slice(newshape);
        Ok(())
    }
}

impl<T> Array<T> {
    pub fn new(shape: &[usize], data: &[T]) -> Self
    where
        T: Clone,
    {
        Array {
            shape: ShapeVec::from_slice(shape),
            data: Vec::from(data),
        }
    }
    pub fn data(&self) -> &[T] {
        &self.data
    }
    pub fn data_raw(self) -> Vec<T> {
        self.data
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
                        *acc = self.shape[i - 1];
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
        let shape = smallvec![data.len()];
        Self { shape, data }
    }
}

// something like this?
pub trait MapAxis<T> {}

pub trait Iota {
    fn iota(n: usize) -> Self;
}

macro_rules! iota_impl {
    ($($prim:ty),*) => {
        $(
        impl Iota for Array<$prim> {
            fn iota(n: usize) -> Self {
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


use itertools::Itertools;
macro_rules! frame_impl {
    ($($variant:ident)*) => {
impl FromIterator<ArrayType> for Result<ArrayType> {
    fn from_iter<T: IntoIterator<Item = ArrayType>>(cells: T) -> Result<ArrayType> {
        let mut cells = cells.into_iter().peekable();
        Ok(match cells.peek() {
            $(
                Some(ArrayType::$variant(first)) => ArrayType::$variant({
                        let mut newshape = smallvec![0];
                        newshape.extend_from_slice(first.shape());
                        Array {
                            data : cells.map(|array| 
                            if let ArrayType::$variant(array_base) = array {
                                newshape[0] +=1;
                                Ok(array_base.data_raw().into_iter())
                            } else  {
                                runtime_err!("Frame error: mismatched types or shape in frame")
                            }).flatten_ok().collect::<Result<Vec<_>>>()?,
                            shape: newshape
                        }
                }),
            )*
            None => panic!("Tried to build frame from empty iterator"),
        })
    }
}
    };
}
frame_impl!{
    UInt8
    Int8
    UInt16
    Int16
    UInt32
    Int32
    Int64
    UInt64
    BFloat16
    Float16
    Float32
    Float64
    Complex
    Character
    Boolean
}

impl<T: Display> Display for Array<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn fmt_help<T: Display>(data_slice: &[T], shape_remaining: &[usize]) -> String {
            let mut r = String::new();
            for i in 0..shape_remaining[0] {
                r = format!(
                    "{}{}",
                    r,
                    if shape_remaining.len() == 1 {
                        format!("{} ", data_slice[i])
                    } else {
                        format!(
                            "\n{}",
                            fmt_help(
                                &data_slice[(shape_remaining[1..].iter().product::<usize>() * i)..],
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
