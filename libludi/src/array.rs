use std::fmt::Display;
use std::mem::Discriminant;
use std::ops::Index;

use crate::ast::FrameNode;
// use crate::atomic::AtomicType;
use crate::data::{ArrayType, AtomicType, Data, DataType, DataTypeTag, TypeSignature};
use crate::err::{LangError, Result};
use crate::{ops::*, runtime_err};
use smallvec::{smallvec, SmallVec};

pub type ShapeVec = SmallVec<[usize; 4]>; // TODO: use newtype pattern

#[derive(Clone, Hash, Debug, PartialEq, Eq)]
pub struct Shape {
    s: ShapeVec,
}

#[ambassador::delegatable_trait]
pub trait ArrayProps {
    fn shape_slice(&self) -> &[usize];
    fn shape(&self) -> crate::array::Shape;
    fn rank(&self) -> usize;
    fn cardinality(&self) -> usize;
}

#[ambassador::delegatable_trait]
pub trait ShapeOps {
    fn reshape(&mut self, newshape: &[usize]) -> Result<()>;
}

// Row-major Array
#[derive(ambassador::Delegate, Clone, Hash, Debug, PartialEq, Eq)]
#[delegate(ArrayProps, target="shape")]
#[repr(C)]
pub struct Array<T> {
    pub shape: Shape,
    pub data: Vec<T>,
}

impl ArrayType {
    fn signature(&self) -> TypeSignature {
        use DataTypeTag::*;
        match self {
            Self::UInt8(_) => TypeSignature(UInt8, self.shape()),
            Self::Int8(_) =>TypeSignature(Int8, self.shape()),
            Self::UInt16(_) =>TypeSignature(UInt16, self.shape()),
            Self::Int16(_) =>TypeSignature(Int16, self.shape()),
            Self::UInt32(_) =>TypeSignature(UInt32, self.shape()),
            Self::Int32(_) =>TypeSignature(Int32, self.shape()),
            Self::Int64(_) =>TypeSignature(Int64, self.shape()),
            Self::UInt64(_) =>TypeSignature(UInt64, self.shape()),
            Self::BFloat16(_) =>TypeSignature(BFloat16, self.shape()),
            Self::Float16(_) =>TypeSignature(Float16, self.shape()),
            Self::Float32(_) =>TypeSignature(Float32, self.shape()),
            Self::Float64(_) =>TypeSignature(Float64, self.shape()),
            Self::Complex(_) =>TypeSignature(Complex, self.shape()),
            Self::Character(_) =>TypeSignature(Character, self.shape()),
            Self::Boolean(_) =>TypeSignature(Boolean, self.shape()),
            Self::Box(_) => todo!(),
            Self::Fn(_) => todo!(),
        }
    }
}

impl ArrayProps for Shape {
    fn shape_slice(&self) ->  &[usize] {
        &self.s
    }
    fn shape(&self) -> Shape {
        self.clone()
    }
    fn rank(&self) -> usize {
        self.s.len()
    }
    fn cardinality(&self) -> usize {
        self.s.iter().sum()
    }
}


impl ShapeOps for Shape {
    fn reshape(&mut self, newshape: &[usize]) -> Result<()> {
        *self = Shape::new(newshape);
        Ok(())
    }
}

impl<T> ShapeOps for Array<T> {
    fn reshape(&mut self, newshape: &[usize]) -> Result<()> {
        self.shape.reshape(newshape);
        Ok(())
    }
}

impl<T: Clone> Array<T> {
    pub fn new(shape: &[usize], data: &[T]) -> Self {
        Array {
            shape: Shape::new(shape),
            data: Vec::from(data),
        }
    }
    pub fn scaler(x: T) -> Self {
        Self::new(&[], &[x])
    }
    pub fn vector(xs: &[T]) -> Self {
        Self::new(&[xs.len()], xs)
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
        let shape = smallvec![data.len()].into();
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
                    shape: smallvec![n].into(),
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
                        let mut newaxis = 0;
                        let rest_of_shape = first.shape.clone();
                        Array {
                            data : cells.map(|array|
                            if let ArrayType::$variant(array_base) = array {
                                newaxis +=1;
                                Ok(array_base.data_raw().into_iter())
                            } else  {
                                runtime_err!("Frame error: mismatched types or shape in frame")
                            }).flatten_ok().collect::<Result<Vec<_>>>()?,
                            shape: Shape::new(&[newaxis]).concat(rest_of_shape),
                        }
                }),
            )*
            None => panic!("Tried to build frame from empty iterator"),
        })
    }
}
    };
}
frame_impl! {
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
    Box
    Fn
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
        write!(f, "{}", fmt_help(&self.data, self.shape.s.as_slice()))
    }
}

impl Shape {
    pub fn new(s: &[usize]) -> Shape {
        Shape {
            s: ShapeVec::from_slice(s),
        }
    }
    pub fn concat(self, other: Self) -> Self {
        Self {
            s: self.s.into_iter().chain(other.s.into_iter()).collect(),
        }
    }
}

impl Index<usize> for Shape {
    type Output = usize;
    fn index(&self, index: usize) -> &Self::Output {
        self.s.index(index)
    }
}

impl From<ShapeVec> for Shape {
    fn from(value: ShapeVec) -> Self {
        Self { s: value }
    }
}
