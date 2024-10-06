
use libludi::shape::ambassador_impl_ArrayProps;
use libludi::shape::ambassador_impl_ShapeOps;
use libludi::shape::{Shape, ArrayProps, ShapeOps};
use libludi::ast::FrameNode;
use libludi::err::{Error, LudiError, Result};
use std::fmt::Display;
use std::mem::Discriminant;
use std::ops::Index;
use crate::datatypes::ArrayType;

// use crate::data::{OptionalTypeSignature, TypeSignature};

// Row-major Array
#[derive(ambassador::Delegate, Clone, Hash, Debug, PartialEq, Eq)]
#[delegate(ArrayProps, target = "shape")]
#[repr(C)]
pub struct Array<T> {
    pub shape: Shape,
    pub data: Vec<T>,
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
        let shape = Shape::new(&[data.len()]);
        Self { shape, data }
    }
}

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
                    shape: Shape::new(&[n]),
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
                                Err(Error::runtime_err("Frame error: mismatched types or shape in frame"))
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
    Int
    Float
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
        write!(f, "{}", fmt_help(&self.data, self.shape.shape_slice()))
    }
}

