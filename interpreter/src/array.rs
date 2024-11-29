use crate::datatypes::{ArrayType, AtomicType};
use libludi::ast::FrameNode;
use libludi::err::{Error, LudiError, Result};
use libludi::shape::ambassador_impl_ArrayProps;
use libludi::shape::ambassador_impl_ShapeOps;
use libludi::shape::{ArrayProps, Shape, ShapeOps};
use std::fmt::Display;
use std::mem::Discriminant;
use std::ops::Index;

// use crate::data::{OptionalTypeSignature, TypeSignature};

// Row-major Array
#[derive(ambassador::Delegate, Hash, Debug, Clone, PartialEq, Eq)]
#[delegate(ArrayProps, target = "shape")]
#[delegate(ShapeOps, target = "shape")]
#[repr(C)]
pub struct Array<T> {
    pub shape: Shape,
    pub data: Vec<T>,
}

// impl<T> FrameView<'_, T> {
//     fn get(idx: &[usize])
// }

impl<T> Array<T> {
    pub fn new(shape: &[usize], data: Vec<T>) -> Self {
        Array {
            shape: Shape::new(shape),
            data: Vec::from(data),
        }
    }
    pub fn scaler(x: T) -> Self {
        Self::new(&[], vec![x])
    }
    pub fn vector(xs: Vec<T>) -> Self {
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
                (0..self.rank())
                    .rev()
                    .scan(1, |acc, i| {
                        let res = *acc * idxs[i];
                        *acc *= self.shape[i];
                        Some(res)
                    })
                    .inspect(|x| {dbg!(x);})
                    .sum::<usize>(),
            )
        }
    }
    pub fn flat_view<'a>(&'a self, frame_shape: &[usize]) -> Option<FlatFrameView<'a, T>> {
        if frame_shape.len() > self.rank() {
            None
        } else {
            if self.shape_slice()[..frame_shape.len()] != *frame_shape {
                None
            } else {
                Some(FlatFrameView {
                    idx: 0,
                    shape: Shape::new(frame_shape),
                    item_shape: Shape::new(&self.shape_slice()[frame_shape.len()..]),
                    buffer: self.data(),
                })
            }
        }
    }
    pub fn get_subarray(&self, idxs: &[usize], shape: &Shape) -> Option<&[T]> {
        if idxs.len() > self.rank() {
            None
        } else {
            let start_ptr = (0..self.rank())
                .rev()
                .scan(1, |acc, i| {
                    let res = *acc * idxs[i];
                    *acc = self.shape[i];
                    Some(res)
                })
                .sum::<usize>();
            self.data.get(start_ptr..start_ptr + shape.volume())
        }
    }
}

pub struct FlatFrameView<'a, T> {
    idx: usize,
    shape: Shape,
    item_shape: Shape,
    buffer: &'a [T],
}

impl<'a, T> Iterator for FlatFrameView<'a, T> {
    type Item = &'a [T];
    fn next(&mut self) -> Option<Self::Item> {
        if self.idx >= self.shape.volume() {
            None
        } else {
            let item = self
                .buffer
                .get((self.idx * self.item_shape.volume())..((self.idx + 1) * self.item_shape.volume()));
            self.idx += 1;
            return item;
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

impl<A> IntoIterator for Array<A> {
    type Item = A;
    type IntoIter = std::vec::IntoIter<A>;
    fn into_iter(self) -> Self::IntoIter {
        self.data.into_iter()
    }
}
impl TryInto<Shape> for Array<isize> {
    type Error = Error;
    fn try_into(self) -> Result<Shape> {
        if self.rank() != 1 {
            Err(anyhow::anyhow!(
                "Non-vector array shape: rank={}",
                self.rank()
            ))
        } else {
            Ok(self
                .data()
                .iter()
                .map(|i| usize::try_from(*i))
                .process_results(|iter| iter.collect::<Shape>())?)
        }
    }
}

impl TryInto<Shape> for Array<usize> {
    type Error = Error;
    fn try_into(self) -> Result<Shape> {
        if self.rank() != 1 {
            Err(anyhow::anyhow!(
                "Non-vector array shape: rank={}",
                self.rank()
            ))
        } else {
            Ok(Shape::new(self.data()))
        }
    }
}

pub trait MapAxis<T> {}

pub trait Iota {
    fn iota(n: usize) -> Self;
    // fn iota_range(s:isize, e:isize) -> Self;
}

macro_rules! iota_impl {
    ($($prim:ty),*) => {
        $(
        impl Iota for Array<$prim> {
            fn iota(n: usize) -> Self {
                Array {
                    shape: Shape::new(&[n]),
                    data: (1..=n as $prim).collect()
                }
            }
            // fn iota_range(s: isize, e:isize) -> Self {
            //     Array {
            //         shape: Shape::new(&[n]),
            //         data: (s..e as $prim).collect()
            //     }
            // }
        }
    )*
    };
}
iota_impl!(i8, i16, i32, i64, isize, u8, u16, u32, u64, usize);

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
    Index
    Float
    Complex
    Character
    Boolean
    // Box
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
                            "\n    {}",
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
        write!(f, "{}", fmt_help(&self.data, self.shape.shape_slice()).trim_start())
    }
}
