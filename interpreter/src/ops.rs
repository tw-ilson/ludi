/* -- libudi::ops --
 * dynamically handle primitive ops supported by the language.
 */

use crate::array::Array;
use libludi::shape::ArrayProps;
// use crate::atomic::{AtomicType};
use crate::datatypes::{ArrayType, AtomicType, Data, DataType};
use itertools::izip;
use libludi::err::{Error, LudiError, Result};
use num::complex::ComplexFloat;
use num::Complex;

// Unary Ops
pub trait Neg {
    fn neg(self) -> Result<Self>
    where
        Self: Sized;
}

pub trait Inv {
    fn inv(self) -> Result<Self>
    where
        Self: Sized;
}
// Binary Ops
pub trait Add {
    type Rhs;
    fn add(self, rhs: Self::Rhs) -> Result<Self>
    where
        Self: Sized;
}

pub trait Sub {
    type Rhs;
    fn sub(self, rhs: Self::Rhs) -> Result<Self>
    where
        Self: Sized;
}
pub trait Mul {
    type Rhs;
    fn mul(self, rhs: Self::Rhs) -> Result<Self>
    where
        Self: Sized;
}
pub trait Div {
    type Rhs;
    fn div(self, rhs: Self::Rhs) -> Result<Self>
    where
        Self: Sized;
}
pub trait UnaryOp: Neg {}
pub trait BinaryOp: Add + Sub + Mul + Div {}

impl BinaryOp for DataType {}
impl UnaryOp for DataType {}

macro_rules! delegate_binops_data {
    ($($trait:ident $fname:ident),*) => {
        $(
            impl $trait for DataType {
                fn $fname(self) -> Result<Self> {
                    match self {
                        DataType::Array(a) => Ok(DataType::Array(a.$fname()?)),
                        DataType::Atomic(a) => Ok(DataType::Atomic(a.$fname()?)),
                        DataType::Unit => Err(Error::runtime_err("operating on unit type is not allowed"))
                    }
                }
            }
        )*
    };
}
delegate_binops_data!(Neg neg);

macro_rules! delegate_binops_data {
    ($($trait:ident $fname:ident),*) => {
        $(
            impl $trait for DataType {
                type Rhs = Self;
                fn $fname(self, rhs: Self::Rhs) -> Result<Self> {
                    match (self, rhs) {
                        (DataType::Array(a), DataType::Array(b)) => Ok(DataType::Array(a.$fname(b)?)),
                        (DataType::Atomic(a), DataType::Atomic(b)) => Ok(DataType::Atomic(a.$fname(b)?)),
                        (DataType::Atomic(atom), DataType::Array(array)) => Ok(DataType::Array(array.$fname(atom.into())?)),
                        (DataType::Array(array), DataType::Atomic(atom)) => Ok(DataType::Array(array.$fname(atom.into())?)),
                        _=> todo!()
                    }
                }
            }
        )*
    };
}
delegate_binops_data!(Add add, Sub sub, Mul mul, Div div);

macro_rules! delegate_binops_arraytype {
    ($($trait:ident $fname:ident),*) => {
        $(
            impl $trait for ArrayType {
                type Rhs = Self;
                fn $fname(self, rhs: Self::Rhs) -> Result<Self>{
                    match (self,rhs) {
                        (ArrayType::Int(a), ArrayType::Int(b))  => Ok(ArrayType::Int(a.$fname(b)?)),
                        (ArrayType::Index(a), ArrayType::Index(b))  => Ok(ArrayType::Index(a.$fname(b)?)),
                        (ArrayType::Float(a), ArrayType::Float(b))  => Ok(ArrayType::Float(a.$fname(b)?)),
                        (ArrayType::Complex(a), ArrayType::Complex(b))  => Ok(ArrayType::Complex(a.$fname(b)?)),
                        _ => {unimplemented!()}
                    }
                }
            }
        )*
    };
}

delegate_binops_arraytype!(
    Add add,
    Sub sub,
    Mul mul,
    Div div
);

macro_rules! delegate_unops_arraytype {
    ($($trait:ident $fname:ident),*) => {
        $(
            impl $trait for ArrayType {
                fn $fname(self) -> Result<Self>{
                    match self {
                        ArrayType::Int(a)  => Ok(ArrayType::Int(a.$fname()?)),
                        ArrayType::Float(a)  => Ok(ArrayType::Float(a.$fname()?)),
                        // ArrayType::Complex(a)  => Ok(ArrayType::Complex(a.$fname()?)),
                        _ => {unimplemented!()}
                    }
                }
            }
        )*
    };
}
delegate_unops_arraytype!(Neg neg);

macro_rules! delegate_binops_numbertype {
    ($($trait:ident $fname:ident),*) => {
        $(
            impl $trait for AtomicType {
                type Rhs = Self;
                fn $fname(self, rhs: Self::Rhs) -> Result<Self>{
                    match (self,rhs) {
                        (AtomicType::Int(a), AtomicType::Int(b))  => Ok(AtomicType::Int(a.$fname(b)?)),
                        (AtomicType::Index(a), AtomicType::Index(b))  => Ok(AtomicType::Index(a.$fname(b)?)),
                        (AtomicType::Float(a), AtomicType::Float(b))  => Ok(AtomicType::Float(a.$fname(b)?)),
                        (AtomicType::Complex(a), AtomicType::Complex(b))  => Ok(AtomicType::Complex(a.$fname(b)?)),
                        _ => {Err(Error::msg(format!("incompatible types for {}", stringify!($fname))))}
                    }
                }
            }
        )*
    };
}

delegate_binops_numbertype!(
    Add add,
    Sub sub,
    Mul mul,
    Div div
);

macro_rules! delegate_binops_std_array {
    ($($trait:ident $fname:ident),*) => {
        $(
        impl<T> $trait for Array<T>
        where
            T: Copy + $trait<Rhs = T>,
        {
            type Rhs = Self;
            fn $fname(self, rhs: Self) -> Result<Self> {
                // if self.shape() != rhs.shape() {
                //     return Err(Error::msg("shape error"))
                // }

                match self.shape().subshape_fit(rhs.shape()) {
                    Some(&[]) => Ok(Array::new(
                        self.shape_slice(),
                        izip!(self.data(), rhs.data())
                            .map_while(|(a, b)| a.$fname(*b).ok())
                            .collect::<Vec<T>>(),
                    )),
                    Some(shape_diff) =>
                        Ok(Array::new(
                                self.shape_slice(),
                                {
                                    let flat_view = self.flat_view(shape_diff).expect("shape error");
                                    flat_view.into_iter().flat_map(|subarray| {
                                        subarray.iter().zip(rhs.data()).map_while(|(a, b)| a.$fname(*b).ok())
                                    }).collect::<Vec<T>>()
                                }
                                )
                            ),
                    None => match rhs.shape().subshape_fit(self.shape()) {
                        Some(shape_diff) =>
                            Ok(Array::new(
                                    rhs.shape_slice(),
                                    {
                                        let flat_view = rhs.flat_view(shape_diff).expect("shape error");
                                        flat_view.into_iter().flat_map(|subarray| {
                                            subarray.iter().zip(self.data()).map_while(|(a, b)| a.$fname(*b).ok())
                                        }).collect::<Vec<T>>()
                                    }
                                    )
                                ),
                        None => return Err(Error::msg("shape error"))},
                }
            }
        }
        )*
    };
}
delegate_binops_std_array!(
    Add add,
    Sub sub,
    Mul mul,
    Div div
);
macro_rules! delegate_binops_std {
    ($($trait:ident $fname:ident ($op:tt)),*) => {
        $(
        impl<T> $trait for T
        where
            T: std::ops::$trait<Output = T>,
        {
            type Rhs = Self;
            fn $fname(self, rhs: Self) -> Result<Self> {
                Ok(self $op rhs)
            }
        }
        )*
    };
}
delegate_binops_std!(
    Add add (+),
    Sub sub (-),
    Mul mul (*),
    Div div (/)
);

impl<T> Neg for Array<T>
where
    T: Neg + Copy,
{
    fn neg(self) -> Result<Self> {
        Ok(Array::new(
            self.shape_slice(),
            self
                .data()
                .iter()
                .map_while(|a| a.neg().ok())
                .collect::<Vec<T>>(),
        ))
    }
}

impl<T> Inv for T {
    fn inv(self) -> Result<Self> {
        todo!()
    }
}

impl Neg for AtomicType {
    fn neg(self) -> Result<Self>
    where
        Self: Sized,
    {
        use AtomicType::*;
        Ok(match self {
            Int(a) => Int(-a),
            Float(a) => Float(-a),
            // Complex(a) => Complex(num::Complex::new(-a.re(), a.im())), //subtracts the real
            _ => return Err(Error::msg("unsupported op")),
        })
    }
}

impl<T> Neg for T
where
    T: std::ops::Neg<Output = T>,
{
    fn neg(self) -> Result<Self> {
        Ok(-self)
    }
}
