/* -- libudi::ops --
 * dynamically handle primitive ops supported by the language.
 */

use crate::array::Array;
use libludi::shape::ArrayProps;
// use crate::atomic::{AtomicType};
use crate::datatypes::{Data, DataType, AtomicType, ArrayType};
use libludi::err::{Error, LudiError, Result};
use itertools::izip;
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
pub trait UnaryOp: Neg + Inv {}
pub trait BinaryOp: Add + Sub + Mul + Div {}

impl BinaryOp for DataType {}
// impl UnaryOp for DataType {}

macro_rules! delegate_binops_data {
    ($($trait:ident $fname:ident),*) => {
        $(
            impl $trait for DataType {
                type Rhs = Self;
                fn $fname(self, rhs: Self::Rhs) -> Result<Self> {
                    match (self, rhs) {
                        (DataType::Array(a), DataType::Array(b)) => Ok(DataType::Array(a.$fname(b)?)),
                        (DataType::Atomic(a), DataType::Atomic(b)) => Ok(DataType::Atomic(a.$fname(b)?)),
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

// macro_rules! delegate_unops_arraytype {
//     ($($trait:ident $fname:ident),*) => {
//         $(
//             impl $trait for ArrayType {
//                 fn $fname(self) -> Result<Self>{
//                     match self {
//                         ArrayType::UInt8(a)  => Ok(ArrayType::UInt8(a.$fname()?)),
//                         ArrayType::Int8(a)   => Ok(ArrayType::Int8(a.$fname()?)),
//                         ArrayType::UInt16(a) => Ok(ArrayType::UInt16(a.$fname()?)),
//                         ArrayType::Int16(a)  => Ok(ArrayType::Int16(a.$fname()?)),
//                         ArrayType::UInt32(a) => Ok(ArrayType::UInt32(a.$fname()?)),
//                         ArrayType::Int32(a)  => Ok(ArrayType::Int32(a.$fname()?)),
//                         ArrayType::UInt64(a) => Ok(ArrayType::UInt64(a.$fname()?)),
//                         ArrayType::Int64(a)  => Ok(ArrayType::Int64(a.$fname()?)),
//                         ArrayType::BFloat16(a) => Ok(ArrayType::BFloat16(a.$fname()?)),
//                         ArrayType::Float16(a)  => Ok(ArrayType::Float16(a.$fname()?)),
//                         ArrayType::Float32(a) => Ok(ArrayType::Float32(a.$fname()?)),
//                         ArrayType::Float64(a)  => Ok(ArrayType::Float64(a.$fname()?)),
//                         ArrayType::Complex(a)  => Ok(ArrayType::Complex(a.$fname()?)),
//                         _ => {unimplemented!()}
//                     }
//                 }
//             }
//         )*
//     };
// }
// delegate_unops_arraytype!(Neg neg);

macro_rules! delegate_binops_numbertype {
    ($($trait:ident $fname:ident),*) => {
        $(
            impl $trait for AtomicType {
                type Rhs = Self;
                fn $fname(self, rhs: Self::Rhs) -> Result<Self>{
                    match (self,rhs) {
                        (AtomicType::Int(a), AtomicType::Int(b))  => Ok(AtomicType::Int(a.$fname(b)?)),
                        (AtomicType::Float(a), AtomicType::Float(b))  => Ok(AtomicType::Float(a.$fname(b)?)),
                        (AtomicType::Complex(a), AtomicType::Complex(b))  => Ok(AtomicType::Complex(a.$fname(b)?)),
                        _ => {unimplemented!()}
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
                if self.shape() != rhs.shape() {
                    return Err(Error::msg("shape error"))
                }
                Ok(Array::new(
                    self.shape_slice(),
                    &izip!(self.data(), rhs.data())
                        .map_while(|(a, b)| a.$fname(*b).ok())
                        .collect::<Vec<T>>(),
                ))
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
            &self
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
    fn neg(self) -> Result<Self> where Self: Sized {
        use AtomicType::*;
        Ok(match self {
            Int(a) => Int(-a),
            Float(a) => Float(-a),
            Complex(a) => Complex(num::Complex::new(-a.re(), a.im())), //subtracts the real
            _ => return Err(Error::msg("unsupported op")),
        })
    }
}

