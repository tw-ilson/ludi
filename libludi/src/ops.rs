/* -- libudi::ops --
 * defines primitive ops supported by the language.
 *
 * TODO: This currently defines runtime behavior of operations for the interpreter, and it needs to
 * eventually be replaced by codegen in some form.
 */

use crate::array::{Array, ArrayType, NumberArrayType};
use crate::atomic::{AtomicType, NumberType};
use crate::data::{Data, DataType};
use crate::err::{LangError, Result};
use either::Either;
use itertools::izip;

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

impl<T> Neg for Array<T>
where
    T: Neg + Copy,
{
    fn neg(self) -> Result<Self> {
        Ok(Array::new(
            self.shape(),
            &self
                .data()
                .iter()
                .map_while(|a| a.neg().ok())
                .collect::<Vec<T>>(),
        ))
    }
}
// impl<T> Inv for Array<T> {
//     fn inv(&self) -> Result<Self>
//         where
//             Self: Sized {
//         todo!()
//     }
// }

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
                fn $fname(self, rhs: Self::Rhs) -> Result<Self> {
                    match (self,rhs) {
                        (ArrayType::Number(a), ArrayType::Number(b)) => Ok(ArrayType::Number(a.$fname(b)?)),
                        _ => {unimplemented!()}
                    }
                }
            }
        )*
    };
}
delegate_binops_arraytype!(Add add, Sub sub, Mul mul, Div div);

macro_rules! delegate_binops_numberarraytype {
    ($($trait:ident $fname:ident),*) => {
        $(
            impl $trait for NumberArrayType {
                type Rhs = Self;
                fn $fname(self, rhs: Self::Rhs) -> Result<Self>{
                    match (self,rhs) {
                        (NumberArrayType::UInt8(a), NumberArrayType::UInt8(b))  => Ok(NumberArrayType::UInt8(a.add(b)?)),
                        (NumberArrayType::Int8(a), NumberArrayType::Int8(b))   => Ok(NumberArrayType::Int8(a.$fname(b)?)),
                        (NumberArrayType::UInt16(a), NumberArrayType::UInt16(b)) => Ok(NumberArrayType::UInt16(a.$fname(b)?)),
                        (NumberArrayType::Int16(a), NumberArrayType::Int16(b))  => Ok(NumberArrayType::Int16(a.$fname(b)?)),
                        (NumberArrayType::UInt32(a), NumberArrayType::UInt32(b)) => Ok(NumberArrayType::UInt32(a.$fname(b)?)),
                        (NumberArrayType::Int32(a), NumberArrayType::Int32(b))  => Ok(NumberArrayType::Int32(a.$fname(b)?)),
                        (NumberArrayType::UInt64(a), NumberArrayType::UInt64(b)) => Ok(NumberArrayType::UInt64(a.$fname(b)?)),
                        (NumberArrayType::Int64(a), NumberArrayType::Int64(b))  => Ok(NumberArrayType::Int64(a.$fname(b)?)),
                        (NumberArrayType::BFloat16(a), NumberArrayType::BFloat16(b)) => Ok(NumberArrayType::BFloat16(a.$fname(b)?)),
                        (NumberArrayType::Float16(a), NumberArrayType::Float16(b))  => Ok(NumberArrayType::Float16(a.$fname(b)?)),
                        (NumberArrayType::Float32(a), NumberArrayType::Float32(b)) => Ok(NumberArrayType::Float32(a.$fname(b)?)),
                        (NumberArrayType::Float64(a), NumberArrayType::Float64(b))  => Ok(NumberArrayType::Float64(a.$fname(b)?)),
                        (NumberArrayType::Complex(a), NumberArrayType::Complex(b))  => Ok(NumberArrayType::Complex(a.$fname(b)?)),
                        _ => {unimplemented!()}
                    }
                }
            }
        )*
    };
}

delegate_binops_numberarraytype!(
    Add add,
    Sub sub,
    Mul mul,
    Div div
);

// macro_rules! delegate_unops_arraytype {
//     ($($trait:ident $fname:ident),*) => {
//         $(
//             impl $trait for NumberArrayType {
//                 fn $fname(self) -> Result<Self>{
//                     match self {
//                         NumberArrayType::UInt8(a)  => Ok(NumberArrayType::UInt8(a.$fname()?)),
//                         NumberArrayType::Int8(a)   => Ok(NumberArrayType::Int8(a.$fname()?)),
//                         NumberArrayType::UInt16(a) => Ok(NumberArrayType::UInt16(a.$fname()?)),
//                         NumberArrayType::Int16(a)  => Ok(NumberArrayType::Int16(a.$fname()?)),
//                         NumberArrayType::UInt32(a) => Ok(NumberArrayType::UInt32(a.$fname()?)),
//                         NumberArrayType::Int32(a)  => Ok(NumberArrayType::Int32(a.$fname()?)),
//                         NumberArrayType::UInt64(a) => Ok(NumberArrayType::UInt64(a.$fname()?)),
//                         NumberArrayType::Int64(a)  => Ok(NumberArrayType::Int64(a.$fname()?)),
//                         NumberArrayType::BFloat16(a) => Ok(NumberArrayType::BFloat16(a.$fname()?)),
//                         NumberArrayType::Float16(a)  => Ok(NumberArrayType::Float16(a.$fname()?)),
//                         NumberArrayType::Float32(a) => Ok(NumberArrayType::Float32(a.$fname()?)),
//                         NumberArrayType::Float64(a)  => Ok(NumberArrayType::Float64(a.$fname()?)),
//                         NumberArrayType::Complex(a)  => Ok(NumberArrayType::Complex(a.$fname()?)),
//                         _ => {unimplemented!()}
//                     }
//                 }
//             }
//         )*
//     };
// }
// delegate_unops_arraytype!(Neg neg);

macro_rules! delegate_binops_atomictype {
    ($($trait:ident $fname:ident),*) => {
        $(
            impl $trait for AtomicType {
                type Rhs = Self;
                fn $fname(self, rhs: Self::Rhs) -> Result<Self>{
                    match (self,rhs) {
                        (AtomicType::Number(a), AtomicType::Number(b)) => Ok(AtomicType::Number(a.$fname(b)?)),
                        _ => {unimplemented!()}
                    }
                }
            }
        )*
    };
}
delegate_binops_atomictype!(Add add, Sub sub, Mul mul, Div div);

macro_rules! delegate_binops_numbertype {
    ($($trait:ident $fname:ident),*) => {
        $(
            impl $trait for NumberType {
                type Rhs = Self;
                fn $fname(self, rhs: Self::Rhs) -> Result<Self>{
                    match (self,rhs) {
                        (NumberType::UInt8(a), NumberType::UInt8(b))  => Ok(NumberType::UInt8(a.add(b)?)),
                        (NumberType::Int8(a), NumberType::Int8(b))   => Ok(NumberType::Int8(a.$fname(b)?)),
                        (NumberType::UInt16(a), NumberType::UInt16(b)) => Ok(NumberType::UInt16(a.$fname(b)?)),
                        (NumberType::Int16(a), NumberType::Int16(b))  => Ok(NumberType::Int16(a.$fname(b)?)),
                        (NumberType::UInt32(a), NumberType::UInt32(b)) => Ok(NumberType::UInt32(a.$fname(b)?)),
                        (NumberType::Int32(a), NumberType::Int32(b))  => Ok(NumberType::Int32(a.$fname(b)?)),
                        (NumberType::UInt64(a), NumberType::UInt64(b)) => Ok(NumberType::UInt64(a.$fname(b)?)),
                        (NumberType::Int64(a), NumberType::Int64(b))  => Ok(NumberType::Int64(a.$fname(b)?)),
                        (NumberType::BFloat16(a), NumberType::BFloat16(b)) => Ok(NumberType::BFloat16(a.$fname(b)?)),
                        (NumberType::Float16(a), NumberType::Float16(b))  => Ok(NumberType::Float16(a.$fname(b)?)),
                        (NumberType::Float32(a), NumberType::Float32(b)) => Ok(NumberType::Float32(a.$fname(b)?)),
                        (NumberType::Float64(a), NumberType::Float64(b))  => Ok(NumberType::Float64(a.$fname(b)?)),
                        (NumberType::Complex(a), NumberType::Complex(b))  => Ok(NumberType::Complex(a.$fname(b)?)),
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

// macro_rules! delegate_unops_numbertype {
//     ($($trait:ident $fname:ident),*) => {
//         $(
//             impl $trait for NumberType {
//                 fn $fname(self) -> Result<Self>{
//                     match self {
//                         NumberType::UInt8(a)  => Ok(NumberType::UInt8(a.$fname()?)),
//                         NumberType::Int8(a)   => Ok(NumberType::Int8(a.$fname()?)),
//                         NumberType::UInt16(a) => Ok(NumberType::UInt16(a.$fname()?)),
//                         NumberType::Int16(a)  => Ok(NumberType::Int16(a.$fname()?)),
//                         NumberType::UInt32(a) => Ok(NumberType::UInt32(a.$fname()?)),
//                         NumberType::Int32(a)  => Ok(NumberType::Int32(a.$fname()?)),
//                         NumberType::UInt64(a) => Ok(NumberType::UInt64(a.$fname()?)),
//                         NumberType::Int64(a)  => Ok(NumberType::Int64(a.$fname()?)),
//                         NumberType::BFloat16(a) => Ok(NumberType::BFloat16(a.$fname()?)),
//                         NumberType::Float16(a)  => Ok(NumberType::Float16(a.$fname()?)),
//                         NumberType::Float32(a) => Ok(NumberType::Float32(a.$fname()?)),
//                         NumberType::Float64(a)  => Ok(NumberType::Float64(a.$fname()?)),
//                         NumberType::Complex(a)  => Ok(NumberType::Complex(a.$fname()?)),
//                         _ => {unimplemented!()}
//                     }
//                 }
//             }
//         )*
//     };
// }
// delegate_unops_numbertype!(Neg neg);

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
                    return Err(LangError::RuntimeErr("shape error".to_owned()))
                }
                Ok(Array::new(
                    self.shape().into(),
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

impl<T> Inv for T {
    fn inv(self) -> Result<Self> {
        todo!()
    }
}
