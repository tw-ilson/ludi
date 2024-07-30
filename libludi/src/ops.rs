/* -- libudi::ops --
 * defines primitive ops supported by the language.
 *
 * TODO: This currently defines runtime behavior of operations for the interpreter, and it needs to
 * eventually be replaced by codegen in some form.
 */

use crate::array::{Array, ArrayType, ArrayProps};
use crate::atomic::{AtomicType};
use crate::data::{Data, DataType};
use crate::err::{LangError, Result};
use either::Either;
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
                        (ArrayType::UInt8(a), ArrayType::UInt8(b))  => Ok(ArrayType::UInt8(a.add(b)?)),
                        (ArrayType::Int8(a), ArrayType::Int8(b))   => Ok(ArrayType::Int8(a.$fname(b)?)),
                        (ArrayType::UInt16(a), ArrayType::UInt16(b)) => Ok(ArrayType::UInt16(a.$fname(b)?)),
                        (ArrayType::Int16(a), ArrayType::Int16(b))  => Ok(ArrayType::Int16(a.$fname(b)?)),
                        (ArrayType::UInt32(a), ArrayType::UInt32(b)) => Ok(ArrayType::UInt32(a.$fname(b)?)),
                        (ArrayType::Int32(a), ArrayType::Int32(b))  => Ok(ArrayType::Int32(a.$fname(b)?)),
                        (ArrayType::UInt64(a), ArrayType::UInt64(b)) => Ok(ArrayType::UInt64(a.$fname(b)?)),
                        (ArrayType::Int64(a), ArrayType::Int64(b))  => Ok(ArrayType::Int64(a.$fname(b)?)),
                        (ArrayType::BFloat16(a), ArrayType::BFloat16(b)) => Ok(ArrayType::BFloat16(a.$fname(b)?)),
                        (ArrayType::Float16(a), ArrayType::Float16(b))  => Ok(ArrayType::Float16(a.$fname(b)?)),
                        (ArrayType::Float32(a), ArrayType::Float32(b)) => Ok(ArrayType::Float32(a.$fname(b)?)),
                        (ArrayType::Float64(a), ArrayType::Float64(b))  => Ok(ArrayType::Float64(a.$fname(b)?)),
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
                        (AtomicType::UInt8(a), AtomicType::UInt8(b))  => Ok(AtomicType::UInt8(a.add(b)?)),
                        (AtomicType::Int8(a), AtomicType::Int8(b))   => Ok(AtomicType::Int8(a.$fname(b)?)),
                        (AtomicType::UInt16(a), AtomicType::UInt16(b)) => Ok(AtomicType::UInt16(a.$fname(b)?)),
                        (AtomicType::Int16(a), AtomicType::Int16(b))  => Ok(AtomicType::Int16(a.$fname(b)?)),
                        (AtomicType::UInt32(a), AtomicType::UInt32(b)) => Ok(AtomicType::UInt32(a.$fname(b)?)),
                        (AtomicType::Int32(a), AtomicType::Int32(b))  => Ok(AtomicType::Int32(a.$fname(b)?)),
                        (AtomicType::UInt64(a), AtomicType::UInt64(b)) => Ok(AtomicType::UInt64(a.$fname(b)?)),
                        (AtomicType::Int64(a), AtomicType::Int64(b))  => Ok(AtomicType::Int64(a.$fname(b)?)),
                        (AtomicType::BFloat16(a), AtomicType::BFloat16(b)) => Ok(AtomicType::BFloat16(a.$fname(b)?)),
                        (AtomicType::Float16(a), AtomicType::Float16(b))  => Ok(AtomicType::Float16(a.$fname(b)?)),
                        (AtomicType::Float32(a), AtomicType::Float32(b)) => Ok(AtomicType::Float32(a.$fname(b)?)),
                        (AtomicType::Float64(a), AtomicType::Float64(b))  => Ok(AtomicType::Float64(a.$fname(b)?)),
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

impl<T> Inv for T {
    fn inv(self) -> Result<Self> {
        todo!()
    }
}

impl Neg for AtomicType {
    fn neg(self) -> Result<Self> where Self: Sized {
        use AtomicType::*;
        Ok(match self {
            Int8(a) => Int8(-a),    
            Int16(a) => Int16(-a),  
            Int32(a) => Int32(-a),  
            Int64(a) => Int64(-a),
            BFloat16(a) => BFloat16(-a),
            Float16(a) => Float16(-a),
            Float32(a) => Float32(-a),
            Float64(a) => Float64(-a),
            Complex(a) => Complex(num::Complex::new(-a.re(), a.im())), //subtracts the real
            _ => return Err(LangError::RuntimeErr("unsupported op".to_string())),
        })
    }
}

