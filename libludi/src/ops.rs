use crate::array::Array;
use crate::atomic::NumberType;
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

// impl<T> Neg for Array<T>
// where T: Neg {
//     fn neg(&self) -> Result<Self>
//         where
//             Self: Sized {
//                 Ok(Array {
//                     shape: self.shape().into(),
//                     data: self.data().iter().map_while(|a| a.neg().ok()).collect()
//
//                 })
//     }
// }
// impl<T> Inv for Array<T> {
//     fn inv(&self) -> Result<Self>
//         where
//             Self: Sized {
//         todo!()
//     }
// }
//
//

macro_rules! delegate_binops_std_array {
    ($($trait:ident $fnname:ident),*) => {
    impl<T> Add for Array<T>
    where
        T: Add,
    {
        type Rhs = Self;
        fn add(self, rhs: Self) -> Result<Self> {
            if self.shape() != rhs.shape() {
                return Err(LangError::RuntimeErr("shape error".to_owned()))
            }
            Ok(Array {
                shape: self.shape().into(),
                data: izip!(self.data(), rhs.data())
                    .map_while(|(a, b)| a.add(b).ok())
                    .collect(),
            })
        }
    }
    };
}
// delegate_binops_std_array!(Add add);
// impl<T> Add for Array<T>
// where
//     T: Add,
// {
//     type Rhs = Self;
//     fn add(&self, rhs: &Self) -> Result<Self> {
//         if self.shape() != rhs.shape() {
//             // invalid operation
//             panic!("shape error")
//         }
//         Ok(Array {
//             shape: self.shape().into(),
//             data: izip!(self.data(), rhs.data())
//                 .map_while(|(a, b)| a.add(b).ok())
//                 .collect(),
//         })
//     }
// }



macro_rules! delegate_binops_numbertype {
    ($($trait:ident $fname:ident),*) => {
        $(
            impl $trait for NumberType {
                type Rhs = Self;
                fn $fname(self, rhs: Self::Rhs) -> Result<Self>{
                    match (self,rhs) {
                        (NumberType::UInt8(a), NumberType::UInt8(b))  => {Ok(NumberType::UInt8(a.add(b)?))},
                        (NumberType::Int8(a), NumberType::Int8(b))   => {Ok(NumberType::Int8(a.$fname(b)?))},
                        (NumberType::UInt16(a), NumberType::UInt16(b)) => {Ok(NumberType::UInt16(a.$fname(b)?))},
                        (NumberType::Int16(a), NumberType::Int16(b))  => {Ok(NumberType::Int16(a.$fname(b)?))},
                        (NumberType::UInt32(a), NumberType::UInt32(b)) => {Ok(NumberType::UInt32(a.$fname(b)?))},
                        (NumberType::Int32(a), NumberType::Int32(b))  => {Ok(NumberType::Int32(a.$fname(b)?))},
                        (NumberType::UInt64(a), NumberType::UInt64(b)) => {Ok(NumberType::UInt64(a.$fname(b)?))},
                        (NumberType::Int64(a), NumberType::Int64(b))  => {Ok(NumberType::Int64(a.$fname(b)?))},
                        (NumberType::BFloat16(a), NumberType::BFloat16(b)) => {Ok(NumberType::BFloat16(a.$fname(b)?))},
                        (NumberType::Float16(a), NumberType::Float16(b))  => {Ok(NumberType::Float16(a.$fname(b)?))},
                        (NumberType::Float32(a), NumberType::Float32(b)) => {Ok(NumberType::Float32(a.$fname(b)?))},
                        (NumberType::Float64(a), NumberType::Float64(b))  => {Ok(NumberType::Float64(a.$fname(b)?))},
                        (NumberType::Complex(a), NumberType::Complex(b))  => {Ok(NumberType::Complex(a.$fname(b)?))},
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

impl<T> Neg for T
where
    T: std::ops::Neg<Output = T>,
{
    fn neg(self) -> Result<Self> {
        Ok(-self)
    }
}

impl<T> Inv for T {
    fn inv(self) -> Result<Self> {
        todo!()
    }
}
