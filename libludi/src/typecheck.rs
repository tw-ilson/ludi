use crate::{data::{ArrayType, AtomicType}, err::Result};

pub enum Sort {
    Dim, 
    Shape
}

pub enum Kind {
    Atom,
    Array
}

trait TypeCheck {
    fn check(self) -> Result<Self>
    where
        Self: Sized;
}
// Dependent Product type
pub struct Pi<X, Y>(X, Y);
pub fn pi<X, Y>(x: X, y: Y) -> Pi<X, Y> {
    Pi(x, y)
}

// Dependent Sum Type
pub struct Sigma<X, Y>(X, Y);
pub fn sigma<X, Y>(x: X, y: Y) -> Sigma<X, Y> {
    Sigma(x, y)
}

impl<X, Y> TypeCheck for Pi<X, Y> {
    fn check(self) -> Result<Self> {
        todo!()
    }
}
impl<X, Y> TypeCheck for Sigma<X, Y> {
    fn check(self) -> Result<Self> {
        todo!()
    }
}

impl TypeCheck for AtomicType {
    fn check(self) -> Result<Self>
        where
            Self: Sized {
        todo!()
    }
}

