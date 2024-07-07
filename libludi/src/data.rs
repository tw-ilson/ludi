use crate::array::ArrayType;
use crate::atomic::AtomicType;
use crate::err::Result;
use crate::ops::*;
use std::cell::Cell;
use std::hash::Hash;
use std::ops::Deref;
use std::ptr::NonNull;
use std::rc::Rc;


// A data is either Array or Atomic
pub trait Data:
 BinaryOp +
 UnaryOp +
 // Hash +
 std::fmt::Display +
 std::fmt::Debug
{}

#[derive(derive_more::Display, Clone, Debug, PartialEq)]
pub enum DataType {
    Array(ArrayType),
    Atomic(AtomicType),
}
