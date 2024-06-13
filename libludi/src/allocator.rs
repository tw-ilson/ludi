use std::borrow::Cow;
use std::mem::size_of;
use std::ptr::NonNull;
use std::alloc::{alloc, dealloc, Layout};

use crate::err::*;
use crate::primitive::*;

#[derive(Debug, Clone)]
pub enum BlockError {
    BadRequest,
    OOM,
}

// Find a good kind of backing data pointer
// pub type BlockPtr<'a, T> = Cow<'a, [T]>;
pub type BlockPtr<T> = NonNull<T>;
pub type BlockSize = usize;

#[repr(C)]
pub struct Block<T: Data> {
    data: BlockPtr<T>,
    size: BlockSize,
}

impl<T: Data> Block<T> {
    fn new(size: BlockSize) -> Result<Self> {
        if !size.is_power_of_two() {
            return Err(LangErrorType::AllocErr(BlockError::BadRequest))
        }

        Ok(Block {
            data: alloc_block(size)?,
            size,
        })
    }
}

trait AllocRaw {
    fn alloc<T>(&self, object: T) -> *const T;
}

pub fn alloc_block<T>(size: BlockSize) -> Result<BlockPtr<T>> {
    unsafe {
        let layout = Layout::from_size_align_unchecked(size, size);

        let ptr = alloc(layout);
        if ptr.is_null() {
            Err(LangErrorType::AllocErr(BlockError::OOM))
        } else {
            Ok(NonNull::new_unchecked(ptr as *mut _))
        }
    }
}

pub fn dealloc_block<T>(ptr: BlockPtr<T>, size: BlockSize) {
    unsafe {
        let layout = Layout::from_size_align_unchecked(size, size);

        dealloc(ptr.as_ptr() as *mut _, layout);
    }
}

fn obj_size<T>() {
    let alignment = size_of::<usize>() * 2;
    // mask out the least significant bits that correspond to the alignment - 1
    // then add the full alignment
    let size = (size_of::<T>() & !(alignment - 1)) + alignment;
}
