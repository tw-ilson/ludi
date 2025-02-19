// #![feature(macro_metavar_expr_concat)]

#![allow(refining_impl_trait)]
#![allow(non_camel_case_types)]
#![allow(unused_imports)]
#![allow(dead_code)]

const LLVM_MAJOR_VERSION: usize = 18;

pub mod utils;
pub mod err;
pub mod token;
pub mod lex;
pub mod parser;
pub mod shape;
pub mod ast;
pub mod atomic;
pub mod env;
pub mod types;
pub mod codegen;
pub mod normalize;
