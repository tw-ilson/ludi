// #![feature(macro_metavar_expr_concat)]

#![allow(refining_impl_trait)]
#![allow(non_camel_case_types)]
#![allow(unused_imports)]
#![allow(dead_code)]

pub mod err;
pub mod token;
pub mod lex;
pub mod parser;
pub mod shape;
pub mod ast;
mod atomic;
mod env;
mod types;
mod pipeline;
mod codegen;
mod optimize;
