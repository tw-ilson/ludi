#![allow(refining_impl_trait)]
#![allow(non_camel_case_types)]
#![allow(unused_imports)]
#![allow(dead_code)]
pub mod err;
pub mod tokens;
pub mod lex;
pub mod parser;
pub mod ast;
pub mod data;
pub mod atomic;
pub mod array;
pub mod env;
pub mod ops;

pub mod r#fn;
// pub mod pipeline;
