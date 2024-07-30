
use crate::{
    // allocator::BlockError, 
    tokens::TokenData};
use std::error::Error;
use thiserror;
use anyhow;

#[derive(thiserror::Error, Debug, Clone)]
pub enum LangError {
    ScanErr(String),
    ParseErr(String),
    CompileErr(String),
    RuntimeErr(String)
    // AllocErr(BlockError),
}
impl std::fmt::Display for LangError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ScanErr(msg) => write!(f, "Syntax Error: {}", msg),
            Self::ParseErr(msg) => write!(f, "Parsing Error: {}", msg),
            Self::CompileErr(msg) => write!(f, "Compile Error: {}",  msg),
            Self::RuntimeErr(msg) => write!(f, "Runtime Error: {}", msg),
            // Self::AllocErr(msg) => write!(f, "Allocation Error"),
        }
    }
}

pub type Result<T> = anyhow::Result<T, LangError>;

#[macro_export]
macro_rules! err_at_tok {
    ($tok:expr, $message:expr) => {
        format!(
            "Error line {}: unexpected {:?}; {}",
            $tok.line, $tok.token, $message
        )
    };
}
#[macro_export]
macro_rules! runtime_err {
    ($message:expr) => {
        Err(LangError::RuntimeErr($message.to_string()))
    };
}
#[macro_export]
macro_rules! parse_err {
    ($message:expr) => {
        Err(LangError::ParseErr($message.to_string()))
    };
}
#[macro_export]
macro_rules! compile_err {
    ($message:expr) => {
        Err(LangError::CompileErr($message.to_string()))
    };
}
pub use err_at_tok;
pub use runtime_err;
pub use parse_err;
pub use compile_err;
