use crate::{
    // allocator::BlockError,
    token::TokenData,
};
use anyhow;
use thiserror;

// For errors related to the parsing of grammer
#[derive(thiserror::Error, Debug, Clone)]
pub enum ParseError {}
// For errors related to semantics
#[derive(thiserror::Error, Debug, Clone)]
pub enum CompileError {}
// For errors related to code generation & backends
#[derive(thiserror::Error, Debug, Clone)]
pub enum CodeGenError {}
// For errors related to runtime & interpreter
#[derive(thiserror::Error, Debug, Clone)]
pub enum RuntimeError {}

#[derive(thiserror::Error, derive_more::Display, Debug, Clone)]
pub enum ErrorKind {
    LexErr,
    ParseErr,   // additional information
    CompileErr, //additional information
    CodeGenErr, // additional information
    RuntimeErr, // additional information
}

// impl std::fmt::Display for LangError {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             Self::LexErr(msg) => write!(f, "Lexical Error: {}", msg),
//             Self::ParseErr(msg) => write!(f, "Parsing Error: {}", msg),
//             Self::CompileErr(msg) => write!(f, "Compile Error: {}",  msg),
//             Self::RuntimeErr(msg) => write!(f, "Runtime Error: {}", msg),
//             // Self::AllocErr(msg) => write!(f, "Allocation Error"),
//         }
//     }
// }

// re-export of anyhow result type
pub type Result<T> = anyhow::Result<T>;
pub type Error = anyhow::Error;

#[macro_export]
macro_rules! err_at_tok_msg {
    ($tok:expr, $message:expr) => {
        format!(
            "Error line {}: unexpected {:?}; {}",
            $tok.loc, $tok.token, $message
        )
    };
}
#[macro_export]
macro_rules! runtime_err {
    ($message:expr) => {
        anyhow::Error::new(crate::err::ErrorKind::RuntimeErr).context($message)
    };
}
#[macro_export]
macro_rules! lex_err {
    ($message:expr) => {
        anyhow::Error::new(crate::err::ErrorKind::LexErr).context($message)
    };
}
#[macro_export]
macro_rules! parse_err {
    ($message:expr) => {
        anyhow::Error::new(crate::err::ErrorKind::ParseErr).context($message)
    };
}
#[macro_export]
macro_rules! compile_err {
    ($message:expr) => {
        anyhow::Error::new(crate::err::ErrorKind::CompileErr).context($message)
    };
}
#[macro_export]
macro_rules! codegen_err {
    ($message:expr) => {
        anyhow::Error::new(crate::err::ErrorKind::CodeGenErr).context($message)
    };
}
pub use codegen_err;
pub use compile_err;
pub use err_at_tok_msg;
pub use lex_err;
pub use parse_err;
pub use runtime_err;
