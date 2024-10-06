use crate::{
    // allocator::BlockError,
    env::Name,
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
pub trait LudiError {
    fn with_name(name: Name, msg: &str) -> Self;
    fn at_token(tok: TokenData, msg: &str) -> Self;
    fn runtime_err(msg: &'static str) -> Self;
    fn parse_err(msg: &'static str) -> Self;
    fn compile_err(msg: &'static str) -> Self;
    fn codegen_err(msg: &'static str) -> Self;
}

impl LudiError for Error {
    fn with_name(name: Name, msg: &str) -> Self {
        Error::msg(format!("Error at {}: {}", name, msg))
    }
    fn at_token(tok: TokenData, msg: &str) -> Self {
        Error::msg(format!(
            "Error line {}: unexpected {:?}; {}",
            tok.loc, tok.token, msg
        ))
    }
    fn runtime_err(msg: &'static str) -> Self {
        Error::new(ErrorKind::RuntimeErr).context(msg)
    }
    fn parse_err(msg: &'static str) -> Self {
        Error::new(ErrorKind::ParseErr).context(msg)
    }
    fn compile_err(msg: &'static str) -> Self {
        Error::new(ErrorKind::CompileErr).context(msg)
    }
    fn codegen_err(msg: &'static str) -> Self {
        Error::new(ErrorKind::CodeGenErr).context(msg)
    }
}
