use std::fmt::write;

use crate::{
    // allocator::BlockError,
    env::Name,
    token::TokenData,
};
use anyhow;
use thiserror;

// re-export of anyhow result type
pub type Result<T> = anyhow::Result<T>;

#[derive(thiserror::Error, derive_more::Display, Debug, Clone)]
pub enum LexErrorKind {
    TokenError,
}
#[derive(thiserror::Error, derive_more::Display, Debug, Clone)]
pub enum ParseErrorKind {
    Expr,
    Literal,
    Ident,
    Frame,
    LetExpr,
    FnDef,
    FnCall,
}
#[derive(thiserror::Error, derive_more::Display, Debug, Clone)]
pub enum CompileErrorKind {
    TailCallOptError,
}

#[derive(thiserror::Error, derive_more::Display, Debug, Clone)]
pub enum SemanticErrorKind {
    NormalizeError,
    CanonicalizeError,
}
#[derive(thiserror::Error, derive_more::Display, Debug, Clone)]
pub enum TypeErrorKind {
    TypeMismatch,
    ShapeMismatch,
    Unknown,
    Unsupported,
}
#[derive(thiserror::Error, derive_more::Display, Debug, Clone)]
pub enum CodeGenErrorKind {
    MLIRError,
}
#[derive(thiserror::Error, derive_more::Display, Debug, Clone)]
pub enum RuntimeErrorKind {
    InterpretError,
}

#[derive(thiserror::Error, Debug, Clone)]
pub enum LudiError {
    LexError(LexErrorKind),
    CompileError(CompileErrorKind),
    ParseError(ParseErrorKind),
    SemanticError(SemanticErrorKind),
    TypeError(TypeErrorKind),
    CodeGenError(CodeGenErrorKind),
    RuntimeError(RuntimeErrorKind),
    // Add additional information
}

impl std::fmt::Display for LudiError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LexError(kind) => write!(f, "Lexical Error: {}", kind),
            Self::ParseError(kind) => write!(f, "Parsing Error: {}", kind),
            Self::CompileError(kind) => write!(f, "Compile Error: {}", kind),
            Self::SemanticError(kind) => write!(f, "Semantic Error: {}", kind),
            Self::TypeError(kind) => write!(f, "Type Error: {}", kind),
            Self::CodeGenError(kind) => write!(f, "Codegen Error: {}", kind),
            Self::RuntimeError(kind) => write!(f, "Runtime Error: {}", kind),
            // Self::AllocErr(msg) => write!(f, "Allocation Error"),
        }
    }
}


#[derive(thiserror::Error, Debug, Clone)]
pub struct Error {
    error: LudiError,
    msg: String,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} -- {}", self.error, self.msg)
    }
}


impl Error {
    // pub fn with_name(name: Name, msg: &str) -> Self {
    //     Error::msg(format!("Error at {}: {}", name, msg))
    // }
    pub fn at_token(mut self, tok: TokenData) -> Self {
        self.msg = format!("Error line {} at \"{}\" -- {}", tok.loc, tok.token, self.msg);
        return self;
    }
    pub fn lex_err(kind: LexErrorKind, msg: &str) -> Self {
        Self {
            error: LudiError::LexError(kind),
            msg: msg.to_string(),
        }
    }
    pub fn runtime_err(kind: RuntimeErrorKind, msg: &str) -> Self {
        Self {
            error: LudiError::RuntimeError(kind),
            msg: msg.to_string(),
        }
    }
    pub fn parse_err(kind: ParseErrorKind, msg: &str) -> Self {
        Self {
            error: LudiError::ParseError(kind),
            msg: msg.to_string(),
        }
    }
    pub fn compile_err(kind: CompileErrorKind, msg: &str) -> Self {
        Self {
            error: LudiError::CompileError(kind),
            msg: msg.to_string(),
        }
    }
    pub fn semantic_err(kind: SemanticErrorKind, msg: &str) -> Self {
        Self {
            error: LudiError::SemanticError(kind),
            msg: msg.to_string(),
        }
    }
    pub fn type_err(kind: TypeErrorKind, msg: &str) -> Self {
        Self {
            error: LudiError::TypeError(kind),
            msg: msg.to_string(),
        }
    }
    pub fn codegen_err(kind: CodeGenErrorKind, msg: &str) -> Self {
        Self {
            error: LudiError::CodeGenError(kind),
            msg: msg.to_string(),
        }
    }
}
