use std::fmt::Binary;

use crate::vm::{OpCode, Chunk, ChunkMachine};
use crate::scanner::Scanner;
use crate::err::Result;
use crate::ast::*;
use crate::tokens::Token;

pub trait Compile {
    fn compile(self, chunk: Chunk) -> Result<Chunk>;
}

impl Compile for Vec<Stmt> {
    fn compile(self, mut chunk: Chunk) -> Result<Chunk> {
        for stmt in self {
            chunk = stmt.compile(chunk)?;
        }
        Ok(chunk)
    }
}

impl Compile for Stmt {
    fn compile(self, chunk: Chunk) -> Result<Chunk> {
        match self {
            Stmt::ExprStmt(node) => {node.expression.compile(chunk)},
            _=> unimplemented!()
        }
    }
}

impl Compile for Expr {
    fn compile(self, chunk: Chunk) -> Result<Chunk> {
        use Expr::*;
        match self {
            Literal(node) => node.compile(chunk),
            Grouping(node) => node.compile(chunk),
            Unary(node) => node.compile(chunk),
            // Binary(node) => node.compile(chunk),
            // AtomicCast(node) => {node.compile(chunk)},
            // FnCall(node) => {node.compile(chunk)},
            // Conditional(node) => {node.compile(chunk)},
            // Logical(node) => {node.compile(chunk)},
            // Sequence(node) => {node.compile(chunk)},
            // Assignment(node) => {node.compile(chunk)},
            _ => unimplemented!()
        }
    }
}

impl Compile for LiteralNode {
    fn compile(self, chunk: Chunk) -> Result<Chunk> {
        Ok(chunk + OpCode::CONSTANT { value: self.value })
    }
}
impl Compile for GroupingNode {
    fn compile(self, chunk: Chunk) -> Result<Chunk> {
        self.expression.compile(chunk)
    }
}
impl Compile for UnaryNode {
    fn compile(self, chunk: Chunk) -> Result<Chunk> {
        match self.operator.token {
            Token::MINUS => {
                Ok(self.right.compile(chunk)? + OpCode::NEGATE)
            },
            _=>unreachable!()
        }
    }
}
impl Compile for BinaryNode {
    fn compile(self, mut chunk: Chunk) -> Result<Chunk> {
        match self.operator.token {
            Token::PLUS => Ok(self.right.compile(self.left.compile(chunk)?)? + OpCode::ADD),
            Token::MINUS => Ok(self.right.compile(self.left.compile(chunk)?)? + OpCode::SUBTRACT),
            Token::STAR => Ok(self.right.compile(self.left.compile(chunk)?)? + OpCode::MULTIPLY),
            Token::SLASH => Ok(self.right.compile(self.left.compile(chunk)?)? + OpCode::DIVIDE),
            _=>unreachable!()
        }
    }
}
