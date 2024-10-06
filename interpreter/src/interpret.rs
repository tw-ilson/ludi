use itertools::Itertools;
use libludi::{
    ast::*,
    env::{Env, EnvRef},
    err::{Error, Result},
    token::Token,
    types::PrimitiveFuncType,
};
use std::{borrow::Borrow, rc::Rc};

use crate::datatypes::DataType;
use crate::ops::*;

pub type DynamicEnv = Env<DataType>;

type InterpretResult = Result<DataType>;

pub trait Interpret {
    fn interpret(self, e: &mut DynamicEnv) -> InterpretResult;
}
pub trait Resolve<R> {}
pub trait Analyze<R> {}

impl Interpret for Stmt {
    fn interpret(self, e: &mut DynamicEnv) -> InterpretResult {
        match self {
            Stmt::ExprStmt(node) => node.expression.interpret(e),
            Stmt::PrintStmt(node) => {
                let r = node.expression.interpret(e)?;
                // also print type
                println!("    {}", r);
                Ok(r)
            }
        }
    }
}

impl Interpret for Expr {
    fn interpret(self, e: &mut DynamicEnv) -> InterpretResult {
        match self {
            Expr::Frame(node) => node.interpret(e),
            Expr::AtomLiteral(node) => node.interpret(e),
            Expr::Term(node) => node.interpret(e),
            Expr::FnCall(node) => node.interpret(e),
            Expr::FnDef(node) => node.interpret(e),
            Expr::Let(node) => node.interpret(e),
            Expr::ArrayLiteral(_) => todo!(),
        }
    }
}

impl Interpret for FrameNode {
    fn interpret(self, e: &mut DynamicEnv) -> InterpretResult {
        self.expression_list
            .into_iter()
            .map(|expr| expr.interpret(e))
            .collect::<Result<Result<DataType>>>()?
    }
}
impl Interpret for FnDefNode {
    fn interpret(self, e: &mut DynamicEnv) -> InterpretResult {
        todo!()
    }
}

impl Interpret for FnCallNode {
    fn interpret(self, e: &mut DynamicEnv) -> InterpretResult {
        use libludi::types::PrimitiveFuncType;
        match self.callee {
            Callee::Primitive(primitive_fn) => match primitive_fn {
                PrimitiveFuncType::Add => {
                    assert_eq!(self.args.len(), 2);
                    let mut args = self.args.into_iter();
                    let (arg1, arg2) = (args.next().unwrap(), args.next().unwrap());
                    Add::add(arg1.interpret(e)?, arg2.interpret(e)?)
                }
                PrimitiveFuncType::Sub => {
                    assert_eq!(self.args.len(), 2);
                    let mut args = self.args.into_iter();
                    let (arg1, arg2) = (args.next().unwrap(), args.next().unwrap());
                    Sub::sub(arg1.interpret(e)?, arg2.interpret(e)?)
                }
                PrimitiveFuncType::Mul => {
                    assert_eq!(self.args.len(), 2);
                    let mut args = self.args.into_iter();
                    let (arg1, arg2) = (args.next().unwrap(), args.next().unwrap());
                    Mul::mul(arg1.interpret(e)?, arg2.interpret(e)?)
                }
                PrimitiveFuncType::Div => {
                    assert_eq!(self.args.len(), 2);
                    let mut args = self.args.into_iter();
                    let (arg1, arg2) = (args.next().unwrap(), args.next().unwrap());
                    Div::div(arg1.interpret(e)?, arg2.interpret(e)?)
                }
                _ => todo!(),
            },
            Callee::Expression(expr) => todo!(),
        }
    }
}

impl Interpret for AtomLiteralNode {
    fn interpret(self, e: &mut DynamicEnv) -> InterpretResult {
        Ok(self.value.clone().into())
    }
}

impl Interpret for LetNode {
    fn interpret(self, e: &mut DynamicEnv) -> InterpretResult {
        todo!();
    }
}

impl Interpret for TermNode {
    fn interpret(self, e: &mut DynamicEnv) -> InterpretResult {
        Ok(e.get(&self.name)?)
    }
}
