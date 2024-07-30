use itertools::Itertools;
use libludi::{
    array::{Array, ArrayType},
    ast::*,
    atomic::AtomicType,
    data::{Data, DataType},
    env::{Env, EnvRef},
    err::{LangError, Result},
    ops::*,
    runtime_err,
    tokens::Token,
};
use std::{borrow::Borrow, rc::Rc};

type InterpretResult = Result<DataType>;

pub trait Interpret {
    fn interpret(self, e: EnvRef) -> InterpretResult;
}
pub trait Resolve<R> {}
pub trait Analyze<R> {}

impl Interpret for Stmt {
    fn interpret(self, e: EnvRef) -> InterpretResult {
        match self {
            Stmt::ExprStmt(node) => node.expression.interpret(e),
            // Stmt::BlockStmt(node) => node.interpret(Env::new(Some(e)).into()),
            // Stmt::FnStmt(node) => {
            //     unimplemented!();
            // }
            Stmt::PrintStmt(node) => {
                let r = node.expression.interpret(e)?;
                println!("    {}", r);
                Ok(r)
            }
            Stmt::AssignStmt(node) => {
                // &node.name.name
                let val = node.initializer.interpret(e.clone())?;
                e.put(node.name, val.clone());
                Ok(val)
            }
        }
    }
}

impl Interpret for Expr {
    fn interpret(self, e: EnvRef) -> InterpretResult {
        match self {
            Expr::Frame(node) => node.interpret(e),
            Expr::Array(node) => node.interpret(e),
            Expr::BinaryOperation(node) => node.interpret(e),
            Expr::UnaryOperation(node) => node.interpret(e),
            Expr::Grouping(node) => node.interpret(e),
            Expr::Literal(node) => node.interpret(e),
            Expr::AtomicCast(node) => node.interpret(e),
            Expr::Assignment(node) => node.interpret(e),
            Expr::FnCall(node) => node.interpret(e),
            Expr::FnDef(_) => todo!(),
            Expr::ShapeCast(_) => todo!()
        }
    }
}

impl Interpret for FrameNode {
    fn interpret(self, e: EnvRef) -> InterpretResult {
        self.expression_list
            .into_iter()
            .map(|expr| expr.interpret(e.clone()))
            .collect::<Result<Result<DataType>>>()?
    }
}

impl Interpret for ArrayNode {
    fn interpret(self, e: EnvRef) -> InterpretResult {
        Ok(DataType::Array(self.value))
    }
}

impl Interpret for FnCallNode {
    fn interpret(self, e: EnvRef) -> InterpretResult {
        unimplemented!();
    }
}

impl Interpret for GroupingNode {
    fn interpret(self, e: EnvRef) -> InterpretResult {
        self.expression.interpret(e)
    }
}

impl Interpret for AtomicCastNode {
    fn interpret(self, e: EnvRef) -> InterpretResult {
        // make cast explicit here
        Ok(DataType::Atomic(self.value))
    }
}

impl Interpret for LiteralNode {
    fn interpret(self, e: EnvRef) -> InterpretResult {
        Ok(DataType::Atomic(self.value))
    }
}

impl Interpret for AssignmentNode {
    fn interpret(self, e: EnvRef) -> InterpretResult {
        Ok(e.get(self.name)?.into_owned())
    }
}

impl Interpret for UnaryNode {
    fn interpret(self, e: EnvRef) -> InterpretResult {
        todo!()
        //         match self.operator.token {
        //             Token::MINUS => {
        //                 let val = self.right.interpret(e)?;
        //                 Ok(Neg::neg(val).into())
        //             },
        //             _ => panic!(),
        //         }
    }
}

impl Interpret for BinaryNode {
    fn interpret(self, e: EnvRef) -> InterpretResult {
        let left = self.left.interpret(e.clone())?;
        let right = self.right.interpret(e.clone())?;
        match self.operator.token {
            Token::PLUS => Ok(Add::add(left, right)?),
            Token::MINUS => Ok(Sub::sub(left, right)?),
            Token::STAR => Ok(Mul::mul(left, right)?),
            Token::SLASH => Ok(Div::div(left, right)?),
            _ => panic!(),
        }
    }
}
