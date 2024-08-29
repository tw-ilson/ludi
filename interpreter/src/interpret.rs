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
            Stmt::PrintStmt(node) => {
                let r = node.expression.interpret(e)?;
                println!("    {}", r);
                Ok(r)
            }
            // Stmt::AssignStmt(node) => {
            //     // &node.name.name
            //     let val = node.initializer.interpret(e.clone())?;
            //     e.put(node.name, val.clone());
            //     Ok(val)
            // }
        }
    }
}

impl Interpret for Expr {
    fn interpret(self, e: EnvRef) -> InterpretResult {
        match self {
            Expr::Frame(node) => node.interpret(e),
            Expr::BinaryOperation(node) => node.interpret(e),
            Expr::UnaryOperation(node) => node.interpret(e),
            Expr::Grouping(node) => node.interpret(e),
            Expr::Literal(node) => node.interpret(e),
            Expr::Term(node) => node.interpret(e),
            Expr::FnCall(node) => node.interpret(e),
            Expr::FnDef(node) => node.interpret(e),
            Expr::Let(_) => todo!(),
            Expr::AtomicCast(_) => todo!(),
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
impl Interpret for FnDefNode {
    fn interpret(self, e: EnvRef) -> InterpretResult {
        todo!()
    }
}

impl Interpret for FnCallNode {
    fn interpret(self, e: EnvRef) -> InterpretResult {
        todo!();
    }
}

impl Interpret for GroupingNode {
    fn interpret(self, e: EnvRef) -> InterpretResult {
        self.expression.interpret(e)
    }
}

impl Interpret for AtomicCastNode {
    fn interpret(self, e: EnvRef) -> InterpretResult {
        // do something
        Ok(self.value)
    }
}

impl Interpret for LiteralNode {
    fn interpret(self, e: EnvRef) -> InterpretResult {
        Ok(self.value)
    }
}

impl Interpret for TermNode {
    fn interpret(self, e: EnvRef) -> InterpretResult {
        Ok(e.get(self.name)?.into_inner().unwrap())
    }
}

impl Interpret for UnaryOperationNode {
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

impl Interpret for BinaryOperationNode {
    fn interpret(self, e: EnvRef) -> InterpretResult {
        let left = self.left.interpret(e.clone())?;
        let right = self.right.interpret(e.clone())?;
        match self.operator {
            BinaryOpType::ADD => Ok(Add::add(left, right)?),
            BinaryOpType::SUB => Ok(Sub::sub(left, right)?),
            BinaryOpType::MUL => Ok(Mul::mul(left, right)?),
            BinaryOpType::DIV => Ok(Div::div(left, right)?),
        }
    }
}
