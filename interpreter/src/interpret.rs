use libludi::{
    ast::*,
    env::{Env, EnvRef},
    err::{LangError, Result},
    atomic::NumberType,
    array::Array,
    data::{DataType, Data},
    ops::*,
    tokens::Token,
};
use std::rc::Rc;
extern crate proc_macro;

macro_rules! eval_err {
    ($msg:literal) => {
        LangErrorType::RuntimeErr()
    };
}

type InterpretResult = Result<Rc<DataType>>;

pub trait Interpret {
    fn interpret(&self, e: EnvRef) -> InterpretResult;
}
pub trait Resolve<R> {}
pub trait Analyze<R> {}


impl Interpret for Stmt {
    fn interpret(&self, e: EnvRef) -> InterpretResult {
        match self {
            Stmt::ExprStmt(node) => node.expression.interpret(e),
            Stmt::BlockStmt(node) => node.interpret(Env::new(Some(e)).into()),
            Stmt::FnStmt(node) => {unimplemented!();},
            Stmt::PrintStmt(node) => {
                let r = node.expression.interpret(e)?;
                println!("    {}", r);
                Ok(r)
            }
            Stmt::AssignStmt(node) => {
                if let Token::IDENTIFIER(name) = &node.name.token {
                    let val = node.initializer.interpret(e.clone())?;
                    e.put(name, val.clone());
                    Ok(val)
                } else {
                    panic!()
                }
            }
        }
    }
}

impl Interpret for Expr {
    fn interpret(&self, e: EnvRef) -> InterpretResult {
        match self {
            Expr::Sequence(node) => node.interpret(e),
            Expr::Logical(node) => node.interpret(e),
            Expr::Conditional(node) => node.interpret(e),
            Expr::Binary(node) => node.interpret(e),
            Expr::Unary(node) => node.interpret(e),
            Expr::Grouping(node) => node.interpret(e),
            Expr::Literal(node) => node.interpret(e),
            Expr::AtomicCast(node) => node.interpret(e),
            Expr::Assignment(node) => node.interpret(e),
            Expr::FnCall(node) => node.interpret(e)
        }
    }
}

impl Interpret for SequenceNode {
    fn interpret(&self, e: EnvRef) -> InterpretResult {
        unimplemented!();
    }
}

impl Interpret for FnCallNode {
    fn interpret(&self, e: EnvRef) -> InterpretResult {
        unimplemented!();
    }
}

impl Interpret for ConditionalNode {
    fn interpret(&self, e: EnvRef) -> InterpretResult {
        let cond = self.cond.interpret(e.clone())?;
        // if AtomicType::BoolType == cond.ty {
        //     if unsafe{cond.data.bool8} {
        //         self.body.interpret(e)
        //     } else {
        //         if let Some(body) = &self.elsebody {
        //             body.interpret(e)
        //         } else {
        //             // implement a 'no data' type
        //             todo!()
        //         }
        //     }
        // if cond.truthy() {
        //     self.body.interpret(e)
        // } else {
        //     // panic!("expected boolean condition, got {:?}", cond.ty)
        //     if let Some(body) = &self.elsebody {
        //         body.interpret(e)
        //     } else {
        //         // implement a 'no data' type
        //         todo!()
        //     }
        // }
        todo!()
    }
}
impl Interpret for GroupingNode {
    fn interpret(&self, e: EnvRef) -> InterpretResult {
        self.expression.interpret(e)
    }
}

impl Interpret for BlockStmtNode {
    fn interpret(&self, e: EnvRef) -> InterpretResult {
        self.statements
            .iter()
            .map(|s| s.interpret(e.clone()))
            .last()
            .expect("recoverable error")
    }
}

impl Interpret for AtomicCastNode {
    fn interpret(&self, e: EnvRef) -> InterpretResult {
        // make cast explicit here
        Ok(DataType::Number(self.value.value).into())
    }
}
impl Interpret for LogicalNode {
    fn interpret(&self, e: EnvRef) -> InterpretResult {
        let left = self.left.interpret(e.clone())?;
        if self.operator.token == Token::OR {}
        todo!(); // handle the logical operators
    }
}

impl Interpret for LiteralNode {
    fn interpret(&self, e: EnvRef) -> InterpretResult {
        Ok(DataType::Number(self.value).into())
    }
}

impl Interpret for AssignmentNode {
    fn interpret(&self, e: EnvRef) -> InterpretResult {
        if let Token::IDENTIFIER(name) = &self.name.token {
             Ok(e.get(name, self.name.line)?)
        } else { panic!() }
    }
}

impl Interpret for UnaryNode {
    fn interpret(&self, e: EnvRef) -> InterpretResult {
        match self.operator.token {
            Token::MINUS => {
                let val = self.right.interpret(e)?;
                Ok(val.neg().into())
            },
            _ => panic!(),
        }
    }
}


impl Interpret for BinaryNode {
    fn interpret(&self, e: EnvRef) -> InterpretResult {
        let left = self.left.interpret(e.clone())?;
        let right = self.right.interpret(e.clone())?;
        match self.operator.token {
            Token::PLUS =>  Ok(left.add(&right).into()),
            Token::MINUS => Ok(left.sub(&right).into()),
            Token::STAR =>  Ok(left.mul(&right).into()),
            Token::SLASH => Ok(left.div(&right).into()),
            _ => panic!(),
        }
    }
}
