use itertools::Itertools;
use libludi::{
    ast::*,
    env::{Env, EnvRef},
    err::{Error, LudiError, Result},
    shape::{ArrayProps, Shape, ShapeOps},
    token::Token,
    types::PrimitiveFuncType,
};
use std::{borrow::Borrow, rc::Rc};

use crate::ops::*;
use crate::{
    array::Iota,
    datatypes::{ArrayType, AtomicType, DataType},
};

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
            Stmt::Expr(node) => node.expression.interpret(e),
            Stmt::Print(node) => {
                let r = node.expression.interpret(e)?;
                // also print type
                println!("\n    {}", r);
                Ok(r)
            }
            Stmt::Unit(_) => Ok(DataType::Unit),
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

impl Interpret for AtomLiteralNode {
    fn interpret(self, _e: &mut DynamicEnv) -> InterpretResult {
        Ok(self.value.clone().into())
    }
}

impl Interpret for LetNode {
    fn interpret(self, e: &mut DynamicEnv) -> InterpretResult {
        let init_val = self.initializer.interpret(e)?;
        e.put(self.name, init_val);
        if let Some(body) = self.region {
            body.interpret(e)
        } else {
            Ok(DataType::Unit)
        }
    }
}

impl Interpret for TermNode {
    fn interpret(self, e: &mut DynamicEnv) -> InterpretResult {
        Ok(e.get(&self.name)?)
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
    fn interpret(self, _e: &mut DynamicEnv) -> InterpretResult {
        todo!()
    }
}

impl Interpret for FnCallNode {
    fn interpret(self, e: &mut DynamicEnv) -> InterpretResult {
        use libludi::types::PrimitiveFuncType;
        match self.callee {
            Callee::Expression(_expr) => todo!(),
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
                PrimitiveFuncType::If => {
                    assert_eq!(self.args.len(), 3);
                    let mut args = self.args.into_iter();
                    let (cond_expr, then_expr, else_expr) = (
                        args.next().unwrap(),
                        args.next().unwrap(),
                        args.next().unwrap(),
                    );
                    match cond_expr.interpret(e)? {
                        DataType::Atomic(AtomicType::Boolean(branch)) => {
                            if branch {
                                Ok(then_expr.interpret(e)?)
                            } else {
                                Ok(else_expr.interpret(e)?)
                                // interpreter should prevent branches diverge...
                            }
                        }
                        _ => Err(Error::runtime_err(
                            "if expression expected boolean condition",
                        )),
                    }
                }
                PrimitiveFuncType::Gt => {
                    assert_eq!(self.args.len(), 2);
                    let mut args = self.args.into_iter();
                    let (arg1, arg2) = (args.next().unwrap(), args.next().unwrap());
                    match (arg1.interpret(e)?, arg2.interpret(e)?) {
                        (
                            DataType::Atomic(AtomicType::Int(a)),
                            DataType::Atomic(AtomicType::Int(b)),
                        ) => Ok(DataType::Atomic(AtomicType::Boolean(a > b))),
                        (
                            DataType::Atomic(AtomicType::Float(a)),
                            DataType::Atomic(AtomicType::Float(b)),
                        ) => Ok(DataType::Atomic(AtomicType::Boolean(a > b))),
                        _ => Err(Error::runtime_err(
                            "type error: '>' op is not defined for between these types",
                        )),
                    }
                }
                PrimitiveFuncType::GtEq => {
                    assert_eq!(self.args.len(), 2);
                    let mut args = self.args.into_iter();
                    let (arg1, arg2) = (args.next().unwrap(), args.next().unwrap());
                    match (arg1.interpret(e)?, arg2.interpret(e)?) {
                        (
                            DataType::Atomic(AtomicType::Int(a)),
                            DataType::Atomic(AtomicType::Int(b)),
                        ) => Ok(DataType::Atomic(AtomicType::Boolean(a >= b))),
                        (
                            DataType::Atomic(AtomicType::Float(a)),
                            DataType::Atomic(AtomicType::Float(b)),
                        ) => Ok(DataType::Atomic(AtomicType::Boolean(a >= b))),
                        _ => Err(Error::runtime_err(
                            "type error: '>=' op is not defined for between these types",
                        )),
                    }
                }
                PrimitiveFuncType::Lt => {
                    assert_eq!(self.args.len(), 2);
                    let mut args = self.args.into_iter();
                    let (arg1, arg2) = (args.next().unwrap(), args.next().unwrap());
                    match (arg1.interpret(e)?, arg2.interpret(e)?) {
                        (
                            DataType::Atomic(AtomicType::Int(a)),
                            DataType::Atomic(AtomicType::Int(b)),
                        ) => Ok(DataType::Atomic(AtomicType::Boolean(a < b))),
                        (
                            DataType::Atomic(AtomicType::Float(a)),
                            DataType::Atomic(AtomicType::Float(b)),
                        ) => Ok(DataType::Atomic(AtomicType::Boolean(a < b))),
                        _ => Err(Error::runtime_err(
                            "type error: '<' op is not defined for between these types",
                        )),
                    }
                }
                PrimitiveFuncType::LtEq => {
                    assert_eq!(self.args.len(), 2);
                    let mut args = self.args.into_iter();
                    let (arg1, arg2) = (args.next().unwrap(), args.next().unwrap());
                    match (arg1.interpret(e)?, arg2.interpret(e)?) {
                        (
                            DataType::Atomic(AtomicType::Int(a)),
                            DataType::Atomic(AtomicType::Int(b)),
                        ) => Ok(DataType::Atomic(AtomicType::Boolean(a <= b))),
                        (
                            DataType::Atomic(AtomicType::Float(a)),
                            DataType::Atomic(AtomicType::Float(b)),
                        ) => Ok(DataType::Atomic(AtomicType::Boolean(a <= b))),
                        _ => Err(Error::runtime_err(
                            "type error: '<=' op is not defined for between these types",
                        )),
                    }
                }
                PrimitiveFuncType::Ne => {
                    assert_eq!(self.args.len(), 2);
                    let mut args = self.args.into_iter();
                    let (arg1, arg2) = (args.next().unwrap(), args.next().unwrap());
                    match (arg1.interpret(e)?, arg2.interpret(e)?) {
                        (
                            DataType::Atomic(AtomicType::Int(a)),
                            DataType::Atomic(AtomicType::Int(b)),
                        ) => Ok(DataType::Atomic(AtomicType::Boolean(a != b))),
                        (
                            DataType::Atomic(AtomicType::Boolean(a)),
                            DataType::Atomic(AtomicType::Boolean(b)),
                        ) => Ok(DataType::Atomic(AtomicType::Boolean(a != b))),
                        (
                            DataType::Atomic(AtomicType::Character(a)),
                            DataType::Atomic(AtomicType::Character(b)),
                        ) => Ok(DataType::Atomic(AtomicType::Boolean(a != b))),
                        _ => Err(Error::runtime_err(
                            "type error: '!=' op is not defined for between these types",
                        )),
                    }
                }
                PrimitiveFuncType::Eq => {
                    assert_eq!(self.args.len(), 2);
                    let mut args = self.args.into_iter();
                    let (arg1, arg2) = (args.next().unwrap(), args.next().unwrap());
                    match (arg1.interpret(e)?, arg2.interpret(e)?) {
                        (
                            DataType::Atomic(AtomicType::Int(a)),
                            DataType::Atomic(AtomicType::Int(b)),
                        ) => Ok(DataType::Atomic(AtomicType::Boolean(a == b))),
                        (
                            DataType::Atomic(AtomicType::Boolean(a)),
                            DataType::Atomic(AtomicType::Boolean(b)),
                        ) => Ok(DataType::Atomic(AtomicType::Boolean(a == b))),
                        (
                            DataType::Atomic(AtomicType::Character(a)),
                            DataType::Atomic(AtomicType::Character(b)),
                        ) => Ok(DataType::Atomic(AtomicType::Boolean(a == b))),
                        _ => Err(Error::runtime_err(
                            "type error: '==' op is not defined for between these types",
                        )),
                    }
                }
                PrimitiveFuncType::And => {
                    assert_eq!(self.args.len(), 2);
                    let mut args = self.args.into_iter();
                    let (arg1, arg2) = (args.next().unwrap(), args.next().unwrap());

                    match (arg1.interpret(e)?, arg2.interpret(e)?) {
                        (
                            DataType::Atomic(AtomicType::Boolean(a)),
                            DataType::Atomic(AtomicType::Boolean(b)),
                        ) => Ok(DataType::Atomic(AtomicType::Boolean(a && b))),
                        _ => Err(Error::runtime_err(
                            "type error: 'and' op is not defined for between these types",
                        )),
                    }
                }
                PrimitiveFuncType::Or => {
                    assert_eq!(self.args.len(), 2);
                    let mut args = self.args.into_iter();
                    let (arg1, arg2) = (args.next().unwrap(), args.next().unwrap());

                    match (arg1.interpret(e)?, arg2.interpret(e)?) {
                        (
                            DataType::Atomic(AtomicType::Boolean(a)),
                            DataType::Atomic(AtomicType::Boolean(b)),
                        ) => Ok(DataType::Atomic(AtomicType::Boolean(a || b))),
                        _ => Err(Error::runtime_err(
                            "type error: 'or' op is not defined for between these types",
                        )),
                    }
                }
                PrimitiveFuncType::Not => {
                    assert_eq!(self.args.len(), 1);
                    let arg1 = self.args.into_iter().next().unwrap();
                    match arg1.interpret(e)? {
                        DataType::Atomic(AtomicType::Boolean(a)) => {
                            Ok(DataType::Atomic(AtomicType::Boolean(!a)))
                        }
                        _ => Err(Error::runtime_err(
                            "type error: 'not' op is not defined for between these types",
                        )),
                    }
                }
                PrimitiveFuncType::Neg => {
                    assert_eq!(self.args.len(), 1);
                    let arg1 = self.args.into_iter().next().unwrap();
                    Neg::neg(arg1.interpret(e)?)
                }
                PrimitiveFuncType::Iota => {
                    assert_eq!(self.args.len(), 1);
                    let arg1 = self.args.into_iter().next().unwrap();
                    match arg1.interpret(e)? {
                        DataType::Atomic(AtomicType::Int(i)) => Ok(DataType::Array(
                            crate::datatypes::ArrayType::Int(Iota::iota(i.try_into()?)),
                        )),
                        DataType::Array(ArrayType::Int(_a_i)) => todo!(),
                        _ => Err(Error::runtime_err("error: Iota expects integer")),
                    }
                }
                PrimitiveFuncType::Reshape => {
                    assert_eq!(self.args.len(), 2);
                    let mut args = self.args.into_iter();
                    let (arg1, arg2) = (args.next().unwrap(), args.next().unwrap());
                    match (arg1.interpret(e)?, arg2.interpret(e)?) {
                        (DataType::Array(mut array), DataType::Array(ArrayType::Int(shape_array))) => {
                            let newshape: Shape = shape_array.try_into()?;
                            array.reshape(newshape.shape_slice())?;
                            Ok(DataType::Array(array))
                        }
                        _ => panic!(),
                    }
                }
                PrimitiveFuncType::IntToBool => {
                    todo!()
                }
                PrimitiveFuncType::BoolToInt => {
                    todo!()
                }
                PrimitiveFuncType::IntToFloat => {
                    todo!()
                }
                PrimitiveFuncType::FloatToInt => {
                    todo!()
                }

                _ => unimplemented!(),
            },
        }
    }
}
