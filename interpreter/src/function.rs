use crate::{
    datatypes::{ArrayType, AtomicType, DataType},
    interpret::{DynamicEnv, Interpret},
};
use libludi::{
    ast::{Arg, FnDefNode},
    err::Result,
};
use std::{
    fmt::{Debug, Display},
    iter::zip,
    rc::Rc,
};

#[derive(PartialEq, Clone)]
pub struct FunctionData {
    pub data: FnDefNode,
}

impl FunctionData {
    fn params(&self) -> &[Arg] {
        &self.data.signature.args
    }
    fn arity(&self) -> usize {
        self.data.signature.args.len()
    }
}

pub trait Callable {
    fn call(self, arguments: Vec<DataType>, e: &mut DynamicEnv) -> Result<DataType>;
}

impl Callable for FunctionData {
    fn call(self, arguments: Vec<DataType>, e: &mut DynamicEnv) -> Result<DataType> {
        if arguments.len() != self.arity() {
            return Err(anyhow::anyhow!(
                "Error: '{}' expected {} arguments, got {}",
                "fn_name",
                self.arity(),
                arguments.len()
            ));
        }
        e.push_with(
            zip(self.params().into_iter(), arguments.into_iter()).map(|(Arg(name, _ty), value)| {
                //TODO: check type agreement
                (name.clone(), value.into())
            }),
        );
        let result = self.data.body.interpret(e);
        e.pop();
        result
    }
}

impl Callable for DataType {
    fn call(self, arguments: Vec<DataType>, e: &mut DynamicEnv) -> Result<DataType> {
        match self {
            Self::Atomic(AtomicType::Fn(data)) => data.call(arguments, e),
            Self::Array(ArrayType::Fn(_data)) => todo!(),
            _ => Err(anyhow::anyhow!(
                "Error: this type cannot be called as a function!"
            )),
        }
    }
}

impl Debug for FunctionData{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<function>")
    }
}

impl Display for FunctionData{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<function>")
    }
}
