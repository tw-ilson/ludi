use crate::ast::Expr;
use crate::atomic::AtomicData;

pub struct Condition(bool);

impl From<Expr> for Condition {
    fn from(value: Expr) -> Self {todo!()}
}

impl From<AtomicData> for Condition {
    fn from(value: AtomicData) -> Self {todo!()}
}
