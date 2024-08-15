use crate::{array::ShapeVec, ast, env::Name};
use itertools::izip;

#[derive(Debug, Eq, Clone)]
pub struct CallSignature {
    pub args: Vec<(Name, ShapeVec)>,
    pub ret: Option<ShapeVec>,
}
// #[derive(Clone)]
// pub type ClosureType = ast::FnDefNode;

// pub type FnData = ast::FnDefNode;
pub struct FnDataSSA {
    signature: CallSignature,
    // body: shvm::vm::BasicBlock,
}
trait FrameAgreement {}
impl FrameAgreement for FnDataSSA {}

impl PartialEq for CallSignature {
    fn eq(&self, other: &Self) -> bool {
        izip!(self.args.clone(), other.args.clone()).all(|((_, s1), (_, s2))| s1 == s2)
            && self.ret == other.ret
    }
}
