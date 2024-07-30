
use crate::{
    env::Name,
    array::ShapeVec,
};

pub struct CallSignature {
    args: Vec<(Name, ShapeVec)>, 
    ret: Option<ShapeVec>
}

// pub type FnData = ast::FnDefNode;
pub struct FnDataSSA {
    signature: CallSignature,
    body: shvm::vm::BasicBlock,
}
trait FrameAgreement {}
impl FrameAgreement for FnData {}
