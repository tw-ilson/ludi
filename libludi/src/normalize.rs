use crate::{ast, err::Result};

trait IsImmediate {
    fn is_immediate(&self) -> bool;
}

impl IsImmediate for ast::Expr {
    fn is_immediate(&self) -> bool {
        matches!(
            self,
            Self::Term(_) | Self::AtomLiteral(_) | Self::ArrayLiteral(_) | Self::FnDef(_)
        )
    }
}

trait IsANF {
    fn is_anf(&self) -> bool;
}

impl IsANF for ast::Expr {
    fn is_anf(&self) -> bool {
        match self {
            Self::Let(node) => {
                node.initializer.is_anf() && node.region.as_ref().is_some_and(|e| e.is_anf())
            }
            Self::FnCall(node) => node.args.iter().all(|a| a.is_anf()),
            _ => self.is_immediate(),
        }
    }
}

trait Normalize {
    fn anf(self) -> Result<Self> where Self: Sized;
}
