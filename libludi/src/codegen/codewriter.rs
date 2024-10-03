use melior::dialect::arith;
use melior::ir::attribute::IntegerAttribute;
use melior::ir::r#type::IntegerType;

use crate::ast;
use crate::err::Result;

pub trait MLIRGen<R> {
    // generate some MLIR code
    fn mlir_gen(self, state: &mut CodeWriter) -> Result<R>;
}

// MLIR modules are the top level container for code
trait MLIRGenModule: MLIRGen<melior::ir::Module<'static>> {}
// MLIR regions consist of a list of ordered blocks
trait MLIRGenRegion<'a>: MLIRGen<melior::ir::Region<'a>> {}
// MLIR blocks consist of a list of ordered operations, and are assigned to values
trait MLIRGenBlock<'b>: MLIRGen<melior::ir::block::Block<'b>> {}
// MLIR operations consist of values, and/or regions, and are assigned to values
trait MLIRGenOp<'c>: MLIRGen<melior::ir::operation::Operation<'c>> {}

// an object which contains an MLIR context and produces a module
struct CodeWriter {
    // manages a single thread of MLIR core state
    pub context: melior::Context,
    // MLIR modules are the top-level container for MLIR code,
    pub module: melior::ir::Module<'static>,
}

// setup for code generation in all standard dialects
pub fn load_builtin_dialects() -> melior::Context {
    let registry = melior::dialect::DialectRegistry::new();
    melior::utility::register_all_dialects(&registry);
    let context = melior::Context::new();
    context.append_dialect_registry(&registry);
    context.load_all_available_dialects();
    context
}

impl CodeWriter {
    fn new(// what are the arguments
    ) -> Self {
        let context = load_builtin_dialects();
        // debug locations in code
        let location = melior::ir::Location::unknown(&context);
        let module = melior::ir::Module::new(location);
        CodeWriter { context, module }
    }
}

impl MLIRGenBlock<'_> for ast::Expr {}
impl MLIRGen<melior::ir::block::Block<'static>> for ast::Expr {
    fn mlir_gen(self, state: &mut CodeWriter) -> Result<melior::ir::block::Block<'static>> {
        match self {
            // Self::AtomLiteral(node) => node.mlir_gen(state)?,
            // Self::ArrayLiteral(node) => node.mlir_gen(state),
            // Self::FnDef(node) => node.mlir_gen(state),
            // Self::FnCall(node) => node.mlir_gen(state),
            // Self::Frame(node) => node.mlir_gen(state),
            // Self::Let(node) => node.mlir_gen(state),
            // Self::Term(node) => node.mlir_gen(state),
            _ => todo!(),
        }
    }
}

impl MLIRGenOp<'_> for ast::AtomLiteralNode {}
impl MLIRGen<melior::ir::operation::Operation<'static>> for ast::AtomLiteralNode {
    fn mlir_gen(self, state: &mut CodeWriter) -> Result<melior::ir::operation::Operation<'static>> {
        use crate::atomic::Literal;
        Ok(arith::constant(
            &state.context,
            match self.value {
                Literal::Int { atom, loc:_} => IntegerAttribute::new(, integer)
                Literal::Float { atom, loc:_} => IntegerAttribute::new(, integer)
            },
            mlir::ir::Location::unknown(&state.context),
        ))
    }
}

