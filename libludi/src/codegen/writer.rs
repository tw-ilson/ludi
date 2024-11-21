use itertools::Itertools;
use melior::ir::r#type::FunctionType;
use melior::ir::Location;

use crate::ast::Arg;
use crate::err::{Error, LudiError, Result};
use crate::shape::ArrayProps;
use crate::types;
use crate::types::typed_ast;
use crate::types::GetType;

// an object which contains an MLIR context and produces a module
pub struct CodeWriter {
    // manages a single thread of MLIR core context
    pub context: melior::Context,
}
impl CodeWriter {
    pub fn new(// what are the arguments
    ) -> Self {
        let context = load_builtin_dialects();
        // debug locations in code
        CodeWriter { context }
    }

    pub fn write_ast(&self, ast: typed_ast::TypedExpr) -> Result<melior::ir::Module> {
        let builder = melior::dialect::ods::builtin::ModuleOperationBuilder::new(
            &self.context,
            melior::ir::Location::unknown(&self.context),
        );
        let builder = builder.body_region(ast.mlir_gen(&self.context)?);
        Ok(melior::ir::Module::from_operation(builder.build().into())
            .expect("failed to convert the operation to module?"))
    }
}

pub trait MLIRGen<'c, R> {
    // generate some MLIR code
    fn mlir_gen(&self, context: &'c melior::Context) -> Result<R>;
}
// MLIR modules are the top level container for code
pub trait MLIRGenModule<'c>: MLIRGen<'c, melior::ir::Module<'c>> {}
// MLIR regions consist of a list of ordered blocks
pub trait MLIRGenRegion<'c>: MLIRGen<'c, melior::ir::Region<'c>> {}
// MLIR blocks consist of a list of ordered operations, and are assigned to values
pub trait MLIRGenBlock<'c>: MLIRGen<'c, melior::ir::block::Block<'c>> {}
// MLIR operations consist of values, and/or regions, and are assigned to values
pub trait MLIRGenOp<'c>: MLIRGen<'c, melior::ir::operation::Operation<'c>> {}

// setup for code generation in all standard dialects
pub fn load_builtin_dialects() -> melior::Context {
    let registry = melior::dialect::DialectRegistry::new();
    melior::utility::register_all_dialects(&registry);
    let context = melior::Context::new();
    context.append_dialect_registry(&registry);
    context.load_all_available_dialects();
    context
}

impl MLIRGenRegion<'_> for typed_ast::TypedExpr {}
impl<'c> MLIRGen<'c, melior::ir::Region<'c>> for typed_ast::TypedExpr {
    fn mlir_gen(&self, context: &'c melior::Context) -> Result<melior::ir::Region<'c>> {
        let region = melior::ir::Region::new();
        region.append_block(self.mlir_gen(context)?);
        Ok(region)
    }
}

impl MLIRGenBlock<'_> for typed_ast::TypedExpr {}
impl<'c> MLIRGen<'c, melior::ir::block::Block<'c>> for typed_ast::TypedExpr {
    fn mlir_gen(&self, context: &'c melior::Context) -> Result<melior::ir::block::Block<'c>> {
        let block = melior::ir::block::Block::new(&[]);
        let operation = match self {
            Self::AtomLiteral { node, ty } => node.mlir_gen(context)?,
            Self::FnDef { node, ty } => node.mlir_gen(context)?,
            Self::Let { node, ty } => node.mlir_gen(context)?,
            // Self::Term(node) => node.mlir_gen(context),
            // Self::FnCall(node) => node.mlir_gen(context),
            // Self::Frame(node) => node.mlir_gen(context),
            _ => todo!(),
        };
        block.append_operation(operation);
        Ok(block)
    }
}

impl MLIRGenOp<'_> for typed_ast::AtomLiteralNode {}
impl<'c> MLIRGen<'c, melior::ir::operation::Operation<'c>> for typed_ast::AtomLiteralNode {
    fn mlir_gen(
        &self,
        context: &'c melior::Context,
    ) -> Result<melior::ir::operation::Operation<'c>> {
        use crate::atomic::Literal;
        use melior::dialect::arith;
        use melior::ir::attribute::{FloatAttribute, IntegerAttribute};
        Ok(arith::constant(
            context,
            match &self.value {
                Literal::Int { atom, loc: _ } => IntegerAttribute::new(
                    melior::ir::r#type::IntegerType::signed(context, 64).into(),
                    atom.parse()?,
                )
                .into(),
                Literal::Float { atom, loc: _ } => FloatAttribute::new(
                    context,
                    melior::ir::r#type::Type::float64(context),
                    atom.parse()?,
                )
                .into(),
                // Literal::Char { atom, loc:_} => IntegerAttribute::new(, integer)
                // Literal::Bool { atom, loc:_} => IntegerAttribute::new(, integer)
                _ => todo!(),
            },
            melior::ir::Location::unknown(context),
        ))
    }
}

impl MLIRGenOp<'_> for typed_ast::LetNode {}
impl<'c> MLIRGen<'c, melior::ir::operation::Operation<'c>> for typed_ast::LetNode {
    fn mlir_gen(
        &self,
        context: &'c melior::Context,
    ) -> Result<melior::ir::operation::Operation<'c>> {
        Ok(melior::dialect::scf::execute_region(
            &[self.region.get_type().mlir_gen(context)?],
            self.region.mlir_gen(context)?,
            Location::unknown(context),
        ))
    }
}

impl MLIRGenOp<'_> for typed_ast::FnDefNode {}
impl<'c> MLIRGen<'c, melior::ir::operation::Operation<'c>> for typed_ast::FnDefNode {
    fn mlir_gen(
        &self,
        context: &'c melior::Context,
    ) -> Result<melior::ir::operation::Operation<'c>> {
        use melior::dialect::func;

        // Ok(func::func(
        //     context,
        //     melior::ir::attribute::StringAttribute::new(context, ""),
        //     melior::ir::attribute::TypeAttribute::new(
        //         melior::ir::r#type::FunctionType::new(context, inputs, results).into(),
        //     ),
        //     region,
        //     attributes,
        //     location,
        // ))
        todo!()
    }
}
