use itertools::Itertools;
use melior::{self, dialect, ir};
// use melior::dialect::func::func;
// use melior::ir::attribute::{StringAttribute, TypeAttribute};
// use melior::ir::operation::OperationPrintingFlags;
// use melior::ir::r#type::FunctionType;
// use melior::ir::Location;

use crate::ast::Arg;
use crate::err::{Error, LudiError, Result};
use crate::shape::ArrayProps;
use crate::types;
use crate::types::typecheck::TypedTree;
use crate::types::typed_ast;
use crate::types::GetType;

pub trait WriteMLIR {
    fn write(&self) -> String;
}

impl WriteMLIR for TypedTree {
    fn write(&self) -> String {
        let writer = CodeWriter::new();

        let module = writer
            .write_ast(self)
            .expect("error: failed to convert AST to MLIR");
        module
            .as_operation()
            .to_string_with_flags(melior::ir::operation::OperationPrintingFlags::default())
            .expect("error: failed to convert MLIR Module to String")
    }
}

// an object which contains an MLIR context and produces a module
pub struct CodeWriter {
    // manages a single thread of MLIR core context
    pub context: melior::Context,
}

impl CodeWriter {
    pub fn new() -> Self {
        let context = load_builtin_dialects();
        // debug locations in code
        CodeWriter { context }
    }

    pub fn write_ast(&self, ast: &TypedTree) -> Result<ir::Module> {
        let _builder = melior::dialect::ods::builtin::ModuleOperationBuilder::new(
            &self.context,
            melior::ir::Location::unknown(&self.context),

        );
        let location = ir::Location::unknown(&self.context);
        let module = ir::Module::new(location);
        for expr in ast.toplevel_expressions.iter() {
            let region = ir::Region::new();
            region.append_block(expr.mlir_gen(&self.context)?);
            let main = Self::add_main_function(&self.context, region);
            module.body().append_operation(main);
        }
        // Ok(melior::ir::Module::from_operation(builder.build().into())
        //     .expect("failed to convert the operation to module?"))
        Ok(module)
    }

    fn add_main_function<'c>(
        context: &'c melior::Context,
        region: melior::ir::Region<'c>,
    ) -> ir::Operation<'c> {
        // let block = ir::Block::new(&[]);
        // block.append_operation(dialect::func::r#return(, ir::Location::unknown(context)));
        // region.append_block(block);
        dialect::func::func(
            context,
            ir::attribute::StringAttribute::new(context, "main"),
            ir::attribute::TypeAttribute::new(
                ir::r#type::FunctionType::new(context, &[], &[]).into(),
            ),
            region,
            &[],
            ir::Location::unknown(context),
        )
    }
}

pub trait MLIRGen<'c, R> {
    // generate some MLIR code
    fn mlir_gen(&self, context: &'c melior::Context) -> Result<R>;
}
// // MLIR modules are the top level container for code
// pub trait MLIRGenModule<'c>: MLIRGen<'c, melior::ir::Module<'c>> {}
// // MLIR regions consist of a list of ordered blocks
// pub trait MLIRGenRegion<'c>: MLIRGen<'c, melior::ir::Region<'c>> {}
// // MLIR blocks consist of a list of ordered operations, and are assigned to values
// pub trait MLIRGenBlock<'c>: MLIRGen<'c, melior::ir::block::Block<'c>> {}
// // MLIR operations consist of values, and/or regions, and are assigned to values
// pub trait MLIRGenOp<'c>: MLIRGen<'c, melior::ir::operation::Operation<'c>> {}

// setup for code generation in all standard dialects
pub fn load_builtin_dialects() -> melior::Context {
    let registry = melior::dialect::DialectRegistry::new();
    melior::utility::register_all_dialects(&registry);
    let context = melior::Context::new();
    context.append_dialect_registry(&registry);
    context.load_all_available_dialects();
    context
}

impl<'c> MLIRGen<'c, melior::ir::block::Block<'c>> for typed_ast::TypedExpr {
    fn mlir_gen(&self, context: &'c melior::Context) -> Result<melior::ir::block::Block<'c>> {
        match self {
            Self::AtomLiteral { node, ty } => node.mlir_gen(context),
            // Self::Let { node, ty } => node.mlir_gen(context)?,
            // Self::FnDef { node, ty } => node.mlir_gen(context)?,
            // Self::Term(node) => node.mlir_gen(context),
            // Self::FnCall(node) => node.mlir_gen(context),
            // Self::Frame(node) => node.mlir_gen(context),
            _ => todo!(),
        }
    }
}

impl<'c> MLIRGen<'c, melior::ir::block::Block<'c>> for typed_ast::AtomLiteralNode {
    fn mlir_gen(&self, context: &'c melior::Context) -> Result<melior::ir::block::Block<'c>> {
        let block = melior::ir::block::Block::new(&[]);
        block.append_operation(self.mlir_gen(context)?);
        Ok(block)
    }
}

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

impl<'c> MLIRGen<'c, melior::ir::block::Block<'c>> for typed_ast::LetNode {
    fn mlir_gen(&self, context: &'c melior::Context) -> Result<melior::ir::block::Block<'c>> {
        let init_code_block = self.initializer.mlir_gen(context)?;
        let init_ident = init_code_block
            .terminator()
            .expect("No operations from initializer?");
        Ok(match &self.region {
            Some(region_expr) => region_expr.mlir_gen(context)?,
            None => init_code_block,
        })
    }
}

// impl MLIRGenOp<'_> for typed_ast::FnDefNode {}
// impl<'c> MLIRGen<'c, melior::ir::operation::Operation<'c>> for typed_ast::FnDefNode {
//     fn mlir_gen(
//         &self,
//         context: &'c melior::Context,
//     ) -> Result<melior::ir::operation::Operation<'c>> {
//         use melior::dialect::func;
//         Ok(func::func(
//             context,
//             melior::ir::attribute::StringAttribute::new(context, ""),
//             melior::ir::attribute::TypeAttribute::new(
//                 melior::ir::r#type::FunctionType::new(context, inputs, results).into(),
//             ),
//             region,
//             attributes,
//             location,
//         ))
//     }
// }
