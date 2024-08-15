use crate::ast;
use crate::data::DataType;
use crate::atomic::AtomicType;
use crate::array::ArrayType;
use crate::dialect;
use crate::err::Result;

//
trait MLIRGen<R> {
    fn mlir_gen(&self, state: &CodeState) -> anyhow::Result<R>;
}

pub fn setup_mlir() -> melior::Context {
    let registry = melior::dialect::DialectRegistry::new();
    melior::utility::register_all_dialects(&registry);
    let context = melior::Context::new();
    context.append_dialect_registry(&registry);
    context.load_all_available_dialects();
    context
}

struct CodeState {
    pub context: melior::Context,
    pub location: melior::ir::Location<'static>,
    pub module: melior::ir::Module<'static>,
}

impl MLIRGen<dialect::ludi::ConstantOperation<'_>> for ast::LiteralNode {
    fn mlir_gen(
        &self,
        state: &CodeState,
    ) -> anyhow::Result<dialect::ludi::ConstantOperation<'static>> {
        use DataType::*;
        use AtomicType::*;
        use ArrayType::*;
        match &self.value {
            DataType::Array(a) => Ok(dialect::ludi::constant(
                &state.context,
                todo!(),
                state.location,
            )),
            DataType::Atomic(a) => Ok(dialect::ludi::constant(
                &state.context,
                melior::ir::attribute::DenseElementsAttribute::new(
                    melior::ir::r#type::Type::float64(&state.context),
                    match a {
                        AtomicType::Float64(n) => &[
                            melior::ir::attribute::FloatAttribute::new(
                                &state.context,
                                melior::ir::r#type::Type::float64(&state.context)
                                , *n).into()
                        ],
                        _ => panic!()
                    }
                )?,
                state.location,
            )),
        }
    }
}

impl MLIRGen<melior::dialect::ods::builtin::ModuleOperation<'_>> for ast::AstProgram {
    fn mlir_gen(
        &self,
        state: &CodeState,
    ) -> anyhow::Result<melior::dialect::ods::builtin::ModuleOperation<'static>> {
        // for stmt in self {
        //     stmt.mlir_gen()
        // }
        todo!()
    }
}
impl MLIRGen<melior::ir::Region<'_>> for ast::ExprStmtNode {
    fn mlir_gen(&self, state: &CodeState) -> anyhow::Result<melior::ir::Region<'static>> {
        todo!()
    }
}
impl MLIRGen<melior::ir::Region<'_>> for ast::Expr {
    fn mlir_gen(&self, state: &CodeState) -> anyhow::Result<melior::ir::Region<'static>> {
        todo!()
    }
}

impl MLIRGen<dialect::ludi::FuncOperation<'_>> for ast::FnDefNode {
    fn mlir_gen(&self, state: &CodeState) -> anyhow::Result<dialect::ludi::FuncOperation<'static>> {
        todo!()
    }
}

// impl<'c, 'a> MLIRGen<melior::ir::Value<'c,'a>> for ast::LiteralNode {
//     fn mlir_gen(&self, state: &CodeState) -> Result<melior::ir::Value<'c, 'a>> {
//         todo!()
//     }
// }
impl MLIRGen<dialect::ludi::AddF64Operation<'_>> for ast::BinaryNode {
    fn mlir_gen(
        &self,
        state: &CodeState,
    ) -> anyhow::Result<dialect::ludi::AddF64Operation<'static>> {
        todo!()
    }
}
impl MLIRGen<dialect::ludi::PrintOperation<'_>> for ast::PrintStmtNode {
    fn mlir_gen(
        &self,
        state: &CodeState,
    ) -> anyhow::Result<dialect::ludi::PrintOperation<'static>> {
        todo!()
    }
}

impl TryInto<melior::ir::attribute::DenseElementsAttribute<'_>> for DataType {
    type Error = anyhow::Error;
    fn try_into(self) -> anyhow::Result<melior::ir::attribute::DenseElementsAttribute<'static>> {
        todo!()
    }
}
