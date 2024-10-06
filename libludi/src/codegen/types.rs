use super::codewriter;
use crate::err::Result;
use crate::types;

impl<'c> codewriter::MLIRGen<'c, melior::ir::Type<'c>> for types::Type {
    fn mlir_gen(self, context: &'c melior::Context) -> Result<melior::ir::Type<'c>> {
        Ok(match self {
            Self::Atom(atom) => match atom {
                types::Atom::Literal(datatype) => match datatype {
                    types::AtomicDataType::Float16 => melior::ir::r#type::Type::float16(context),
                    types::AtomicDataType::BFloat16 => melior::ir::r#type::Type::bfloat16(context),
                    types::AtomicDataType::Float32 => melior::ir::r#type::Type::float32(context),
                    types::AtomicDataType::Float64 => melior::ir::r#type::Type::float64(context),
                    types::AtomicDataType::Complex => melior::ir::r#type::Type::complex(
                        melior::ir::r#type::Type::float32(context),
                    ),
                    types::AtomicDataType::UInt8 => {
                        melior::ir::r#type::IntegerType::unsigned(context, 8).into()
                    }
                    types::AtomicDataType::Int8 => {
                        melior::ir::r#type::IntegerType::signed(context, 8).into()
                    }
                    types::AtomicDataType::UInt16 => {
                        melior::ir::r#type::IntegerType::unsigned(context, 16).into()
                    }
                    types::AtomicDataType::Int16 => {
                        melior::ir::r#type::IntegerType::signed(context, 16).into()
                    }
                    types::AtomicDataType::UInt32 => {
                        melior::ir::r#type::IntegerType::unsigned(context, 32).into()
                    }
                    types::AtomicDataType::Int32 => {
                        melior::ir::r#type::IntegerType::signed(context, 32).into()
                    }
                    types::AtomicDataType::UInt64 => {
                        melior::ir::r#type::IntegerType::unsigned(context, 64).into()
                    }
                    types::AtomicDataType::Int64 => {
                        melior::ir::r#type::IntegerType::signed(context, 64).into()
                    }
                    types::AtomicDataType::Boolean => {
                        melior::ir::r#type::IntegerType::unsigned(context, 1).into()
                    }
                    types::AtomicDataType::Character => todo!(),
                },
                types::Atom::AtomRef(name) => todo!(),
                types::Atom::Pi(pi) => todo!(),
                types::Atom::Sigma(sigma) => todo!(),
                types::Atom::Func(func) => todo!(),
                types::Atom::Forall(forall) => todo!(),
            },
            Self::Array(types::Array::ArrayRef(name)) => todo!(),
            Self::Array(types::Array::Arr(arr)) => todo!(),
        })
    }
}
