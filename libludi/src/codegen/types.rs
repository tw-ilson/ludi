use super::writer::{self, MLIRGen};
use crate::err::Result;
use crate::shape::ArrayProps;
use crate::types;

// Convert from native types to MLIR types

impl<'c> writer::MLIRGen<'c, melior::ir::Type<'c>> for types::Type {
    fn mlir_gen(&self, context: &'c melior::Context) -> Result<melior::ir::Type<'c>> {
        Ok(match self {
            Self::Atom(atom) => atom.mlir_gen(context)?,
            Self::Array(array) => array.mlir_gen(context)?,
        })
    }
}

impl<'c> writer::MLIRGen<'c, melior::ir::Type<'c>> for types::Array {
    fn mlir_gen(&self, context: &'c melior::Context) -> Result<melior::ir::Type<'c>> {
        Ok(match self {
            types::Array::Arr(arr) => arr.mlir_gen(context)?,
            types::Array::ArrayRef(_arrayref) => todo!(),
        })
    }
}

impl<'c> writer::MLIRGen<'c, melior::ir::Type<'c>> for types::Arr {
    fn mlir_gen(&self, context: &'c melior::Context) -> Result<melior::ir::Type<'c>> {
        Ok(melior::ir::r#type::RankedTensorType::new(
            unsafe {
                assert!(
                    std::mem::size_of::<usize>() == 8,
                    "This conversion is only valid on 64-bit systems."
                );
                std::slice::from_raw_parts(
                    self.shape.shape_slice().as_ptr() as *const u64,
                    self.shape.rank(),
                )
            },
            self.element.mlir_gen(context)?,
            None,
        )
        .into())
    }
}

impl<'c> writer::MLIRGen<'c, melior::ir::Type<'c>> for types::Atom {
    fn mlir_gen(&self, context: &'c melior::Context) -> Result<melior::ir::Type<'c>> {
        Ok(match self {
            types::Atom::Literal(datatype) => datatype.mlir_gen(context)?,
            types::Atom::Func(func) => func.mlir_gen(context)?,
            types::Atom::Tuple(type_list) => todo!(),
            types::Atom::AtomRef(_name) => todo!(),
            types::Atom::Pi(_pi) => todo!(),
            types::Atom::Sigma(_sigma) => todo!(),
            types::Atom::Forall(_forall) => todo!(),
            types::Atom::Unit => todo!()
        })
    }
}

impl<'c> writer::MLIRGen<'c, melior::ir::Type<'c>> for types::Func {
    fn mlir_gen(&self, context: &'c melior::Context) -> Result<melior::ir::Type<'c>> {
        let types::Func {
            parameters,
            return_type,
        } = self;
        Ok(melior::ir::r#type::FunctionType::new(
            context,
            &parameters
                .into_iter()
                .map(|param| param.mlir_gen(context))
                .collect::<Result<Vec<melior::ir::Type<'c>>>>()?,
            &[return_type.mlir_gen(context)?] // make sure this works with tuple type
        )
        .into())
    }
}

impl<'c> writer::MLIRGen<'c, melior::ir::Type<'c>> for types::AtomicDataType {
    fn mlir_gen(&self, context: &'c melior::Context) -> Result<melior::ir::Type<'c>> {
        Ok(match self {
            types::AtomicDataType::Float16 => melior::ir::r#type::Type::float16(context),
            types::AtomicDataType::BFloat16 => melior::ir::r#type::Type::bfloat16(context),
            types::AtomicDataType::Float32 => melior::ir::r#type::Type::float32(context),
            types::AtomicDataType::Float64 => melior::ir::r#type::Type::float64(context),
            types::AtomicDataType::Complex => {
                melior::ir::r#type::Type::complex(melior::ir::r#type::Type::float32(context))
            }
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
        })
    }
}
