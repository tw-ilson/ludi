use crate::types;

impl super::codewriter::MLIRGen<melior::ir::Type<'_>> for types::Type {
    fn mlir_gen(self, context: &melior::Context) -> melior::ir::Type<'static> {
        match self {
            Self::Atom(atom) => match atom {
                crate::types::Atom::Literal(datatype) => match datatype {
                    types::AtomicDataType::Boolean => melior::ir::Type::from(melior::ir::r#type::IntegerType::),
                    types::AtomicDataType::Character => todo!(),
                    types::AtomicDataType::UInt8 => todo!(),
                    types::AtomicDataType::Int8 => todo!(),
                    types::AtomicDataType::UInt16 => todo!(),
                    types::AtomicDataType::Int16 => todo!(),
                    types::AtomicDataType::UInt32 => todo!(),
                    types::AtomicDataType::Int32 => todo!(),
                    types::AtomicDataType::UInt64 => todo!(),
                    types::AtomicDataType::Int64 => todo!(),
                    types::AtomicDataType::Float16 => todo!(),
                    types::AtomicDataType::BFloat16 => todo!(),
                    types::AtomicDataType::Float32 => todo!(),
                    types::AtomicDataType::Float64 => todo!(),
                    types::AtomicDataType::Complex => todo!()
                },
                _ => todo!()
            },
            Self::Array(..) => todo!()
        }
    }
}
