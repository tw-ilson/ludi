use libludi::err::Result;
use melior::{
    dialect::{arith, func, DialectRegistry},
    ir::{
        attribute::{StringAttribute, TypeAttribute},
        r#type::FunctionType,
        *,
    },
    utility::register_all_dialects,
    Context,
};

struct ContextBuilder {
    context: melior::Context,
    region: melior::ir::Region<'static>,
    module: melior::ir::Module<'static>,
}

pub fn setup_mlir() -> Context {
    let registry = DialectRegistry::new();
    register_all_dialects(&registry);
    let context = Context::new();
    context.append_dialect_registry(&registry);
    context.load_all_available_dialects();
    context
}
impl ContextBuilder {
    pub fn write_op() {}
    pub fn write_block() {}
}

melior::dialect! {
    name: "ludi",
    td_file: "shvm/src/ods/dialect.td",
}

