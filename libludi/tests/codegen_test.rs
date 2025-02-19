use libludi::{
    codegen::{load_builtin_dialects, CodeWriter},
    lex::Lex,
    parser::expression,
    types::{typecheck::TypeCheck, TypeEnv},
    err::Result,
};
use melior::{
    dialect::{arith, func},
    ir::{
        attribute::{StringAttribute, TypeAttribute},
        operation::OperationPrintingFlags,
        r#type::FunctionType,
        Block, Location, Module, Region, Type,
    },
};

fn verify_codegen(program: &str) -> Result<bool> {
    let expr = expression(&mut program.lex())?.type_check(&mut TypeEnv::new())?;
    let writer = CodeWriter::new();
    let module = writer.write_ast(&expr)?;
    println!(
        "{}",
        module
            .as_operation()
            .to_string_with_flags(OperationPrintingFlags::new())?
    );
    Ok(module.as_operation().verify())
}

#[test]
fn return_constant() -> anyhow::Result<()> {
    assert!(verify_codegen(" 1 ")?);
    // assert!(verify_codegen(" -1 ")?);
    assert!(verify_codegen(" 2.4 ")?);
    Ok(())
}

#[test]
fn identity_func() -> anyhow::Result<()> {
    assert!(verify_codegen("fn id(a) { a }")?);
    Ok(())
}


#[test]
fn fn_simple() -> anyhow::Result<()> {
    assert!(verify_codegen(
        "fn add(x, y) {
            x + y
        }"
    )?);
    Ok(())
}

#[test]
//credit: edgl
fn melior_example_simple() -> anyhow::Result<()> {
    let context = load_builtin_dialects();
    // A location is a debug location like in LLVM, in MLIR all
    // operations need a location, even if its "unknown".
    let location = Location::unknown(&context);

    // A MLIR module is akin to a LLVM module.
    let module = Module::new(location);

    // A integer-like type with platform dependent bit width. (like size_t or usize)
    // This is a type defined in the Builtin dialect.
    let index_type = Type::index(&context);

    // Append a `func::func` operation to the body (a block) of the module.
    // This operation accepts a string attribute, which is the name.
    // A type attribute, which contains a function type in this case.
    // Then it accepts a single region, which is where the body
    // of the function will be, this region can have
    // multiple blocks, which is how you may implement
    // control flow within the function.
    // These blocks each can have more operations.
    module.body().append_operation(func::func(
        &context,
        // accepts a StringAttribute which is the function name.
        StringAttribute::new(&context, "add"),
        // A type attribute, defining the function signature.
        TypeAttribute::new(
            FunctionType::new(&context, &[index_type, index_type], &[index_type]).into(),
        ),
        {
            // The first block within the region, blocks accept arguments
            // In regions with control flow, MLIR leverages
            // this structure to implicitly represent
            // the passage of control-flow dependent ealues without the complex nuances
            // of PHI nodes in traditional SSA representations.
            let block = Block::new(&[(index_type, location), (index_type, location)]);
            // Use the arith dialect to add the 2 arguments.
            let sum = block.append_operation(arith::addi(
                block.argument(0).unwrap().into(),
                block.argument(1).unwrap().into(),
                location,
            ));
            // Return the result using the "func" dialect return operation.
            block.append_operation(func::r#return(&[sum.result(0).unwrap().into()], location));
            // The Func operation requires a region,
            // we add the block we created to the region and return it,
            // which is passed as an argument to the `func::func` function.
            let region = Region::new();
            region.append_block(block);
            region
        },
        &[],
        location,
    ));
    println!(
        "{}",
        module
            .as_operation()
            .to_string_with_flags(OperationPrintingFlags::new())?
    );
    assert!(module.as_operation().verify());
    Ok(())
}
