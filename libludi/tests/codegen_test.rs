use libludi::{
    codegen::{load_builtin_dialects, CodeWriter},
    lex::Lex,
    parser::expression,
    types::{typecheck::{TypeCheck, TypedTree}, TypeEnv},
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

/// Helper to generate MLIR string for snapshot testing
fn codegen_to_string(program: &str) -> Result<String> {
    let expr = expression(&mut program.lex())?.type_check(&mut TypeEnv::new())?;
    let tree = TypedTree {
        toplevel_expressions: vec![expr],
    };
    let writer = CodeWriter::new();
    let module = writer.write_ast(&tree)?;
    let mlir_str = module
        .as_operation()
        .to_string_with_flags(OperationPrintingFlags::new())?;
    Ok(mlir_str)
}

#[test]
fn return_constant_int() -> anyhow::Result<()> {
    let mlir = codegen_to_string(" 1 ")?;
    insta::assert_snapshot!(mlir);
    Ok(())
}

#[test]
fn return_constant_float() -> anyhow::Result<()> {
    let mlir = codegen_to_string(" 2.4 ")?;
    insta::assert_snapshot!(mlir);
    Ok(())
}

// =============================================================================
// Phase 1: Let Bindings Tests
// =============================================================================

#[test]
#[ignore = "Let bindings not yet implemented"]
fn let_simple_literal() -> anyhow::Result<()> {
    let mlir = codegen_to_string("let x = 42; x")?;
    insta::assert_snapshot!(mlir);
    Ok(())
}

#[test]
#[ignore = "Let bindings not yet implemented"]
fn let_with_expression() -> anyhow::Result<()> {
    let mlir = codegen_to_string("let x = 10 + 5; x")?;
    insta::assert_snapshot!(mlir);
    Ok(())
}

#[test]
#[ignore = "Let bindings with regions not yet implemented"]
fn let_with_scoped_region() -> anyhow::Result<()> {
    let mlir = codegen_to_string("let x = 5 { x + 10 }")?;
    insta::assert_snapshot!(mlir);
    Ok(())
}

#[test]
#[ignore = "Nested let bindings not yet implemented"]
fn let_nested() -> anyhow::Result<()> {
    let mlir = codegen_to_string("let x = 5 { let y = 10 { x + y } }")?;
    insta::assert_snapshot!(mlir);
    Ok(())
}

#[test]
#[ignore = "Type coercion in let bindings not yet implemented"]
fn let_with_type_inference() -> anyhow::Result<()> {
    let mlir = codegen_to_string("let x = 3.14; let y = 2; x + y")?;
    insta::assert_snapshot!(mlir);
    Ok(())
}

// =============================================================================
// Phase 2: Function Definition Tests
// =============================================================================

#[test]
#[ignore = "Function definitions not yet implemented"]
fn fn_no_params() -> anyhow::Result<()> {
    let mlir = codegen_to_string("fn get_answer() { 42 }")?;
    insta::assert_snapshot!(mlir);
    Ok(())
}

#[test]
#[ignore = "Function definitions not yet implemented"]
fn fn_single_param() -> anyhow::Result<()> {
    let mlir = codegen_to_string("fn double(x) { x + x }")?;
    insta::assert_snapshot!(mlir);
    Ok(())
}

#[test]
#[ignore = "Function definitions not yet implemented"]
fn fn_multiple_params() -> anyhow::Result<()> {
    let mlir = codegen_to_string("fn add(x, y) { x + y }")?;
    insta::assert_snapshot!(mlir);
    Ok(())
}

#[test]
#[ignore = "Function definitions not yet implemented"]
fn identity_func() -> anyhow::Result<()> {
    let mlir = codegen_to_string("fn id(a) { a }")?;
    insta::assert_snapshot!(mlir);
    Ok(())
}

#[test]
#[ignore = "Function definitions not yet implemented"]
fn fn_simple() -> anyhow::Result<()> {
    let mlir = codegen_to_string(
        "fn add(x, y) {
            x + y
        }"
    )?;
    insta::assert_snapshot!(mlir);
    Ok(())
}

#[test]
#[ignore = "Function calls not yet implemented"]
fn fn_call_basic() -> anyhow::Result<()> {
    let mlir = codegen_to_string("fn add(x, y) { x + y } add(5, 3)")?;
    insta::assert_snapshot!(mlir);
    Ok(())
}

#[test]
#[ignore = "Function definitions with let bindings not yet implemented"]
fn fn_with_let_binding() -> anyhow::Result<()> {
    let mlir = codegen_to_string("fn compute(x) { let y = x * 2 { y + 10 } }")?;
    insta::assert_snapshot!(mlir);
    Ok(())
}

#[test]
#[ignore = "Recursive functions require control flow - deferred"]
fn fn_recursive() -> anyhow::Result<()> {
    let mlir = codegen_to_string(
        "fn factorial(n) { if (n <= 1) { 1 } else { n * factorial(n - 1) } }"
    )?;
    insta::assert_snapshot!(mlir);
    Ok(())
}

// =============================================================================
// Phase 3: Array Operations Tests
// =============================================================================

#[test]
#[ignore = "Array literals not yet implemented"]
fn array_literal_1d() -> anyhow::Result<()> {
    let mlir = codegen_to_string("[1.0, 2.0, 3.0]")?;
    insta::assert_snapshot!(mlir);
    Ok(())
}

#[test]
#[ignore = "Multi-dimensional array literals not yet implemented"]
fn array_literal_2d() -> anyhow::Result<()> {
    let mlir = codegen_to_string("[[1.0, 2.0], [3.0, 4.0]]")?;
    insta::assert_snapshot!(mlir);
    Ok(())
}

#[test]
#[ignore = "Array operations not yet implemented"]
fn array_elementwise_add() -> anyhow::Result<()> {
    let mlir = codegen_to_string(
        "let a = [1.0, 2.0, 3.0]; let b = [4.0, 5.0, 6.0]; a + b"
    )?;
    insta::assert_snapshot!(mlir);
    Ok(())
}

#[test]
#[ignore = "Scalar-array broadcasting not yet implemented"]
fn array_scalar_broadcast() -> anyhow::Result<()> {
    let mlir = codegen_to_string(
        "let scalar = 2.0; let arr = [1.0, 2.0, 3.0]; scalar * arr"
    )?;
    insta::assert_snapshot!(mlir);
    Ok(())
}

#[test]
#[ignore = "Array reduction not yet implemented"]
fn array_reduction_sum() -> anyhow::Result<()> {
    let mlir = codegen_to_string("reduce(+, [1.0, 2.0, 3.0, 4.0])")?;
    insta::assert_snapshot!(mlir);
    Ok(())
}

// =============================================================================
// Phase 4: Arithmetic Operations Tests
// =============================================================================

#[test]
#[ignore = "Arithmetic operations not yet implemented"]
fn arith_add_constants() -> anyhow::Result<()> {
    let mlir = codegen_to_string("1 + 2")?;
    insta::assert_snapshot!(mlir);
    Ok(())
}

#[test]
#[ignore = "Arithmetic operations not yet implemented"]
fn arith_sub_constants() -> anyhow::Result<()> {
    let mlir = codegen_to_string("10 - 3")?;
    insta::assert_snapshot!(mlir);
    Ok(())
}

#[test]
#[ignore = "Arithmetic operations not yet implemented"]
fn arith_mul_constants() -> anyhow::Result<()> {
    let mlir = codegen_to_string("4 * 5")?;
    insta::assert_snapshot!(mlir);
    Ok(())
}

#[test]
#[ignore = "Arithmetic operations not yet implemented"]
fn arith_div_constants() -> anyhow::Result<()> {
    let mlir = codegen_to_string("20 / 4")?;
    insta::assert_snapshot!(mlir);
    Ok(())
}

#[test]
#[ignore = "Complex arithmetic expressions not yet implemented"]
fn arith_complex_expression() -> anyhow::Result<()> {
    let mlir = codegen_to_string("(2 + 3) * 4")?;
    insta::assert_snapshot!(mlir);
    Ok(())
}

#[test]
#[ignore = "Float arithmetic not yet implemented"]
fn arith_float_operations() -> anyhow::Result<()> {
    let mlir = codegen_to_string("3.14 * 2.0")?;
    insta::assert_snapshot!(mlir);
    Ok(())
}

// =============================================================================
// Additional Edge Cases and Integration Tests
// =============================================================================

#[test]
#[ignore = "Negative numbers not yet implemented"]
fn negative_constant() -> anyhow::Result<()> {
    let mlir = codegen_to_string("-1")?;
    insta::assert_snapshot!(mlir);
    Ok(())
}

#[test]
#[ignore = "Boolean literals not yet implemented"]
fn boolean_constant() -> anyhow::Result<()> {
    let mlir = codegen_to_string("true")?;
    insta::assert_snapshot!(mlir);
    Ok(())
}

#[test]
#[ignore = "Character literals not yet implemented"]
fn char_constant() -> anyhow::Result<()> {
    let mlir = codegen_to_string("'a'")?;
    insta::assert_snapshot!(mlir);
    Ok(())
}

#[test]
fn zero_constant_int() -> anyhow::Result<()> {
    let mlir = codegen_to_string("0")?;
    insta::assert_snapshot!(mlir);
    Ok(())
}

#[test]
fn zero_constant_float() -> anyhow::Result<()> {
    let mlir = codegen_to_string("0.0")?;
    insta::assert_snapshot!(mlir);
    Ok(())
}

#[test]
fn large_constant_int() -> anyhow::Result<()> {
    let mlir = codegen_to_string("9223372036854775807")?; // i64::MAX
    insta::assert_snapshot!(mlir);
    Ok(())
}

#[test]
#[ignore = "Parser doesn't support scientific notation yet"]
fn scientific_notation_float() -> anyhow::Result<()> {
    let mlir = codegen_to_string("1.23e-4")?;
    insta::assert_snapshot!(mlir);
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
