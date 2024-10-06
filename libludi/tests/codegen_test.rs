use libludi::{codegen::codewriter::CodeWriter, lex::lex, parser::expression};
use melior::ir::operation::OperationPrintingFlags;

#[test]
fn atom_literally() -> anyhow::Result<()> {
    let expr = expression(&mut lex("1"))?;
    let writer = CodeWriter::new();
    let module = writer.write_ast(expr)?;
    println!("{}", module.as_operation().to_string_with_flags(OperationPrintingFlags::new())?);
    assert!(module.as_operation().verify());
    Ok(())
}
