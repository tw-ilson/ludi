use std::str;
use std::{
    fs,
    io::Write,
    path::{Path, PathBuf},
    process::Command,
};

use libludi::utils::{llvm_config, mlir_opt, mlir_translate};

#[test]
fn diff_of_squares_to_llvm() -> anyhow::Result<()> {
    llvm_translate("tests/mlir_tests/diff_of_squares.mlir")
}
#[test]
fn loop_fusion_to_gpu() -> anyhow::Result<()> {
    let path = fs::canonicalize(Path::new("tests/mlir_tests/sum_buffer.mlir"))?;
    let out = mlir_opt(
        r#"--pass-pipeline="builtin.module(
            func.func(
                affine-loop-coalescing,           
                affine-loop-fusion,               
                affine-loop-invariant-code-motion,
                affine-loop-normalize,            
                affine-loop-tile,                 
                affine-loop-unroll,               
                affine-loop-unroll-jam,           
                ),
                convert-affine-for-to-gpu
            )"
        "#,
        &path)?;
    println!("{}", out);
    panic!()
}

// fn spirv_translate(file_name: &str) -> anyhow::Result<()> {
//     let path = fs::canonicalize(Path::new(file_name))?;
//     if !path.exists() {
//         return Err(anyhow::anyhow!("File does not exist."));
//     }
//     let llvm_dialect = mlir_opt(
//         "--convert-to-spirv",
//         &path
//     )?;
//     let mut spirv_dialect_file = tempfile::NamedTempFile::with_suffix("mlir")?;
//     spirv_dialect_file.write(llvm_dialect.as_bytes())?;
//     let spirv_ir = mlir_translate("--mlir-to-spirv", spirv_dialect_file.path())?;
//     println!("{}", spirv_ir);
//     Ok(())
// }

fn llvm_run(file_name: &str) -> anyhow::Result<()> {
    let call = format!(
        "{} {}",
        PathBuf::from(llvm_config("--bindir")?)
            .join("llc")
            .display(),
        file_name
    );
    let output = if cfg!(target_os = "windows") {
        Command::new("cmd").args(["/C", &call]).output()?
    } else {
        Command::new("sh").arg("-c").arg(&call).output()?
    };
    Ok(())
}

fn llvm_translate(file_name: &str) -> anyhow::Result<()> {
    // Create a PathBuf from the file name
    let path = fs::canonicalize(Path::new(file_name))?;
    if !path.exists() {
        return Err(anyhow::anyhow!("File does not exist."));
    }

    // mlir-opt your_file.mlir --convert-arith-to-llvm --convert-func-to-llvm
    let llvm_dialect = mlir_opt(
        "--convert-to-llvm --llvm-legalize-for-export",
        &path
    )?;
    let mut llvm_dialect_file = tempfile::NamedTempFile::with_suffix("mlir")?;
    llvm_dialect_file.write(llvm_dialect.as_bytes())?;

    //mlir-translate --mlir-to-llvmir your_file.mlir -o output.ll
    let llvm_ir = mlir_translate("--mlir-to-llvmir", llvm_dialect_file.path())?;
    println!("{}", llvm_ir);
    Ok(())
}

