use std::str;
use std::{
    fs,
    io::Write,
    path::{Path, PathBuf},
    process::Command,
};

use libludi::utils::llvm_config;

#[test]
fn diff_of_squares_llvm() -> anyhow::Result<()> {
    llvm_translate("tests/mlir_tests/diff_of_squares.mlir")
}
#[test]
fn matmul_llvm() -> anyhow::Result<()> {
    llvm_translate("tests/mlir_tests/matmul.mlir")
}

fn spirv_translate(file_name: &str) -> anyhow::Result<()> {
    let path = fs::canonicalize(Path::new(file_name))?;
    if !path.exists() {
        return Err(anyhow::anyhow!("File does not exist."));
    }
    let llvm_dialect = mlir_opt(
        "--convert-to-spirv",
        &path
    )?;
    let mut spirv_dialect_file = tempfile::NamedTempFile::with_suffix("mlir")?;
    spirv_dialect_file.write(llvm_dialect.as_bytes())?;
    let spirv_ir = mlir_translate("--mlir-to-spirv", spirv_dialect_file.path())?;
    println!("{}", spirv_ir);
    Ok(())
}

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

pub fn mlir_translate(argument: &str, mlir_path: &Path) -> anyhow::Result<String> {
    let call = format!(
        "{} {} {}",
        PathBuf::from(llvm_config("--bindir")?)
            .join("mlir-translate")
            .display(),
        mlir_path.display(),
        argument
    );

    let output = if cfg!(target_os = "windows") {
        Command::new("cmd").args(["/C", &call]).output()?
    } else {
        Command::new("sh").arg("-c").arg(&call).output()?
    };

    let stdout = str::from_utf8(&output.stdout)?.trim().to_string();
    let stderr = str::from_utf8(&output.stderr)?.trim().to_string();
    if !output.status.success() {
        Err(anyhow::Error::msg(stderr))
    } else {
        Ok(stdout)
    }
}

pub fn mlir_opt(argument: &str, mlir_path: &Path) -> anyhow::Result<String> {
    let call = format!(
        "{} {} {}",
        PathBuf::from(llvm_config("--bindir")?)
            .join("mlir-opt")
            .display(),
        mlir_path.display(),
        argument
    );
    let output = if cfg!(target_os = "windows") {
        Command::new("cmd").args(["/C", &call]).output()?
    } else {
        Command::new("sh").arg("-c").arg(&call).output()?
    };
    let stdout = str::from_utf8(&output.stdout)?.trim().to_string();
    let stderr = str::from_utf8(&output.stderr)?.trim().to_string();
    if !output.status.success() {
        Err(anyhow::Error::msg(stderr))
    } else {
        Ok(stdout)
    }
}
