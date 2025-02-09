// took from melior's build.rs

use std::{
    env,
    error::Error,
    path::{Path, PathBuf},
    process::Command,
    str,
};

use crate::{err::Result, LLVM_MAJOR_VERSION};

pub fn llvm_config(argument: &str) -> Result<String> {
    let version_variable = format!("MLIR_SYS_{}0_PREFIX", LLVM_MAJOR_VERSION);
    let prefix = env::var(version_variable)
        .map(|path| Path::new(&path).join("bin"))
        .unwrap_or_default();
    let call = format!(
        "{} --link-static {}",
        prefix.join("llvm-config").display(),
        argument
    );

    Ok(str::from_utf8(
        &if cfg!(target_os = "windows") {
            Command::new("cmd").args(["/C", &call]).output()?
        } else {
            Command::new("sh").arg("-c").arg(&call).output()?
        }
        .stdout,
    )?
    .trim()
    .to_string())
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
