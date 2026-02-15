# Testing Setup for Ludi

## Snapshot Testing Integration

The codegen tests have been refactored to use `insta` for snapshot testing, which provides better regression testing for MLIR output.

### Changes Made

1. **Added `insta` dependency**: Added `insta = "1.40.0"` to `[dev-dependencies]` in `libludi/Cargo.toml`

2. **Created helper function**: Added `codegen_to_string()` helper function that:
   - Parses Ludi source code
   - Type checks the AST
   - Generates MLIR code
   - Verifies the MLIR module
   - Returns the MLIR string for snapshot comparison

3. **Refactored existing tests**:
   - `return_constant` → split into `return_constant_int` and `return_constant_float`
   - `identity_func` → now uses snapshot testing
   - `fn_simple` → now uses snapshot testing
   - `melior_example_simple` → kept unchanged as requested

### Snapshot Testing Benefits

- **Regression Detection**: Automatically detects unintended changes to MLIR output
- **Review Workflow**: Easy to review MLIR output changes via git diffs of snapshot files
- **Documentation**: Snapshots serve as documentation of expected MLIR output
- **Test Maintenance**: No need to manually write expected MLIR strings in tests

## Prerequisites for Running Tests

### LLVM and MLIR Installation

The tests require LLVM 18 with MLIR development headers installed. Currently, the system has:
- ✅ LLVM 18 runtime installed (`llvm-18`, `llvm-18-linker-tools`, `llvm-18-runtime`)
- ❌ LLVM 18 development headers NOT installed

**To install MLIR headers** (requires sudo):
```bash
sudo apt-get install llvm-18-dev libmlir-18-dev mlir-18-tools
```

### Melior (Rust MLIR Bindings)

The project uses a local checkout of `melior` for LLVM 18 compatibility:

1. **Clone melior** (if not already done):
   ```bash
   cd /home/user
   git clone https://github.com/raviqqe/melior.git
   ```

2. **Checkout LLVM 18 compatible version**:
   ```bash
   cd melior
   git checkout a681a8ca9f  # Last commit before LLVM 19 update
   ```

3. **Verify version**: The `melior/melior/Cargo.toml` should show `version = "0.18.6"`

### Cargo Configuration

The `libludi/Cargo.toml` has been configured to:
- Use `melior = "0.18.6"` from local path `../../melior/melior`
- Removed direct `mlir-sys` dependency (provided by melior)
- Added `insta = "1.40.0"` for snapshot testing

## Running Tests

Once MLIR headers are installed:

```bash
# Run all codegen tests
cargo test --test codegen_test

# Run specific test
cargo test --test codegen_test return_constant_int

# Review and accept snapshot changes
cargo insta review

# Accept all snapshots automatically
cargo insta accept
```

## Snapshot Management

### First Test Run
On the first run, `insta` will create snapshot files in:
```
libludi/tests/snapshots/codegen_test/
├── return_constant_int.snap
├── return_constant_float.snap
├── identity_func.snap
└── fn_simple.snap
```

### Reviewing Changes
When MLIR output changes:
```bash
# Interactive review
cargo insta review

# Or manually inspect .snap.new files
diff tests/snapshots/codegen_test/return_constant_int.snap \
     tests/snapshots/codegen_test/return_constant_int.snap.new
```

### CI Integration
For CI pipelines, use:
```bash
# Fail if snapshots don't match
cargo insta test

# Or verify explicitly
cargo test --test codegen_test
cargo insta test --check
```

## Current Status

- ✅ Code refactoring complete
- ✅ `insta` integrated as dev dependency
- ✅ Helper functions added
- ✅ Tests refactored (except `melior_example_simple`)
- ✅ Melior checked out to LLVM 18 compatible version
- ❌ Tests cannot run yet (MLIR headers not installed)
- ⏳ Snapshots will be generated on first successful test run

## Next Steps

1. **Install MLIR development headers** on system with sudo access
2. **Run tests** to generate initial snapshots:
   ```bash
   cargo test --test codegen_test
   ```
3. **Review and commit snapshots**:
   ```bash
   cargo insta review
   git add libludi/tests/snapshots/
   git commit -m "Add initial MLIR output snapshots"
   ```

## Troubleshooting

### "mlir-c/AffineExpr.h not found"
- MLIR development headers are not installed
- Install `llvm-18-dev` and `libmlir-18-dev` packages

### "failed to find correct version of llvm-config"
- Wrong LLVM version in PATH
- Ensure `llvm-config-18` is available and points to LLVM 18

### "package links to native library MLIR conflicts"
- Multiple versions of `mlir-sys` in dependency tree
- Ensure only one version via `melior` (removed direct dependency)

### Melior version mismatch
- Checkout the correct commit in `/home/user/melior`:
  ```bash
  cd /home/user/melior
  git checkout a681a8ca9f  # LLVM 18 compatible
  ```

## References

- [insta documentation](https://docs.rs/insta/)
- [MLIR documentation](https://mlir.llvm.org/)
- [melior repository](https://github.com/raviqqe/melior)
- [Cargo dependencies](https://doc.rust-lang.org/cargo/reference/specifying-dependencies.html)
