# Code Generation Test Coverage

## Overview
Comprehensive test suite for Ludi's MLIR code generation backend based on `CODEGEN_TESTING_PLAN.md`.

## Test Summary

### ✅ Passing Tests (6)
Tests that currently generate valid MLIR and pass verification:

| Test | Description | Status |
|------|-------------|--------|
| `return_constant_int` | Integer literal constant | ✅ PASS |
| `return_constant_float` | Float literal constant | ✅ PASS |
| `zero_constant_int` | Zero integer literal | ✅ PASS |
| `zero_constant_float` | Zero float literal | ✅ PASS |
| `large_constant_int` | Large integer (i64::MAX) | ✅ PASS |
| `melior_example_simple` | Direct MLIR API usage example | ✅ PASS |

### ⏸️ Ignored Tests (28)
Tests for unimplemented features, ready to be enabled as features are completed:

#### Phase 1: Let Bindings (5 tests)
- `let_simple_literal` - Simple let with literal initialization
- `let_with_expression` - Let with expression initialization
- `let_with_scoped_region` - Let with scoped region
- `let_nested` - Nested let bindings
- `let_with_type_inference` - Let with type coercion

#### Phase 2: Function Definitions (8 tests)
- `fn_no_params` - Function with no parameters
- `fn_single_param` - Function with single parameter
- `fn_multiple_params` - Function with multiple parameters
- `identity_func` - Identity function
- `fn_simple` - Simple add function
- `fn_call_basic` - Basic function call
- `fn_with_let_binding` - Function with let binding in body
- `fn_recursive` - Recursive function (requires control flow)

#### Phase 3: Array Operations (5 tests)
- `array_literal_1d` - 1D array literal
- `array_literal_2d` - 2D array literal
- `array_elementwise_add` - Element-wise array addition
- `array_scalar_broadcast` - Scalar-array broadcasting
- `array_reduction_sum` - Array reduction (sum)

#### Phase 4: Arithmetic Operations (6 tests)
- `arith_add_constants` - Addition of constants
- `arith_sub_constants` - Subtraction of constants
- `arith_mul_constants` - Multiplication of constants
- `arith_div_constants` - Division of constants
- `arith_complex_expression` - Complex arithmetic expression
- `arith_float_operations` - Float arithmetic operations

#### Edge Cases (4 tests)
- `negative_constant` - Negative number literals
- `boolean_constant` - Boolean literals
- `char_constant` - Character literals
- `scientific_notation_float` - Scientific notation (parser limitation)

## Running Tests

### Run All Passing Tests
```bash
cargo test --test codegen_test
```

Output:
```
test result: ok. 6 passed; 0 failed; 28 ignored
```

### Run All Tests (Including Ignored)
```bash
cargo test --test codegen_test -- --include-ignored
```

### Run Specific Test Category
```bash
# Run only let binding tests
cargo test --test codegen_test let_ -- --include-ignored

# Run only function tests
cargo test --test codegen_test fn_ -- --include-ignored

# Run only array tests
cargo test --test codegen_test array_ -- --include-ignored
```

### Review Snapshots
```bash
# After running tests with new features
cargo insta review

# Accept all new snapshots
cargo insta accept
```

## Snapshot Testing

All tests use `insta` for snapshot testing of MLIR output. Snapshots are stored in:
```
libludi/tests/snapshots/
├── codegen_test__return_constant_int.snap
├── codegen_test__return_constant_float.snap
├── codegen_test__zero_constant_int.snap
├── codegen_test__zero_constant_float.snap
└── codegen_test__large_constant_int.snap
```

### Example Snapshot
```mlir
---
source: libludi/tests/codegen_test.rs
expression: mlir
---
module {
  func.func @main() {
    %c1_i64 = arith.constant 1 : i64
    return
  }
}
```

## Development Workflow

### When Implementing a Feature

1. **Find the relevant ignored test(s)**
   ```bash
   grep -n "ignore.*Let bindings" libludi/tests/codegen_test.rs
   ```

2. **Implement the feature** in `libludi/src/codegen/`

3. **Remove the `#[ignore]` attribute** from the test

4. **Run the test**
   ```bash
   cargo test --test codegen_test let_simple_literal
   ```

5. **Review and accept the snapshot**
   ```bash
   cargo insta review
   ```

6. **Commit both code and snapshots**
   ```bash
   git add libludi/src/codegen/ libludi/tests/snapshots/
   git commit -m "Implement let bindings codegen"
   ```

## Test Organization

Tests are organized by phase following the implementation plan:

```rust
// Phase 1: Let Bindings Tests
#[test]
#[ignore = "Let bindings not yet implemented"]
fn let_simple_literal() -> anyhow::Result<()> { ... }

// Phase 2: Function Definition Tests
#[test]
#[ignore = "Function definitions not yet implemented"]
fn fn_no_params() -> anyhow::Result<()> { ... }

// Phase 3: Array Operations Tests
#[test]
#[ignore = "Array literals not yet implemented"]
fn array_literal_1d() -> anyhow::Result<()> { ... }

// Phase 4: Arithmetic Operations Tests
#[test]
#[ignore = "Arithmetic operations not yet implemented"]
fn arith_add_constants() -> anyhow::Result<()> { ... }
```

## Current State

- **Total Tests**: 34
- **Passing**: 6 (18%)
- **Ignored**: 28 (82%)
- **Snapshot Coverage**: All passing tests have snapshots

## Next Steps

Following the `CODEGEN_TESTING_PLAN.md`:

1. **Sprint 1-2**: Implement let bindings (5 tests to enable)
2. **Sprint 3-4**: Implement function definitions (8 tests to enable)
3. **Sprint 5-6**: Implement array operations (5 tests to enable)
4. **Additional**: Implement arithmetic operations (6 tests to enable)

As each feature is implemented, remove the `#[ignore]` attribute and verify the MLIR output via snapshots.

## CI Integration

For continuous integration, add:

```yaml
# .github/workflows/test.yml
- name: Run codegen tests
  run: cargo test --test codegen_test

- name: Check snapshot freshness
  run: cargo insta test --check
```

This ensures:
- All non-ignored tests pass
- No uncommitted snapshot changes exist
