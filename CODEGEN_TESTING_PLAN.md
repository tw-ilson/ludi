# Backend Code Generation Testing Plan

## Overview
This plan outlines comprehensive unit tests for partially implemented backend features in Ludi's MLIR code generation system.

## Partially Implemented Features

### 1. Let Bindings
**Status**: Implementation exists (`writer.rs:163`) but incomplete
**Current Issues**:
- Region handling incomplete
- Value propagation from initializer not implemented
- Scoped variable binding not functional

### 2. Function Definitions
**Status**: Commented out (`writer.rs:176-194`)
**Current Issues**:
- FnDefNode MLIRGen implementation commented out
- Parameter binding not implemented
- Function body code generation incomplete
- Return handling missing

### 3. Array Operations
**Status**: Type support exists, codegen incomplete
**Current Issues**:
- Array literals not implemented in codegen
- Array indexing/slicing missing
- Array primitive operations not mapped to MLIR
- Multi-dimensional array handling incomplete

---

## Testing Strategy

### Phase 1: Let Bindings (Priority: HIGH)

#### Test 1.1: Simple Let with Literal Initialization
```ludi
let x = 42;
x
```
**Expected MLIR**:
```mlir
func.func @main() {
  %x = arith.constant 42 : i64
  func.return %x : i64
}
```
**Implementation Gaps**:
- [ ] Store constant result to variable binding
- [ ] Retrieve variable value in subsequent expressions
- [ ] Handle variable lifetime/scope

#### Test 1.2: Let with Expression Initialization
```ludi
let x = 10 + 5;
x
```
**Expected MLIR**:
```mlir
func.func @main() {
  %0 = arith.constant 10 : i64
  %1 = arith.constant 5 : i64
  %x = arith.addi %0, %1 : i64
  func.return %x : i64
}
```
**Implementation Gaps**:
- [ ] Evaluate initializer expression first
- [ ] Bind result to variable name
- [ ] Maintain SSA form with proper value references

#### Test 1.3: Let with Scoped Region
```ludi
let x = 5 {
  x + 10
}
```
**Expected MLIR**:
```mlir
func.func @main() {
  %x = arith.constant 5 : i64
  %0 = arith.constant 10 : i64
  %result = arith.addi %x, %0 : i64
  func.return %result : i64
}
```
**Implementation Gaps**:
- [ ] Handle optional region expression
- [ ] Pass variable binding into region scope
- [ ] Return value from region

#### Test 1.4: Nested Let Bindings
```ludi
let x = 5 {
  let y = 10 {
    x + y
  }
}
```
**Expected MLIR**:
```mlir
func.func @main() {
  %x = arith.constant 5 : i64
  %y = arith.constant 10 : i64
  %result = arith.addi %x, %y : i64
  func.return %result : i64
}
```
**Implementation Gaps**:
- [ ] Handle nested scopes
- [ ] Maintain environment with multiple bindings
- [ ] Proper variable shadowing semantics

#### Test 1.5: Let with Type Inference
```ludi
let x = 3.14;
let y = 2;
x + y  // float + int should coerce
```
**Implementation Gaps**:
- [ ] Type coercion in arithmetic operations
- [ ] Proper type propagation from initializer

---

### Phase 2: Function Definitions (Priority: HIGH)

#### Test 2.1: Function with No Parameters
```ludi
fn get_answer() {
  42
}
```
**Expected MLIR**:
```mlir
func.func @get_answer() -> i64 {
  %0 = arith.constant 42 : i64
  func.return %0 : i64
}
```
**Implementation Gaps**:
- [ ] Uncomment and complete FnDefNode implementation
- [ ] Generate func.func operation with proper signature
- [ ] Add return operation
- [ ] Use function name from AST

#### Test 2.2: Function with Single Parameter
```ludi
fn double(x) {
  x + x
}
```
**Expected MLIR**:
```mlir
func.func @double(%arg0: f64) -> f64 {
  %result = arith.addf %arg0, %arg0 : f64
  func.return %result : f64
}
```
**Implementation Gaps**:
- [ ] Map parameters to block arguments
- [ ] Bind parameter names to argument values
- [ ] Infer parameter types from type checker
- [ ] Use proper MLIR types (f64, i64, tensor, etc.)

#### Test 2.3: Function with Multiple Parameters
```ludi
fn add(x, y) {
  x + y
}
```
**Expected MLIR**:
```mlir
func.func @add(%arg0: f64, %arg1: f64) -> f64 {
  %result = arith.addf %arg0, %arg1 : f64
  func.return %result : f64
}
```
**Implementation Gaps**:
- [ ] Handle multiple parameters
- [ ] Preserve parameter order
- [ ] Map each parameter to correct block argument

#### Test 2.4: Function Call (Non-Recursive)
```ludi
fn add(x, y) { x + y }
add(5, 3)
```
**Expected MLIR**:
```mlir
func.func @add(%arg0: i64, %arg1: i64) -> i64 {
  %result = arith.addi %arg0, %arg1 : i64
  func.return %result : i64
}
func.func @main() -> i64 {
  %0 = arith.constant 5 : i64
  %1 = arith.constant 3 : i64
  %result = func.call @add(%0, %1) : (i64, i64) -> i64
  func.return %result : i64
}
```
**Implementation Gaps**:
- [ ] Implement FnCall codegen
- [ ] Generate func.call operations
- [ ] Resolve callee name/reference
- [ ] Pass arguments in correct order
- [ ] Handle return value

#### Test 2.5: Function with Let Binding in Body
```ludi
fn compute(x) {
  let y = x * 2 {
    y + 10
  }
}
```
**Expected MLIR**:
```mlir
func.func @compute(%arg0: f64) -> f64 {
  %two = arith.constant 2.0 : f64
  %y = arith.mulf %arg0, %two : f64
  %ten = arith.constant 10.0 : f64
  %result = arith.addf %y, %ten : f64
  func.return %result : f64
}
```
**Implementation Gaps**:
- [ ] Combine function and let binding implementations
- [ ] Maintain proper scope with parameters and local variables

#### Test 2.6: Recursive Function
```ludi
fn factorial(n) {
  if (n <= 1) { 1 } else { n * factorial(n - 1) }
}
```
**Note**: Requires control flow (if/else) implementation
**Deferred**: Mark as stretch goal

---

### Phase 3: Array Operations (Priority: MEDIUM)

#### Test 3.1: Array Literal (1D, Homogeneous)
```ludi
[1.0, 2.0, 3.0]
```
**Expected MLIR**:
```mlir
func.func @main() -> tensor<3xf64> {
  %arr = arith.constant dense<[1.0, 2.0, 3.0]> : tensor<3xf64>
  func.return %arr : tensor<3xf64>
}
```
**Implementation Gaps**:
- [ ] Implement ArrayLiteral MLIRGen
- [ ] Use DenseElementsAttr for constant arrays
- [ ] Infer shape from literal
- [ ] Handle homogeneous element types

#### Test 3.2: Array Literal (Multi-dimensional)
```ludi
[[1.0, 2.0], [3.0, 4.0]]
```
**Expected MLIR**:
```mlir
func.func @main() -> tensor<2x2xf64> {
  %arr = arith.constant dense<[[1.0, 2.0], [3.0, 4.0]]> : tensor<2x2xf64>
  func.return %arr : tensor<2x2xf64>
}
```
**Implementation Gaps**:
- [ ] Handle nested array literals
- [ ] Compute multi-dimensional shape
- [ ] Validate shape consistency (rectangular arrays)

#### Test 3.3: Element-wise Array Addition
```ludi
let a = [1.0, 2.0, 3.0];
let b = [4.0, 5.0, 6.0];
a + b
```
**Expected MLIR**:
```mlir
func.func @main() -> tensor<3xf64> {
  %a = arith.constant dense<[1.0, 2.0, 3.0]> : tensor<3xf64>
  %b = arith.constant dense<[4.0, 5.0, 6.0]> : tensor<3xf64>
  %result = ludi.addf64 %a, %b : tensor<3xf64>
  func.return %result : tensor<3xf64>
}
```
**Implementation Gaps**:
- [ ] Implement primitive Add for arrays
- [ ] Use custom ludi dialect operations
- [ ] Handle tensor types in primitive operations

#### Test 3.4: Scalar-Array Operations (Broadcasting)
```ludi
let scalar = 2.0;
let arr = [1.0, 2.0, 3.0];
scalar * arr
```
**Expected MLIR** (with broadcasting):
```mlir
func.func @main() -> tensor<3xf64> {
  %scalar = arith.constant 2.0 : f64
  %arr = arith.constant dense<[1.0, 2.0, 3.0]> : tensor<3xf64>
  %broadcasted = tensor.splat %scalar : tensor<3xf64>
  %result = ludi.mulf64 %broadcasted, %arr : tensor<3xf64>
  func.return %result : tensor<3xf64>
}
```
**Implementation Gaps**:
- [ ] Implement broadcasting semantics
- [ ] Use tensor.splat or similar for scalar->tensor promotion
- [ ] Handle mixed scalar/array operations

#### Test 3.5: Array Reduction (Sum)
```ludi
reduce(+, [1.0, 2.0, 3.0, 4.0])
```
**Expected MLIR**:
```mlir
func.func @main() -> f64 {
  %arr = arith.constant dense<[1.0, 2.0, 3.0, 4.0]> : tensor<4xf64>
  %init = arith.constant 0.0 : f64
  %result = linalg.reduce ins(%arr : tensor<4xf64>)
                          outs(%init : f64)
                          dimensions = [0]
                          (%a: f64, %b: f64) {
    %sum = arith.addf %a, %b : f64
    linalg.yield %sum : f64
  }
  func.return %result : f64
}
```
**Implementation Gaps**:
- [ ] Implement reduce primitive
- [ ] Use linalg dialect for reductions
- [ ] Handle different reduction operators
- [ ] Support multi-dimensional reductions

---

## Implementation Order

### Sprint 1: Basic Let Bindings
**Duration**: 3-5 days
**Tests**: 1.1, 1.2, 1.3
**Goal**: Get simple let bindings working with value propagation

**Tasks**:
1. Fix LetNode MLIRGen to properly bind initializer result
2. Implement variable lookup in environment
3. Add tests for simple cases
4. Verify MLIR output and module verification

### Sprint 2: Advanced Let Bindings
**Duration**: 2-3 days
**Tests**: 1.4, 1.5
**Goal**: Handle nested scopes and type coercion

**Tasks**:
1. Implement nested scope handling
2. Add variable shadowing tests
3. Verify type coercion in arithmetic operations

### Sprint 3: Basic Function Definitions
**Duration**: 5-7 days
**Tests**: 2.1, 2.2, 2.3
**Goal**: Get function definitions working without calls

**Tasks**:
1. Uncomment FnDefNode implementation
2. Implement parameter binding to block arguments
3. Generate proper func.func operations
4. Add return statement handling
5. Test function signature generation

### Sprint 4: Function Calls
**Duration**: 3-5 days
**Tests**: 2.4, 2.5
**Goal**: Enable function invocation

**Tasks**:
1. Implement FnCall MLIRGen
2. Generate func.call operations
3. Handle argument passing
4. Test with let bindings inside functions

### Sprint 5: Basic Array Literals
**Duration**: 3-5 days
**Tests**: 3.1, 3.2
**Goal**: Support array literal syntax

**Tasks**:
1. Implement ArrayLiteral MLIRGen
2. Use DenseElementsAttr for constants
3. Handle shape inference
4. Support multi-dimensional arrays

### Sprint 6: Array Operations
**Duration**: 5-7 days
**Tests**: 3.3, 3.4
**Goal**: Element-wise array operations

**Tasks**:
1. Extend primitive operations to handle tensors
2. Implement broadcasting for scalar-array ops
3. Use custom ludi dialect operations
4. Test element-wise arithmetic

---

## Test Infrastructure Improvements

### 1. Enhanced Test Utilities
```rust
/// Helper to verify codegen and check specific MLIR patterns
fn verify_codegen_contains(program: &str, expected_pattern: &str) -> Result<bool> {
    let expr = expression(&mut program.lex())?.type_check(&mut TypeEnv::new())?;
    let writer = CodeWriter::new();
    let module = writer.write_ast(&expr)?;
    let mlir_str = module.as_operation().to_string_with_flags(OperationPrintingFlags::new())?;

    println!("Generated MLIR:\n{}", mlir_str);

    Ok(module.as_operation().verify() && mlir_str.contains(expected_pattern))
}

/// Helper to get the generated MLIR as a string for inspection
fn codegen_to_string(program: &str) -> Result<String> {
    let expr = expression(&mut program.lex())?.type_check(&mut TypeEnv::new())?;
    let writer = CodeWriter::new();
    let module = writer.write_ast(&expr)?;
    module.as_operation().to_string_with_flags(OperationPrintingFlags::new())
}
```

### 2. Snapshot Testing
Consider adding `insta` crate for snapshot testing of MLIR output:
```rust
#[test]
fn test_let_binding_snapshot() {
    let mlir = codegen_to_string("let x = 42; x").unwrap();
    insta::assert_snapshot!(mlir);
}
```

### 3. Integration Tests
Create end-to-end tests that:
1. Parse Ludi source
2. Type check
3. Generate MLIR
4. Verify MLIR
5. Lower to LLVM IR
6. Execute with JIT and verify output

---

## Success Metrics

### Coverage Goals
- [ ] 90%+ code coverage of writer.rs
- [ ] 100% of TypedExpr variants handled (no `todo!()`)
- [ ] All tests pass with valid MLIR verification

### Quality Goals
- [ ] All generated MLIR passes `module.as_operation().verify()`
- [ ] Generated MLIR can be lowered to LLVM IR without errors
- [ ] MLIR output follows best practices (proper SSA form, type consistency)

### Documentation Goals
- [ ] Each test documents expected MLIR output
- [ ] Implementation gaps clearly documented
- [ ] Error messages guide developers to missing features

---

## Known Blockers

1. **Control Flow**: If/else statements needed for Test 2.6 (recursive functions)
   - Requires MLIR region handling for branches
   - Needs SCF (Structured Control Flow) dialect integration

2. **Higher-Order Functions**: Not covered in this plan
   - Requires closure/lambda implementation
   - Significant type system extensions needed

3. **Array Indexing**: Not included in current plan
   - Requires tensor.extract operations
   - Bounds checking considerations

4. **Memory Management**: All tests assume stack allocation
   - Heap allocation not addressed
   - Reference types not covered

---

## Future Considerations

### Performance Testing
- Benchmark MLIR generation time
- Measure lowering performance
- Compare output quality with hand-written MLIR

### Optimization Passes
After basic codegen works:
- Dead code elimination
- Constant folding
- Common subexpression elimination
- Array fusion

### Error Handling
- Better error messages during codegen
- Source location preservation
- Type mismatch diagnostics

---

## Appendix: Quick Reference

### Test File Structure
```
libludi/tests/
├── codegen_test.rs          # Basic codegen tests
├── codegen_let_test.rs      # NEW: Let binding tests
├── codegen_function_test.rs # NEW: Function def/call tests
├── codegen_array_test.rs    # NEW: Array operation tests
└── mlir_tests/
    └── integration/         # NEW: End-to-end tests
```

### Useful MLIR Resources
- [MLIR Language Reference](https://mlir.llvm.org/docs/LangRef/)
- [Arith Dialect](https://mlir.llvm.org/docs/Dialects/ArithOps/)
- [Func Dialect](https://mlir.llvm.org/docs/Dialects/Func/)
- [Tensor Dialect](https://mlir.llvm.org/docs/Dialects/TensorOps/)

### Melior (Rust MLIR Bindings) Resources
- [Melior Documentation](https://docs.rs/melior/latest/melior/)
- [Melior Examples](https://github.com/edg-l/melior/tree/main/examples)
