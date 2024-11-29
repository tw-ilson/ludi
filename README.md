# Ludi
This is 'Ludi' an experimental programming language. Inspired by APL, it is focused on multidimensional arrays, and has a number of basic features that you can play around with in the interpreter. I am logging and documenting the developement at [twilson.xyz](https://twilson.xyz/post) 

## Example
```
> let x = [4 3 2];
> let y = 5;
> print(x*y)
    20 15 10
> let z = reshape(iota(8), [2 2 2])
> print(z)
    1 2
    3 4

    5 6
    7 8
```
## Usage
With a standard Rust toolchain it the interpreter can be built and run:
```
cargo run -p interpreter
```

## Current Goals & Progress
- [ ] Interpreter (80%)
- [ ] Type checking (75%)
- [ ] Code generation (50%)
- [ ] Optimizations (10%)

## Dependencies
- Rust 2021 edition
- [LLVM 18 and MLIR](https://llvm.org) via the [`melior`](https://github.com/raviqqe/melior) crate
    - It is recommended that you clone & build `melior` in the same directory as you put this one
