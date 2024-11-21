# Ludi
This is 'Ludi' an experimental programming language.It is focused on arrays, and has a number of basic features that you can play around with in the interpreter.

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

With a standard Rust toolchain it can be built and run:
```
cargo run -p interpreter
```
