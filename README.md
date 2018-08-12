# kolga

kolga is an educational compiler with the following goals:

1. Learn the LLVM API by using it to generate LLVM IR and machine code.
2. Learn about type systems by building a type checker, type inference, and more complex ADT's.
3. Support object oriented composability.
4. Eventually, build some light concurrency primitives.

### Requirements

1. [rust](https://rust-lang.org)
2. [llvm (supports 6.0)](https://llvm.org)

### Running
```sh
cargo run [filename]
```

### Testing
```sh
cargo test
```
