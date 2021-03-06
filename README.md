# kolga

kolga is a compiler (indefinitely WIP) with the following goals:

1. Use the LLVM API to generate IR and machine code.
2. Build a type checker, implement type inference, and provide more complex ADT's.
3. Support object/struct composability.
4. Build some light concurrency primitives like coroutines.

Right now, kolga features basic language syntax:
1. Functions (with recursion)
2. Classes
3. Basic control flow
4. A few different types: 64-bit numbers, ASCII strings, bool, and void

Some compiler features so far:
1. Lexing and parsing into an AST
2. Type checking, including for ADT's
3. Basic type inference for variable assignments. Functions still require type annotations.
4. LLVM IR codegen directly from AST (no additional IR) and optimization manager

### Requirements

1. [rust](https://rust-lang.org)
2. [llvm (supports 10.0)](https://llvm.org)

### Running
```sh
cargo run [filename]
```

### Testing
```sh
cargo test -- --nocapture
```

### Some Examples
```
# some basic type inference
fn basicFunc(x~num)~num {
  let inferMe ~= x + 10;
  let dontInferMe~num = x - 1;
  return inferMe;
}

let plsInferMe ~= basicFunc(1);
```

```
fn basicFunc(x~num, y~num)~num {
  let z~num = 10;
  return x + y + z;
}

fn double(x~num)~num {
  return x * 2;
}

fn negate(y~bool)~bool {
  return !y;
}
```

```
# simple class declaration and use
class mClass {
  let z~num;
  let y~num;

  fn nop()~void {
    return;
  }

  fn triple()~num {
    return self.y * 3;
  }
}

let instance~mClass {
  z = 1,
  y = 10,
};

instance.nop();
let tripled ~= instance.triple(); // 30
```

### Project Layout
`kolgac` contains code for lexing and parsing, as well as appropriate token and AST data structures. This is the core compiler.

`kolgac_types` contains type checking/inference

`kolgac_codegen` handles generating the llvm ir from the ast

`kolgac_errors` contains error handling/emitting functions, are well as error types for each stage in the compiler

The `src` directory contains the file `kolga.rs`, which is the main entry point into the compiler.
