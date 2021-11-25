# Tunk

**Tunk** is a

- strongly and dynamically typed
- pure functional
- interpreted
- general use

programming language. Tunk is a hobby project written for educational purposes, and is inspired by languages such as Elm and Haskell.

## Running Tunk

Build the `tunk` interpreter with `cargo`:

    cargo build --release

Run `tunk` on a source file:

    target/release/tunk run samples/sample1.tunk

Run `tunk` REPL:

    target/release/tunk repl

## TODO

- 𝛂-conversion to avoid variable name collisions in recursive functions
- A pipeline operator (`argument |> function`)
- A function composition operator (`inner >> outer`)
- tail call recursion optimization
- more primitives: `list`, `tuple`
- list/monad operations `map`, `filter`, `reduce`
- an effect system for handling IO etc.

## Author

Jan Tuomi, <<jans.tuomi@gmail.com>>
