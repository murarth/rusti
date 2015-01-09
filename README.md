# Rusti

A REPL for the Rust programming language.

Rusti is a work in progress.

## Building

Rusti builds with the latest Rust nightly, using the Cargo build system.

Rusti requires GNU Readline.

Build with Cargo:

    cargo build

Run tests:

    cargo test

Run `rusti`:

    cargo run

Cargo does not currently support an `install` subcommand, so if you would like
to run `rusti` outside the build directory, you must manually copy `target/rusti`
into a directory in your `PATH` environment variable.

## Usage

Running `rusti` gives a prompt that accepts (most) any valid Rust code.
If the final statement is an expression, the result will be displayed.

```rust
rusti=> println!("Hello, world!");
Hello, world!
rusti=> 2 + 2
4i32
rusti=> (0..5).collect::<Vec<_>>()
[0i32, 1i32, 2i32, 3i32, 4i32]
```

If any delimiters are left open, `rusti` will continue reading input until they are closed.
Only then will the code be executed.

```rust
rusti=> fn factorial(n: u32) -> u32 {
rusti.>     match n {
rusti.>         0 => 0,
rusti.>         1 => 1,
rusti.>         n => n * factorial(n - 1),
rusti.>     }
rusti.> }
rusti=> factorial(3)
6u32
rusti=> factorial(4)
24u32
rusti=> factorial(5)
120u32
```

`rusti` can also run a file given on the command line.  
Note that a `rusti` input file is not quite the same as a typical Rust program.
A typical Rust program contains a function named `main`. While a `rusti`
program can define functions, no functions will be called automatically.
Instead, all statements not within a function body will be executed sequentially,
just like interactive mode.

## Commands

These are special inputs interpreted by `rusti` that are not directly
evaluated as Rust code, though they may operate on Rust code.

Commands are invoked by entering a line beginning with `.`, followed by the
name of the command and, perhaps, some text used by the command.

Command names may be arbitrarily abbreviated.  
For example, `.type` may be abbreviated as `.typ`, `.ty`, or `.t`.

### `.block`

The `.block` command will run multiple lines of Rust code as one program.

To end the command and run all code, input `.` on its own line.

```rust
rusti=> .block
rusti+> let a = 1;
rusti+> let b = a * 2;
rusti+> let c = b * 3;
rusti+> c
rusti+> .
6
```

Entering `.q` instead will end the command without running code.

### `.type`

The `.type` command will display the type of an expression without running it.

```rust
rusti=> .type 42
42 = i32
rusti=> .t 'x'
'x' = char
rusti=> .t "Hello!"
"Hello!" = &'static str
rusti=> .t (1i32, 2u32)
(1i32, 2u32) = (i32, u32)
rusti=> fn foo() -> i32 { 1 }
rusti=> .t foo
foo = fn() -> i32
rusti=> .t foo()
foo() = i32
```

## Limitations

Currently, Rusti has the following limitations.
I hope to fix each of them, but some may prove to be large problems to tackle.

* Functions and types are redefined in each round of input.  
  This is inefficient.
* `static` items are also redefined in each round of input.  
  This means that the address of a `static` item will change in every round
  of input and that the values of `mut` items or those with interior mutability
  will be reset to their initial definition on each round of input.  
  This is bad.
* Use of `thread_local!` causes a crash.  
  This is bad.
* `let` declarations are local to the input in which they are defined.  
  They cannot be referenced later and are destroyed after that round of input
  completes its execution.  
  This is inconvenient.
* And more!

## License

Rusti is distributed under the terms of both the MIT license and the
Apache License (Version 2.0).

See LICENSE-APACHE and LICENSE-MIT for details.
