# Rusti

A REPL for the Rust programming language.

Rusti is a work in progress.

## Dependencies

Rusti requires GNU Readline.

### Ubuntu / Debian

```
sudo apt-get install libreadline-dev
```

### Fedora / CentOS

```
sudo yum install readline-devel
```

## Building

Rusti builds with the latest Rust nightly, using the Cargo build system.  
Currently, it **must** be built using a nightly release of the Rust compiler.

Nightly releases can be found at the bottom of the
[Rust install page](http://www.rust-lang.org/install.html)
or installed using the `--channel=nightly` option to
[rustup](https://github.com/rust-lang/rustup).

For users of stable or beta, [multirust](https://github.com/brson/multirust)
can be used to maintain multiple concurrent Rust installs.

### Installation using Cargo

Rusti can be installed directly using Cargo. The following command will
download, build, and compile Rusti, placing it in `~/.cargo/bin/` or your
operating system equivalent.

    cargo install --git https://github.com/murarth/rusti

If you are using `multirust`, that command will look like this:

    # update to the latest nightly version
    multirust update nightly
    multirust run nightly cargo install --git https://github.com/murarth/rusti


### Building from a Git clone

If using [multirust](https://github.com/brson/multirust), the following command
will create an override to use the nightly branch within the `rusti` source tree:

    multirust override nightly

Build with Cargo:

    cargo build

Run tests:

    cargo test

Run `rusti`:

    cargo run

Install:

    cargo install

## Usage

Running `rusti` gives a prompt that accepts (most) any valid Rust code.
If the final statement is an expression, the result will be displayed using the
`std::fmt::Debug` trait. This is equivalent to `println!("{:?}", expr);`.

```rust
rusti=> println!("Hello, world!");
Hello, world!
rusti=> 2 + 2
4
rusti=> (0..5).collect::<Vec<_>>()
[0, 1, 2, 3, 4]
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
6
rusti=> factorial(4)
24
rusti=> factorial(5)
120
```

`rusti` can also run a file given on the command line.  
Note that a `rusti` input file is not quite the same as a typical Rust program.
A typical Rust program contains a function named `main`. While a `rusti`
program can define functions, no functions will be called automatically.
Instead, all statements not within a function body will be executed sequentially,
just like interactive mode.

### Loading Crates

Loading crates which are part of the standard Rust distribution is as easy as
declaring the crate, thusly:

```rust
extern crate foo;
```

However, loading a crate that you have compiled yourself requires some extra steps:

* First, `rusti` must be able to find the location of compiled crate.  
  You can add a path to its search list using the command line option `-L path`.  
  `rusti` accepts any number of `-L` arguments.
* Secondly, `rusti` requires both an `rlib` and a `dylib` version of the
  compiled crate. If you're building your crate with Cargo, the following
  command will build the required files for your project's library:

      cargo rustc --lib -- --crate-type=rlib,dylib

  If you're building with rustc directly, simply add `--crate-type=rlib,dylib`
  to the build command to produce the required files.

### Code completion

`rusti` provides optional support for code completion using [Racer](https://github.com/phildawes/racer).

To enable code completion, install Racer as outlined in the [Installation Instructions](https://github.com/phildawes/racer#installation) and place the `racer` executable into your `PATH`.

## Commands

These are special inputs interpreted by `rusti` that are not directly
evaluated as Rust code, though they may operate on Rust code.

Commands are invoked by entering a line beginning with `.` or `:`, followed by the
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

### `.help`

The `.help` command shows usage text for any available commands.

### `.load`

The `.load` command evaluates the contents of a named file.

### `.print`

The `.print` command will display the value of an expression, using the
`std::fmt::Display` trait. This is equivalent to `println!("{}", expr);`.

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
foo = fn() -> i32 {foo}
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
