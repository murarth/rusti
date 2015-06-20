// Copyright 2014-2015 Rusti Project
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! A REPL for the Rust programming language.

#![crate_name = "rusti"]
#![feature(path_ext, rustc_private, set_stdio, slice_extras)]

extern crate getopts;
extern crate libc;
extern crate rustc;
extern crate rustc_driver;
extern crate rustc_lint;
extern crate rustc_resolve;
extern crate syntax;
extern crate tempfile;

#[macro_use] extern crate log;
extern crate env_logger;

use getopts::Options;

use std::fs::PathExt;
use std::path::PathBuf;

pub mod completion;
pub mod exec;
pub mod input;
pub mod readline;
pub mod repl;

/// Run `rusti` executable using `env::args`.
/// Returns desired process exit status.
pub fn run() -> i32 {
    env_logger::init().unwrap();

    let args = std::env::args().collect::<Vec<_>>();
    let mut opts = Options::new();

    opts.optopt("c", "", "Execute a rusti command and exit", "COMMAND");
    opts.optopt("e", "", "Execute a one-line program and exit", "PROGRAM");
    opts.optflag("h", "help", "Print this help message and exit");
    opts.optflag("i", "interactive", "Run rusti interactively even with a file");
    opts.optflag("v", "version", "Print version and exit");
    opts.optmulti("L", "", "Add a directory to the library search path", "PATH");
    opts.optflag("", "no-rc", "Do not run $HOME/.rustirc.rs");
    opts.optopt("", "sysroot", "Use an alternate Rust sysroot", "PATH");

    let matches = match opts.parse(args.tail()) {
        Ok(m) => m,
        Err(e) => {
            println!("{}: {}", args[0], e);
            return 1;
        }
    };

    if matches.opt_present("version") {
        print_version();
        return 0;
    }
    if matches.opt_present("help") {
        print_usage(&args[0], &opts);
        return 0;
    }

    let interactive = matches.opt_present("interactive") ||
        (matches.free.is_empty() &&
        !matches.opt_present("c") &&
        !matches.opt_present("e"));

    let addl_libs = matches.opt_strs("L");
    let sysroot = matches.opt_str("sysroot").map(|s| PathBuf::from(&s));

    let mut repl = repl::Repl::new_with_libs(addl_libs, sysroot);

    if !matches.opt_present("no-rc") {
        if let Some(p) = std::env::home_dir() {
            let rc = p.join(".rustirc.rs");
            if rc.is_file() {
                if !repl.run_file(&rc) {
                    return 1;
                }
            }
        }
    }

    if let Some(cmd) = matches.opt_str("c") {
        repl.run_command(&cmd);
    } else if let Some(expr) = matches.opt_str("e") {
        repl.eval(&expr);
    } else if !matches.free.is_empty() {
        let path = PathBuf::from(&matches.free[0]);

        if !repl.run_file(&path) {
            return 1;
        }
    }

    if interactive {
        repl.run();
    }

    0
}

/// Returns a version string.
pub fn version() -> &'static str {
    env!("CARGO_PKG_VERSION")
}

fn print_usage(arg0: &str, opts: &Options) {
    print!("{}", opts.usage(&format!(
        "Usage: {} [OPTIONS] [FILE]", arg0)));
}

fn print_version() {
    println!("rusti {}", version());
}
