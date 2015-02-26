// Copyright 2014 Murarth
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! A REPL for the Rust programming language.

#![crate_name = "rusti"]
#![feature(unsafe_destructor)]
#![feature(collections, env, old_io, os, libc, old_path, rustc_private, std_misc)]
#![unstable]

extern crate getopts;
extern crate rustc;
extern crate syntax;

#[macro_use] extern crate log;

use getopts::Options;

use std::old_io::fs::PathExtensions;

pub mod exec;
pub mod input;
pub mod readline;
pub mod repl;

/// Run `rusti` executable using `env::args`
pub fn run() {
    let args = std::env::args().collect::<Vec<_>>();
    let mut opts = Options::new();

    opts.optopt("c", "", "Execute a rusti command and exit", "COMMAND");
    opts.optopt("e", "", "Execute a one-line program and exit", "PROGRAM");
    opts.optflag("h", "help", "Print this help message and exit");
    opts.optflag("i", "interactive", "Run rusti interactively even with a file");
    opts.optflag("v", "version", "Print version and exit");
    opts.optmulti("L", "", "Add a directory to the library search path", "PATH");
    opts.optflag("", "no-rc", "Do not run $HOME/.rustirc.rs");

    let matches = match opts.parse(args.tail()) {
        Ok(m) => m,
        Err(e) => {
            println!("{}: {}", args[0], e);
            std::env::set_exit_status(1);
            return;
        }
    };

    if matches.opt_present("version") {
        print_version();
        return;
    }
    if matches.opt_present("help") {
        print_usage(&args[0], &opts);
        return;
    }

    let interactive = matches.opt_present("interactive") ||
        (matches.free.is_empty() &&
        !matches.opt_present("c") &&
        !matches.opt_present("e"));

    let addl_libs = matches.opt_strs("L");

    let mut repl = repl::Repl::new_with_libs(addl_libs);

    if !matches.opt_present("no-rc") {
        if let Some(p) = std::os::homedir() {
            let rc = p.join(".rustirc.rs");
            if rc.is_file() {
                if !repl.run_file(rc) {
                    std::env::set_exit_status(1);
                    return;
                }
            }
        }
    }

    if let Some(cmd) = matches.opt_str("c") {
        repl.run_command(&cmd);
    } else if let Some(expr) = matches.opt_str("e") {
        repl.eval(&expr);
    } else if !matches.free.is_empty() {
        let path = Path::new(&matches.free[0]);

        if !repl.run_file(path) {
            std::env::set_exit_status(1);
        }
    }

    if interactive {
        repl.run();
    }
}

/// Returns a version string.
pub fn version() -> String {
    // Is this really the best way to do this?
    format!("{}.{}.{}{}",
        env!("CARGO_PKG_VERSION_MAJOR"),
        env!("CARGO_PKG_VERSION_MINOR"),
        env!("CARGO_PKG_VERSION_PATCH"),
        option_env!("CARGO_PKG_VERSION_PRE").unwrap_or(""))
}

fn print_usage(arg0: &str, opts: &Options) {
    print!("{}", opts.usage(&format!(
        "Usage: {} [OPTIONS] [FILE]", arg0)));
}

fn print_version() {
    println!("rusti {}", version());
}
