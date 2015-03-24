// Copyright 2014 Murarth
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Runs Rust code in an encapsulated environment

use std::env::args;
use std::fs::File;
use std::mem::transmute;
use std::path::{Path, PathBuf};

use rustc::middle::ty;
use rustc::util::ppaux::Repr;

use syntax::{ast, codemap, visit};
use syntax::ast::Stmt_::StmtSemi;
use syntax::parse::token;

use exec::ExecutionEngine;
use input::{parse_command, parse_program};
use input::{FileReader, Input, InputReader};
use input::InputResult::*;

/// Starting prompt
const DEFAULT_PROMPT: &'static str = "rusti=> ";
/// Prompt when further input is being read
const MORE_PROMPT: &'static str = "rusti.> ";
/// Prompt when a `.block` command is in effect
const BLOCK_PROMPT: &'static str = "rusti+> ";

// TODO: Implement commands:
//     def <name>; shows the definition of type or fn
//     doc <name>; links to rustdoc page for name

struct CommandDef {
    name: &'static str,
    args: Option<&'static str>,
    help: &'static str,
}

/// List of commands
static COMMANDS: &'static [CommandDef] = &[
    CommandDef{name: "block", args: None, help: "Run a multi-line block of code, terminated by `.`"},
    CommandDef{name: "help", args: Some("[command]"), help: "Show help for commands"},
    CommandDef{name: "type", args: Some("<expr>"), help: "Show the type of expr"},
];

/// Executes input code and maintains state of persistent items.
pub struct Repl {
    /// First entry of `env::args`
    argv0: String,
    engine: ExecutionEngine,
    /// Module-level attributes applied to every program
    attributes: Vec<String>,
    /// View items compiled into every program
    view_items: Vec<String>,
    /// Items compiled into every program
    /// TODO: When type/def-injection is implemented,
    /// it will not be necessary to re-compile all functions on every input.
    items: Vec<String>,
    /// true if the next input should be a block
    read_block: bool,
}

/// Looks up a command name by what may be an abbreviated prefix.
/// Returns the `CommandDef` structure if one is found.
fn lookup_command(name: &str) -> Option<&'static CommandDef> {
    for cmd in COMMANDS.iter() {
        if cmd.name.starts_with(name) {
            return Some(cmd);
        }
    }
    None
}

impl Repl {
    /// Constructs a new `Repl`.
    pub fn new(sysroot: Option<PathBuf>) -> Repl {
        Repl::new_with_libs(Vec::new(), sysroot)
    }

    /// Constructs a new `Repl` with additional library lookup paths.
    pub fn new_with_libs(libs: Vec<String>, sysroot: Option<PathBuf>) -> Repl {
        let argv0 = args().next()
            .unwrap_or_else(|| "rusti".to_string());

        Repl{
            argv0: argv0,
            engine: ExecutionEngine::new(libs, sysroot),
            attributes: Vec::new(),
            view_items: Vec::new(),
            items: Vec::new(),
            read_block: false,
        }
    }

    /// Evaluates a single round of input, printing the result to `stdout`.
    pub fn eval(&mut self, input: &str) {
        match parse_program(input, false, None) {
            Program(i) => self.handle_input(i),
            _ => (),
        }
    }

    /// Runs the REPL interactively.
    pub fn run(&mut self) {
        let mut more = false;
        let mut input = InputReader::new();

        loop {
            let res = if self.read_block {
                self.read_block = false;
                input.read_block_input(BLOCK_PROMPT)
            } else {
                input.read_input(if more { MORE_PROMPT } else { DEFAULT_PROMPT })
            };

            match res {
                Command(name, args) => {
                    debug!("read command: {} {:?}", name, args);

                    self.handle_command(name, args);
                },
                Program(input) => {
                    debug!("read program: {:?}", input);

                    more = false;
                    self.handle_input(input);
                },
                Empty => (),
                More => { more = true; },
                Eof => {
                    if stdin_tty() {
                        println!("");
                    }
                    break;
                }
                InputError(err) => {
                    if let Some(err) = err {
                        println!("{}", err);
                    }
                    more = false;
                },
            };
        }
    }

    /// Runs a single `rusti` command.
    pub fn run_command(&mut self, cmd: &str) {
        match parse_command(cmd) {
            Command(name, args) => self.handle_command(name, args),
            InputError(Some(err)) => println!("{}", err),
            _ => ()
        }
    }

    /// Runs rusti input from the named file.
    /// Returns `true` if it was compiled successfully.
    pub fn run_file(&mut self, path: &Path) -> bool {
        let f = match File::open(path) {
            Ok(f) => f,
            Err(e) => {
                println!("{}: failed to open {}: {}",
                    self.argv0, path.display(), e);
                return false;
            }
        };

        let mut input = FileReader::new(f);

        loop {
            if self.read_block {
                println!("{}: `.block` command is not necessary when running a file",
                    self.argv0);
                return false;
            }

            let input = input.read_input();

            match input {
                Program(input) => self.handle_input(input),
                Command(name, args) => self.handle_command(name, args),
                InputError(Some(e)) => {
                    println!("{}: {}", self.argv0, e);
                    return false;
                }
                InputError(None) => return false,
                Eof => break,
                _ => unreachable!(),
            }
        }

        true
    }

    /// Build a program text containing all persistent items seen so far and,
    /// optionally, those from an `Input` instance. The `statements` field of
    /// `input` will be ignored.
    fn build_program(&self, input: Option<&Input>, program: &str) -> String {
        let (attrs, vitems, items) = if let Some(input) = input {
            let attrs = self.attributes.iter().map(|s| &s[..])
                .chain(input.attributes.iter().map(|s| &s[..]))
                .collect::<Vec<_>>();

            let vitems = self.view_items.iter().map(|s| &s[..])
                .chain(input.view_items.iter().map(|s| &s[..]))
                .collect::<Vec<_>>();

            let items = self.items.iter().map(|s| &s[..])
                .chain(input.items.iter().map(|s| &s[..]))
                .collect::<Vec<_>>();

            (attrs, vitems, items)
        } else {
            let attrs = self.attributes.iter().map(|s| &s[..])
                .collect::<Vec<_>>();

            let vitems = self.view_items.iter().map(|s| &s[..])
                .collect::<Vec<_>>();

            let items = self.items.iter().map(|s| &s[..])
                .collect::<Vec<_>>();

            (attrs, vitems, items)
        };

        let attrs = attrs.connect("\n");
        let vitems = vitems.connect("\n");
        let items = items.connect("\n");

        format!(
r#"#![allow(dead_code, unused_imports, unused_features, unstable_features)]
#![feature(std_misc)]
{attrs}
{vitems}
{items}
{program}
"#
        , attrs = attrs
        , vitems = vitems
        , items = items
        , program = program)
    }

    /// Runs a single command input.
    fn handle_command(&mut self, cmd: String, args: Option<String>) {
        match lookup_command(&cmd).map(|c| c.name) {
            Some("block") => {
                if args.is_some() {
                    println!("command `block` takes no arguments");
                } else {
                    self.read_block = true;
                }
            },
            Some("help") => {
                self.help_command(args.as_ref().map(|s| &s[..]));
            }
            Some("type") => {
                if let Some(args) = args {
                    self.type_command(args);
                } else {
                    println!("command `type` expects an expression");
                }
            },
            _ => println!("unrecognized command `{}`", cmd),
        }
    }

    /// Runs a single program input.
    fn handle_input(&mut self, mut input: Input) {
        let name = "_rusti_run";

        if input.last_expr && !input.statements.is_empty() {
            let stmt = input.statements.last_mut().unwrap();
            *stmt = format!(r#"println!("{{:?}}", {{ {} }});"#, stmt);
        }

        let stmts = input.statements.connect("\n");

        let prog = self.build_program(Some(&input),
            &format!(
r#"
#[no_mangle]
pub fn {name}() {{
    let _ = unsafe {{ std::rt::unwind::try(_rusti_inner) }};
}}

fn _rusti_inner() {{
{stmts}
}}
"#
            , name = name
            , stmts = stmts
            )
        );

        if let Some(_) = self.engine.add_module(prog) {
            let fp = self.engine.get_function(name).unwrap();
            let f: fn() = unsafe { transmute(fp) };

            f();

            // NOTE: The module cannot be removed after it is run because tasks
            // may still be running in the module code. This means that rusti's
            // memory footprint will only grow over time.
            // Hopefully, this will not be noticeable in normal use.

            // Successful compile means we can add the new items to every program
            self.attributes.extend(input.attributes.into_iter());
            self.view_items.extend(input.view_items.into_iter());
            self.items.extend(input.items.into_iter());
        }
    }

    fn help_command(&self, command: Option<&str>) {
        if let Some(cmd) = command {
            match lookup_command(cmd) {
                None => println!("unrecognized command: {}", cmd),
                Some(cmd) => {
                    println!("");

                    match cmd.args {
                        None => println!("{}", cmd.name),
                        Some(args) => println!("{} {}", cmd.name, args),
                    }
                    println!("  {}", cmd.help);
                    println!("");
                }
            }
        } else {
            println!("Available commands:");
            println!("");

            for cmd in COMMANDS {
                match cmd.args {
                    None => println!("  {:<16} {}", cmd.name, cmd.help),
                    Some(args) => println!("  {:<16} {}",
                        format!("{} {}", cmd.name, args), cmd.help)
                }
            }

            println!("");
        }
    }

    fn expr_type(&self, fn_name: &str, prog: String) -> Option<String> {
        let fn_name = fn_name.to_string();

        self.engine.with_analysis(prog, move |analysis| {
            let mut v = ExprType{
                fn_name: fn_name,
                result: None,
                ty_cx: &analysis.ty_cx,
            };

            visit::walk_crate(&mut v, analysis.ty_cx.map.krate());

            if let Some(ty) = v.result {
                ty
            } else {
                panic!("no type found");
            }
        })
    }

    fn type_command(&mut self, expr: String) {
        let name = "_rusti_type";
        let prog = self.build_program(None, &format!(
r#"
#[allow(path_statements)]
fn {name}() {{
{expr} ;
}}
"#
        , name = name
        , expr = expr
        ));

        if let Some(t) = self.expr_type(name, prog) {
            println!("{} = {}", expr, t);
        }
    }
}

struct ExprType<'a, 'tcx: 'a> {
    fn_name: String,
    result: Option<String>,
    ty_cx: &'a ty::ctxt<'tcx>,
}

impl<'v, 'a, 'tcx> visit::Visitor<'v> for ExprType<'a, 'tcx> {
    fn visit_fn(&mut self, fk: visit::FnKind<'v>, _fd: &'v ast::FnDecl,
            b: &'v ast::Block, _s: codemap::Span, _n: ast::NodeId) {
        if let visit::FkItemFn(ident, _, _, _) = fk {
            if &*token::get_ident(ident) == self.fn_name {
                if let Some(ref stmt) = b.stmts.last() {
                    if let StmtSemi(ref expr, _) = stmt.node {
                        let id = expr.id;
                        if let Some(ty) = self.ty_cx.node_types.borrow().get(&id) {
                            self.result = Some(ty.repr(self.ty_cx));
                        }
                    }
                }
            }
        }
    }
}

#[cfg(unix)]
fn stdin_tty() -> bool {
    use libc::{isatty, STDIN_FILENO};
    unsafe { isatty(STDIN_FILENO) != 0 }
}

#[cfg(not(unix))]
fn stdin_tty() -> bool { false }
