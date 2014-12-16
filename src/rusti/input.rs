// Copyright 2014 Murarth
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Parsing REPL input statements, including Rust code and `rusti` commands.

use std::io::stdio::stderr;
use std::io::util::NullWriter;
use std::task::TaskBuilder;

use super::rustc;

use super::syntax::ast::Decl_::*;
use super::syntax::ast::Stmt_::*;
use super::syntax::ast::ViewItem_::*;
use super::syntax::codemap::{BytePos, CodeMap, Span};
use super::syntax::diagnostic::{Auto, Emitter, EmitterWriter};
use super::syntax::diagnostic::{Level, RenderSpan, mk_handler};
use super::syntax::diagnostic::Level::*;
use super::syntax::diagnostics::registry::Registry;
use super::syntax::parse::{new_parse_sess, string_to_filemap, filemap_to_parser};
use super::syntax::parse::attr::ParserAttr;
use super::syntax::parse::token::{mod, keywords};
use super::syntax::print::pprust;

use super::readline;

pub use self::InputResult::*;
use self::ViewItem::*;

/// Reads input from `stdin`
pub struct InputReader {
    buffer: String,
}

impl InputReader {
    /// Constructs a new `InputReader` reading from `stdin`.
    pub fn new() -> InputReader {
        InputReader{
            buffer: String::new(),
        }
    }

    /// Reads a single command, item, or statement from `stdin`.
    /// Returns `More` if further input is required for a complete result.
    /// In this case, the input received so far is buffered internally.
    pub fn read_input(&mut self, prompt: &str) -> InputResult {
        let line = match readline::read_line(prompt) {
            Some(s) => s,
            None => return Eof,
        };

        self.buffer.push_str(line.as_slice());

        if self.buffer.is_empty() {
            return Empty;
        }

        readline::push_history(line.as_slice());

        let res = if self.buffer.starts_with(".") {
            parse_command(self.buffer.as_slice())
        } else {
            self.buffer.push('\n');
            parse_program(self.buffer.as_slice(), true)
        };

        match res {
            More => (),
            _ => self.buffer.clear(),
        };

        res
    }

    /// Reads a block of input until receiving a line consisting only of `.`,
    /// which will return input, or `.q`, which will cancel and return `Empty`.
    ///
    /// # Panics
    ///
    /// If the internal buffer contains any data; i.e. if the last
    /// result from a call to `read_input` returned `More`.
    pub fn read_block_input(&mut self, prompt: &str) -> InputResult {
        assert!(self.buffer.is_empty());

        let mut buf = String::new();

        loop {
            let line = match readline::read_line(prompt) {
                Some(s) => s,
                None => return Eof,
            };

            if !line.is_empty() {
                readline::push_history(line.as_slice());
            }

            if line == ".q" {
                return Empty;
            } else if line == "." {
                return parse_program(buf.as_slice(), true);
            }

            buf.push_str(line.as_slice());
            buf.push('\n');
        }
    }
}

/// Possible results from reading input from `InputReader`
#[deriving(Show)]
pub enum InputResult {
    /// rusti command as input; (name, rest of line)
    Command(String, Option<String>),
    /// Code as input
    Program(Input),
    /// An empty line
    Empty,
    /// Needs more input; i.e. there is an unclosed delimiter
    More,
    /// End of file reached
    Eof,
    /// Error while parsing input; a Rust parsing error will have printed out
    /// error messages and therefore contain no error message.
    ParseError(Option<&'static str>),
}

/// `ast::ViewItem` type; listed in the order in which they appear in source code
#[deriving(Copy, PartialEq, Eq, PartialOrd, Ord, Show)]
pub enum ViewItem {
    ExternCrate,
    Use,
}

/// Represents an input program
#[deriving(Show)]
pub struct Input {
    /// Module attributes
    pub attributes: Vec<String>,
    /// Module-level view items (`use`, `extern crate`)
    pub view_items: Vec<(ViewItem, String)>,
    /// Module-level items (`fn`, `enum`, `type`, `struct`, etc.)
    pub items: Vec<String>,
    /// Inner statements and declarations
    pub statements: Vec<String>,
    /// Whether the final statement (if there are any) is an expression
    /// without a trailing semicolon
    pub last_expr: bool,
}

impl Input {
    pub fn new() -> Input {
        Input{
            attributes: Vec::new(),
            view_items: Vec::new(),
            items: Vec::new(),
            statements: Vec::new(),
            last_expr: false,
        }
    }
}

/// Parses a line of input as a command.
/// Returns either a `Command` value or a `ParseError` value.
pub fn parse_command(line: &str) -> InputResult {
    if !line.starts_with(".") {
        return ParseError(Some("command must begin with `.`"));
    }

    let line = line.slice(1, line.len());
    let mut words = line.trim_right_chars(' ').splitn(1, ' ');

    let cmd = match words.next() {
        Some(cmd) if !cmd.is_empty() => cmd.to_string(),
        _ => return ParseError(Some("expected command after `.`")),
    };

    let args = words.next().map(|s| s.to_string());

    Command(cmd, args)
}

/// Parses a line of input.
pub fn parse_input(line: &str) -> InputResult {
    if line.starts_with(".") {
        parse_command(line)
    } else {
        parse_program(line, false)
    }
}

/// Parses a line of input as a program.
///
/// If there are parse errors, they will be printed to `stderr`.
/// If `filter` is true, certain errors that indicate an incomplete input
/// will result in a value of `More`. Otherwise, these errors will be emitted
/// and `ParseError` will be returned.
pub fn parse_program(code: &str, filter: bool) -> InputResult {
    let (tx, rx) = channel();

    let task = TaskBuilder::new().stderr(box NullWriter);

    // Items are not returned in data structures; nor are they converted back
    // into strings. Instead, to preserve user input formatting, we use
    // byte offsets to return the input as it was received.
    fn slice(s: &String, lo: BytePos, hi: BytePos) -> String {
        s.as_slice().slice(lo.0 as uint, hi.0 as uint).to_string()
    }

    let code = code.to_string();

    let res = task.try(move || {
        let mut input = Input::new();
        let handler = mk_handler(box ErrorEmitter::new(tx, filter));
        let mut sess = new_parse_sess();

        sess.span_diagnostic.handler = handler;

        let mut p = filemap_to_parser(&sess,
            string_to_filemap(&sess, code.to_string(), "<input>".to_string()),
            vec![]);

        // Whether the last statement is an expression without a semicolon
        let mut last_expr = false;

        while p.token != token::Eof {
            if let token::DocComment(_) = p.token {
                p.bump();
                continue;
            }

            let lo = p.span.lo;

            let attrs = if p.token == token::Pound {
                if p.look_ahead(1, |t| *t == token::Not) {
                    let _ = p.parse_attribute(true);
                    input.attributes.push(slice(&code, lo, p.last_span.hi));
                    continue;
                }

                p.parse_outer_attributes()
            } else {
                vec![]
            };

            if p.token == token::Eof {
                sess.span_diagnostic.handler.fatal("expected item after attributes");
            }

            let is_view_item = if p.token.is_keyword(keywords::Use) {
                true
            } else if p.token.is_keyword(keywords::Extern) {
                p.look_ahead(1, |t| t.is_keyword(keywords::Crate))
            } else if p.token.is_keyword(keywords::Pub) {
                p.look_ahead(1, |t| t.is_keyword(keywords::Use))
            } else {
                false
            };

            if is_view_item {
                let vitem = p.parse_view_item(attrs);

                let vi_ty = match vitem.node {
                    ViewItemExternCrate(..) => ExternCrate,
                    ViewItemUse(..) => Use,
                };

                let hi = p.last_span.hi;

                input.view_items.push((vi_ty, slice(&code, lo, hi)));
            } else {
                let stmt = p.parse_stmt(attrs);

                debug!("parsed stmt: {}", pprust::stmt_to_string(&*stmt));

                last_expr = match stmt.node {
                    StmtExpr(..) => true,
                    StmtMac(_, sem) => !sem,
                    _ => false,
                };

                // Semicolons are not always consumed automatically
                if p.eat(&token::Semi) {
                    last_expr = false;
                }

                let dest = match stmt.node {
                    StmtDecl(ref decl, _) => {
                        match decl.node {
                            DeclLocal(..) => &mut input.statements,
                            _ => &mut input.items,
                        }
                    },
                    _ => &mut input.statements,
                };

                dest.push(slice(&code, lo, p.last_span.hi));
            }
        }

        input.last_expr = last_expr;

        input
    });

    match res {
        Ok(input) => Program(input),
        Err(_) => {
            if rx.iter().any(|fatal| fatal) {
                ParseError(None)
            } else {
                More
            }
        }
    }
}

/// Filters error messages and reports to a channel
struct ErrorEmitter {
    /// Sends true for fatal errors; false for `More` errors
    errors: Sender<bool>,
    emitter: EmitterWriter,
    filter: bool,
}

impl ErrorEmitter {
    /// Constructs a new `ErrorEmitter` which will report fatal-ness of errors
    /// to the given channel and emit non-fatal error messages to `stderr`.
    /// If `filter` is false, all errors are considered fatal.
    fn new(tx: Sender<bool>, filter: bool) -> ErrorEmitter {
        ErrorEmitter{
            errors: tx,
            emitter: EmitterWriter::stderr(Auto,
                Some(Registry::new(&rustc::DIAGNOSTICS))),
            filter: filter,
        }
    }
}

impl Emitter for ErrorEmitter {
    fn emit(&mut self, cmsp: Option<(&CodeMap, Span)>, msg: &str,
            code: Option<&str>, lvl: Level) {
        if !self.filter {
            self.emitter.emit(cmsp, msg, code, lvl);
            self.errors.send(true);
            return;
        }

        match lvl {
            Bug | Fatal | Error => {
                if msg.contains("un-closed delimiter") ||
                        msg.contains("expected item after attributes") ||
                        msg.contains("unterminated block comment") ||
                        msg.contains("unterminated double quote string") ||
                        msg.contains("unterminated raw string") {
                    self.errors.send(false);
                } else {
                    self.emitter.emit(cmsp, msg, code, lvl);
                    self.errors.send(true);
                    // Send any "help" messages that may follow
                    self.filter = false;
                }
            }
            _ => ()
        }
    }

    fn custom_emit(&mut self, _cm: &CodeMap, _sp: RenderSpan,
            _msg: &str, _lvl: Level) {
        panic!("ErrorEmitter does not implement custom_emit");
    }
}
