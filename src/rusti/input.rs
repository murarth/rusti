// Copyright 2014 Murarth
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Parsing REPL input statements, including Rust code and `rusti` commands.

use std::borrow::Cow::*;
use std::old_io::{BufferedReader, EndOfFile, File, IoResult, stderr};
use std::old_io::util::NullWriter;
use std::mem::swap;
use std::string::CowString;
use std::sync::mpsc::{channel, Sender};
use std::thread::Builder;

use super::rustc;

use super::syntax::ast::Decl_::*;
use super::syntax::ast::Item_::*;
use super::syntax::ast::MacStmtStyle::*;
use super::syntax::ast::Stmt_::*;
use super::syntax::codemap::{BytePos, CodeMap, Span};
use super::syntax::diagnostic::{Auto, Emitter, EmitterWriter};
use super::syntax::diagnostic::{Level, RenderSpan, mk_handler};
use super::syntax::diagnostic::Level::*;
use super::syntax::diagnostics::registry::Registry;
use super::syntax::parse::classify;
use super::syntax::parse::{new_parse_sess, string_to_filemap, filemap_to_parser};
use super::syntax::parse::attr::ParserAttr;
use super::syntax::parse::token;

use super::readline;

pub use self::InputResult::*;

pub struct FileReader {
    reader: BufferedReader<File>,
    buffer: String,
}

impl FileReader {
    pub fn new(f: File) -> FileReader {
        FileReader{
            reader: BufferedReader::new(f),
            buffer: String::new(),
        }
    }

    pub fn read_input(&mut self) -> InputResult {
        let mut buf = String::new();

        loop {
            let mut line = match self.read_line() {
                Ok(line) => line,
                Err(ref e) if e.kind == EndOfFile => break,
                Err(e) => return InputError(Some(Owned(format!("{}", e)))),
            };

            if is_command(&line[]) {
                if buf.is_empty() {
                    truncate_newline(&mut line);
                    return parse_command(&line[]);
                } else {
                    self.buffer = line;
                    break;
                }
            } else {
                buf.push_str(&line[]);
            }
        }

        if !buf.is_empty() {
            parse_program(&buf[], false,
                self.reader.get_ref().path().as_str())
        } else {
            Eof
        }
    }

    fn read_line(&mut self) -> IoResult<String> {
        if self.buffer.is_empty() {
            self.reader.read_line()
        } else {
            let mut buf = String::new();
            swap(&mut buf, &mut self.buffer);
            Ok(buf)
        }
    }
}

fn truncate_newline(s: &mut String) {
    if s.ends_with("\n") {
        let n = s.len() - 1;
        s.truncate(n);
    }
}

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

        self.buffer.push_str(&line[]);

        if self.buffer.is_empty() {
            return Empty;
        }

        readline::push_history(&line[]);

        let res = if is_command(&self.buffer[]) {
            parse_command(&self.buffer[])
        } else {
            self.buffer.push('\n');
            parse_program(&self.buffer[], true, None)
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
                readline::push_history(&line[]);
            }

            if line == ".q" || line == ":q" {
                return Empty;
            } else if line == "." {
                return parse_program(&buf[], true, None);
            }

            buf.push_str(&line[]);
            buf.push('\n');
        }
    }
}

/// Possible results from reading input from `InputReader`
#[derive(Debug)]
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
    InputError(Option<CowString<'static>>),
}

/// Represents an input program
#[derive(Debug)]
pub struct Input {
    /// Module attributes
    pub attributes: Vec<String>,
    /// Module-level view items (`use`, `extern crate`)
    pub view_items: Vec<String>,
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

pub fn is_command(line: &str) -> bool {
    (line.starts_with(".") && !line.starts_with("..")) ||
        (line.starts_with(":") && !line.starts_with("::"))
}

/// Parses a line of input as a command.
/// Returns either a `Command` value or an `InputError` value.
pub fn parse_command(line: &str) -> InputResult {
    if !is_command(line) {
        return InputError(Some(Borrowed("command must begin with `.` or `:`")));
    }

    let line = &line[1..];
    let mut words = line.trim_right_matches(' ').splitn(1, ' ');

    let cmd = match words.next() {
        Some(cmd) if !cmd.is_empty() => cmd.to_string(),
        _ => return InputError(Some(Borrowed("expected command name"))),
    };

    let args = words.next().map(|s| s.to_string());

    Command(cmd, args)
}

/// Parses a line of input.
pub fn parse_input(line: &str) -> InputResult {
    if is_command(line) {
        parse_command(line)
    } else {
        parse_program(line, false, None)
    }
}

/// Parses a line of input as a program.
///
/// If there are parse errors, they will be printed to `stderr`.
/// If `filter` is true, certain errors that indicate an incomplete input
/// will result in a value of `More`. Otherwise, these errors will be emitted
/// and `InputError` will be returned.
pub fn parse_program(code: &str, filter: bool, filename: Option<&str>) -> InputResult {
    let (tx, rx) = channel();

    let task = Builder::new().stderr(Box::new(NullWriter));

    // Items are not returned in data structures; nor are they converted back
    // into strings. Instead, to preserve user input formatting, we use
    // byte offsets to return the input as it was received.
    fn slice(s: &String, lo: BytePos, hi: BytePos) -> String {
        s[lo.0 as usize .. hi.0 as usize].to_string()
    }

    let code = code.to_string();
    let filename = filename.unwrap_or("<input>").to_string();

    let res = task.scoped(move || {
        let mut input = Input::new();
        let handler = mk_handler(false, Box::new(ErrorEmitter::new(tx, filter)));
        let mut sess = new_parse_sess();

        sess.span_diagnostic.handler = handler;

        let mut p = filemap_to_parser(&sess,
            string_to_filemap(&sess, code.to_string(), filename),
            Vec::new());

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

            let stmt = p.parse_stmt(attrs);

            let mut hi = None;

            last_expr = match stmt.node {
                StmtExpr(ref e, _) => {
                    if classify::expr_requires_semi_to_be_stmt(&**e) {
                        p.commit_stmt(&[], &[token::Semi, token::Eof]);
                    }
                    !p.eat(&token::Semi)
                }
                StmtMac(_, MacStmtWithoutBraces) => {
                    p.expect_one_of(&[], &[token::Semi, token::Eof]);
                    !p.eat(&token::Semi)
                }
                StmtMac(_, _) => false,
                StmtDecl(ref decl, _) => {
                    if let DeclLocal(_) = decl.node {
                        p.expect(&token::Semi);
                    } else {
                        // Consume the semicolon if there is one,
                        // but don't add it to the item
                        hi = Some(p.last_span.hi);
                        p.eat(&token::Semi);
                    }
                    false
                }
                _ => false
            };

            let dest = match stmt.node {
                StmtDecl(ref decl, _) => {
                    match decl.node {
                        DeclLocal(..) => &mut input.statements,
                        DeclItem(ref item) => {
                            match item.node {
                                ItemExternCrate(..) | ItemUse(..) =>
                                    &mut input.view_items,
                                _ => &mut input.items,
                            }
                        }
                    }
                },
                StmtMac(_, MacStmtWithBraces) => &mut input.items,
                _ => &mut input.statements,
            };

            dest.push(slice(&code, lo, hi.unwrap_or(p.last_span.hi)));
        }

        input.last_expr = last_expr;

        input
    }).join();

    match res {
        Ok(input) => Program(input),
        Err(_) => {
            if rx.iter().any(|fatal| fatal) {
                InputError(None)
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
                Some(Registry::new(&rustc::diagnostics::DIAGNOSTICS))),
            filter: filter,
        }
    }
}

impl Emitter for ErrorEmitter {
    fn emit(&mut self, cmsp: Option<(&CodeMap, Span)>, msg: &str,
            code: Option<&str>, lvl: Level) {
        if !self.filter {
            self.emitter.emit(cmsp, msg, code, lvl);
            self.errors.send(true).unwrap();
            return;
        }

        match lvl {
            Bug | Fatal | Error => {
                if msg.contains("un-closed delimiter") ||
                        msg.contains("expected item after attributes") ||
                        msg.contains("unterminated block comment") ||
                        msg.contains("unterminated block doc-comment") ||
                        msg.contains("unterminated double quote string") ||
                        msg.contains("unterminated double quote byte string") ||
                        msg.contains("unterminated raw string") {
                    self.errors.send(false).unwrap();
                } else {
                    self.emitter.emit(cmsp, msg, code, lvl);
                    self.errors.send(true).unwrap();
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
