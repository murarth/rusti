// Copyright 2014-2016 Rusti Project
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Parsing REPL input statements, including Rust code and `rusti` commands.

use std::borrow::Cow;
use std::borrow::Cow::*;
use std::fs::File;
use std::io::{self, stdin, BufRead, BufReader};
use std::mem::swap;
use std::path::PathBuf;
use std::rc::Rc;
use std::sync::{Arc, Mutex};
use std::sync::mpsc::channel;
use std::thread::Builder;

use syntax::ast::{ItemKind, MacStmtStyle, StmtKind};
use syntax::codemap::{BytePos, CodeMap};
use syntax::errors::{ColorConfig, DiagnosticBuilder, Handler};
use syntax::errors::emitter::{Emitter, EmitterWriter};
use syntax::errors::snippet::FormatMode;
use syntax::errors::Level::*;
use syntax::parse::{classify, token};
use syntax::parse::{filemap_to_parser, ParseSess};

use linefeed::{Reader, ReadResult};
use linefeed::terminal::DefaultTerminal;

use completion::Completer;
use repl::{lookup_command, CmdArgs};

use self::InputResult::*;

pub struct FileReader {
    reader: BufReader<File>,
    path: PathBuf,
    buffer: String,
}

impl FileReader {
    pub fn new(f: File, path: PathBuf) -> FileReader {
        FileReader{
            reader: BufReader::new(f),
            path: path,
            buffer: String::new(),
        }
    }

    pub fn read_input(&mut self) -> InputResult {
        let mut buf = String::new();

        loop {
            let mut line = String::new();

            match self.read_line(&mut line) {
                Ok(0) => break,
                Ok(_) => (),
                Err(e) => return InputError(Some(Owned(format!("{}", e)))),
            };

            if is_command(&line) {
                if buf.is_empty() {
                    return parse_command(&line, true);
                } else {
                    self.buffer = line;
                    break;
                }
            } else {
                buf.push_str(&line);
            }
        }

        if !buf.is_empty() {
            parse_program(&buf, false,
                self.path.as_os_str().to_str())
        } else {
            Eof
        }
    }

    fn read_line(&mut self, buf: &mut String) -> io::Result<usize> {
        if self.buffer.is_empty() {
            self.reader.read_line(buf)
        } else {
            swap(buf, &mut self.buffer);
            Ok(buf.len())
        }
    }
}

/// Reads input from `stdin`
pub struct InputReader {
    buffer: String,
    reader: Option<Reader<DefaultTerminal>>,
}

impl InputReader {
    /// Constructs a new `InputReader` reading from `stdin`.
    pub fn new() -> InputReader {
        let r = match Reader::new("rusti") {
            Ok(mut r) => {
                r.set_completer(Rc::new(Completer));
                r.set_completion_append_character(None);
                r.set_word_break_chars(" \t\n!\"#$%&'()*+,-./:;<=>?@[\\]^`");
                Some(r)
            }
            Err(_) => None
        };

        InputReader{
            buffer: String::new(),
            reader: r,
        }
    }

    /// Reads a single command, item, or statement from `stdin`.
    /// Returns `More` if further input is required for a complete result.
    /// In this case, the input received so far is buffered internally.
    pub fn read_input(&mut self, prompt: &str) -> InputResult {
        let line = match self.read_line(prompt) {
            ReadResult::Eof => return Eof,
            ReadResult::Input(s) => s,
            ReadResult::Signal(_) => {
                self.buffer.clear();
                return Empty;
            }
        };

        self.buffer.push_str(&line);

        if self.buffer.is_empty() {
            return Empty;
        }

        self.add_history(&line);

        let res = if is_command(&self.buffer) {
            parse_command(&self.buffer, true)
        } else {
            self.buffer.push('\n');
            parse_program(&self.buffer, true, None)
        };

        match res {
            More => (),
            _ => self.buffer.clear(),
        };

        res
    }

    /// Returns whether the `InputReader` is reading from a TTY.
    pub fn is_tty(&self) -> bool {
        self.reader.is_some()
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
            let line = match self.read_line(prompt) {
                ReadResult::Eof => return Eof,
                ReadResult::Input(s) => s,
                ReadResult::Signal(_) => {
                    self.buffer.clear();
                    return Empty;
                }
            };

            if !line.is_empty() {
                self.add_history(&line);
            }

            if line == ".q" || line == ":q" {
                self.buffer.clear();
                return Empty;
            } else if line == "." {
                return parse_program(&buf, true, None);
            }

            buf.push_str(&line);
            buf.push('\n');
        }
    }

    fn read_line(&mut self, prompt: &str) -> ReadResult {
        match self.reader {
            Some(ref mut r) => {
                r.set_prompt(prompt);

                r.read_line().ok().unwrap_or(ReadResult::Eof)
            }
            None => self.read_stdin()
        }
    }

    fn read_stdin(&self) -> ReadResult {
        let mut s = String::new();

        match stdin().read_line(&mut s) {
            Ok(0) | Err(_) => ReadResult::Eof,
            Ok(_) => ReadResult::Input(s)
        }
    }

    fn add_history(&mut self, line: &str) {
        if let Some(ref mut r) = self.reader {
            r.add_history(line.to_owned());
        }
    }
}

/// Possible results from reading input from `InputReader`
#[derive(Clone, Debug)]
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
    InputError(Option<Cow<'static, str>>),
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum ErrorState {
    Success,
    Fatal,
    NonFatal,
}

/// Represents an input program
#[derive(Clone, Debug)]
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
pub fn parse_command(line: &str, filter: bool) -> InputResult {
    debug!("parse_command: {:?}", line);
    if !is_command(line) {
        return InputError(Some(Borrowed("command must begin with `.` or `:`")));
    }

    let line = &line[1..];
    let mut words = line.trim_right().splitn(2, ' ');

    let name = match words.next() {
        Some(name) if !name.is_empty() => name,
        _ => return InputError(Some(Borrowed("expected command name"))),
    };

    let cmd = match lookup_command(name) {
        Some(cmd) => cmd,
        None => return InputError(Some(Owned(
            format!("unrecognized command: {}", name))))
    };

    let args = words.next();

    match cmd.accepts {
        CmdArgs::Nothing if args.is_some() => InputError(
            Some(Owned(format!("command `{}` takes no arguments", cmd.name)))),
        CmdArgs::Expr if args.is_none() => InputError(
            Some(Owned(format!("command `{}` expects an expression", cmd.name)))),
        CmdArgs::Expr => {
            let args = args.unwrap();
            match parse_program(args, filter, None) {
                Program(_) => Command(name.to_owned(), Some(args.to_owned())),
                i => i,
            }
        }
        _ => Command(name.to_owned(), args.map(|s| s.to_owned()))
    }
}

macro_rules! try_parse {
    ( $e:expr ) => {
        match $e {
            Ok(t) => t,
            Err(mut diag) => {
                diag.emit();
                return;
            }
        }
    }
}

/// Parses a line of input as a program.
///
/// If there are parse errors, they will be printed to `stderr`.
/// If `filter` is true, certain errors that indicate an incomplete input
/// will result in a value of `More`. Otherwise, these errors will be emitted
/// and `InputError` will be returned.
pub fn parse_program(code: &str, filter: bool, filename: Option<&str>) -> InputResult {
    let err = Arc::new(Mutex::new(ErrorState::Success));
    let (tx, rx) = channel();

    let task = Builder::new().name("parse_program".to_owned());

    // Items are not returned in data structures; nor are they converted back
    // into strings. Instead, to preserve user input formatting, we use
    // byte offsets to return the input as it was received.
    fn slice(s: &str, lo: BytePos, hi: BytePos) -> String {
        s[lo.0 as usize .. hi.0 as usize].to_owned()
    }

    let code = code.to_owned();
    let filename = filename.unwrap_or("<input>").to_owned();
    let em_err = err.clone();

    let handle = task.spawn(move || {
        if !log_enabled!(::log::LogLevel::Debug) {
            io::set_panic(Box::new(io::sink()));
        }
        let mut input = Input::new();
        let cm = Rc::new(CodeMap::new());
        let handler = Handler::with_emitter(false, false,
            Box::new(ErrorEmitter::new(cm.clone(), em_err.clone(), filter)));
        let sess = ParseSess::with_span_handler(handler, cm);

        let mut p = filemap_to_parser(&sess,
            sess.codemap().new_filemap(filename, None, code.clone()),
            Vec::new());

        // Whether the last statement is an expression without a semicolon
        let mut last_expr = false;

        while p.token != token::Eof {
            if let token::DocComment(_) = p.token {
                p.bump();
                continue;
            }

            let lo = p.span.lo;

            if p.token == token::Pound {
                if p.look_ahead(1, |t| *t == token::Not) {
                    try_parse!(p.parse_attribute(true));
                    input.attributes.push(slice(&code, lo, p.last_span.hi));
                    continue;
                }
            }

            let stmt = match try_parse!(p.parse_stmt()) {
                None => break,
                Some(stmt) => stmt,
            };

            {
                let lock = em_err.lock().unwrap();

                if *lock != ErrorState::Success {
                    return;
                }
            }

            let mut hi = None;

            last_expr = match stmt.node {
                StmtKind::Expr(ref e) => {
                    if classify::expr_requires_semi_to_be_stmt(&**e) {
                        try_parse!(p.expect_one_of(&[], &[token::Semi, token::Eof]));
                    }
                    !p.eat(&token::Semi)
                }
                StmtKind::Mac(ref mac) if mac.1 == MacStmtStyle::NoBraces => {
                    try_parse!(p.expect_one_of(&[], &[token::Semi, token::Eof]));
                    !p.eat(&token::Semi)
                }
                StmtKind::Mac(_) => false,
                StmtKind::Local(_) => {
                    try_parse!(p.expect(&token::Semi));
                    false
                }
                StmtKind::Item(_) => {
                    // Consume the semicolon if there is one,
                    // but don't add it to the item
                    hi = Some(p.last_span.hi);
                    p.eat(&token::Semi);
                    false
                }
                _ => false
            };

            let dest = match stmt.node {
                StmtKind::Local(..) => &mut input.statements,
                StmtKind::Item(ref item) => {
                    match item.node {
                        ItemKind::ExternCrate(..) | ItemKind::Use(..) =>
                            &mut input.view_items,
                        _ => &mut input.items,
                    }
                },
                StmtKind::Mac(ref mac) if mac.1 == MacStmtStyle::Braces =>
                    &mut input.items,
                _ => &mut input.statements,
            };

            dest.push(slice(&code, lo, hi.unwrap_or(p.last_span.hi)));
        }

        input.last_expr = last_expr;

        tx.send(input).unwrap();
    }).unwrap();

    let _ = handle.join();

    let lock = err.lock().unwrap();

    match *lock {
        ErrorState::Success => Program(rx.recv().unwrap()),
        ErrorState::Fatal => InputError(None),
        ErrorState::NonFatal => More
    }
}

/// Filters error messages and reports to a channel
struct ErrorEmitter {
    error: Arc<Mutex<ErrorState>>,
    emitter: EmitterWriter,
    filter: bool,
}

impl ErrorEmitter {
    /// Constructs a new `ErrorEmitter` which will report fatal-ness of errors
    /// to the given channel and emit non-fatal error messages to `stderr`.
    /// If `filter` is false, all errors are considered fatal.
    fn new(cm: Rc<CodeMap>, err: Arc<Mutex<ErrorState>>, filter: bool) -> ErrorEmitter {
        ErrorEmitter{
            error: err,
            emitter: EmitterWriter::stderr(ColorConfig::Auto, None, Some(cm),
                FormatMode::NewErrorFormat),
            filter: filter,
        }
    }

    fn set_error(&self, err: ErrorState) {
        *self.error.lock().unwrap() = err;
    }
}

impl Emitter for ErrorEmitter {
    fn emit(&mut self, db: &DiagnosticBuilder) {
        if !self.filter {
            self.emitter.emit(db);
            self.set_error(ErrorState::Fatal);
            return;
        }

        match db.level() {
            Bug | Fatal | Error => {
                if is_non_fatal(db.message()) {
                    self.set_error(ErrorState::NonFatal);
                } else {
                    self.emitter.emit(db);
                    self.set_error(ErrorState::Fatal);
                    // Send any "help" messages that may follow
                    self.filter = false;
                }
            }
            _ => ()
        }
    }
}

fn is_non_fatal(msg: &str) -> bool {
    msg.contains("un-closed delimiter") ||
        msg.contains("expected item after attributes") ||
        msg.contains("unterminated block comment") ||
        msg.contains("unterminated block doc-comment") ||
        msg.contains("unterminated double quote string") ||
        msg.contains("unterminated double quote byte string") ||
        msg.contains("unterminated raw string")
}

#[cfg(test)]
mod test {
    use super::{InputResult, parse_program};

    fn parse(s: &str) -> InputResult {
        parse_program(s, true, None)
    }

    #[test]
    fn test_unclosed_delimiter() {
        assert_matches!(parse("fn foo() {"), InputResult::More);
        assert_matches!(parse("("), InputResult::More);
        assert_matches!(parse("{"), InputResult::More);
        assert_matches!(parse("let a = ("), InputResult::More);
        assert_matches!(parse("let a = {"), InputResult::More);
        assert_matches!(parse("let a = foo("), InputResult::More);
        assert_matches!(parse("let a = \""), InputResult::More);
    }
}
