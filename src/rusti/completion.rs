// Copyright 2014-2016 Rusti Project
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Provides text completion for user input.

use std::io::Write;
use std::iter::repeat;
use std::process::Command;

use linefeed::{self, Completion, Reader, Terminal};
use linefeed::complete::complete_path;

use tempfile::NamedTempFile;

use input::is_command;
use repl::{lookup_command, search_command, CmdArgs};

pub struct Completer;

impl<Term: Terminal> linefeed::Completer<Term> for Completer {
    fn complete(&self, _word: &str, reader: &Reader<Term>,
            start: usize, end: usize) -> Option<Vec<Completion>> {
        let is_whitespace = reader.buffer()[..start]
            .chars().all(|ch| ch.is_whitespace());

        if is_whitespace && start == end {
            // Indent when there's no word to complete
            let n = 4 - start % 4;

            Some(vec![Completion::simple(repeat(' ').take(n).collect())])
        } else {
            complete(reader.buffer(), end)
        }
    }
}

/// Returns a series of possible completions for the given input.
/// Input text may be a word or the entire line buffer.
/// `end` indicates the position of the cursor.
/// This typically one past the end of the word, in bytes.
///
/// If no matches are found, returns `None`.
pub fn complete(text: &str, end: usize) -> Option<Vec<Completion>> {
    debug!("completion input: text={:?} end={:?}", text, end);

    // Don't attempt to complete when the input is empty
    if text.chars().all(|c| c.is_whitespace()) {
        return None;
    }

    if is_command(text) {
        let line = &text[0..end];

        // If there's a space before the end of input,
        // try to complete the command's arguments.
        if line.chars().any(|c| c.is_whitespace()) {
            let mut words = line[1..].splitn(2, ' ');

            let name = words.next().unwrap_or("");

            let accepts = match lookup_command(name) {
                Some(cmd) => cmd.accepts,
                None => return None
            };

            let args = words.next().map(|s| s.trim_left());

            match accepts {
                CmdArgs::Expr => args.and_then(|arg| complete_code(arg, arg.len())),
                CmdArgs::Filename => Some(complete_path(args.unwrap_or(""))),
                _ => None
            }
        } else {
            // Complete command name
            let mut names = Vec::new();

            search_command(&line[1..],
                |cmd| names.push(Completion{
                    completion: cmd.name.to_owned(),
                    display: None,
                    suffix: Some(' '),
                }));

            if names.is_empty() {
                None
            } else {
                Some(names)
            }
        }
    } else {
        complete_code(text, end)
    }
}

/// Performs completion for Rust code.
fn complete_code(text: &str, end: usize) -> Option<Vec<Completion>> {
    let mut file = NamedTempFile::new().unwrap();
    file.write_all(text.as_bytes()).unwrap();
    file.write_all(b"\n").unwrap();

    let result = Command::new("racer").arg("complete").arg("1")
        .arg(format!("{}", end)).arg(file.path()).output();

    if let Err(e) = result {
        warn!("error: couldn't invoke racer: {:?}", e);
        return None;
    }

    let res_string = String::from_utf8(result.unwrap().stdout).unwrap();
    let mut lines = res_string.lines();
    let mut completions = vec![];

    // Skip the "PREFIX" line from racer output
    lines.next();

    for line in lines {
        let (restype, rest) = {
            match line.find(' ') {
                Some(pos) => (&line[..pos], &line[pos + 1..]),
                None => (line, "")
            }
        };

        match restype {
            "MATCH" => {
                debug!("MATCH line: {:?}", rest);
                let mut fields = rest.split(',');

                let mut name = match fields.next() {
                    Some(name) => name.to_owned(),
                    None => {
                        warn!("missing name in MATCH value: {:?}", rest);
                        return None;
                    }
                };

                let mut display = None;

                let mtype = match fields.nth(3) {
                    Some(ty) => ty,
                    None => {
                        warn!("missing type in MATCH value: {:?}", rest);
                        return None;
                    }
                };

                let suffix = match mtype {
                    "Crate" | "Module" => Some("::"),
                    "Function" => Some("("),
                    _ => None
                };

                if let Some(suffix) = suffix {
                    display = Some(name.clone());
                    name.push_str(suffix);
                }

                debug!("completion: {:?}", name);
                completions.push(Completion{
                    completion: name,
                    display: display,
                    suffix: None,
                });
            }
            "END" => break,
            _ => warn!("unexpected racer command: {:?}", line)
        }
    }

    if completions.is_empty() {
        None
    } else {
        Some(completions)
    }
}
