// Copyright 2014-2015 Rusti Project
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Provides text completion for user input.

use std::fs::read_dir;
use std::io::{self, Write};
use std::path::is_separator;
use std::process::Command;

use tempfile::NamedTempFile;

use input::is_command;
use repl::{lookup_command, search_command, CmdArgs};

/// Returns a series of possible completions for the given input.
/// Input text may be a word or the entire line buffer.
/// `end` indicates the position of the cursor.
/// This typically one past the end of the word, in bytes.
///
/// Results are returned as the (possibly empty) common prefix of all matches
/// and the series of suffixes to the existing portion of the word.
///
/// If no matches are found, returns `None`.
pub fn complete(text: &str, end: usize) -> Option<(String, Vec<String>)> {
    debug!("completion input: text={:?} end={:?}", text, end);

    // Don't attempt to complete when the input is empty
    if text.chars().all(|c| c.is_whitespace()) {
        return None;
    }

    let completions = if is_command(text) {
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
                CmdArgs::Filename => complete_filename(args.unwrap_or("")),
                _ => None
            }
        } else {
            // Complete command name
            let mut names = Vec::new();

            let prefix_len = line.len() - 1;

            search_command(&line[1..],
                |cmd| names.push(cmd.name[prefix_len..].to_owned()));

            if names.is_empty() {
                None
            } else {
                Some(names)
            }
        }
    } else {
        complete_code(text, end)
    };

    completions.and_then(|c| {
        if c.is_empty() {
            None
        } else {
            Some((common_prefix(&c), c))
        }
    })
}

/// Performs completion for Rust code.
fn complete_code(text: &str, end: usize) -> Option<Vec<String>> {
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

    // read the prefix length from the first line of output.
    // used to remove the prefix from the completions.
    let prefix_len = {
        let prefix_line = match lines.next() {
            Some(l) => l,
            None => {
                warn!("unexpected racer output: {:?}", res_string);
                return None;
            },
        };

        let prefix_parts: Vec<_> = prefix_line.splitn(2, ' ').collect();
        if prefix_parts.len() != 2 || prefix_parts[0] != "PREFIX" {
            warn!("invalid PREFIX line: {:?}", res_string);
            return None;
        }

        let args: Vec<_> = prefix_parts[1].splitn(3, ',').collect();
        if args.len() != 3 {
            warn!("invalid PREFIX value: {:?}", prefix_line);
            return None;
        }

        let (start, end) = match (args[0].parse::<usize>(), args[1].parse::<usize>()) {
            (Ok(start), Ok(end)) if start <= end => (start, end),
            _ => {
                warn!("invalid PREFIX values: {:?}", prefix_line);
                return None;
            }
        };

        end - start
    };

    for line in lines {
        let (restype, rest) = {
            let vec: Vec<_> = line.splitn(2, ' ').collect();
            if vec.len() != 2 {
                warn!("unexpected racer output: {:?}", line);
                return None;
            }
            (vec[0], vec[1])
        };

        match restype {
            "MATCH" => {
                debug!("MATCH line: {:?}", rest);
                let mut fields = rest.split(',');

                let mut name = match fields.next() {
                    // Remove item's prefix
                    Some(name) => name[prefix_len..].to_string(),
                    None => {
                        warn!("missing name in MATCH value: {:?}", rest);
                        return None;
                    }
                };

                let mtype = match fields.nth(3) {
                    Some(ty) => ty,
                    None => {
                        warn!("missing type in MATCH value: {:?}", rest);
                        return None;
                    }
                };

                match mtype {
                    "Crate" | "Module" => name.push_str("::"),
                    "Function" => name.push('('),
                    _ => ()
                }

                debug!("completion: {:?}", name);
                completions.push(name);
            }
            _ => warn!("unexpected racer output: {:?}", line)
        }
    }

    if completions.is_empty() {
        None
    } else {
        Some(completions)
    }
}

/// Returns a set of possible filename completions.
fn complete_filename(name: &str) -> Option<Vec<String>> {
    if name.is_empty() {
        list_dir("./", "").ok()
    } else {
        let (path, prefix) = match name.rfind(|c| is_separator(c)) {
            Some(pos) => (&name[..pos + 1], &name[pos + 1..]),
            None => ("./", name)
        };

        list_dir(path, prefix).ok()
    }
}

/// Returns a series of completion suffixes for files in the given directory
/// and beginning with the given prefix.
fn list_dir(path: &str, prefix: &str) -> io::Result<Vec<String>> {
    let mut names = Vec::new();
    let path_len = path.len();
    let prefix_len = prefix.len();

    for ent in try!(read_dir(path)) {
        let ent = try!(ent);
        let path = ent.path();
        let name = match path.to_str() {
            Some(name) => &name[path_len..],
            None => continue,
        };

        if !name.starts_with(prefix) {
            continue;
        }

        let mut name = name[prefix_len..].to_owned();

        let ty = try!(ent.file_type());

        if ty.is_dir() {
            name.push('/');
        }

        names.push(name);
    }

    debug!("list_dir({:?}, {:?}) -> {:?}", path, prefix, names);
    Ok(names)
}

/// Returns the (possibly empty) common prefix of the given strings.
/// Input strings must be non-empty.
fn common_prefix(strs: &[String]) -> String {
    assert!(!strs.is_empty());

    let mut prefix: String = strs[0].clone();

    for c in strs[1..].iter() {
        while !c.starts_with(&prefix) {
            prefix.pop();
        }

        if prefix.is_empty() {
            break;
        }
    }

    prefix
}
