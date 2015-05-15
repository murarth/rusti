// Copyright 2014-2015 Rusti Project
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Provides racer-based completion

// TODO: use a Cargo feature for this?

use std::io::Write;
use std::process::Command;

use tempfile::NamedTempFile;

/// Runs racer to provide code completion on the given input.
///
/// Returns the common prefix of all completions and the list of matched completions.
pub fn complete(text: &str, _start: usize, end: usize)
        -> Option<(String, Vec<String>)> {
    // don't actually attempt to search when the input is empty (it doesn't work).
    let text = text.trim();
    if text.is_empty() {
        return None;
    }

    debug!("completion input: {:?}", text);

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
                let name = match rest.split(',').next() {
                    // Remove item's prefix
                    Some(name) => &name[prefix_len..],
                    None => {
                        warn!("invalid MATCH value: {:?}", rest);
                        return None;
                    }
                };

                debug!("completion: {:?}", name);
                completions.push(name.to_string());
            }
            _ => warn!("unexpected racer output: {:?}", line)
        }
    }

    if completions.is_empty() {
        None
    } else {
        // find the longest common prefix of all completions
        let mut prefix: String = completions[0].clone();

        for c in completions[1..].iter() {
            while !c.starts_with(&prefix) {
                prefix.pop();
            }

            if prefix.is_empty() {
                break;
            }
        }

        debug!("prefix: {:?}", prefix);

        Some((prefix, completions))
    }
}
