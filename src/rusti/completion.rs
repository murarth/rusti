// Copyright 2014 Murarth
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
pub fn complete(text: &str, _start: usize, _end: usize) -> (String, Vec<String>) {
    // don't actually attempt to search when the input is empty (it doesn't work).
    let text = text.trim();
    if text == "" { return (String::new(), vec![]); }

    let mut file = NamedTempFile::new().unwrap();
    file.write_all(text.as_bytes()).unwrap();
    file.write_fmt(format_args!("\n")).unwrap();

    let result = Command::new("racer").arg("complete").arg("1")
        .arg(format!("{}", text.len())).arg(file.path().to_str().unwrap()).output();

    if let Err(e) = result {
        warn!("error: couldn't invoke racer: {:?}", e);
        return (String::new(), vec![]);
    }

    let res_string = String::from_utf8(result.unwrap().stdout).unwrap();
    let mut lines = res_string.lines();
    let mut completions = vec![];

    // read the prefix length from the first line of output. used to remove the prefix from the
    // completions.
    let prefixlen = {
        let prefix_line = match lines.next() {
            Some(l) => l,
            None => {
                warn!("error: unexpected racer output: {}", res_string);
                return (String::new(), vec![]);
            },
        };

        let prefix_parts: Vec<_> = prefix_line.splitn(2, " ").collect();
        if prefix_parts[0] != "PREFIX" {
            warn!("error: unexpected racer output: {}", res_string);
            return (String::new(), vec![]);
        }

        let args: Vec<_> = prefix_parts[1].splitn(3, ",").collect();
        if args.len() != 3 {
            warn!("error: unexpected racer output: {}", res_string);
            return (String::new(), vec![]);
        }

        let (start, end, _pre): (usize, usize, &str) = (args[0].parse().unwrap(), args[1].parse().unwrap(), args[2]);

        end - start
    };

    for line in lines {
        let (restype, rest) = {
            let vec: Vec<_> = line.splitn(2, " ").collect();
            (vec[0], vec[1])
        };

        match restype {
            "MATCH" => {
                let (name, _decl_line, _decl_col, _file, _kind, _decl) = {
                    let vec: Vec<_> = rest.split(",").collect();
                    (vec[0], vec[1], vec[2], vec[3], vec[4], vec[5])
                };

                // remove item's prefix and append to input. yes, this means completion only works
                // at the end of the input.
                let mut name = name.to_string();
                for _ in 0..prefixlen {
                    name.remove(0);
                }

                let completion = String::from_str(name.as_ref());
                debug!("completion: {}", completion);
                completions.push(completion);
            }
            _ => warn!("unexpected racer output: {}", line)
        }
    }

    // find the longest common prefix of all completions
    if completions.len() > 0 {
        let mut prefix: String = completions[0].clone();

        for c in completions[1..].iter() {
            while !c.starts_with(&prefix) {
                prefix.pop();
            }

            if prefix == "" { break; }
        }

        debug!("prefix: {}", prefix);

        (prefix, completions)
    } else {
        ("".to_string(), completions)
    }
}
