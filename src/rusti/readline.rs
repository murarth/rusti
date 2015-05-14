// Copyright 2014 Murarth
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Wrapper to GNU Readline library

use std::mem;
use std::ffi::{CStr, CString};
use std::ptr;
use std::slice;
use std::str::from_utf8;
use std::sync::{Once, ONCE_INIT};

use libc::{self, c_char, c_int, size_t};

use completion::complete;

static INIT_READLINE: Once = ONCE_INIT;

/// Readline completion function. Called to perform text completion.
/// Takes arguments `text` (segment of input being completed),
/// `start` (start of input within line buffer),
/// and `end` (end of input within line buffer).
/// Returns an array whose first element is the substitution text
/// (e.g. longest common prefix) and whose remaining elements are possible
/// substitutions and which is terminated by a NULL element.
type RlCompletionFn = extern "C" fn(*const c_char, c_int, c_int) -> *mut *const c_char;

#[link(name = "readline")]
extern "C" {
    static mut rl_attempted_completion_function: RlCompletionFn;
    static mut rl_attempted_completion_over: c_int;
    static mut rl_line_buffer: *mut c_char;
    static mut rl_completion_suppress_append: c_int;

    static mut rl_basic_word_break_characters: *const c_char;

    #[link_name = "add_history"]
    fn rl_add_history(line: *const c_char);
    #[link_name = "readline"]
    fn rl_readline(prompt: *const c_char) -> *const c_char;
    fn rl_insert_text(text: *const c_char) -> c_int;
}

fn init_readline() {
    unsafe {
        // Set up our custom completion function.
        rl_attempted_completion_function = completion_fn;
        // Set up word break characters.
        // These are anything not permitted in identifiers.
        rl_basic_word_break_characters =
            b" \t\n!\"#$%&'()*+,-./:;<=>?@[\\]^`\0" // NUL-terminated
            .as_ptr() as *const _;
    }
}

/// Pushes a single line into `readline` history.
pub fn push_history(line: &str) {
    let line = CString::new(line.as_bytes()).unwrap();
    unsafe { rl_add_history(line.as_ptr()) };
}

/// Reads a line from the input stream. The trailing newline is truncated.
/// Returns `None` if end-of-file is signaled.
pub fn read_line(prompt: &str) -> Option<String> {
    INIT_READLINE.call_once(init_readline);

    let pr = CString::new(prompt.as_bytes()).unwrap();
    let sp = unsafe { rl_readline(pr.as_ptr()) };

    if sp.is_null() {
        None
    } else {
        let cs = unsafe { CStr::from_ptr(sp) };
        Some(from_utf8(cs.to_bytes()).unwrap().to_string())
    }
}

extern "C" fn completion_fn(text: *const c_char, start: c_int, end: c_int) -> *mut *const c_char {
    unsafe {
        // Prevent readline from calling its default completion function
        // if this function returns NULL.
        rl_attempted_completion_over = 1;

        // Do not append a space if a single match was inserted
        rl_completion_suppress_append = 1;
    }

    let input = unsafe { CStr::from_ptr(rl_line_buffer) };
    let text = unsafe { CStr::from_ptr(text) };

    debug!("completion fn on {:?}, {} - {} ({:?})",
        String::from_utf8_lossy(input.to_bytes()),
        String::from_utf8_lossy(text.to_bytes()),
        start, end);

    // Tab with no text inserts indentation
    if input.to_bytes().iter().all(|&b| (b as char).is_whitespace()) {
        let sp = CString::new(&b"    "[..]).unwrap();
        unsafe { rl_insert_text(sp.as_ptr()) };
    }

    assert!(start >= 0 && end >= 0);
    let start = start as usize;
    let end = end as usize;

    let input = from_utf8(input.to_bytes()).unwrap();
    let (prefix, completions) = match complete(input, start, end) {
        Some(r) => r,
        None => return ptr::null_mut()
    };

    let text = from_utf8(text.to_bytes()).unwrap();

    unsafe {
        let size = mem::size_of::<*const c_char>();
        let n = completions.len() + 2;  // +1 for the prefix, +1 for the leading nullptr
        let buf = libc::calloc(n as size_t, size as size_t) as *mut *const c_char;
        let s = slice::from_raw_parts_mut(buf, n);

        /// Allocates space for the given string and copies it into it. Also appends a terminating
        /// zero.
        unsafe fn c_str(s: &str) -> *const c_char {
            let len = s.as_bytes().len();
            let dest = libc::malloc((len + 1) as size_t) as *mut c_char;
            ptr::copy_nonoverlapping(s.as_bytes().as_ptr() as *const c_char, dest, len);

            ptr::write(dest.offset(len as isize), 0 as c_char);
            dest
        }

        {
            s[0] = c_str(&format!("{}{}", text, prefix));

            let mut i = 1;
            for c in completions {
                let completion = format!("{}{}", text, c);
                s[i] = c_str(&completion);
                i += 1;
            }

            // Last element remains NULL
        }

        buf
    }
}
