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
use std::str::from_utf8;
use std::sync::{Once, ONCE_INIT};

use libc::{self, c_char, c_int, size_t};

use c_vec::CSlice;

use completion;

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

    #[link_name = "add_history"]
    fn rl_add_history(line: *const c_char);
    #[link_name = "readline"]
    fn rl_readline(prompt: *const c_char) -> *const c_char;
    fn rl_insert_text(text: *const c_char) -> c_int;
}

fn init_readline() {
    unsafe {
        rl_attempted_completion_function = completion_fn;
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

    debug!("completion fn on \"{:?}\", {} - {}", from_utf8(text.to_bytes()).ok(), start, end);

    // Tab with no text inserts indentation
    if input.to_bytes().is_empty() {
        let sp = CString::new(&b"    "[..]).unwrap();
        unsafe { rl_insert_text(sp.as_ptr()) };
    }

    let input = ::std::str::from_utf8(input.to_bytes()).unwrap();
    let (prefix, completions) = completion::complete(input, start as usize, end as usize);

    unsafe {
        let size = mem::size_of::<*const c_char>();
        let n = completions.len() + 2;  // +1 for the prefix, +1 for the leading nullptr
        let buf = libc::calloc(n as size_t, size as size_t) as *mut *const c_char;
        let mut vec: CSlice<*const c_char> = CSlice::new(buf, n);

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
            let s = vec.as_mut();

            s[0] = c_str(prefix.as_ref());

            let mut i = 1;
            for c in completions {
                s[i] = c_str(&*c);
                i += 1;
            }

            // Last element remains NULL
        }

        buf
    }
}
