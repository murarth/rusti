// Copyright 2014 Murarth
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Wrapper to GNU Readline library

extern crate libc;

use std::ffi::{c_str_to_bytes, CString};
use std::ptr;
use std::str::from_utf8;
use std::sync::{Once, ONCE_INIT};

use self::libc::{c_char, c_int, /* size_t */};

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
    let line = CString::from_slice(line.as_bytes());
    unsafe { rl_add_history(line.as_ptr()) };
}

/// Reads a line from the input stream. The trailing newline is truncated.
/// Returns `None` if end-of-file is signaled.
pub fn read_line(prompt: &str) -> Option<String> {
    INIT_READLINE.call_once(init_readline);

    let pr = CString::from_slice(prompt.as_bytes());
    let sp = unsafe { rl_readline(pr.as_ptr()) };

    if sp.is_null() {
        None
    } else {
        let cs = unsafe { c_str_to_bytes(&sp) };
        Some(from_utf8(cs).unwrap().to_string())
    }
}

extern "C" fn completion_fn(text: *const c_char,
        _start: c_int, _end: c_int) -> *mut *const c_char {
    unsafe {
        // Prevent readline from calling its default completion function
        // if this function returns NULL.
        rl_attempted_completion_over = 1;
    }

    let text = unsafe { c_str_to_bytes(&text) };

    debug!("completion fn on \"{}\"", from_utf8(text).ok());

    // Tab with no text inserts indentation
    if text.is_empty() {
        let sp = CString::from_slice(b"    ");
        unsafe { rl_insert_text(sp.as_ptr()) };
    }

    // TODO: Completion stuff

    // For now, just return NULL.
    ptr::null_mut()

    /*
    // This is how a C array should be created.
    unsafe {
        let size: size_t = std::mem::size_of::<*const c_char>();
        let n: size_t = 2;
        let buf = libc::calloc(n, size);
        let mut vec = CVec::new(buf as *mut *const c_char, n);

        {
            let s = vec.as_mut_slice();

            s[0] = "".to_c_str().into_inner();
            // Last element remains NULL
        }

        vec.into_inner()
    }
    */
}
