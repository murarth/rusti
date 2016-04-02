use std::io::Write;
use std::process::{Command, Stdio};

fn rusti_cmd() -> Command {
    let rusti = if cfg!(windows) {
        "target/debug/rusti.exe"
    } else {
        "target/debug/rusti"
    };

    let mut cmd = Command::new(rusti);
    cmd.env("HOME", "data");
    cmd
}

fn repl_run(args: &[&str]) -> String {
    match rusti_cmd().args(args).output() {
        Ok(out) => {
            String::from_utf8(out.stdout).unwrap()
        }
        Err(e) => panic!("failed to spawn process: {}", e)
    }
}

fn repl_cmd(cmd: &str) -> String {
    repl_run(&["--no-rc", "-c", cmd])
}

fn repl_eval(code: &str) -> String {
    repl_run(&["--no-rc", "-e", code])
}

fn repl_file(path: &str) -> String {
    repl_run(&["--no-rc", path])
}

fn repl_input(input: &str) -> String {
    let mut cmd = match rusti_cmd().arg("--no-rc")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn() {
        Ok(cmd) => cmd,
        Err(e) => panic!("failed to spawn process: {}", e)
    };

    match cmd.stdin.as_mut().expect("no process stdin")
            .write_all(input.as_bytes()) {
        Ok(()) => (),
        Err(e) => panic!("failed to write child: {}", e)
    }

    match cmd.wait_with_output() {
        Ok(out) => String::from_utf8(out.stdout).unwrap(),
        Err(e) => panic!("failed to run command: {}", e)
    }
}

#[test]
fn test_eval() {
    assert_eq!(repl_eval(r#"println!("Hello, world!");"#), "Hello, world!\n");
    assert_eq!(repl_eval(r#"vec![1, 2, 3]"#), "[1, 2, 3]\n");
    assert_eq!(repl_eval("let a = 1; a"), "1\n");
    assert_eq!(repl_eval("fn foo() -> u32 { 2 } foo()"), "2\n");
    assert_eq!(repl_eval("fn foo() -> u32 { 3 }; foo()"), "3\n");
}

#[test]
fn test_file() {
    assert_eq!(repl_file("data/test_run.rs"), "foo\n123 = i32\nbar\n");
}

#[test]
fn test_load() {
    assert_eq!(repl_input(".l data/test_load.rs\nhello(\"world\");"), "Hello, world!\n");
}

#[test]
fn test_print() {
    assert_eq!(repl_cmd(".print 1"), "1\n");
    assert_eq!(repl_cmd(r#".p "Hello!""#), "Hello!\n");
}

#[test]
fn test_rc() {
    assert_eq!(repl_run(&["-e", r#"println!("hi, rc!");"#]), "rc says hi\nhi, rc!\n");
}

#[test]
fn test_type() {
    assert_eq!(repl_cmd(".type 1"), "1 = i32\n");
    assert_eq!(repl_cmd(r#".t "hai2u""#), "\"hai2u\" = &'static str\n");
    assert_eq!(repl_cmd(":t &1"), "&1 = &i32\n");
    assert_eq!(repl_cmd(".t vec![1u32]"), "vec![1u32] = std::vec::Vec<u32>\n");
}
