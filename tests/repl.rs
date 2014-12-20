use std::io::process::Command;

fn repl_run(args: &[&str]) -> String {
    let rusti = if cfg!(windows) { "target/rusti.exe" } else { "target/rusti" };

    match Command::new(rusti).args(args).env("HOME", "data").output() {
        Ok(out) => String::from_utf8(out.output).unwrap(),
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

#[test]
fn test_eval() {
    assert_eq!(repl_eval(r#"println!("Hello, world!");"#), "Hello, world!\n");
    assert_eq!(repl_eval(r#"vec![1i, 2, 3]"#), "[1, 2, 3]\n");
    assert_eq!(repl_eval(r#""foo".to_string().as_slice()"#), "foo\n");
    assert_eq!(repl_eval("let a = 1i; a"), "1\n");
    assert_eq!(repl_eval("fn foo() -> int { 2i } foo()"), "2\n");
    assert_eq!(repl_eval("fn foo() -> int { 3i }; foo()"), "3\n");
    assert_eq!(repl_eval(
        "bitflags!{ flags Flags: u32 { const A = 4 } } A.bits"), "4\n");
}

#[test]
fn test_file() {
    assert_eq!(repl_file("data/test_file.rs"), "foo\n123i = int\nbar\n");
}

#[test]
fn test_rc() {
    assert_eq!(repl_run(&["-e", r#""hi, rc!""#]), "rc says hi\nhi, rc!\n");
}

#[test]
fn test_type() {
    assert_eq!(repl_cmd(".type 1i"), "1i = int\n");
    assert_eq!(repl_cmd(r#".t "hai2u""#), "\"hai2u\" = &'static str\n");
    assert_eq!(repl_cmd(".t &1i"), "&1i = &int\n");
    assert_eq!(repl_cmd(".t vec![1u]"), "vec![1u] = collections::vec::Vec<uint>\n");
}
