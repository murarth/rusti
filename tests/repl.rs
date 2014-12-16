use std::io::process::Command;

fn repl_run(args: &[&str]) -> String {
    let rusti = if cfg!(windows) { "target/rusti.exe" } else { "target/rusti" };

    match Command::new(rusti).args(args).output() {
        Ok(out) => String::from_utf8(out.output).unwrap(),
        Err(e) => panic!("failed to spawn process: {}", e)
    }
}

fn repl_cmd(cmd: &str) -> String {
    repl_run(&["-c", cmd])
}

fn repl_eval(code: &str) -> String {
    repl_run(&["-e", code])
}

#[test]
fn test_eval() {
    assert_eq!(repl_eval(r#"println!("Hello, world!");"#), "Hello, world!\n");
    assert_eq!(repl_eval(r#"vec![1i, 2, 3]"#), "[1, 2, 3]\n");
    assert_eq!(repl_eval(r#""foo".to_string().as_slice()"#), "foo\n");
}

#[test]
fn test_type() {
    assert_eq!(repl_cmd(".type 1i"), "1i = int\n");
    assert_eq!(repl_cmd(r#".t "hai2u""#), "\"hai2u\" = &str\n");
    assert_eq!(repl_cmd(".t &1i"), "&1i = &int\n");
    assert_eq!(repl_cmd(".t vec![1u]"), "vec![1u] = collections::vec::Vec<uint>\n");
}
