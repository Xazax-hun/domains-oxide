use crate::*;

fn run_driver(source: &str, opts: Opt) -> Option<String> {
    let errors: Box<Vec<u8>> = Box::default();
    let regular: Box<Vec<u8>> = Box::default();
    let mut diag = DiagnosticEmitter::new(regular, errors);
    process_source(source, &mut diag, &opts)?;
    Some(diag.out_buffer().to_string() + diag.err_buffer())
}

#[test]
fn simple_execution() {
    let source = r"@main {
  v: int = const 5;
  print v;
  ret;
}";
    let expected = "5\n";
    let output = run_driver(source, Opt::default()).unwrap();
    assert_eq!(output, expected);
}


#[test]
fn cfg_dump() {
    let source = r"@main {
  v: int = const 5;
  print v;
  ret;
}";
    let expected = r#"digraph "@main" {
  Node_0[label="v: int = const 5;\nprint v;\nret;"]

}

5
"#;
    let opts = Opt { dump_cfg: true, ..Opt::default()};
    let output = run_driver(source, opts).unwrap();
    assert_eq!(output, expected);
}
