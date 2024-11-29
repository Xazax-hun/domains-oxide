use crate::*;

fn run_driver(source: &str, opts: Opt) -> Option<String> {
    let mut diag = DiagnosticEmitter::log_to_buffer();
    process_source(source, &mut diag, &opts)?;
    Some(diag.out_buffer().unwrap() + &diag.err_buffer().unwrap())
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
fn parse_main_arguments() {
    let source = r"@main(x: int, b: bool) {
  print x;
  print b;
  ret;
}";
    let expected = "5\ntrue\n";
    let opts = Opt::parse_from(["bril-driver", "source", "5", "true"].iter());
    let output = run_driver(source, opts).unwrap();
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
    let opts = Opt {
        dump_cfg: true,
        ..Opt::default()
    };
    let output = run_driver(source, opts).unwrap();
    assert_eq!(output, expected);
}

#[test]
fn analyze() {
    let source = r"@main {
  v: int = const 5;
  ret;
}
";

    let expected = r"@main {
  v: int = const 5; /* v: Positive */
  ret;
}
";
    let opts = Opt {
        analyze: Some(CLIAnalyses::Sign),
        ..Opt::default()
    };
    let output = run_driver(source, opts).unwrap();
    assert_eq!(output, expected);
}
