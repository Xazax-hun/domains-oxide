use crate::*;
use utils::DiagnosticEmitter;

fn run_driver(source: &str, opts: Opt) -> Option<String> {
    let errors: Box<Vec<u8>> = Box::new(Vec::new());
    let regular: Box<Vec<u8>> = Box::new(Vec::new());
    let mut diag = DiagnosticEmitter::new(regular, errors);
    process_source(source, &mut diag, &opts)?;
    Some(diag.out_buffer().to_string() + diag.err_buffer())
}

#[test]
fn simple_execution() {
    let source = r"init(50, 50, 0, 0);
translation(10, 0);
rotation(0, 0, 90);
translation(0, 10)";
    let expected = r"{ x: 50, y: 50 }
{ x: 60, y: 50 }
{ x: -50, y: 60 }
{ x: -50, y: 70 }
";
    let output = run_driver(source, Opt::default()).unwrap();
    assert_eq!(output, expected);
}

#[test]
fn multiple_executions() {
    let source = r"init(50, 50, 0, 0);
translation(10, 0);
rotation(0, 0, 90);
translation(0, 10)";
    let expected = r"1. execution:
{ x: 50, y: 50 }
{ x: 60, y: 50 }
{ x: -50, y: 60 }
{ x: -50, y: 70 }
2. execution:
{ x: 50, y: 50 }
{ x: 60, y: 50 }
{ x: -50, y: 60 }
{ x: -50, y: 70 }
3. execution:
{ x: 50, y: 50 }
{ x: 60, y: 50 }
{ x: -50, y: 60 }
{ x: -50, y: 70 }
";
    let output = run_driver(
        source,
        Opt {
            executions: 3,
            ..Opt::default()
        },
    )
    .unwrap();
    assert_eq!(output, expected);
}

#[test]
fn annotate() {
    let source = r"init(50, 50, 0, 0);
translation(10, 0);
rotation(0, 0, 90);
translation(0, 10)";
    let expected = r"{ x: 50, y: 50 }
{ x: 60, y: 50 }
{ x: -50, y: 60 }
{ x: -50, y: 70 }
init(50, 50, 0, 0) /* {{x: 50, y: 50}} */;
translation(10, 0) /* {{x: 60, y: 50}} */;
rotation(0, 0, 90) /* {{x: -50, y: 60}} */;
translation(0, 10) /* {{x: -50, y: 70}} */
";
    let output = run_driver(
        source,
        Opt {
            annotate_trace: true,
            ..Opt::default()
        },
    )
    .unwrap();
    assert_eq!(output, expected);
}

#[test]
fn dump_cfg() {
    let source = r"init(50, 50, 50, 50);
iter {
  {
    translation(10, 0)
  } or {
    translation(0, 10)
  }
};
rotation(0, 0, 180)";
    let expected = r#"digraph CFG {
  Node_0[label="init(50, 50, 50, 50)\n"]
  Node_1[label=""]
  Node_2[label="translation(10, 0)\n"]
  Node_3[label="translation(0, 10)\n"]
  Node_4[label=""]
  Node_5[label="rotation(0, 0, 180)\n"]

  Node_0 -> Node_1
  Node_1 -> Node_2
  Node_1 -> Node_3
  Node_2 -> Node_4
  Node_3 -> Node_4
  Node_4 -> Node_1
  Node_4 -> Node_5
}
"#;
    let output = run_driver(
        source,
        Opt {
            dump_cfg: true,
            ..Opt::default()
        },
    )
    .unwrap();
    assert_eq!(output, expected);
}
