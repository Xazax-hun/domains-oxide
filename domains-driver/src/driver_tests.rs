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
  Node_0[label="init(50, 50, 50, 50)"]
  Node_1[label=""]
  Node_2[label="translation(10, 0)"]
  Node_3[label="translation(0, 10)"]
  Node_4[label=""]
  Node_5[label="rotation(0, 0, 180)"]

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

#[test]
fn parser_error_does_not_panic() {
    let source = r"init";
    assert!(run_driver(source, Opt::default()).is_none());
}

#[test]
fn lexer_error_does_not_panic() {
    let source = r"/ ";
    assert!(run_driver(source, Opt::default()).is_none());
}

#[test]
fn render_svg() {
    let source = r"init(50, 50, 0, 0);
translation(10, 0);
rotation(0, 0, 90);
translation(0, 10)";
    let expected = r#"<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" width="500" height="500" viewBox="0 0 500 500">
<rect x="-50" y="-50" width="600" height="600" fill="rgb(100%, 100%, 100%)" fill-opacity="1"/>
<path fill="none" stroke-width="2" stroke-linecap="butt" stroke-linejoin="miter" stroke="rgb(0%, 0%, 0%)" stroke-opacity="1" stroke-miterlimit="10" d="M 0 -250 L 0 250 M -250 0 L 250 0 " transform="matrix(1, 0, 0, 1, 250, 250)"/>
<path fill="none" stroke-width="2" stroke-linecap="butt" stroke-linejoin="miter" stroke="rgb(90.196078%, 9.803922%, 29.411765%)" stroke-opacity="1" stroke-miterlimit="10" d="M 50 -50 L 60 -50 " transform="matrix(1, 0, 0, 1, 250, 250)"/>
<path fill="none" stroke-width="2" stroke-linecap="butt" stroke-linejoin="miter" stroke="rgb(90.196078%, 9.803922%, 29.411765%)" stroke-opacity="1" stroke-miterlimit="10" d="M -50 -60 C -34.085938 -73.261719 -13.558594 -79.65625 7.070312 -77.78125 C 27.699219 -75.90625 46.738281 -65.914062 60 -50 " transform="matrix(1, 0, 0, 1, 250, 250)"/>
<path fill="none" stroke-width="2" stroke-linecap="butt" stroke-linejoin="miter" stroke="rgb(90.196078%, 9.803922%, 29.411765%)" stroke-opacity="1" stroke-miterlimit="10" d="M -50 -60 L -50 -70 " transform="matrix(1, 0, 0, 1, 250, 250)"/>
<path fill-rule="nonzero" fill="rgb(0%, 100%, 0%)" fill-opacity="1" d="M 303 200 C 303 204 297 204 297 200 C 297 196 303 196 303 200 "/>
<path fill-rule="nonzero" fill="rgb(0%, 0%, 0%)" fill-opacity="1" d="M 313 200 C 313 204 307 204 307 200 C 307 196 313 196 313 200 "/>
<path fill-rule="nonzero" fill="rgb(0%, 0%, 0%)" fill-opacity="1" d="M 203 190 C 203 194 197 194 197 190 C 197 186 203 186 203 190 "/>
<path fill-rule="nonzero" fill="rgb(0%, 0%, 0%)" fill-opacity="1" d="M 203 180 C 203 184 197 184 197 180 C 197 176 203 176 203 180 "/>
</svg>
"#;
    let output = run_driver(
        source,
        Opt {
            svg: true,
            ..Opt::default()
        },
    )
    .unwrap();
    assert_eq!(output, expected);
}

#[test]
fn render_svg_no_dots() {
    let source = r"init(50, 50, 0, 0);
translation(10, 0);
rotation(0, 0, 90);
translation(0, 10)";
    let expected = r#"<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" width="500" height="500" viewBox="0 0 500 500">
<rect x="-50" y="-50" width="600" height="600" fill="rgb(100%, 100%, 100%)" fill-opacity="1"/>
<path fill="none" stroke-width="2" stroke-linecap="butt" stroke-linejoin="miter" stroke="rgb(0%, 0%, 0%)" stroke-opacity="1" stroke-miterlimit="10" d="M 0 -250 L 0 250 M -250 0 L 250 0 " transform="matrix(1, 0, 0, 1, 250, 250)"/>
<path fill-rule="nonzero" fill="rgb(0%, 100%, 0%)" fill-opacity="1" d="M 303 200 C 303 204 297 204 297 200 C 297 196 303 196 303 200 "/>
<path fill-rule="nonzero" fill="rgb(0%, 0%, 0%)" fill-opacity="1" d="M 313 200 C 313 204 307 204 307 200 C 307 196 313 196 313 200 "/>
<path fill-rule="nonzero" fill="rgb(0%, 0%, 0%)" fill-opacity="1" d="M 203 190 C 203 194 197 194 197 190 C 197 186 203 186 203 190 "/>
<path fill-rule="nonzero" fill="rgb(0%, 0%, 0%)" fill-opacity="1" d="M 203 180 C 203 184 197 184 197 180 C 197 176 203 176 203 180 "/>
</svg>
"#;
    let output = run_driver(
        source,
        Opt {
            svg: true,
            dots_only: true,
            ..Opt::default()
        },
    )
    .unwrap();
    assert_eq!(output, expected);
}

#[test]
fn render_svg_no_dots_executions() {
    let source = r"init(50, 50, 0, 0);
translation(10, 0);
rotation(0, 0, 90);
translation(0, 10)";
    let expected = r#"<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" width="500" height="500" viewBox="0 0 500 500">
<rect x="-50" y="-50" width="600" height="600" fill="rgb(100%, 100%, 100%)" fill-opacity="1"/>
<path fill="none" stroke-width="2" stroke-linecap="butt" stroke-linejoin="miter" stroke="rgb(0%, 0%, 0%)" stroke-opacity="1" stroke-miterlimit="10" d="M 0 -250 L 0 250 M -250 0 L 250 0 " transform="matrix(1, 0, 0, 1, 250, 250)"/>
<path fill-rule="nonzero" fill="rgb(0%, 100%, 0%)" fill-opacity="1" d="M 303 200 C 303 204 297 204 297 200 C 297 196 303 196 303 200 "/>
<path fill-rule="nonzero" fill="rgb(0%, 0%, 0%)" fill-opacity="1" d="M 313 200 C 313 204 307 204 307 200 C 307 196 313 196 313 200 "/>
<path fill-rule="nonzero" fill="rgb(0%, 0%, 0%)" fill-opacity="1" d="M 203 190 C 203 194 197 194 197 190 C 197 186 203 186 203 190 "/>
<path fill-rule="nonzero" fill="rgb(0%, 0%, 0%)" fill-opacity="1" d="M 203 180 C 203 184 197 184 197 180 C 197 176 203 176 203 180 "/>
<path fill-rule="nonzero" fill="rgb(0%, 100%, 0%)" fill-opacity="1" d="M 303 200 C 303 204 297 204 297 200 C 297 196 303 196 303 200 "/>
<path fill-rule="nonzero" fill="rgb(0%, 0%, 0%)" fill-opacity="1" d="M 313 200 C 313 204 307 204 307 200 C 307 196 313 196 313 200 "/>
<path fill-rule="nonzero" fill="rgb(0%, 0%, 0%)" fill-opacity="1" d="M 203 190 C 203 194 197 194 197 190 C 197 186 203 186 203 190 "/>
<path fill-rule="nonzero" fill="rgb(0%, 0%, 0%)" fill-opacity="1" d="M 203 180 C 203 184 197 184 197 180 C 197 176 203 176 203 180 "/>
</svg>
"#;
    let output = run_driver(
        source,
        Opt {
            svg: true,
            dots_only: true,
            executions: 2,
            ..Opt::default()
        },
    )
    .unwrap();
    assert_eq!(output, expected);
}
