use super::ir::*;
use super::lexer::*;
use super::parser::*;
use utils::DiagnosticEmitter;

fn parse_string(source: &str) -> Result<Unit, String> {
    let mut diag = DiagnosticEmitter::new(Box::new(Vec::new()), Box::new(Vec::new()));
    let lexer = Lexer::new(source, &mut diag);
    let tokens = lexer.lex_all();
    if tokens.tokens.is_empty() {
        return Err(diag.out_buffer().to_string() + diag.err_buffer());
    }
    let parser = Parser::new(tokens, &mut diag);
    let Some(unit) = parser.parse()
    else {
        return Err(diag.out_buffer().to_string() + diag.err_buffer());
    };
    Ok(unit)
}

#[test]
fn parse_single_function() -> Result<(), String> {
    let source = r"@main {
  v: int = const 5;
  print v;
}";
    let expected = r#"digraph "@main" {
  Node_0[label="v: int = const 5;\nprint v;"]

}
"#;
    let unit = parse_string(source)?;
    let printed = print(&unit);
    assert_eq!(printed, expected);

    Ok(())
}

#[test]
fn parse_multiple_functions() -> Result<(), String> {
    let source = r"
@mul(x: int, y: int): int {
    w: int = mul x y;
    print w;
    ret w;
}
    
@main {
  v: int = const 5;
  u: int = call @mul v v;
  nop;
  ret u;
}";
    let expected = r#"digraph "@mul" {
  Node_0[label="w: int = mul x y;\nprint w;\nret w;"]

}

digraph "@main" {
  Node_0[label="v: int = const 5;\nu: int = call @mul v v;\nnop;\nret u;"]

}
"#;
    let unit = parse_string(source)?;
    let printed = print(&unit);
    assert_eq!(printed, expected);

    Ok(())
}

#[test]
fn parse_multiple_blocks() -> Result<(), String> {
    let source = r"
@main {
  v: int = const 5;
  b: bool = gt v v;
  br b .then .else;
.then:
  u: int = const 42;
  print u;
  ret;
.else:
  ret;
}";
    let expected = r#"digraph "@main" {
  Node_0[label="v: int = const 5;\nb: bool = gt v v;\nbr b .then .else;"]
  Node_1[label="u: int = const 42;\nprint u;\nret;"]
  Node_2[label="ret;"]

  Node_0 -> Node_1
  Node_0 -> Node_2
}
"#;
    let unit = parse_string(source)?;
    let printed = print(&unit);
    assert_eq!(printed, expected);

    Ok(())
}
