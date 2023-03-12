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
fn parse_empty() {
    parse_string("").expect("");
}

#[test]
fn parse_single_function() -> Result<(), String> {
    // TODO: Should we allow implicit ret in void functions?
    let source = r"@main {
  v: int = const 5;
  print v;
  ret;
}
";
    let expected = r#"digraph "@main" {
  Node_0[label="v: int = const 5;\nprint v;\nret;"]

}
"#;
    let unit = parse_string(source)?;
    let printed = print(&unit);
    assert_eq!(printed, source);
    let printed_dot = print_dot(&unit);
    assert_eq!(printed_dot, expected);

    Ok(())
}

#[test]
fn parse_multiple_functions() -> Result<(), String> {
    let source = r"@mul(x: int, y: int): int {
  w: int = mul x y;
  print w;
  ret w;
}

@main(): int {
  v: int = const 5;
  u: int = call @mul v v;
  nop;
  ret u;
}
";
    let expected = r#"digraph "@mul" {
  Node_0[label="w: int = mul x y;\nprint w;\nret w;"]

}

digraph "@main" {
  Node_0[label="v: int = const 5;\nu: int = call @mul v v;\nnop;\nret u;"]

}
"#;
    let unit = parse_string(source)?;
    let printed = print(&unit);
    assert_eq!(printed, source);
    let printed_dot = print_dot(&unit);
    assert_eq!(printed_dot, expected);

    Ok(())
}

#[test]
fn parse_multiple_blocks() -> Result<(), String> {
    let source = r"@main {
  v: int = const 5;
  b: bool = gt v v;
  br b .then .else;

.then:
  u: int = const 42;
  print u;
  ret;

.else:
  ret;
}
";
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
    assert_eq!(printed, source);
    let printed_dot = print_dot(&unit);
    assert_eq!(printed_dot, expected);

    Ok(())
}

#[test]
fn parse_starting_label() -> Result<(), String> {
    let source = r"
@main {
.start:
  ret;
}";
    let expected = r#"digraph "@main" {
  Node_0[label="ret;"]

}
"#;
    let unit = parse_string(source)?;
    let printed = print_dot(&unit);
    assert_eq!(printed, expected);

    Ok(())
}

#[test]
fn parse_syntactic_errors() {
    let source = r"@main";
    let err = parse_string(source).expect_err("");
    assert_eq!(err, "[line 1] Error at end of file: '{' expected.\n");

    let source = r"@main(";
    let err = parse_string(source).expect_err("");
    assert_eq!(
        err,
        "[line 1] Error at end of file: Local identifier expected.\n"
    );

    let source = r"@main(x";
    let err = parse_string(source).expect_err("");
    assert_eq!(err, "[line 1] Error at end of file: ':' expected.\n");

    let source = r"@main(x: ";
    let err = parse_string(source).expect_err("");
    assert_eq!(err, "[line 1] Error at end of file: Type expected.\n");

    let source = r"@main(x: int";
    let err = parse_string(source).expect_err("");
    assert_eq!(err, "[line 1] Error at end of file: ')' expected.\n");

    let source = r"@main(x: int,";
    let err = parse_string(source).expect_err("");
    assert_eq!(
        err,
        "[line 1] Error at end of file: Local identifier expected.\n"
    );

    let source = r"@main(x: int)";
    let err = parse_string(source).expect_err("");
    assert_eq!(err, "[line 1] Error at end of file: '{' expected.\n");

    let source = r"@main(x: int):";
    let err = parse_string(source).expect_err("");
    assert_eq!(err, "[line 1] Error at end of file: Type expected.\n");

    let source = r"@main(x: int): int";
    let err = parse_string(source).expect_err("");
    assert_eq!(err, "[line 1] Error at end of file: '{' expected.\n");

    // TODO: better error message.
    let source = r"@main(x: int) {";
    let err = parse_string(source).expect_err("");
    assert_eq!(
        err,
        "[line 1] Error at end of file: Local identifier expected.\n"
    );

    // TODO: better error message, what if the next token should be ';'?
    let source = r"@main(x: int) {
      ret";
    let err = parse_string(source).expect_err("");
    assert_eq!(
        err,
        "[line 2] Error at end of file: Local identifier expected.\n"
    );

    let source = r"@main(x: int): int {
      ret x";
    let err = parse_string(source).expect_err("");
    assert_eq!(err, "[line 2] Error at end of file: ';' expected.\n");
}

#[test]
fn parse_identifier_errors() {
    let source = r"@main {
.start:
.start:
  ret;
}";
    let err = parse_string(source).expect_err("");
    assert_eq!(err, "[line 3] Error at '.start': Duplicate label found.\n");

    let source = r"@main {
  jmp .nonexistent;
}";
    let err = parse_string(source).expect_err("");
    assert_eq!(
        err,
        "[line 2] Error : Branch target '.nonexistent' is missing\n"
    );

    let source = r"@main {
  x: int = add y y;
}";
    let err = parse_string(source).expect_err("");
    assert_eq!(err, "[line 2] Error at 'y': Undefined identifier.\n");

    let source = r"@main {
  x: int = add x x;
}";
    let err = parse_string(source).expect_err("");
    assert_eq!(err, "[line 2] Error at 'x': Undefined identifier.\n");

    let source = r"@main {
  y: int = const 5; 
  z: bool = const true; 
  x: int = add y y;
  x: bool = id z;
}";
    let err = parse_string(source).expect_err("");
    assert_eq!(
        err,
        "[line 5] Error at 'x': Unexpected type 'bool'. Expected 'int'.\n"
    );

    let source = r"@main {
  call x;
}";
    let err = parse_string(source).expect_err("");
    assert_eq!(
        err,
        "[line 2] Error at 'local_1': Global identifier expected.\n"
    );

    let source = r"@main {
  x: int = add .x .y;
}";
    let err = parse_string(source).expect_err("");
    assert_eq!(
        err,
        "[line 2] Error at 'label_2': Local identifier expected.\n"
    );

    // TODO: better error location.
    let source = r"main {
  ret;
}";
    let err = parse_string(source).expect_err("");
    assert_eq!(
        err,
        "[line 1] Error at 'local_0': Global identifier expected.\n"
    );
}

#[test]
fn parse_type_errors() {
    let source = r"@main {
  x: int = const true;
  ret;
}";
    let err = parse_string(source).expect_err("");
    assert_eq!(
        err,
        "[line 2] Error at 'true': 'int' type expected; 'bool' found\n"
    );

    let source = r"@main {
  x: bool = const true;
  y: int = add x x;
  ret;
}";
    let err = parse_string(source).expect_err("");
    assert_eq!(
        err,
        "[line 3] Error at 'add': 'int' type expected; 'bool' found\n"
    );

    let source = r"@main {
  x: int = const 5;
  y: bool = mul x x;
  ret;
}";
    let err = parse_string(source).expect_err("");
    assert_eq!(
        err,
        "[line 3] Error at 'mul': 'int' type expected; 'bool' found\n"
    );

    let source = r"@main {
  x: bool = const true;
  y: int = not x;
  ret;
}";
    let err = parse_string(source).expect_err("");
    assert_eq!(
        err,
        "[line 3] Error at 'not': 'bool' type expected; 'int' found\n"
    );

    let source = r"@main {
  x: bool = const true;
  y: int = id x;
  ret;
}";
    let err = parse_string(source).expect_err("");
    assert_eq!(
        err,
        "[line 3] Error at 'id': 'int' type expected; 'bool' found\n"
    );

    let source = r"@foo(a: bool, b: bool) {
  ret;
}

@main {
  x: bool = const true;
  call @foo x;
  ret;
}";
    let err = parse_string(source).expect_err("");
    assert_eq!(
        err,
        "[line 7] Error at 'call': 2 arguments expected, got 1\n"
    );

    let source = r"@foo(a: bool, b: int) {
  ret;
}

@main {
  x: bool = const true;
  call @foo x x;
  ret;
}";
    let err = parse_string(source).expect_err("");
    assert_eq!(
        err,
        "[line 7] Error at 'call': 'int' type expected; 'bool' found\n"
    );

    let source = r"@foo(a: bool, b: bool) {
  ret;
}

@main {
  x: bool = const true;
  y: int = call @foo x x;
  ret;
}";
    let err = parse_string(source).expect_err("");
    assert_eq!(
        err,
        "[line 7] Error at 'call': Void functions cannot return a value.\n"
    );

    let source = r"@foo(a: bool, b: bool) : bool {
  ret a;
}

@main {
  x: bool = const true;
  call @foo x x;
  ret;
}";
    let err = parse_string(source).expect_err("");
    assert_eq!(
        err,
        "[line 7] Error at 'call': Non-void functions must return a value.\n"
    );

    let source = r"@foo(a: bool, b: bool) {
  ret a;
}";
    let err = parse_string(source).expect_err("");
    assert_eq!(
        err,
        "[line 2] Error at 'ret': Void functions cannot return a value.\n"
    );

    let source = r"@foo(a: bool, b: bool): bool {
  ret;
}";
    let err = parse_string(source).expect_err("");
    assert_eq!(
        err,
        "[line 2] Error at 'ret': Non-void functions must return a value.\n"
    );
}

#[test]
fn parse_misc_errors() {
    let source = r"
@main {
  v: int = const 5;
.then:
  ret;
.else:
  ret;
}";
    let err = parse_string(source).expect_err("");
    assert_eq!(
        err,
        "[line 3] Error at '5': Block terminator expected to be jump, br, or ret.\n"
    );

    let source = r"
@main {
  v: int = const 5;
  jmp .else;
  ret;
.else:
  ret;
}";
    let err = parse_string(source).expect_err("");
    assert_eq!(
        err,
        "[line 5] Error at 'ret': Basic block must start with a label.\n"
    );
}

// TODO: support and test when use is before def lexically.
// TODO: support any function definition order.
