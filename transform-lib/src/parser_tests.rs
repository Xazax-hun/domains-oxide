use super::ast::*;

use super::lexer::Lexer;
use super::parser::Parser;
use utils::DiagnosticEmitter;

pub fn parse_string(source: &str) -> Result<ASTContext, String> {
    let mut diag = DiagnosticEmitter::log_to_buffer();
    let lexer = Lexer::new(source, &mut diag);
    let tokens = lexer.lex_all();
    if tokens.is_empty() {
        return Err(diag.out_buffer().unwrap() + &diag.err_buffer().unwrap());
    }
    let parser = Parser::new(tokens, &mut diag);
    if let Some(ctx) = parser.parse() {
        Ok(ctx)
    } else {
        Err(diag.out_buffer().unwrap() + &diag.err_buffer().unwrap())
    }
}

#[test]
fn all_nodes_parsed() -> Result<(), String> {
    let source = r"init(50, 50, 50, 50);
translation(10, 0);
iter {
  {
    translation(10, 0);
    iter {
      translation(10, 0)
    }
  } or {
    rotation(0, 0, 90)
  }
}";
    let ctx = parse_string(source)?;
    let pretty_printed = print(ctx.get_root(), &ctx, &Annotations::new());
    assert_eq!(source, pretty_printed);
    Ok(())
}

#[test]
fn empty_alternative_in_or() -> Result<(), String> {
    let source = r"init(50, 50, 50, 50);
{
  translation(10, 0)
} or {

}";
    let ctx = parse_string(source)?;
    let pretty_printed = print(ctx.get_root(), &ctx, &Annotations::new());
    assert_eq!(source, pretty_printed);
    Ok(())
}

#[test]
fn empty_input() {
    let source = r"";
    let output = parse_string(source).expect_err("");
    assert_eq!(
        output,
        "[line 1] Error at end of file: 'init' expected at the beginning of the program.\n"
    );
}

#[test]
fn illegal_init() {
    let source = r"init(50, 50, -1, 0)";
    let output = parse_string(source).expect_err("");
    assert_eq!(
        output,
        "[line 1] Error at 'init': the width of the initial area cannot be negative.\n"
    );

    let source = r"init(50, 50, 0, -1)";
    let output = parse_string(source).expect_err("");
    assert_eq!(
        output,
        "[line 1] Error at 'init': the height of the initial area cannot be negative.\n"
    );
}

#[test]
fn empty_or() {
    let source = r"init(50, 50, 50, 50); {} or {}";
    let output = parse_string(source).expect_err("");
    assert_eq!(
        output,
        "[line 1] Error at 'or': at most one alternative can be empty.\n"
    );
}

#[test]
fn typo_in_or() {
    let source = r"init(50, 50, 50, 50); {} 10 { translation(0, 0) }";
    let output = parse_string(source).expect_err("");
    assert_eq!(output, "[line 1] Error at '10': 'or' expected.\n");
}

#[test]
fn empty_loop() {
    let source = r"init(50, 50, 50, 50); iter {}";
    let output = parse_string(source).expect_err("");
    assert_eq!(
        output,
        "[line 1] Error at 'iter': the body of 'iter' must not be empty.\n"
    );
}

#[test]
fn redundant_semicolon() {
    let source = r"init(50, 50, 50, 50); iter { translation(0, 0); }";
    let output = parse_string(source).expect_err("");
    assert_eq!(output, "[line 1] Error at '}': redundant semicolon?\n");

    let source = r"init(50, 50, 50, 50);";
    let output = parse_string(source).expect_err("");
    assert_eq!(
        output,
        "[line 1] Error at end of file: redundant semicolon?\n"
    );
}

#[test]
fn from_fuzzing() {
    let source = r"init";
    let output = parse_string(source).expect_err("");
    assert_eq!(output, "[line 1] Error at end of file: '(' expected.\n");
}
