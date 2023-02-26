use super::ast::*;

use super::lexer::Lexer;
use super::parser::Parser;
use utils::DiagnosticEmitter;

#[derive(Debug)]
struct ParseResult {
    output: String,
    ctx: Option<ASTContext>,
}

fn parse_string(source: &str) -> ParseResult {
    let mut diag = DiagnosticEmitter::new(Box::new(Vec::new()), Box::new(Vec::new()));
    let lexer = Lexer::new(source, &mut diag);
    let tokens = lexer.lex_all();
    let parser = Parser::new(tokens, &mut diag);
    let ctx = parser.parse();
    ParseResult {
        output: diag.out_buffer().to_string() + diag.err_buffer(),
        ctx,
    }
}

#[test]
fn all_nodes_parsed() {
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
    let ParseResult { output, ctx } = parse_string(source);
    assert!(output.is_empty());
    let ctx = ctx.unwrap();
    let pretty_printed = print(ctx.get_root(), &ctx, &Annotations::new());
    assert_eq!(source, pretty_printed);
}

#[test]
fn empty_alternative_in_or() {
    let source = r"init(50, 50, 50, 50);
{
  translation(10, 0)
} or {

}";
    let ParseResult { output, ctx } = parse_string(source);
    assert!(output.is_empty());
    let ctx = ctx.unwrap();
    let pretty_printed = print(ctx.get_root(), &ctx, &Annotations::new());
    assert_eq!(source, pretty_printed);
}

#[test]
fn empty_input() {
    let source = r"";
    let ParseResult { output, ctx } = parse_string(source);
    assert!(ctx.is_none());
    assert_eq!(
        output,
        "[line 1] Error at end of file: 'init' expected at the beginning of the program.\n"
    );
}

#[test]
fn illegal_init() {
    let source = r"init(50, 50, -1, 0)";
    let ParseResult { output, ctx } = parse_string(source);
    assert!(ctx.is_none());
    assert_eq!(
        output,
        "[line 1] Error at 'init': the width of the initial area cannot be negative.\n"
    );

    let source = r"init(50, 50, 0, -1)";
    let ParseResult { output, ctx } = parse_string(source);
    assert!(ctx.is_none());
    assert_eq!(
        output,
        "[line 1] Error at 'init': the height of the initial area cannot be negative.\n"
    );
}

#[test]
fn empty_or() {
    let source = r"init(50, 50, 50, 50); {} or {}";
    let ParseResult { output, ctx } = parse_string(source);
    assert!(ctx.is_none());
    assert_eq!(
        output,
        "[line 1] Error at 'or': at most one alternative can be empty.\n"
    );
}

#[test]
fn typo_in_or() {
    let source = r"init(50, 50, 50, 50); {} 10 { translation(0, 0) }";
    let ParseResult { output, ctx } = parse_string(source);
    assert!(ctx.is_none());
    assert_eq!(output, "[line 1] Error at '10': 'or' expected.\n");
}

#[test]
fn empty_loop() {
    let source = r"init(50, 50, 50, 50); iter {}";
    let ParseResult { output, ctx } = parse_string(source);
    assert!(ctx.is_none());
    assert_eq!(
        output,
        "[line 1] Error at 'iter': the body of 'iter' must not be empty.\n"
    );
}

#[test]
fn redundant_semicolon() {
    let source = r"init(50, 50, 50, 50); iter { translation(0, 0); }";
    let ParseResult { output, ctx } = parse_string(source);
    assert!(ctx.is_none());
    assert_eq!(output, "[line 1] Error at '}': redundant semicolon?\n");

    let source = r"init(50, 50, 50, 50);";
    let ParseResult { output, ctx } = parse_string(source);
    assert!(ctx.is_none());
    assert_eq!(
        output,
        "[line 1] Error at end of file: redundant semicolon?\n"
    );
}

#[test]
fn from_fuzzing() {
    let source = r"init";
    let ParseResult { output, ctx } = parse_string(source);
    assert!(ctx.is_none());
    assert_eq!(output, "[line 1] Error at end of file: '(' expected.\n");
}
