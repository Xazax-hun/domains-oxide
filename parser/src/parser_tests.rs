use super::ast::*;

use super::lexer::*;
use super::parser::*;
use utils::DiagnosticEmitter;

struct ParseResult {
    output: String,
    ctx: ASTContext,
    root: Option<Node>,
}

fn parse_string(source: &str) -> ParseResult {
    let errors: Vec<u8> = Vec::new();
    let regular: Vec<u8> = Vec::new();
    let mut diag = DiagnosticEmitter::new(regular, errors);
    let mut lexer = Lexer::new(source, &mut diag);
    let tokens = lexer.lex_all();
    let mut parser = Parser::new(tokens, &mut diag);
    let (root, ctx) = parser.parse();
    let out = std::str::from_utf8(diag.out.buffer()).unwrap();
    let err = std::str::from_utf8(diag.err.buffer()).unwrap();
    ParseResult {
        output: out.to_string() + err,
        ctx,
        root,
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
    let ParseResult { output, ctx, root } = parse_string(source);
    assert!(output.is_empty());
    if let Some(root) = root {
        let pretty_printed = print(root, &ctx, &Annotations::new());
        assert_eq!(source, pretty_printed);
    } else {
        assert!(false);
    }
}

#[test]
fn empty_alternative_in_or() {
    let source = r"init(50, 50, 50, 50);
{
  translation(10, 0)
} or {

}";
    let ParseResult { output, ctx, root } = parse_string(source);
    assert!(output.is_empty());
    if let Some(root) = root {
        let pretty_printed = print(root, &ctx, &Annotations::new());
        assert_eq!(source, pretty_printed);
    } else {
        assert!(false);
    }
}

#[test]
fn empty_input() {
    let source = r"";
    let ParseResult { output, ctx: _, root } = parse_string(source);
    assert!(root.is_none());
    assert_eq!(output, "[line 1] Error at end of file: 'init' expected at the beginning of the program.\n");
}

#[test]
fn illegal_init() {
    let source = r"init(50, 50, -1, 0)";
    let ParseResult { output, ctx: _, root } = parse_string(source);
    assert!(root.is_none());
    assert_eq!(output, "[line 1] Error at 'init': the width of the initial area cannot be negative.\n");

    let source = r"init(50, 50, 0, -1)";
    let ParseResult { output, ctx: _, root } = parse_string(source);
    assert!(root.is_none());
    assert_eq!(output, "[line 1] Error at 'init': the height of the initial area cannot be negative.\n");
}

#[test]
fn empty_or() {
    let source = r"init(50, 50, 50, 50); {} or {}";
    let ParseResult { output, ctx: _, root } = parse_string(source);
    assert!(root.is_none());
    assert_eq!(output, "[line 1] Error at 'or': at most one alternative can be empty.\n");
}

#[test]
fn typo_in_or() {
    let source = r"init(50, 50, 50, 50); {} 10 { translation(0, 0) }";
    let ParseResult { output, ctx: _, root } = parse_string(source);
    assert!(root.is_none());
    assert_eq!(output, "[line 1] Error at '10': 'or' expected.\n[line 1] Error at '10': end of file expected.\n");
}

#[test]
fn empty_loop() {
    let source = r"init(50, 50, 50, 50); iter {}";
    let ParseResult { output, ctx: _, root } = parse_string(source);
    assert!(root.is_none());
    assert_eq!(output, "[line 1] Error at 'iter': the body of 'iter' must not be empty.\n");
}

#[test]
fn redundant_semicolon() {
    let source = r"init(50, 50, 50, 50); iter { translation(0, 0); }";
    let ParseResult { output, ctx: _, root } = parse_string(source);
    assert!(root.is_none());
    assert_eq!(output, "[line 1] Error at '}': redundant semicolon?\n[line 1] Error at '}': end of file expected.\n");

    let source = r"init(50, 50, 50, 50);";
    let ParseResult { output, ctx: _, root } = parse_string(source);
    assert!(root.is_none());
    assert_eq!(output, "[line 1] Error at end of file: redundant semicolon?\n");
}

#[test]
fn from_fuzzing() {
    let source = r"init";
    let ParseResult { output, ctx: _, root } = parse_string(source);
    assert!(root.is_none());
    assert_eq!(output, "[line 1] Error at end of file: '(' expected.\n");
}
