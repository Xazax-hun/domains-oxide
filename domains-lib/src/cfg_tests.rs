use super::ast::*;
use super::cfg::*;

use super::lexer::*;
use super::parser::*;
use utils::DiagnosticEmitter;

struct ParseResult {
    output: String,
    ctx: ASTContext,
    cfg: Cfg,
}

fn parse_string(source: &str) -> Option<ParseResult> {
    let errors: Box<Vec<u8>> = Box::new(Vec::new());
    let regular: Box<Vec<u8>> = Box::new(Vec::new());
    let mut diag = DiagnosticEmitter::new(regular, errors);
    let mut lexer = Lexer::new(source, &mut diag);
    let tokens = lexer.lex_all();
    let mut parser = Parser::new(tokens, &mut diag);
    let ctx = parser.parse()?;
    let cfg = Cfg::new(&ctx);
    let out = std::str::from_utf8(diag.out_buffer()).unwrap();
    let err = std::str::from_utf8(diag.err_buffer()).unwrap();
    Some(ParseResult {
        output: out.to_string() + err,
        ctx,
        cfg,
    })
}

#[test]
fn basic_cfg_printed() {
    let source = r"init(50, 50, 50, 50);
translation(10, 0);
iter {
  {
    translation(10, 0)
  } or {
    rotation(0, 0, 90)
  }
}";
    let ParseResult { output, ctx, cfg } = parse_string(source).unwrap();
    assert!(output.is_empty());
    let pretty_printed = crate::cfg::print(&cfg, &ctx);
    let expected = r#"digraph CFG {
  Node_0[label="init(50, 50, 50, 50)\ntranslation(10, 0)\n"]
  Node_1[label=""]
  Node_2[label="translation(10, 0)\n"]
  Node_3[label="rotation(0, 0, 90)\n"]
  Node_4[label=""]
  Node_5[label=""]

  Node_0 -> Node_1
  Node_1 -> Node_2
  Node_1 -> Node_3
  Node_2 -> Node_4
  Node_3 -> Node_4
  Node_4 -> Node_1
  Node_4 -> Node_5
}
"#;
    assert_eq!(expected, pretty_printed);
}

#[test]
fn more_nested_cfg_printed() {
    let source = r"init(50, 50, 50, 50);
translation(10, 0);
iter {
  iter {
    translation(10, 0)
  };
  {
    translation(10, 0)
  } or {
    {
      translation(10, 0)
    } or {
      iter {
        rotation(0, 0, 90)
      }
    }
  }
}";
    let ParseResult { output, ctx, cfg } = parse_string(source).unwrap();
    assert!(output.is_empty());
    let pretty_printed = crate::cfg::print(&cfg, &ctx);
    let expected = r#"digraph CFG {
  Node_0[label="init(50, 50, 50, 50)\ntranslation(10, 0)\n"]
  Node_1[label=""]
  Node_2[label="translation(10, 0)\n"]
  Node_3[label=""]
  Node_4[label="translation(10, 0)\n"]
  Node_5[label=""]
  Node_6[label="translation(10, 0)\n"]
  Node_7[label=""]
  Node_8[label="rotation(0, 0, 90)\n"]
  Node_9[label=""]
  Node_10[label=""]
  Node_11[label=""]
  Node_12[label=""]

  Node_0 -> Node_1
  Node_1 -> Node_2
  Node_2 -> Node_2
  Node_2 -> Node_3
  Node_3 -> Node_4
  Node_3 -> Node_5
  Node_4 -> Node_11
  Node_5 -> Node_6
  Node_5 -> Node_7
  Node_6 -> Node_10
  Node_7 -> Node_8
  Node_8 -> Node_8
  Node_8 -> Node_9
  Node_9 -> Node_10
  Node_10 -> Node_11
  Node_11 -> Node_1
  Node_11 -> Node_12
}
"#;
    assert_eq!(expected, pretty_printed);
}
