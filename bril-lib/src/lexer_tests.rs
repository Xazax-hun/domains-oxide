use super::lexer::*;
use utils::DiagnosticEmitter;

#[derive(Debug)]
struct LexTestResult {
    output: String,
    result: LexResult,
}

fn lex_string(source: &str) -> LexTestResult {
    let mut diag = DiagnosticEmitter::new(Box::new(Vec::new()), Box::new(Vec::new()));
    let lexer = Lexer::new(source, &mut diag);
    let tokens = lexer.lex_all();
    LexTestResult {
        output: diag.out_buffer().to_string() + diag.err_buffer(),
        result: tokens,
    }
}

fn to_token_values(tokens: Vec<Token>) -> Vec<TokenValue> {
    tokens.into_iter().map(|tok| tok.value).collect()
}

use TokenValue::*;

#[test]
fn test_empty_input() {
    let LexTestResult { output, result } = lex_string("");
    let expected = vec![EndOfFile];

    assert_eq!(to_token_values(result.tokens), expected);
    assert_eq!(output, "");

    let LexTestResult { output, result } = lex_string("  \n\t\n");
    let expected = vec![EndOfFile];

    assert_eq!(to_token_values(result.tokens), expected);
    assert_eq!(output, "");
}

#[test]
fn test_all_tokens() {
    let LexTestResult { output, result } = lex_string(
        r"ident @global ident .label 50 -50 add mul sub div eq lt gt le ge not and or
              jmp br call ret = (){} ;:, const print nop id int bool true false",
    );
    let expected = vec![
        Local(Identifier(0)),
        Global(Identifier(1)),
        Local(Identifier(0)),
        Label(Identifier(2)),
        Integer(50),
        Integer(-50),
        Add,
        Mul,
        Sub,
        Div,
        Equal,
        LessThan,
        GreaterThan,
        LessThanOrEq,
        GreaterThanOrEq,
        Not,
        And,
        Or,
        Jump,
        Branch,
        Call,
        Return,
        Define,
        LeftParen,
        RightParen,
        LeftBrace,
        RightBrace,
        Semicolon,
        Colon,
        Comma,
        Const,
        Print,
        Nop,
        Identity,
        Int,
        Bool,
        True,
        False,
        EndOfFile,
    ];

    assert_eq!(to_token_values(result.tokens), expected);
    assert_eq!(result.identifiers.0, vec!["ident", "@global", ".label"]);
    assert_eq!(output, "");
}

#[test]
fn test_comments() {
    let LexTestResult { output, result } =
        lex_string("0 // the rest is ignored\n\n//so is this\n  // and this");
    let expected = vec![Integer(0), EndOfFile];
    assert_eq!(to_token_values(result.tokens), expected);
    assert_eq!(output, "");

    let LexTestResult { output, result } =
        lex_string("0 # the rest is ignored\n\n#so is this\n  # and this");
    let expected = vec![Integer(0), EndOfFile];
    assert_eq!(to_token_values(result.tokens), expected);
    assert_eq!(output, "");

    let LexTestResult { output, result } =
        lex_string("/* foo */ 0 /* the rest * is */  /* ignored\n\n so is this\n  // and this */");
    let expected = vec![Integer(0), EndOfFile];
    assert_eq!(to_token_values(result.tokens), expected);
    assert_eq!(output, "");
}

#[test]
fn test_error_messages() {
    let LexTestResult { output, result } = lex_string("|");
    assert!(result.tokens.is_empty());
    assert_eq!(output, "[line 1] Error : Unexpected token: '|'.\n");

    let LexTestResult { output, result } = lex_string("@");
    assert!(result.tokens.is_empty());
    assert_eq!(output, "[line 1] Error : Unexpected token: '@'.\n");

    let LexTestResult { output, result } = lex_string(".@");
    assert!(result.tokens.is_empty());
    assert_eq!(output, "[line 1] Error : Unexpected token: '.'.\n");

    let LexTestResult { output, result } = lex_string("/and");
    assert!(result.tokens.is_empty());
    assert_eq!(output, "[line 1] Error : Unexpected token: '/'.\n");

    let LexTestResult { output, result } = lex_string("-or");
    assert!(result.tokens.is_empty());
    assert_eq!(output, "[line 1] Error : Expected number after '-'.\n");

    let LexTestResult { output, result } = lex_string("a̐");
    assert!(result.tokens.is_empty());
    assert_eq!(output, "[line 1] Error : Only ASCII input is supported.\n");
}
