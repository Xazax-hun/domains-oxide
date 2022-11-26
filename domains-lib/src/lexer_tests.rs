use super::lexer::*;
use utils::DiagnosticEmitter;

struct LexResult {
    output: String,
    tokens: Vec<Token>,
}

fn lex_string(source: &str) -> LexResult {
    let mut diag = DiagnosticEmitter::new(Box::new(Vec::new()), Box::new(Vec::new()));
    let lexer = Lexer::new(source, &mut diag);
    let tokens = lexer.lex_all();
    LexResult {
        output: diag.out_buffer().to_string() + diag.err_buffer(),
        tokens,
    }
}

fn to_token_values(tokens: Vec<Token>) -> Vec<TokenValue> {
    tokens.into_iter().map(|tok| tok.value).collect()
}

use TokenValue::*;

#[test]
fn test_empty_input() {
    let LexResult { output, tokens } = lex_string("");
    let expected = vec![EndOfFile];

    assert_eq!(to_token_values(tokens), expected);
    assert_eq!(output, "");

    let LexResult { output, tokens } = lex_string("  \n\t\n");
    let expected = vec![EndOfFile];

    assert_eq!(to_token_values(tokens), expected);
    assert_eq!(output, "");
}

#[test]
fn test_all_tokens() {
    let LexResult { output, tokens } = lex_string("{}(),;50 init translation rotation iter or");
    let expected = vec![
        LeftBrace,
        RightBrace,
        LeftParen,
        RightParen,
        Comma,
        Semicolon,
        Number(50),
        Init,
        Translation,
        Rotation,
        Iter,
        Or,
        EndOfFile,
    ];

    assert_eq!(to_token_values(tokens), expected);
    assert_eq!(output, "");
}

#[test]
fn test_numbers() {
    let LexResult { output, tokens } = lex_string("0 50 -0 -50");
    let expected = vec![Number(0), Number(50), Number(0), Number(-50), EndOfFile];

    assert_eq!(to_token_values(tokens), expected);
    assert_eq!(output, "");
}

#[test]
fn test_comments() {
    let LexResult { output, tokens } =
        lex_string("0 // the rest is ignored\n\n//so is this\n  // and this");
    let expected = vec![Number(0), EndOfFile];

    assert_eq!(to_token_values(tokens), expected);
    assert_eq!(output, "");

    let LexResult { output, tokens } =
        lex_string("/* foo */ 0 /* the rest * is */  /* ignored\n\n so is this\n  // and this */");
    let expected = vec![Number(0), EndOfFile];

    assert_eq!(to_token_values(tokens), expected);
    assert_eq!(output, "");
}

#[test]
fn test_error_messages() {
    let LexResult { output, tokens } = lex_string("|");
    assert!(tokens.is_empty());
    assert_eq!(output, "[line 1] Error : Unexpected token: '|'.\n");

    let LexResult { output, tokens } = lex_string("/iter");
    assert!(tokens.is_empty());
    assert_eq!(output, "[line 1] Error : Unexpected token: '/'.\n");

    let LexResult { output, tokens } = lex_string("-iter");
    assert!(tokens.is_empty());
    assert_eq!(output, "[line 1] Error : Expected number after '-'.\n");

    let LexResult { output, tokens } = lex_string("a̐");
    assert!(tokens.is_empty());
    assert_eq!(output, "[line 1] Error : Only ASCII input is supported.\n");
}
