use super::lexer::*;
use utils::DiagnosticEmitter;

use TokenValue::*;

struct LexResult {
    output: String,
    tokens: Vec<Token>,
}

fn lex_string(source: &str) -> LexResult {
    let errors: Vec<u8> = Vec::new();
    let regular: Vec<u8> = Vec::new();
    let mut diag = DiagnosticEmitter::new(regular, errors);
    let mut lexer = Lexer::new(source, &mut diag);
    let tokens = lexer.lex_all();
    let out = std::str::from_utf8(diag.out.buffer()).unwrap();
    let err = std::str::from_utf8(diag.err.buffer()).unwrap();
    LexResult {
        output: out.to_string() + err,
        tokens,
    }
}

fn to_token_values(tokens: Vec<Token>) -> Vec<TokenValue> {
    tokens.into_iter().map(|tok| tok.value).collect()
}

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
    let LexResult { output, tokens } = lex_string("0 // the rest is ignored\n\n//so is this\n  // and this");
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
    let LexResult { output, tokens } = lex_string("-iter");
    assert!(tokens.is_empty());
    assert_eq!(output, "[line 1] Error : Expected number after '-'.\n");
}
