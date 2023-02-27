use super::lexer::*;
use utils::DiagnosticEmitter;

fn lex_string(source: &str) -> Result<Vec<Token>, String> {
    let mut diag = DiagnosticEmitter::new(Box::new(Vec::new()), Box::new(Vec::new()));
    let lexer = Lexer::new(source, &mut diag);
    let tokens = lexer.lex_all();
    if tokens.is_empty() {
        Err(diag.out_buffer().to_string() + diag.err_buffer())
    } else {
        Ok(tokens)
    }
}

fn to_token_values(tokens: Vec<Token>) -> Vec<TokenValue> {
    tokens.into_iter().map(|tok| tok.value).collect()
}

use TokenValue::*;

#[test]
fn test_empty_input() -> Result<(), String> {
    let tokens = lex_string("")?;
    assert_eq!(to_token_values(tokens), vec![EndOfFile]);

    let tokens = lex_string("  \n\t\n")?;
    assert_eq!(to_token_values(tokens), vec![EndOfFile]);

    Ok(())
}

#[test]
fn test_all_tokens() -> Result<(), String> {
    let tokens = lex_string("{}(),;50 init translation rotation iter or")?;
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
    Ok(())
}

#[test]
fn test_numbers() -> Result<(), String> {
    let tokens = lex_string("0 50 -0 -50")?;
    let expected = vec![Number(0), Number(50), Number(0), Number(-50), EndOfFile];

    assert_eq!(to_token_values(tokens), expected);
    Ok(())
}

#[test]
fn test_comments() -> Result<(), String> {
    let tokens = lex_string("0 // the rest is ignored\n\n//so is this\n  // and this")?;
    assert_eq!(to_token_values(tokens), vec![Number(0), EndOfFile]);

    let tokens =
        lex_string("/* foo */ 0 /* the rest * is */  /* ignored\n\n so is this\n  // and this */")?;
    assert_eq!(to_token_values(tokens), vec![Number(0), EndOfFile]);

    Ok(())
}

#[test]
fn test_error_messages() {
    let output = lex_string("|").expect_err("");
    assert_eq!(output, "[line 1] Error : Unexpected token: '|'.\n");

    let output = lex_string("/iter").expect_err("");
    assert_eq!(output, "[line 1] Error : Unexpected token: '/'.\n");

    let output = lex_string("-iter").expect_err("");
    assert_eq!(output, "[line 1] Error : Expected number after '-'.\n");

    let output = lex_string("a̐").expect_err("");
    assert_eq!(output, "[line 1] Error : Only ASCII input is supported.\n");
}
