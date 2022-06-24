use std::collections::HashMap;
use std::fmt;
use utils::DiagnosticEmitter;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenValue {
    // Single-character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Semicolon,

    // Literals
    Number(i32),

    // Keywords
    Init,
    Translation,
    Rotation,
    Iter,
    Or,

    EndOfFile,
}

use TokenValue::*;

impl fmt::Display for TokenValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            LeftParen => write!(f, "("),
            RightParen => write!(f, ")"),
            LeftBrace => write!(f, "{{"),
            RightBrace => write!(f, "}}"),
            Comma => write!(f, ","),
            Semicolon => write!(f, ";"),
            Number(n) => write!(f, "{n}"),
            Init => write!(f, "init"),
            Translation => write!(f, "translation"),
            Rotation => write!(f, "rotation"),
            Iter => write!(f, "iter"),
            Or => write!(f, "or"),
            EndOfFile => write!(f, "END_OF_FILE"),
        }
    }
}

lazy_static! {
    static ref KEYWORDS: HashMap<String, TokenValue> = {
        let mut m = HashMap::new();
        m.insert(format!("{}", Init), Init);
        m.insert(format!("{}", Or), Or);
        m.insert(format!("{}", Translation), Translation);
        m.insert(format!("{}", Rotation), Rotation);
        m.insert(format!("{}", Iter), Iter);
        m
    };
}

#[derive(Debug)]
pub struct Token {
    pub value: TokenValue,

    // TODO: add better location info: could be an index into a
    //       table that has the line number, column number and a
    //       file path.
    pub line_num: u32,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

pub struct Lexer<'a, W: std::io::Write> {
    source: String,
    start: usize,
    current: usize,
    line_num: u32,
    has_error: bool,
    diagnostic_emitter: &'a mut DiagnosticEmitter<W>,
}

impl<'a, W: std::io::Write> Lexer<'a, W> {
    pub fn new(src: String, diag: &'a mut DiagnosticEmitter<W>) -> Lexer<W> {
        Lexer {
            source: src,
            start: 0,
            current: 0,
            line_num: 1,
            has_error: false,
            diagnostic_emitter: diag,
        }
    }

    pub fn lex_all(&mut self) -> Vec<Token> {
        let mut result = Vec::new();

        while !self.is_at_end() {
            if let Some(tok) = self.lex() {
                result.push(tok)
            } else if self.has_error {
                return Vec::new();
            }
        }

        result.push(Token {
            value: EndOfFile,
            line_num: self.line_num,
        });
        result
    }

    fn lex(&mut self) -> Option<Token> {
        loop {
            if self.is_at_end() {
                return None;
            }

            self.start = self.current;
            let c = self.advance();
            match c {
                // Unambiguous single character tokens.
                '(' => {
                    return Some(Token {
                        value: LeftParen,
                        line_num: self.line_num,
                    })
                }
                ')' => {
                    return Some(Token {
                        value: RightParen,
                        line_num: self.line_num,
                    })
                }
                '{' => {
                    return Some(Token {
                        value: LeftBrace,
                        line_num: self.line_num,
                    })
                }
                '}' => {
                    return Some(Token {
                        value: RightBrace,
                        line_num: self.line_num,
                    })
                }
                ',' => {
                    return Some(Token {
                        value: Comma,
                        line_num: self.line_num,
                    })
                }
                ';' => {
                    return Some(Token {
                        value: Semicolon,
                        line_num: self.line_num,
                    })
                }

                // Whitespace
                '\n' => {
                    self.line_num += 1;
                    break;
                }
                ' ' | '\t' | '\r' => continue,

                // Comments
                '/' => {
                    if self.match_char('/') {
                        while self.peek() != '\n' && !self.is_at_end() {
                            self.advance();
                        }
                        continue;
                    }
                    if self.match_char('*') {
                        loop {
                            while self.peek() != '*' && !self.is_at_end() {
                                self.advance();
                            }

                            if self.is_at_end() {
                                self.diagnostic_emitter
                                    .error(self.line_num, "Multiline comment no closed.");
                                self.has_error = true;
                                return None;
                            }

                            self.advance();
                            if self.peek() == '/' {
                                self.advance();
                                break;
                            }
                        }
                        continue;
                    }
                    self.diagnostic_emitter.error(
                        self.line_num,
                        &format!(
                            "Unexpected token: '{}'.",
                            &self.source[self.start..self.current]
                        ),
                    );
                    self.has_error = true;
                    return None;
                }

                // Negative numbers
                '-' => {
                    if let Some(num) = self.lex_number() {
                        return Some(num);
                    }
                    self.diagnostic_emitter
                        .error(self.line_num, "Expected number after '-'.");
                    self.has_error = true;
                    return None;
                }
                _ => {
                    if c.is_digit(10) {
                        return self.lex_number();
                    }
                    if let Some(kw) = self.lex_keyword() {
                        return Some(kw);
                    }
                    self.diagnostic_emitter.error(
                        self.line_num,
                        &format!(
                            "Unexpected token: '{}'.",
                            &self.source[self.start..self.current]
                        ),
                    );
                    self.has_error = true;
                    return None;
                }
            }
        }
        None
    }

    fn lex_number(&mut self) -> Option<Token> {
        while self.peek().is_digit(10) {
            self.advance();
        }

        let value = self.source[self.start..self.current].parse::<i32>().unwrap();

        Some(Token {
            value: Number(value),
            line_num: self.line_num,
        })
    }

    fn lex_keyword(&mut self) -> Option<Token> {
        while self.peek().is_alphabetic() {
            self.advance();
        }

        let text = &self.source[self.start..self.current];
        if let Some(value) = KEYWORDS.get(text) {
            Some(Token {
                value: *value,
                line_num: self.line_num,
            })
        } else {
            None
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn advance(&mut self) -> char {
        let prev = self.peek();
        self.current += 1;
        prev
    }

    fn peek(&self) -> char {
        if let Some(ch) = self.source.chars().nth(self.current) {
            ch
        } else {
            '\0'
        }
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.source.chars().nth(self.current) != Some(expected) {
            false
        } else {
            self.current += 1;
            true
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct LexResult {
        output: String,
        tokens: Vec<Token>,
    }

    fn lex_string(source: &str) -> LexResult {
        let errors: Vec<u8> = Vec::new();
        let regular: Vec<u8> = Vec::new();
        let mut diag = DiagnosticEmitter::new(regular, errors);
        let mut lexer = Lexer::new(source.to_string(), &mut diag);
        let tokens = lexer.lex_all();
        let out = std::str::from_utf8(diag.out.buffer()).unwrap();
        let err = std::str::from_utf8(diag.err.buffer()).unwrap();
        LexResult {
            output: out.to_string() + &err.to_string(),
            tokens,
        }
    }

    fn to_token_values(tokens: Vec<Token>) -> Vec<TokenValue> {
        tokens.into_iter().map(|tok| tok.value).collect()
    }

    #[test]
    fn test_empty_input() {
        let result = lex_string("");
        let expected = vec![EndOfFile];

        assert_eq!(to_token_values(result.tokens), expected);
        assert_eq!(result.output, "");

        let result = lex_string("  \n\t\n");
        let expected = vec![EndOfFile];

        assert_eq!(to_token_values(result.tokens), expected);
        assert_eq!(result.output, "");
    }

    #[test]
    fn test_all_tokens() {
        let result = lex_string("{}(),;50 init translation rotation iter or");
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

        assert_eq!(to_token_values(result.tokens), expected);
        assert_eq!(result.output, "");
    }

    #[test]
    fn test_numbers() {
        let result = lex_string("0 50 -0 -50");
        let expected = vec![Number(0), Number(50), Number(0), Number(-50), EndOfFile];

        assert_eq!(to_token_values(result.tokens), expected);
        assert_eq!(result.output, "");
    }

    #[test]
    fn test_comments() {
        let result = lex_string("0 // the rest is ignored\n\n//so is this\n  // and this");
        let expected = vec![Number(0), EndOfFile];

        assert_eq!(to_token_values(result.tokens), expected);
        assert_eq!(result.output, "");

        let result = lex_string("/* foo */ 0 /* the rest * is */  /* ignored\n\n so is this\n  // and this */");
        let expected = vec![Number(0), EndOfFile];

        assert_eq!(to_token_values(result.tokens), expected);
        assert_eq!(result.output, "");
    }

    #[test]
    fn test_error_messages() {
        let result = lex_string("|");
        assert!(result.tokens.is_empty());
        assert_eq!(result.output, "[line 1] Error : Unexpected token: '|'.\n");
    }
}
