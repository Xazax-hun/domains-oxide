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

fn from_char(c: char) -> Option<TokenValue> {
    match c {
        '(' => Some(LeftParen),
        ')' => Some(RightParen),
        '{' => Some(LeftBrace),
        '}' => Some(RightBrace),
        ',' => Some(Comma),
        ';' => Some(Semicolon),
        _ => None,
    }
}

impl TokenValue {
    pub fn to_num(self) -> i32 {
        match self {
            Number(n) => n,
            _ => panic!(),
        }
    }
}

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

#[derive(Debug, Clone, Copy)]
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

pub struct Lexer<'a> {
    source: &'a str,
    start: usize,
    current: usize,
    line_num: u32,
    has_error: bool, // TODO: can we get rid of this?
    diagnostic_emitter: &'a mut DiagnosticEmitter,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str, diag: &'a mut DiagnosticEmitter) -> Self {
        Lexer {
            source: src,
            start: 0,
            current: 0,
            line_num: 1,
            has_error: false,
            diagnostic_emitter: diag,
        }
    }

    pub fn lex_all(mut self) -> Vec<Token> {
        let mut result = Vec::new();

        while !self.is_at_end() {
            if let Some(tok) = self.lex() {
                result.push(tok);
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
            match self.advance() {
                // Unambiguous single character tokens.
                c @ ('(' | ')' | '{' | '}' | ',' | ';') => {
                    return Some(Token {
                        value: from_char(c).unwrap(),
                        line_num: self.line_num,
                    })
                }

                // Whitespace
                '\n' => {
                    self.line_num += 1;
                    continue;
                }
                ' ' | '\t' | '\r' => continue,

                // Comments
                '/' => {
                    if self.match_char('/') {
                        while self.advance() != '\n' && !self.is_at_end() {}
                        continue;
                    }
                    if self.match_char('*') {
                        loop {
                            while self.advance() != '*' && !self.is_at_end() {}

                            if self.is_at_end() {
                                self.diagnostic_emitter
                                    .error(self.line_num, "Multiline comment no closed.");
                                self.has_error = true;
                                return None;
                            }

                            if self.advance() == '/' {
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
                    if let n @ Some(_) = self.lex_number() {
                        return n;
                    }
                    self.diagnostic_emitter
                        .error(self.line_num, "Expected number after '-'.");
                    self.has_error = true;
                    return None;
                }
                c => {
                    if c.is_ascii_digit() {
                        return self.lex_number();
                    }
                    if let kw @ Some(_) = self.lex_keyword() {
                        return kw;
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
    }

    fn lex_number(&mut self) -> Option<Token> {
        while self.peek().is_ascii_digit() {
            self.advance();
        }

        let value: i32 = self.source[self.start..self.current].parse().ok()?;

        Some(Token {
            value: Number(value),
            line_num: self.line_num,
        })
    }

    fn lex_keyword(&mut self) -> Option<Token> {
        while self.peek().is_ascii_alphabetic() {
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

    fn peek(&self) -> char {
        self.source.chars().nth(self.current).map_or('\0', |x| x)
    }

    fn advance(&mut self) -> char {
        let prev = self.peek();
        self.current += 1;
        prev
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.source.chars().nth(self.current) == Some(expected) {
            self.current += 1;
            true
        } else {
            false
        }
    }
}
