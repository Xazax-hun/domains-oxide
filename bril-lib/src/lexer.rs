use std::collections::HashMap;
use utils::DiagnosticEmitter;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenValue {
    Identifier(usize),
    Integer(i32),

    // Arithmetic
    Add,
    Mul,
    Sub,
    Div,

    // Logic
    Equal,
    LessThan,
    GreaterThan,
    LessThanOrEq,
    GreaterThanOrEq,
    Not,
    And,
    Or,

    // Control flow
    Jump,
    Branch,
    Call,
    Return,

    // Separators
    Define,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Colon,
    Semicolon,

    // Misc
    Const,
    Print,
    Nop,
    Identity,

    EndOfFile,
}

use TokenValue::*;

fn from_char(c: char) -> Option<TokenValue> {
    match c {
        '(' => Some(LeftParen),
        ')' => Some(RightParen),
        '{' => Some(LeftBrace),
        '}' => Some(RightBrace),
        ':' => Some(Colon),
        ';' => Some(Semicolon),
        '=' => Some(Define),
        _ => None,
    }
}

impl core::fmt::Display for TokenValue {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match *self {
            Identifier(i) => write!(f, "ident_{}", i),
            Integer(i) => write!(f, "{}", i),

            Add => write!(f, "add"),
            Mul => write!(f, "mul"),
            Sub => write!(f, "sub"),
            Div => write!(f, "div"),

            Equal => write!(f, "eq"),
            LessThan => write!(f, "lt"),
            GreaterThan => write!(f, "gt"),
            LessThanOrEq => write!(f, "le"),
            GreaterThanOrEq => write!(f, "ge"),
            Not => write!(f, "not"),
            And => write!(f, "and"),
            Or => write!(f, "or"),

            Jump => write!(f, "jmp"),
            Branch => write!(f, "br"),
            Call => write!(f, "call"),
            Return => write!(f, "ret"),

            Define => write!(f, "="),
            LeftParen => write!(f, "("),
            RightParen => write!(f, ")"),
            LeftBrace => write!(f, "{{"),
            RightBrace => write!(f, "}}"),
            Colon => write!(f, ":"),
            Semicolon => write!(f, ";"),

            Const => write!(f, "const"),
            Print => write!(f, "print"),
            Nop => write!(f, "nop"),
            Identity => write!(f, "id"),

            EndOfFile => write!(f, "END_OF_FILE"),
        }
    }
}

lazy_static! {
    static ref KEYWORDS: HashMap<String, TokenValue> = {
        let mut m = HashMap::new();
        m.insert(format!("{Add}"), Add);
        m.insert(format!("{Mul}"), Mul);
        m.insert(format!("{Div}"), Div);
        m.insert(format!("{Sub}"), Sub);

        m.insert(format!("{Equal}"), Equal);
        m.insert(format!("{LessThan}"), LessThan);
        m.insert(format!("{GreaterThan}"), GreaterThan);
        m.insert(format!("{LessThanOrEq}"), LessThanOrEq);
        m.insert(format!("{GreaterThanOrEq}"), GreaterThanOrEq);
        m.insert(format!("{Not}"), Not);
        m.insert(format!("{And}"), And);
        m.insert(format!("{Or}"), Or);

        m.insert(format!("{Jump}"), Jump);
        m.insert(format!("{Branch}"), Branch);
        m.insert(format!("{Call}"), Call);
        m.insert(format!("{Return}"), Return);

        m.insert(format!("{Const}"), Const);
        m.insert(format!("{Print}"), Print);
        m.insert(format!("{Nop}"), Nop);
        m.insert(format!("{Identity}"), Identity);
        m
    };
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token {
    pub value: TokenValue,

    pub line_num: u32,
}

impl core::fmt::Display for Token {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        write!(f, "{}", self.value)
    }
}

pub struct Lexer<'src> {
    source: &'src str,
    start: usize,
    current: usize,
    line_num: u32,
    has_error: bool, // TODO: can we get rid of this?
    diagnostic_emitter: &'src mut DiagnosticEmitter,
    identifier_table: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct LexResult {
    pub tokens: Vec<Token>,
    pub identifier_table: Vec<String>,
}

impl<'src> Lexer<'src> {
    pub fn new(source: &'src str, diagnostic_emitter: &'src mut DiagnosticEmitter) -> Self {
        Lexer {
            source,
            start: 0,
            current: 0,
            line_num: 1,
            has_error: false,
            diagnostic_emitter,
            identifier_table: Vec::default(),
        }
    }

    pub fn lex_all(mut self) -> LexResult {
        let mut tokens = Vec::new();

        // TODO: better support for unicode:
        //   * Point out where the non-ascii character is
        //   * Allow any non-control characters in comments.
        //   * Avoid multiple passes over the input.
        if !self.source.is_ascii() {
            self.diagnostic_emitter
                .error(self.line_num, "Only ASCII input is supported.");
            return LexResult {
                tokens,
                identifier_table: self.identifier_table,
            };
        }

        while !self.is_at_end() {
            if let Some(tok) = self.lex() {
                tokens.push(tok);
            } else if self.has_error {
                return LexResult {
                    tokens,
                    identifier_table: self.identifier_table,
                };
            }
        }

        tokens.push(Token {
            value: EndOfFile,
            line_num: self.line_num,
        });

        LexResult {
            tokens,
            identifier_table: self.identifier_table,
        }
    }

    fn lex(&mut self) -> Option<Token> {
        loop {
            if self.is_at_end() {
                return None;
            }

            self.start = self.current;
            match self.advance() {
                // Unambiguous single character tokens.
                c @ ('=' | '(' | ')' | '{' | '}' | ':' | ';') => {
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
                '@' => {
                    self.advance();
                    if let Some(ident) = self.lex_identifier() {
                        return Some(Token {
                            value: Identifier(self.get_identifier(ident)),
                            line_num: self.line_num,
                        });
                    }
                    self.diagnostic_emitter
                        .error(self.line_num, "Unexpected token: '@'.");
                    self.has_error = true;
                    return None;
                }
                c => {
                    if c.is_ascii_digit() {
                        return self.lex_number();
                    }
                    if let Some(ident) = self.lex_identifier() {
                        let line_num = self.line_num;
                        return Some(KEYWORDS.get(ident).map_or_else(
                            || Token {
                                value: Identifier(self.get_identifier(ident)),
                                line_num,
                            },
                            |value| Token {
                                value: *value,
                                line_num,
                            },
                        ));
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
            value: Integer(value),
            line_num: self.line_num,
        })
    }

    fn lex_identifier(&mut self) -> Option<&'src str> {
        if !self.peek().is_ascii_alphabetic() {
            return None;
        }
        while self.peek().is_ascii_alphabetic() {
            self.advance();
        }

        Some(&self.source[self.start..self.current])
    }

    fn get_identifier(&mut self, ident: &str) -> usize {
        match self.identifier_table.iter().position(|str| str == ident) {
            Some(pos) => pos,
            _ => {
                // TODO: more efficient lookup.
                self.identifier_table.push(ident.to_owned());
                self.identifier_table.len() - 1
            }
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