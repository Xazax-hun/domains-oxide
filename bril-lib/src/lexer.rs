use std::collections::HashMap;
use utils::DiagnosticEmitter;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Identifier(pub usize);

#[derive(Clone, Debug, Copy, Eq, PartialEq, Hash)]
pub struct Location(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenValue {
    Local(Identifier),
    Global(Identifier),
    Label(Identifier),
    Integer(i32),

    // Arithmetic
    Add,
    Mul,
    Sub,
    Div,
    Mod,

    // Logic
    True,
    False,
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
    Comma,

    // Builtin types,
    Int,
    Bool,

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
        ',' => Some(Comma),
        _ => None,
    }
}

impl core::fmt::Display for TokenValue {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match *self {
            Local(i) => write!(f, "local_{}", i.0),
            Global(i) => write!(f, "global_{}", i.0),
            Label(i) => write!(f, "label_{}", i.0),
            Integer(i) => write!(f, "{i}"),

            Add => write!(f, "add"),
            Mul => write!(f, "mul"),
            Sub => write!(f, "sub"),
            Div => write!(f, "div"),
            Mod => write!(f, "mod"),

            True => write!(f, "true"),
            False => write!(f, "false"),
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
            Comma => write!(f, ","),

            Const => write!(f, "const"),
            Print => write!(f, "print"),
            Nop => write!(f, "nop"),
            Identity => write!(f, "id"),

            Int => write!(f, "int"),
            Bool => write!(f, "bool"),

            EndOfFile => write!(f, "END_OF_FILE"),
        }
    }
}

lazy_static! {
    static ref KEYWORDS: HashMap<String, TokenValue> = {
        let mut m = HashMap::new();
        for kw in [Add, Mul, Div, Sub, Mod] {
            m.insert(kw.to_string(), kw);
        }

        for kw in [
            True,
            False,
            Equal,
            LessThan,
            GreaterThan,
            LessThanOrEq,
            GreaterThanOrEq,
            Not,
            And,
            Or,
        ] {
            m.insert(kw.to_string(), kw);
        }

        for kw in [Jump, Branch, Call, Return] {
            m.insert(kw.to_string(), kw);
        }

        for kw in [Const, Print, Nop, Identity] {
            m.insert(kw.to_string(), kw);
        }

        for kw in [Int, Bool] {
            m.insert(kw.to_string(), kw);
        }
        m
    };
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token {
    pub value: TokenValue,

    pub line_num: Location,
}

impl Token {
    pub fn error(&self, diag: &mut DiagnosticEmitter, s: &str) {
        if self.value == EndOfFile {
            diag.report(self.line_num.0, "at end of file", s);
        } else {
            diag.report(self.line_num.0, &format!("at '{self}'"), s);
        }
    }
}

impl core::fmt::Display for Token {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Clone, Default)]
pub struct IdentifierTable(pub Vec<String>);

impl IdentifierTable {
    pub fn lookup(&self, ident: &str) -> Option<Identifier> {
        // TODO: more efficient lookup.
        self.0.iter().position(|str| str == ident).map(Identifier)
    }

    fn get_identifier(&mut self, ident: &str) -> Identifier {
        if let Some(id) = self.lookup(ident) {
            id
        } else {
            self.0.push(ident.to_owned());
            Identifier(self.0.len() - 1)
        }
    }

    pub fn get_name(&self, id: Identifier) -> &str {
        &self.0[id.0]
    }
}

pub struct Lexer<'src> {
    source: &'src str,
    start: usize,
    current: usize,
    line_num: u32,
    has_error: bool, // TODO: can we get rid of this?
    diagnostic_emitter: &'src mut DiagnosticEmitter,
    identifiers: IdentifierTable,
}

#[derive(Debug, Clone, Default)]
pub struct LexResult {
    pub tokens: Vec<Token>,
    pub identifiers: IdentifierTable,
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
            identifiers: IdentifierTable::default(),
        }
    }

    pub fn lex_all(mut self) -> LexResult {
        // TODO: better support for unicode:
        //   * Point out where the non-ascii character is
        //   * Allow any non-control characters in comments.
        //   * Avoid multiple passes over the input.
        if !self.source.is_ascii() {
            self.diagnostic_emitter
                .error(self.line_num, "Only ASCII input is supported.");
            return LexResult::default();
        }

        let mut tokens = Vec::new();
        while !self.is_at_end() {
            if let Some(tok) = self.lex() {
                tokens.push(tok);
            } else if self.has_error {
                return LexResult::default();
            }
        }

        tokens.push(Token {
            value: EndOfFile,
            line_num: Location(self.line_num),
        });

        LexResult {
            tokens,
            identifiers: self.identifiers,
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
                c @ ('=' | '(' | ')' | '{' | '}' | ':' | ';' | ',') => {
                    return Some(Token {
                        value: from_char(c).unwrap(),
                        line_num: Location(self.line_num),
                    })
                }

                // Whitespace
                '\n' => {
                    self.line_num += 1;
                    continue;
                }
                ' ' | '\t' | '\r' => continue,

                // Comments
                '#' => {
                    while self.advance() != '\n' && !self.is_at_end() {}
                    continue;
                }
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
                c @ ('@' | '.') => {
                    if self.peek().is_ascii_alphabetic() {
                        let ident = self.lex_identifier();
                        let value = if c == '@' {
                            Global(self.identifiers.get_identifier(ident))
                        } else {
                            Label(self.identifiers.get_identifier(ident))
                        };
                        return Some(Token {
                            value,
                            line_num: Location(self.line_num),
                        });
                    }
                    self.diagnostic_emitter
                        .error(self.line_num, &format!("Unexpected token: '{c}'."));
                    self.has_error = true;
                    return None;
                }
                c => {
                    if c.is_ascii_digit() {
                        return self.lex_number();
                    }
                    if c.is_ascii_alphabetic() {
                        let ident = self.lex_identifier();
                        let line_num = self.line_num;
                        return Some(KEYWORDS.get(ident).map_or_else(
                            || Token {
                                value: Local(self.identifiers.get_identifier(ident)),
                                line_num: Location(line_num),
                            },
                            |value| Token {
                                value: *value,
                                line_num: Location(line_num),
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
            line_num: Location(self.line_num),
        })
    }

    fn lex_identifier(&mut self) -> &'src str {
        while self.peek().is_ascii_alphanumeric() || self.peek() == '_' {
            self.advance();
        }

        &self.source[self.start..self.current]
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
