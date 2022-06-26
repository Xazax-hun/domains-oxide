use utils::DiagnosticEmitter;

use crate::{
    ast::*,
    lexer::{Token, TokenValue},
};

pub struct Parser<'a, W: std::io::Write> {
    ctx: ASTContext,
    tokens: Vec<Token>,
    current: usize,
    diag: &'a mut DiagnosticEmitter<W>,
}

impl<'a, W: std::io::Write> Parser<'a, W> {
    pub fn new(tokens: Vec<Token>, diag: &'a mut DiagnosticEmitter<W>) -> Parser<'a, W> {
        Parser {
            ctx: ASTContext::new(),
            tokens,
            current: 0,
            diag,
        }
    }

    pub fn parse(&mut self) -> (Option<Node>, ASTContext) {
        let result = self.sequence(true);
        let ctx = std::mem::replace(&mut self.ctx, ASTContext::new());
        if !self.is_at_end() {
            self.error(self.peek(), "end of file expected.");
            return (None, ctx);
        }
        (result, ctx)
    }

    pub fn get_context(&self) -> &ASTContext {
        &self.ctx
    }

    fn sequence(&mut self, root: bool) -> Option<Node> {
        if root && !self.check(TokenValue::Init) {
            self.error(
                self.peek(),
                "'init' expected at the beginning of the program.",
            );
            return None;
        }
        let mut commands = Vec::new();
        loop {
            let com = self.command()?;
            commands.push(com);

            if !self.match_tokens(&[TokenValue::Semicolon]) {
                break;
            }
        }
        Some(self.ctx.make_sequence(Sequence { nodes: commands }))
    }

    fn branch(&mut self) -> Option<Node> {
        // Allow empty sequences in alternatives.
        let lhs = if self.check(TokenValue::RightBrace) {
            self.ctx.make_sequence(Sequence { nodes: Vec::new() })
        } else {
            self.sequence(false)?
        };

        self.consume(TokenValue::RightBrace, "'}' expected.")?;
        let kw = self.consume(TokenValue::Or, "'or' expected.")?;
        self.consume(TokenValue::LeftBrace, "'{' expected.")?;

        let rhs = if self.check(TokenValue::RightBrace) {
            self.ctx.make_sequence(Sequence { nodes: Vec::new() })
        } else {
            self.sequence(false)?
        };
        self.consume(TokenValue::RightBrace, "'}' expected.")?;

        if let (NodeRef::Sequence(lhs_seq), NodeRef::Sequence(rhs_seq)) =
            (self.ctx.node_to_ref(lhs), self.ctx.node_to_ref(rhs))
        {
            if lhs_seq.nodes.is_empty() && rhs_seq.nodes.is_empty() {
                self.error(kw, "at most one alternative can be empty.");
                return None;
            }
        } else {
            panic!()
        }

        Some(self.ctx.make_branch(Branch { kw, lhs, rhs }))
    }

    fn loop_(&mut self) -> Option<Node> {
        let kw = self.previous();
        self.consume(TokenValue::LeftBrace, "'{' expected.")?;

        if self.match_tokens(&[TokenValue::RightBrace]) {
            self.error(kw, "the body of 'iter' must not be empty.");
            return None;
        }

        let body = self.sequence(false)?;
        self.consume(TokenValue::RightBrace, "'}' expected.")?;

        Some(self.ctx.make_loop(Loop { kw, body }))
    }

    fn command(&mut self) -> Option<Node> {
        if self.match_tokens(&[TokenValue::Init]) {
            let kw = self.previous();
            self.consume(TokenValue::LeftParen, "'(' expected.")?;
            let bot_x = self.consume(TokenValue::Number(0), "number expected.")?;
            self.consume(TokenValue::Comma, "',' expected.")?;
            let bot_y = self.consume(TokenValue::Number(0), "number expected.")?;
            self.consume(TokenValue::Comma, "',' expected.")?;
            let width = self.consume(TokenValue::Number(0), "number expected.")?;
            self.consume(TokenValue::Comma, "',' expected.")?;
            let height = self.consume(TokenValue::Number(0), "number expected.")?;
            self.consume(TokenValue::RightParen, "')' expected.")?;

            if width.value.to_num() < 0 {
                self.error(kw, "the width of the initial area cannot be negative.");
                return None;
            }

            if height.value.to_num() < 0 {
                self.error(kw, "the height of the initial area cannot be negative.");
                return None;
            }

            return Some(self.ctx.make_init(Init {
                kw,
                bottom_left: NumPair { x: bot_x, y: bot_y },
                size: NumPair {
                    x: width,
                    y: height,
                },
            }));
        }
        if self.match_tokens(&[TokenValue::Translation]) {
            let kw = self.previous();
            self.consume(TokenValue::LeftParen, "'(' expected.")?;
            let x = self.consume(TokenValue::Number(0), "number expected.")?;
            self.consume(TokenValue::Comma, "',' expected.")?;
            let y = self.consume(TokenValue::Number(0), "number expected.")?;
            self.consume(TokenValue::RightParen, "')' expected.")?;

            return Some(self.ctx.make_translation(Translation {
                kw,
                vector: NumPair { x, y },
            }));
        }
        if self.match_tokens(&[TokenValue::Rotation]) {
            let kw = self.previous();
            self.consume(TokenValue::LeftParen, "'(' expected.")?;
            let x = self.consume(TokenValue::Number(0), "number expected.")?;
            self.consume(TokenValue::Comma, "',' expected.")?;
            let y = self.consume(TokenValue::Number(0), "number expected.")?;
            self.consume(TokenValue::Comma, "',' expected.")?;
            let deg = self.consume(TokenValue::Number(0), "number expected.")?;
            self.consume(TokenValue::RightParen, "')' expected.")?;

            return Some(self.ctx.make_rotation(Rotation {
                kw,
                origin: NumPair { x, y },
                deg,
            }));
        }
        if self.match_tokens(&[TokenValue::Iter]) {
            return self.loop_();
        }
        if self.match_tokens(&[TokenValue::LeftBrace]) {
            return self.branch();
        }

        if self.is_at_end() || self.check(TokenValue::RightBrace) {
            self.error(self.peek(), "redundant semicolon?");
        }

        None
    }

    fn peek(&self) -> Token {
        self.tokens[self.current]
    }

    fn previous(&self) -> Token {
        self.tokens[self.current - 1]
    }

    fn is_at_end(&self) -> bool {
        matches!(self.peek().value, TokenValue::EndOfFile)
    }

    fn check(&self, tok_val: TokenValue) -> bool {
        if self.is_at_end() {
            false
        } else {
            std::mem::discriminant(&self.peek().value) == std::mem::discriminant(&tok_val)
        }
    }

    fn match_tokens(&mut self, tok_vals: &[TokenValue]) -> bool {
        if tok_vals.iter().any(|val| self.check(*val)) {
            self.advance();
            return true;
        }
        false
    }

    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn consume(&mut self, tok_val: TokenValue, s: &str) -> Option<Token> {
        if self.check(tok_val) {
            return Some(self.advance());
        }
        self.error(self.peek(), s);
        None
    }

    fn error(&mut self, tok: Token, s: &str) {
        if tok.value == TokenValue::EndOfFile {
            self.diag.report(tok.line_num, "at end of file", s)
        } else {
            self.diag.report(tok.line_num, &format!("at '{}'", tok), s)
        }
    }
}
