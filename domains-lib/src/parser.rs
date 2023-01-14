use utils::DiagnosticEmitter;

use crate::{
    ast::{self, *},
    lexer::{Token, TokenValue},
};

pub struct Parser<'src> {
    ctx: ASTContext,
    current: usize,
    tokens: Vec<Token>,
    diag: &'src mut DiagnosticEmitter,
}

use TokenValue::*;

impl<'src> Parser<'src> {
    pub fn new(tokens: Vec<Token>, diag: &'src mut DiagnosticEmitter) -> Self {
        Parser {
            ctx: ASTContext::new(),
            current: 0,
            tokens,
            diag,
        }
    }

    pub fn parse(mut self) -> Option<ASTContext> {
        self.sequence(true)?;
        let ctx = core::mem::replace(&mut self.ctx, ASTContext::new());
        if !self.is_at_end() {
            self.error(self.peek(), "end of file expected.");
            return None;
        }
        Some(ctx)
    }

    pub fn get_context(&self) -> &ASTContext {
        &self.ctx
    }

    fn sequence(&mut self, root: bool) -> Option<Node> {
        if root && !self.check(Init) {
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

            if !self.match_tokens(&[Semicolon]) {
                break;
            }
        }
        Some(self.ctx.make_sequence(Sequence { nodes: commands }))
    }

    fn branch(&mut self) -> Option<Node> {
        // Allow empty sequences in alternatives.
        let lhs = if self.check(RightBrace) {
            self.ctx.make_sequence(Sequence { nodes: Vec::new() })
        } else {
            self.sequence(false)?
        };

        self.consume(RightBrace, "")?;
        let kw = self.consume(Or, "")?;
        self.consume(LeftBrace, "")?;

        let rhs = if self.check(RightBrace) {
            self.ctx.make_sequence(Sequence { nodes: Vec::new() })
        } else {
            self.sequence(false)?
        };
        self.consume(RightBrace, "")?;

        if let (NodeRef::Sequence(lhs_seq), NodeRef::Sequence(rhs_seq)) =
            (self.ctx.node_to_ref(lhs), self.ctx.node_to_ref(rhs))
        {
            if lhs_seq.nodes.is_empty() && rhs_seq.nodes.is_empty() {
                self.error(kw, "at most one alternative can be empty.");
                return None;
            }
        } else {
            panic!("Children of the branches should be sequences.")
        }

        Some(self.ctx.make_branch(Branch { kw, lhs, rhs }))
    }

    fn loop_(&mut self) -> Option<Node> {
        let kw = self.previous();
        self.consume(LeftBrace, "")?;

        if self.match_tokens(&[RightBrace]) {
            self.error(kw, "the body of 'iter' must not be empty.");
            return None;
        }

        let body = self.sequence(false)?;
        self.consume(RightBrace, "")?;

        Some(self.ctx.make_loop(Loop { kw, body }))
    }

    fn command(&mut self) -> Option<Node> {
        if self.match_tokens(&[Init]) {
            let kw = self.previous();
            self.consume(LeftParen, "")?;
            let bot_x = self.consume(Number(0), "a number expected.")?;
            self.consume(Comma, "")?;
            let bot_y = self.consume(Number(0), "a number expected.")?;
            self.consume(Comma, "")?;
            let width = self.consume(Number(0), "a number expected.")?;
            self.consume(Comma, "")?;
            let height = self.consume(Number(0), "a number expected.")?;
            self.consume(RightParen, "")?;

            if width.value.to_num() < 0 {
                self.error(kw, "the width of the initial area cannot be negative.");
                return None;
            }

            if height.value.to_num() < 0 {
                self.error(kw, "the height of the initial area cannot be negative.");
                return None;
            }

            return Some(self.ctx.make_init(ast::Init {
                kw,
                bottom_left: NumPair { x: bot_x, y: bot_y },
                size: NumPair {
                    x: width,
                    y: height,
                },
            }));
        }
        if self.match_tokens(&[Translation]) {
            let kw = self.previous();
            self.consume(LeftParen, "")?;
            let x = self.consume(Number(0), "a number expected.")?;
            self.consume(Comma, "")?;
            let y = self.consume(Number(0), "a number expected.")?;
            self.consume(RightParen, "")?;

            return Some(self.ctx.make_translation(ast::Translation {
                kw,
                vector: NumPair { x, y },
            }));
        }
        if self.match_tokens(&[Rotation]) {
            let kw = self.previous();
            self.consume(LeftParen, "")?;
            let x = self.consume(Number(0), "a number expected.")?;
            self.consume(Comma, "")?;
            let y = self.consume(Number(0), "a number expected.")?;
            self.consume(Comma, "")?;
            let deg = self.consume(Number(0), "a number expected.")?;
            self.consume(RightParen, "")?;

            return Some(self.ctx.make_rotation(ast::Rotation {
                kw,
                origin: NumPair { x, y },
                deg,
            }));
        }
        if self.match_tokens(&[Iter]) {
            return self.loop_();
        }
        if self.match_tokens(&[LeftBrace]) {
            return self.branch();
        }

        if self.is_at_end() || self.check(RightBrace) {
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
        matches!(self.peek().value, EndOfFile)
    }

    fn check(&self, tok_val: TokenValue) -> bool {
        if self.is_at_end() {
            false
        } else {
            core::mem::discriminant(&self.peek().value) == core::mem::discriminant(&tok_val)
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
        let msg = if s.is_empty() {
            format!("'{tok_val}' expected.")
        } else {
            s.to_owned()
        };
        self.error(self.peek(), &msg);
        None
    }

    fn error(&mut self, tok: Token, s: &str) {
        if tok.value == EndOfFile {
            self.diag.report(tok.line_num, "at end of file", s);
        } else {
            self.diag.report(tok.line_num, &format!("at '{tok}'"), s);
        }
    }
}
