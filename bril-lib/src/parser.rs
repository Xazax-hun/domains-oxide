use std::collections::HashMap;

use analysis::cfg::{BlockMutableCfg, MutableCfg};
use utils::DiagnosticEmitter;

use crate::{
    ir::{self, *},
    lexer::{Identifier, LexResult, Token, TokenValue},
};

pub struct Parser<'src> {
    current_tok: usize,
    current_block: usize,
    tokens: Vec<Token>,
    unit: Unit,
    diag: &'src mut DiagnosticEmitter,
}

use IdentifierType::*;
use TokenValue::*;

impl<'src> Parser<'src> {
    pub fn new(lexed: LexResult, diag: &'src mut DiagnosticEmitter) -> Self {
        let LexResult {
            tokens,
            identifier_table,
        } = lexed;

        Parser {
            current_tok: 0,
            current_block: 0,
            tokens,
            unit: Unit {
                functions: Vec::new(),
                globals: HashMap::new(),
                context: IRContext {
                    function_types: Vec::new(),
                    identifier_table,
                },
            },
            diag,
        }
    }

    pub fn parse(mut self) -> Option<ir::Unit> {
        while !self.is_at_end() {
            if let Some(cfg) = self.parse_function() {
                self.unit.functions.push(cfg);
            } else {
                return None;
            }
        }
        Some(self.unit)
    }

    fn parse_function(&mut self) -> Option<Cfg> {
        let (func, func_id, _) = self.consume_identifier(&[Global])?;

        let mut formals = Vec::new();
        if self.try_consume(LeftParen).is_some() {
            formals = self.parse_formals()?;
        }
        let formal_tys: Vec<_> = formals.iter().map(|v| v.ty.clone()).collect();

        let mut ty = Type::Void;
        if self.try_consume(Colon).is_some() {
            ty = self.parse_type()?;
        }
        let func_ty = FunctionType {
            ret: ty.clone(),
            formals: formal_tys,
        };
        self.unit.context.function_types.push(func_ty);
        let fn_ty_idx = self.unit.context.function_types.len() - 1;
        self.unit.globals.insert(
            func_id,
            Variable {
                id: func_id,
                ty: Type::Fn(fn_ty_idx),
            },
        );

        let mut symbols = HashMap::new();
        for v @ Variable { id, ty: _ } in &formals {
            symbols.insert(*id, v.clone());
        }
        let mut cfg = Cfg::new(func, ty, formals);
        cfg.new_block();
        self.consume(LeftBrace, "");
        self.parse_function_body(&mut cfg, &mut symbols)?;

        Some(cfg)
    }

    fn parse_formals(&mut self) -> Option<Vec<Variable>> {
        let mut result = Vec::new();
        while !self.check(RightParen) {
            let (_, id, _) = self.consume_identifier(&[Local])?;
            self.consume(Colon, "");
            let ty = self.parse_type()?;
            result.push(Variable { id, ty });
            if self.try_consume(Comma).is_none() {
                break;
            }
        }
        self.consume(RightParen, "");
        Some(result)
    }

    fn parse_type(&mut self) -> Option<Type> {
        // TODO: parse function types.
        if self.try_consume(Int).is_some() {
            return Some(Type::Int);
        }
        if self.try_consume(Bool).is_some() {
            return Some(Type::Bool);
        }
        self.error(self.peek(), "Type expected.");
        None
    }

    fn parse_function_body(&mut self, cfg: &mut Cfg, symbols: &mut SymbolTable) -> Option<()> {
        // TODO: support control flow.
        let mut ops = Vec::new();
        while !self.check(RightBrace) {
            let op = self.parse_instruction(cfg, symbols)?;
            ops.push(op);
        }
        cfg.extend_block(self.current_block, ops.iter());

        self.consume(RightBrace, "");
        Some(())
    }

    fn parse_instruction(
        &mut self,
        _cfg: &mut Cfg,
        symbols: &mut SymbolTable,
    ) -> Option<Operation> {
        if let Some(tok) = self.try_consume(Print) {
            let (_, id, _) = self.consume_identifier(&[Local])?;
            self.consume(Semicolon, "");
            if let Some(var) = symbols.get(&id) {
                return Some(Operation::Print(tok.line_num, var.clone()));
            } else {
                self.undefined_variable(tok, id);
                return None;
            }
        }

        if let Some(tok) = self.try_consume(Return) {
            let (_, id, _) = self.consume_identifier(&[Local])?;
            self.consume(Semicolon, "");
            if let Some(var) = symbols.get(&id) {
                return Some(Operation::Ret(tok.line_num, var.clone()));
            } else {
                self.undefined_variable(tok, id);
                return None;
            }
        }

        if let Some(tok) = self.try_consume(Jump) {
            let (_, _id, _) = self.consume_identifier(&[Label])?;
            self.consume(Semicolon, "");
            return Some(Operation::Jump(tok.line_num, Target(0)));
        }

        if let Some(token) = self.try_consume(Branch) {
            let (_, cond, _) = self.consume_identifier(&[Local])?;
            let (_, _then, _) = self.consume_identifier(&[Label])?;
            let (_, _els, _) = self.consume_identifier(&[Label])?;
            self.consume(Semicolon, "");
            if let Some(var) = symbols.get(&cond) {
                return Some(Operation::Br(ir::Branch {
                    location: token.line_num,
                    cond: var.clone(),
                    then: Target(0),
                    els: Target(0),
                }));
            } else {
                self.undefined_variable(token, cond);
                return None;
            }
        }

        if let Some(tok) = self.try_consume(Nop) {
            self.consume(Semicolon, "");
            return Some(Operation::Nop(tok.line_num));
        }

        if self.check(Call) {
            return self.parse_call(None, symbols);
        }

        let (tok, res_id, id_ty) = self.consume_identifier(&[Local, Label])?;
        if id_ty == Label {
            // TODO: handle basic blocks.
            return Some(Operation::Nop(tok.line_num));
        }
        self.consume(Colon, "");
        let result_ty = self.parse_type()?;
        self.consume(Define, "");

        let result = Variable {
            id: res_id,
            ty: result_ty,
        };

        symbols.insert(res_id, result.clone());

        if self.check(Call) {
            return self.parse_call(Some(result), symbols);
        }

        if let Some(const_tok) = self.try_consume(Const) {
            let Some(tok) = self.match_tokens(&[True, False, Integer(0)])
            else {
                self.error(const_tok, "Integer or boolean constant expected.");
                return None;
            };
            self.consume(Semicolon, "");
            return Some(Operation::Const(tok, result));
        }

        // Unary operations.
        if let Some(token) = self.match_tokens(&[Identity, Not]) {
            let (_, arg, _) = self.consume_identifier(&[Local])?;
            self.consume(Semicolon, "");
            if let Some(operand) = symbols.get(&arg) {
                return Some(Operation::UnOp(ir::UnaryOp {
                    token,
                    result,
                    operand: operand.clone(),
                }));
            } else {
                self.undefined_variable(token, arg);
                return None;
            }
        }

        // Binary operations.
        if let Some(token) = self.match_tokens(&[
            Add,
            Mul,
            Sub,
            Div,
            Equal,
            LessThan,
            GreaterThan,
            LessThanOrEq,
            GreaterThanOrEq,
            And,
            Or,
        ]) {
            let (_, lhs, _) = self.consume_identifier(&[Local])?;
            let (_, rhs, _) = self.consume_identifier(&[Local])?;
            self.consume(Semicolon, "");
            if let Some(lhs_var) = symbols.get(&lhs) {
                if let Some(rhs_var) = symbols.get(&rhs) {
                    return Some(Operation::BinOp(ir::BinaryOp {
                        token,
                        result,
                        lhs: lhs_var.clone(),
                        rhs: rhs_var.clone(),
                    }));
                } else {
                    self.undefined_variable(token, rhs);
                    return None;
                }
            } else {
                self.undefined_variable(token, lhs);
                return None;
            }
        }

        self.error(self.peek(), "Unexpected token.");
        None
    }

    fn parse_call(
        &mut self,
        result: Option<Variable>,
        symbols: &mut SymbolTable,
    ) -> Option<Operation> {
        let tok = self.consume(Call, "")?;
        let (_, id, _) = self.consume_identifier(&[Global])?;
        let Some(func) = self.unit.globals.get(&id).cloned()
        else {
            self.undefined_variable(tok, id);
            return None;
        };
        let mut args = Vec::new();
        while !self.check(Semicolon) {
            let (_, arg, _) = self.consume_identifier(&[Local])?;
            if let Some(var) = symbols.get(&arg) {
                args.push(var.clone());
            } else {
                self.undefined_variable(tok, arg);
                return None;
            }
        }
        self.consume(Semicolon, "");
        Some(Operation::Call(ir::Call {
            location: tok.line_num,
            callee: func,
            result,
            args,
        }))
    }

    fn peek(&self) -> Token {
        self.tokens[self.current_tok]
    }

    fn previous(&self) -> Token {
        self.tokens[self.current_tok - 1]
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

    fn match_tokens(&mut self, tok_vals: &[TokenValue]) -> Option<Token> {
        if tok_vals.iter().any(|val| self.check(*val)) {
            let prev = self.advance();
            return Some(prev);
        }
        None
    }

    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current_tok += 1;
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

    fn consume_identifier(
        &mut self,
        expected: &[IdentifierType],
    ) -> Option<(Token, Identifier, IdentifierType)> {
        if let Id(id) = self.peek().value {
            let token = self.advance();
            let id_type = match self
                .unit
                .context
                .identifier_table
                .get_name(id)
                .chars()
                .next()
                .unwrap()
            {
                '.' => Label,
                '@' => Global,
                _ => Local,
            };

            if !expected.iter().any(|&t| t == id_type) {
                // TODO: better error message.
                self.error(self.peek(), "Unexpected identifier type.");
                return None;
            }

            return Some((token, id, id_type));
        }
        self.error(self.peek(), "Identifier expected.");
        None
    }

    fn try_consume(&mut self, tok_val: TokenValue) -> Option<Token> {
        if self.check(tok_val) {
            return Some(self.advance());
        }
        None
    }

    fn error(&mut self, tok: Token, s: &str) {
        if tok.value == EndOfFile {
            self.diag.report(tok.line_num.0, "at end of file", s);
        } else {
            self.diag.report(tok.line_num.0, &format!("at '{tok}'"), s);
        }
    }

    fn undefined_variable(&mut self, tok: Token, var_id: Identifier) {
        self.error(
            tok,
            &format!(
                "Undefined identifier: '{}'",
                self.unit.context.identifier_table.get_name(var_id)
            ),
        );
    }
}
