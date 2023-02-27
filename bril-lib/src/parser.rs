use std::collections::HashMap;

use analysis::cfg::{BlockMutableCfg, CfgBlock, ControlFlowGraph, MutableCfg};
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

type JumpTable = HashMap<Identifier, usize>;

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
                function_types: Vec::new(),
                globals: HashMap::new(),
                identifier_table,
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

        let mut ret = Type::Void;
        if self.try_consume(Colon).is_some() {
            ret = self.parse_type()?;
        }
        let func_ty = FunctionType {
            ret,
            formals: formal_tys,
        };
        self.unit.function_types.push(func_ty);
        let func_ty = Type::Fn(self.unit.function_types.len() - 1);
        self.unit.globals.insert(
            func_id,
            Variable {
                id: func_id,
                ty: func_ty.clone(),
            },
        );

        let mut symbols = HashMap::new();
        let mut jumps = HashMap::new();
        for v @ Variable { id, ty: _ } in &formals {
            symbols.insert(*id, v.clone());
        }
        let mut cfg = Cfg::new(func, func_ty, formals);
        self.current_block = cfg.new_block();
        self.consume(LeftBrace, "");
        self.parse_function_body(&mut cfg, &mut symbols, &mut jumps)?;

        // Add edges to the Cfg.
        let mut edges = Vec::new();
        for (id, block) in cfg.blocks().iter().enumerate() {
            match block.operations().last().unwrap() {
                Operation::Br(ir::Branch { then, els, .. }) => {
                    let Some(to) = jumps.get(then)
                    else {
                        // TODO: report error.
                        return None;
                    };
                    edges.push((id, *to));
                    let Some(to) = jumps.get(els)
                    else {
                        // TODO: report error.
                        return None;
                    };
                    edges.push((id, *to));
                }
                Operation::Jump(_, next) => {
                    let Some(to) = jumps.get(next)
                    else {
                        // TODO: report error.
                        return None;
                    };
                    edges.push((id, *to));
                }
                _ => continue,
            }
        }
        cfg.add_edges(&edges);

        Some(cfg)
    }

    fn parse_formals(&mut self) -> Option<Vec<Variable>> {
        let mut result = Vec::new();
        if !self.check(RightParen) {
            loop {
                let (_, id, _) = self.consume_identifier(&[Local])?;
                self.consume(Colon, "");
                let ty = self.parse_type()?;
                result.push(Variable { id, ty });
                if self.try_consume(Comma).is_none() {
                    break;
                }
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

    fn parse_function_body(
        &mut self,
        cfg: &mut Cfg,
        symbols: &mut SymbolTable,
        jumps: &mut JumpTable,
    ) -> Option<()> {
        let mut ops = Vec::new();
        while !self.check(RightBrace) {
            let prev = self.current_block;
            let op = self.parse_instruction(cfg, symbols, jumps)?;
            if prev == self.current_block {
                ops.push(op);
            } else {
                cfg.extend_block(prev, ops.iter());
                ops.clear();
                ops.push(op);
            }
        }
        cfg.extend_block(self.current_block, ops.iter());

        self.consume(RightBrace, "");
        Some(())
    }

    fn parse_instruction(
        &mut self,
        cfg: &mut Cfg,
        symbols: &mut SymbolTable,
        jumps: &mut JumpTable,
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
            if self.try_consume(Semicolon).is_some() {
                return Some(Operation::Ret(tok.line_num, None));
            }
            let (_, id, _) = self.consume_identifier(&[Local])?;
            self.consume(Semicolon, "");
            if let Some(var) = symbols.get(&id) {
                return Some(Operation::Ret(tok.line_num, Some(var.clone())));
            } else {
                self.undefined_variable(tok, id);
                return None;
            }
        }

        if let Some(tok) = self.try_consume(Jump) {
            let (_, id, _) = self.consume_identifier(&[Label])?;
            self.consume(Semicolon, "");
            return Some(Operation::Jump(tok.line_num, id));
        }

        if let Some(token) = self.try_consume(Branch) {
            let (_, cond, _) = self.consume_identifier(&[Local])?;
            let (_, then, _) = self.consume_identifier(&[Label])?;
            let (_, els, _) = self.consume_identifier(&[Label])?;
            self.consume(Semicolon, "");
            if let Some(var) = symbols.get(&cond) {
                return Some(Operation::Br(ir::Branch {
                    location: token.line_num,
                    cond: var.clone(),
                    then,
                    els,
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

        let (_, res_id, id_ty) = self.consume_identifier(&[Local, Label])?;
        if id_ty == Label {
            self.consume(Colon, "");
            // Each block has to have at least one instruction,
            // so we can continue to parsing the next one.
            self.current_block = cfg.new_block();
            jumps.insert(res_id, self.current_block);
            return self.parse_instruction(cfg, symbols, jumps);
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
                self.unit.identifier_table.get_name(var_id)
            ),
        );
    }
}
