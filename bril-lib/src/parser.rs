use std::collections::HashMap;

use analysis::cfg::{BlockMutableCfg, CfgBlock, ControlFlowGraph, MutableCfg};
use utils::DiagnosticEmitter;

use crate::{
    ir::{self, *},
    lexer::{Identifier, LexResult, Location, Token, TokenValue},
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
                globals: SymbolTable::default(),
                identifier_table,
            },
            diag,
        }
    }

    pub fn parse(mut self) -> Option<ir::Unit> {
        while !self.is_at_end() {
            let cfg = self.parse_function()?;
            self.unit.functions.push(cfg);
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
            self.diag,
            &self.unit.identifier_table,
            func.line_num,
            func_id,
            func_ty.clone(),
        );

        let mut symbols = SymbolTable::default();
        let mut jumps = HashMap::new();
        for Variable { id, ty } in &formals {
            symbols.insert(
                self.diag,
                &self.unit.identifier_table,
                func.line_num,
                *id,
                ty.clone(),
            );
        }
        let mut cfg = Cfg::new(func, func_ty, formals);
        self.current_block = cfg.new_block();
        self.consume(LeftBrace, "");
        self.parse_function_body(&mut cfg, &mut symbols, &mut jumps)?;

        // Add edges to the Cfg.
        let mut edges = Vec::new();
        for (id, block) in cfg.blocks().iter().enumerate() {
            match block.operations().last().unwrap() {
                Operation::Br(ir::Branch {
                    location,
                    then,
                    els,
                    ..
                }) => {
                    let Some(to) = jumps.get(then)
                    else {
                        self.error_at_loc(*location, &Branch.to_string(), &format!("Branch target '{}' is missing.", self.unit.identifier_table.get_name(*then)));
                        return None;
                    };
                    edges.push((id, *to));
                    let Some(to) = jumps.get(els)
                    else {
                        self.error_at_loc(*location,&Branch.to_string(), &format!("Branch target '{}' is missing.", self.unit.identifier_table.get_name(*els)));
                        return None;
                    };
                    edges.push((id, *to));
                }
                Operation::Jump(location, next) => {
                    let Some(to) = jumps.get(next)
                    else {
                        self.error_at_loc(*location, &Jump.to_string(), &format!("Jump target '{}' is missing.", self.unit.identifier_table.get_name(*next)));
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
            let var = symbols.get(self.diag, &self.unit.identifier_table, tok.line_num, id)?;
            return Some(Operation::Print(tok.line_num, var));
        }

        if let Some(tok) = self.try_consume(Return) {
            if self.try_consume(Semicolon).is_some() {
                return Some(Operation::Ret(tok.line_num, None));
            }
            let (_, id, _) = self.consume_identifier(&[Local])?;
            self.consume(Semicolon, "");
            let var = symbols.get(self.diag, &self.unit.identifier_table, tok.line_num, id)?;
            return Some(Operation::Ret(tok.line_num, Some(var)));
        }

        if let Some(tok) = self.try_consume(Jump) {
            let (_, id, _) = self.consume_identifier(&[Label])?;
            self.consume(Semicolon, "");
            return Some(Operation::Jump(tok.line_num, id));
        }

        if let Some(tok) = self.try_consume(Branch) {
            let (_, cond, _) = self.consume_identifier(&[Local])?;
            let (_, then, _) = self.consume_identifier(&[Label])?;
            let (_, els, _) = self.consume_identifier(&[Label])?;
            self.consume(Semicolon, "");
            let cond = symbols.get(self.diag, &self.unit.identifier_table, tok.line_num, cond)?;
            return Some(Operation::Br(ir::Branch {
                location: tok.line_num,
                cond,
                then,
                els,
            }));
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

        let result = symbols.insert(
            self.diag,
            &self.unit.identifier_table,
            tok.line_num,
            res_id,
            result_ty,
        );

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
            let operand =
                symbols.get(self.diag, &self.unit.identifier_table, token.line_num, arg)?;
            return Some(Operation::UnOp(ir::UnaryOp {
                token,
                result,
                operand,
            }));
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
            let lhs = symbols.get(self.diag, &self.unit.identifier_table, token.line_num, lhs)?;
            let rhs = symbols.get(self.diag, &self.unit.identifier_table, token.line_num, rhs)?;
            return Some(Operation::BinOp(ir::BinaryOp {
                token,
                result,
                lhs,
                rhs,
            }));
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
        let func =
            self.unit
                .globals
                .get(self.diag, &self.unit.identifier_table, tok.line_num, id)?;
        let mut args = Vec::new();
        while !self.check(Semicolon) {
            let (_, arg, _) = self.consume_identifier(&[Local])?;
            let var = symbols.get(self.diag, &self.unit.identifier_table, tok.line_num, arg)?;
            args.push(var);
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

    fn error_at_loc(&mut self, loc: Location, item: &str, s: &str) {
        if item.is_empty() {
            self.diag.error(loc.0, s);
        } else {
            self.diag.report(loc.0, &format!("at '{item}'"), s);
        }
    }
}
