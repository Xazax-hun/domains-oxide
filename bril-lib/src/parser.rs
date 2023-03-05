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

use TokenValue::*;

impl<'src> Parser<'src> {
    pub fn new(lexed: LexResult, diag: &'src mut DiagnosticEmitter) -> Self {
        Self {
            current_tok: 0,
            current_block: 0,
            tokens: lexed.tokens,
            unit: Unit {
                functions: Vec::new(),
                function_types: Vec::new(),
                globals: SymbolTable::default(),
                identifiers: lexed.identifiers,
            },
            diag,
        }
    }

    pub fn parse(mut self) -> Option<ir::Unit> {
        while !self.is_at_end() {
            let cfg = self.parse_function()?;
            self.analyze(&cfg)?;
            self.unit.functions.push(cfg);
        }
        Some(self.unit)
    }

    fn parse_function(&mut self) -> Option<Cfg> {
        let (func, func_id) = self.consume_global()?;

        let formals = if self.try_consume(LeftParen).is_some() {
            self.parse_formals()?
        } else {
            Vec::new()
        };

        let ret = if self.try_consume(Colon).is_some() {
            self.parse_type()?
        } else {
            Type::Void
        };
        let func_ty = FunctionType {
            ret,
            formals: formals.iter().map(|v| v.ty).collect(),
        };
        self.unit.function_types.push(func_ty);
        let func_ty = Type::Fn(self.unit.function_types.len() - 1);
        self.unit.globals.insert(
            self.diag,
            &self.unit.identifiers,
            func.line_num,
            func_id,
            func_ty,
        )?;

        let mut symbols = SymbolTable::default();
        let mut jumps = LabelsToBlocks::default();
        for Variable { id, ty } in &formals {
            symbols.insert(self.diag, &self.unit.identifiers, func.line_num, *id, *ty)?;
        }
        let mut cfg = Cfg::new(func, func_ty, formals);
        self.current_block = cfg.new_block();
        self.consume(LeftBrace, "")?;
        self.parse_function_body(&mut cfg, &mut symbols, &mut jumps)?;

        // Add edges to the Cfg.
        let mut edges = Vec::new();
        for (id, block) in cfg.blocks().iter().enumerate() {
            match block.operations().last().unwrap() {
                Operation::Branch {
                    token, then, els, ..
                } => {
                    let to = jumps.get(self.diag, &self.unit.identifiers, token.line_num, *then)?;
                    edges.push((id, to));
                    let to = jumps.get(self.diag, &self.unit.identifiers, token.line_num, *els)?;
                    edges.push((id, to));
                }
                Operation::Jump(token, next) => {
                    let to = jumps.get(self.diag, &self.unit.identifiers, token.line_num, *next)?;
                    edges.push((id, to));
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
                let (_, id) = self.consume_local()?;
                self.consume(Colon, "")?;
                let ty = self.parse_type()?;
                result.push(Variable { id, ty });
                if self.try_consume(Comma).is_none() {
                    break;
                }
            }
        }
        self.consume(RightParen, "")?;
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
        jumps: &mut LabelsToBlocks,
    ) -> Option<()> {
        let mut ops = Vec::new();
        while !self.check(RightBrace) {
            let prev = self.current_block;
            let op = self.parse_instruction(cfg, symbols, jumps, ops.len())?;
            if prev != self.current_block {
                cfg.extend_block(prev, ops.iter());
                ops.clear();
            } else if let Some(prev_op) = ops.last() {
                if prev_op.is_terminator() {
                    self.error(op.get_token(), "Basic block must start with a label.");
                    return None;
                }
            }
            ops.push(op);
        }
        cfg.extend_block(self.current_block, ops.iter());

        self.consume(RightBrace, "")?;
        Some(())
    }

    fn parse_instruction(
        &mut self,
        cfg: &mut Cfg,
        symbols: &mut SymbolTable,
        jumps: &mut LabelsToBlocks,
        num: usize,
    ) -> Option<Operation> {
        if let Some(tok) = self.try_consume(Print) {
            let (_, id) = self.consume_local()?;
            self.consume(Semicolon, "")?;
            let var = symbols.get(self.diag, &self.unit.identifiers, tok.line_num, id)?;
            return Some(Operation::Print(tok, var));
        }

        if let Some(tok) = self.try_consume(Return) {
            if self.try_consume(Semicolon).is_some() {
                return Some(Operation::Ret(tok, None));
            }
            let (_, id) = self.consume_local()?;
            self.consume(Semicolon, "")?;
            let var = symbols.get(self.diag, &self.unit.identifiers, tok.line_num, id)?;
            return Some(Operation::Ret(tok, Some(var)));
        }

        if let Some(tok) = self.try_consume(Jump) {
            let (_, id) = self.consume_label(true)?;
            self.consume(Semicolon, "")?;
            return Some(Operation::Jump(tok, id));
        }

        if let Some(token) = self.try_consume(Branch) {
            let (_, cond) = self.consume_local()?;
            let (_, then) = self.consume_label(true)?;
            let (_, els) = self.consume_label(true)?;
            self.consume(Semicolon, "")?;
            let cond = symbols.get(self.diag, &self.unit.identifiers, token.line_num, cond)?;
            return Some(Operation::Branch {
                token,
                cond,
                then,
                els,
            });
        }

        if let Some(tok) = self.try_consume(Nop) {
            self.consume(Semicolon, "")?;
            return Some(Operation::Nop(tok));
        }

        if self.check(Call) {
            return self.parse_call(None, symbols);
        }

        if let Some((tok, label)) = self.consume_label(false) {
            self.consume(Colon, "")?;
            // The only way to have a label when the previous block is empty
            // if the function starts with a label.
            if num == 0 {
                assert_eq!(self.current_block, 0);
            } else {
                self.current_block = cfg.new_block();
            }
            jumps.insert(
                self.diag,
                &self.unit.identifiers,
                tok.line_num,
                label,
                self.current_block,
            )?;
            // Each block has to have at least one instruction,
            // so we can continue to parsing the next one.
            return self.parse_instruction(cfg, symbols, jumps, num);
        }

        let (tok, res_id) = self.consume_local()?;
        self.consume(Colon, "")?;
        let result_ty = self.parse_type()?;
        self.consume(Define, "")?;

        let result = Variable {
            id: res_id,
            ty: result_ty,
        };

        // TODO: avoid copying the symbol table.
        let prev_symbols = symbols.clone();
        symbols.insert(
            self.diag,
            &self.unit.identifiers,
            tok.line_num,
            res_id,
            result_ty,
        )?;

        if self.check(Call) {
            return self.parse_call(Some(result), &prev_symbols);
        }

        if let Some(const_tok) = self.try_consume(Const) {
            let Some(tok) = self.match_tokens(&[True, False, Integer(0)])
            else {
                self.error(const_tok, "Integer or boolean constant expected.");
                return None;
            };
            self.consume(Semicolon, "")?;
            return Some(Operation::Const(tok, result));
        }

        // Unary operations.
        if let Some(token) = self.match_tokens(&[Identity, Not]) {
            let (_, arg) = self.consume_local()?;
            self.consume(Semicolon, "")?;
            let operand =
                prev_symbols.get(self.diag, &self.unit.identifiers, token.line_num, arg)?;
            return Some(Operation::UnaryOp {
                token,
                result,
                operand,
            });
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
            let (_, lhs) = self.consume_local()?;
            let (_, rhs) = self.consume_local()?;
            self.consume(Semicolon, "")?;
            let lhs = prev_symbols.get(self.diag, &self.unit.identifiers, token.line_num, lhs)?;
            let rhs = prev_symbols.get(self.diag, &self.unit.identifiers, token.line_num, rhs)?;
            return Some(Operation::BinaryOp {
                token,
                result,
                lhs,
                rhs,
            });
        }

        self.error(self.peek(), "Unexpected token.");
        None
    }

    fn parse_call(&mut self, result: Option<Variable>, symbols: &SymbolTable) -> Option<Operation> {
        let token = self.consume(Call, "")?;
        let (_, id) = self.consume_global()?;
        let callee =
            self.unit
                .globals
                .get(self.diag, &self.unit.identifiers, token.line_num, id)?;
        let mut args = Vec::new();
        while !self.check(Semicolon) {
            let (_, arg) = self.consume_local()?;
            let var = symbols.get(self.diag, &self.unit.identifiers, token.line_num, arg)?;
            args.push(var);
        }
        self.consume(Semicolon, "")?;
        Some(Operation::Call {
            token,
            callee,
            result,
            args,
        })
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

    fn consume_local(&mut self) -> Option<(Token, Identifier)> {
        if let Local(id) = self.peek().value {
            let token = self.advance();
            return Some((token, id));
        }
        self.error(self.peek(), "Local identifier expected.");
        None
    }

    fn consume_global(&mut self) -> Option<(Token, Identifier)> {
        if let Global(id) = self.peek().value {
            let token = self.advance();
            return Some((token, id));
        }
        self.error(self.peek(), "Global identifier expected.");
        None
    }

    fn consume_label(&mut self, err: bool) -> Option<(Token, Identifier)> {
        if let Label(id) = self.peek().value {
            let token = self.advance();
            return Some((token, id));
        }
        if err {
            self.error(self.peek(), "Label identifier expected.");
        }
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

    fn expect_type(&mut self, t: Token, found: Type, expected: Type) -> Option<()> {
        if found == expected {
            return Some(());
        }
        self.error(t, &format!("'{expected}' type expected; '{found}' found"));
        None
    }

    fn analyze(&mut self, cfg: &Cfg) -> Option<()> {
        for block in cfg.blocks() {
            let op = block.operations().last()?;
            if !op.is_terminator() {
                self.error(
                    op.get_token(),
                    "Block terminator expected to be jump, br, or ret.",
                );
                return None;
            };
            for op in block.operations() {
                match op.clone() {
                    Operation::BinaryOp {
                        token,
                        result,
                        lhs,
                        rhs,
                    } => match token.value {
                        Add | Mul | Div | Sub => {
                            self.expect_type(token, result.ty, Type::Int)?;
                            self.expect_type(token, lhs.ty, Type::Int)?;
                            self.expect_type(token, rhs.ty, Type::Int)?;
                        }
                        LessThan | GreaterThan | LessThanOrEq | GreaterThanOrEq => {
                            self.expect_type(token, result.ty, Type::Bool)?;
                            self.expect_type(token, lhs.ty, Type::Int)?;
                            self.expect_type(token, rhs.ty, Type::Int)?;
                        }
                        And | Or => {
                            self.expect_type(token, result.ty, Type::Bool)?;
                            self.expect_type(token, lhs.ty, Type::Bool)?;
                            self.expect_type(token, rhs.ty, Type::Bool)?;
                        }
                        Equal => {
                            self.expect_type(token, lhs.ty, result.ty)?;
                            self.expect_type(token, rhs.ty, result.ty)?;
                        }
                        _ => panic!("Unexpected binary operator."),
                    },
                    Operation::UnaryOp {
                        token,
                        result,
                        operand,
                    } => match token.value {
                        Identity => {
                            self.expect_type(token, operand.ty, result.ty)?;
                        }
                        Not => {
                            self.expect_type(token, result.ty, Type::Bool)?;
                            self.expect_type(token, operand.ty, Type::Bool)?;
                        }
                        _ => continue,
                    },
                    Operation::Call {
                        token,
                        callee,
                        result,
                        args,
                    } => {
                        let fn_ty = callee.ty.get_function_type(&self.unit)?.clone();

                        match result {
                            Some(ret) => {
                                if fn_ty.ret == Type::Void {
                                    self.error(token, "Void functions cannot return a value.");
                                    return None;
                                }
                                self.expect_type(token, ret.ty, fn_ty.ret)?;
                            }
                            None => {
                                if fn_ty.ret != Type::Void {
                                    self.error(token, "Non-void functions must return a value.");
                                    return None;
                                }
                            }
                        }

                        if fn_ty.formals.len() != args.len() {
                            self.error(
                                token,
                                &format!(
                                    "{} arguments expected, got {}",
                                    fn_ty.formals.len(),
                                    args.len()
                                ),
                            );
                            return None;
                        }

                        for (formal, arg) in fn_ty.formals.iter().zip(args.iter()) {
                            self.expect_type(token, arg.ty, *formal)?;
                        }
                    }
                    Operation::Const(token, result) => match token.value {
                        Integer(_) => self.expect_type(token, Type::Int, result.ty)?,
                        True | False => self.expect_type(token, Type::Bool, result.ty)?,
                        _ => panic!("Unexpected constant type"),
                    },
                    Operation::Ret(token, ret) => {
                        let ret_ty = cfg.get_type(&self.unit).ret;
                        match ret {
                            Some(var) => {
                                if ret_ty == Type::Void {
                                    self.error(token, "Void functions cannot return a value.");
                                    return None;
                                }

                                self.expect_type(token, var.ty, ret_ty)?;
                            }
                            None => {
                                if ret_ty != Type::Void {
                                    self.error(token, "Non-void functions must return a value.");
                                    return None;
                                }
                            }
                        }
                    }
                    _ => continue,
                }
            }
        }
        Some(())
    }
}
