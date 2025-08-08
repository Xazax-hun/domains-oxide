use std::collections::{HashMap, hash_map::Entry};

use analysis::cfg::{BlockMutableCfg, CfgBlock, ControlFlowGraph, MutableCfg, RPOWorklist};
use utils::DiagnosticEmitter;

use crate::{
    ir::*,
    lexer::{Identifier, IdentifierTable, LexResult, Location, Token, TokenValue},
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
            unit: Unit::new(lexed.identifiers),
            diag,
        }
    }

    pub fn parse(mut self) -> Option<Unit> {
        let mut functions = Vec::new();
        while !self.is_at_end() {
            let cfg = self.parse_function()?;
            functions.push(cfg);
        }
        let mut sema = Sema {
            unit: &self.unit,
            diag: self.diag,
        };
        for cfg in &mut functions {
            sema.analyze(cfg)?;
        }
        self.unit.functions = functions;
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
        let func_ty = self.unit.add_function_type(func_ty);
        self.unit.globals.insert(
            self.diag,
            &self.unit.identifiers,
            func.line_num,
            func_id,
            func_ty,
        )?;

        let mut jumps = LabelsToBlocks::default();
        let mut cfg = Cfg::new(func, func_ty, formals);
        self.current_block = cfg.new_block();
        self.consume(LeftBrace, "")?;
        self.parse_function_body(&mut cfg, &mut jumps)?;

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
        self.peek().error(self.diag, "Type expected.");
        None
    }

    fn parse_function_body(&mut self, cfg: &mut Cfg, jumps: &mut LabelsToBlocks) -> Option<()> {
        let mut ops = Vec::new();
        while !self.check(RightBrace) {
            let prev = self.current_block;
            let op = self.parse_instruction(cfg, jumps, ops.len())?;
            if prev != self.current_block {
                cfg.extend_block(prev, ops.iter());
                ops.clear();
            } else if let Some(prev_op) = ops.last()
                && prev_op.is_terminator()
            {
                op.get_token()
                    .error(self.diag, "Basic block must start with a label.");
                return None;
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
        jumps: &mut LabelsToBlocks,
        num: usize,
    ) -> Option<Operation> {
        if let Some(tok) = self.try_consume(Print) {
            let (_, id) = self.consume_local()?;
            self.consume(Semicolon, "")?;
            return Some(Operation::Print(tok, Variable::placeholder(id)));
        }

        if let Some(tok) = self.try_consume(Return) {
            if self.try_consume(Semicolon).is_some() {
                return Some(Operation::Ret(tok, None));
            }
            let (_, id) = self.consume_local()?;
            self.consume(Semicolon, "")?;
            return Some(Operation::Ret(tok, Some(Variable::placeholder(id))));
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
            return Some(Operation::Branch {
                token,
                cond: Variable::placeholder(cond),
                then,
                els,
            });
        }

        if let Some(tok) = self.try_consume(Nop) {
            self.consume(Semicolon, "")?;
            return Some(Operation::Nop(tok));
        }

        if self.check(Call) {
            return self.parse_call(None);
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
            return self.parse_instruction(cfg, jumps, num);
        }

        let (_, res_id) = self.consume_local()?;
        self.consume(Colon, "")?;
        let result_ty = self.parse_type()?;
        self.consume(Define, "")?;

        let result = Variable {
            id: res_id,
            ty: result_ty,
        };

        if self.check(Call) {
            return self.parse_call(Some(result));
        }

        if let Some(const_tok) = self.try_consume(Const) {
            let Some(tok) = self.match_tokens(&[True, False, Integer(0)]) else {
                const_tok.error(self.diag, "Integer or boolean constant expected.");
                return None;
            };
            self.consume(Semicolon, "")?;
            return Some(Operation::Const(tok, result));
        }

        // Unary operations.
        if let Some(token) = self.match_tokens(&[Identity, Not]) {
            let (_, arg) = self.consume_local()?;
            self.consume(Semicolon, "")?;
            return Some(Operation::UnaryOp {
                token,
                result,
                operand: Variable::placeholder(arg),
            });
        }

        // Binary operations.
        if let Some(token) = self.match_tokens(&[
            Add,
            Mul,
            Sub,
            Div,
            Mod,
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
            return Some(Operation::BinaryOp {
                token,
                result,
                lhs: Variable::placeholder(lhs),
                rhs: Variable::placeholder(rhs),
            });
        }

        self.peek().error(self.diag, "Unexpected token.");
        None
    }

    fn parse_call(&mut self, result: Option<Variable>) -> Option<Operation> {
        let token = self.consume(Call, "")?;
        let (_, id) = self.consume_global()?;
        let mut args = Vec::new();
        while !self.check(Semicolon) {
            let (_, arg) = self.consume_local()?;
            args.push(Variable::placeholder(arg));
        }
        self.consume(Semicolon, "")?;
        Some(Operation::Call {
            token,
            callee: Variable::placeholder(id),
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
            std::borrow::Cow::from(format!("'{tok_val}' expected."))
        } else {
            std::borrow::Cow::from(s)
        };
        self.peek().error(self.diag, &msg);
        None
    }

    fn consume_local(&mut self) -> Option<(Token, Identifier)> {
        if let Local(id) = self.peek().value {
            let token = self.advance();
            return Some((token, id));
        }
        self.peek().error(self.diag, "Local identifier expected.");
        None
    }

    fn consume_global(&mut self) -> Option<(Token, Identifier)> {
        if let Global(id) = self.peek().value {
            let token = self.advance();
            return Some((token, id));
        }
        self.peek().error(self.diag, "Global identifier expected.");
        None
    }

    fn consume_label(&mut self, err: bool) -> Option<(Token, Identifier)> {
        if let Label(id) = self.peek().value {
            let token = self.advance();
            return Some((token, id));
        }
        if err {
            self.peek().error(self.diag, "Label identifier expected.");
        }
        None
    }

    fn try_consume(&mut self, tok_val: TokenValue) -> Option<Token> {
        if self.check(tok_val) {
            return Some(self.advance());
        }
        None
    }
}

#[derive(Clone, Debug, Default)]
struct LabelsToBlocks(HashMap<Identifier, usize>);

impl LabelsToBlocks {
    fn insert(
        &mut self,
        diag: &mut DiagnosticEmitter,
        ids: &IdentifierTable,
        loc: Location,
        label: Identifier,
        block: usize,
    ) -> Option<()> {
        match self.0.entry(label) {
            Entry::Occupied(_) => {
                diag.report(
                    loc.0,
                    &format!("at '{}'", ids.get_name(label)),
                    "Duplicate label found.",
                );
                None
            }
            Entry::Vacant(v) => {
                v.insert(block);
                Some(())
            }
        }
    }

    fn get(
        &self,
        diag: &mut DiagnosticEmitter,
        ids: &IdentifierTable,
        loc: Location,
        label: Identifier,
    ) -> Option<usize> {
        match self.0.get(&label).copied() {
            None => {
                diag.error(
                    loc.0,
                    &format!("Branch target '{}' is missing", ids.get_name(label)),
                );
                None
            }
            res => res,
        }
    }
}

struct Sema<'unit, 'src> {
    unit: &'unit Unit,
    diag: &'src mut DiagnosticEmitter,
}

impl Sema<'_, '_> {
    fn expect_type(&mut self, t: Token, found: Type, expected: Type) -> Option<()> {
        if found == expected {
            return Some(());
        }
        t.error(
            self.diag,
            &format!("'{expected}' type expected; '{found}' found"),
        );
        None
    }

    fn analyze_bin_op(
        &mut self,
        token: Token,
        result: Variable,
        lhs: Variable,
        rhs: Variable,
        syms: &mut SymbolTable,
    ) -> Option<Operation> {
        // Operand types need to be looked up before the result type is inserted into the symbol table.
        let lhs = syms.get(self.diag, &self.unit.identifiers, token.line_num, lhs.id)?;
        let rhs = syms.get(self.diag, &self.unit.identifiers, token.line_num, rhs.id)?;
        syms.insert(
            self.diag,
            &self.unit.identifiers,
            token.line_num,
            result.id,
            result.ty,
        )?;
        match token.value {
            Add | Mul | Div | Sub | Mod => {
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
                self.expect_type(token, rhs.ty, lhs.ty)?;
                self.expect_type(token, result.ty, Type::Bool)?;
            }
            _ => panic!("Unexpected binary operator."),
        }
        Some(Operation::BinaryOp {
            token,
            result,
            lhs,
            rhs,
        })
    }

    fn analyze_unary_op(
        &mut self,
        token: Token,
        result: Variable,
        operand: Variable,
        syms: &mut SymbolTable,
    ) -> Option<Operation> {
        // Operand type need to be looked up before the result type is inserted into the symbol table.
        let operand = syms.get(
            self.diag,
            &self.unit.identifiers,
            token.line_num,
            operand.id,
        )?;
        syms.insert(
            self.diag,
            &self.unit.identifiers,
            token.line_num,
            result.id,
            result.ty,
        )?;
        match token.value {
            Identity => {
                self.expect_type(token, operand.ty, result.ty)?;
            }
            Not => {
                self.expect_type(token, result.ty, Type::Bool)?;
                self.expect_type(token, operand.ty, Type::Bool)?;
            }
            _ => (),
        };
        Some(Operation::UnaryOp {
            token,
            result,
            operand,
        })
    }

    fn analyze_call(
        &mut self,
        token: Token,
        callee: Variable,
        result: Option<Variable>,
        args: Vec<Variable>,
        syms: &mut SymbolTable,
    ) -> Option<Operation> {
        let callee =
            self.unit
                .globals
                .get(self.diag, &self.unit.identifiers, token.line_num, callee.id)?;
        let fn_ty = callee.ty.get_function_type(self.unit)?.clone();

        match result {
            Some(ret) => {
                if fn_ty.ret == Type::Void {
                    token.error(self.diag, "Void functions cannot return a value.");
                    return None;
                }
                self.expect_type(token, ret.ty, fn_ty.ret)?;
                syms.insert(
                    self.diag,
                    &self.unit.identifiers,
                    token.line_num,
                    ret.id,
                    ret.ty,
                )?;
            }
            None => {
                if fn_ty.ret != Type::Void {
                    token.error(self.diag, "Non-void functions must return a value.");
                    return None;
                }
            }
        }

        if fn_ty.formals.len() != args.len() {
            token.error(
                self.diag,
                &format!(
                    "{} arguments expected, got {}",
                    fn_ty.formals.len(),
                    args.len()
                ),
            );
            return None;
        }

        let mut elaborated_args = Vec::with_capacity(args.len());
        for arg in args {
            elaborated_args.push(syms.get(
                self.diag,
                &self.unit.identifiers,
                token.line_num,
                arg.id,
            )?);
        }

        for (formal, arg) in fn_ty.formals.iter().zip(elaborated_args.iter()) {
            self.expect_type(token, arg.ty, *formal)?;
        }
        Some(Operation::Call {
            token,
            callee,
            result,
            args: elaborated_args,
        })
    }

    fn analyze_op(
        &mut self,
        op: Operation,
        syms: &mut SymbolTable,
        func_ty: &FunctionType,
    ) -> Option<Operation> {
        match op {
            Operation::BinaryOp {
                token,
                result,
                lhs,
                rhs,
            } => self.analyze_bin_op(token, result, lhs, rhs, syms),
            Operation::UnaryOp {
                token,
                result,
                operand,
            } => self.analyze_unary_op(token, result, operand, syms),
            Operation::Call {
                token,
                callee,
                result,
                args,
            } => self.analyze_call(token, callee, result, args, syms),
            Operation::Const(token, result) => {
                match token.value {
                    Integer(_) => self.expect_type(token, Type::Int, result.ty)?,
                    True | False => self.expect_type(token, Type::Bool, result.ty)?,
                    _ => panic!("Unexpected constant type"),
                }
                syms.insert(
                    self.diag,
                    &self.unit.identifiers,
                    token.line_num,
                    result.id,
                    result.ty,
                )?;
                Some(op)
            }
            Operation::Ret(token, ret) => {
                let ret_ty = func_ty.ret;
                match ret {
                    Some(var) => {
                        let var =
                            syms.get(self.diag, &self.unit.identifiers, token.line_num, var.id)?;
                        if ret_ty == Type::Void {
                            token.error(self.diag, "Void functions cannot return a value.");
                            return None;
                        }

                        self.expect_type(token, var.ty, ret_ty)?;
                    }
                    None => {
                        if ret_ty != Type::Void {
                            token.error(self.diag, "Non-void functions must return a value.");
                            return None;
                        }
                    }
                }
                Some(op)
            }
            _ => Some(op),
        }
    }

    fn analyze(&mut self, cfg: &mut Cfg) -> Option<()> {
        let mut symbols = SymbolTable::default();
        for Variable { id, ty } in cfg.get_formals() {
            symbols.insert(
                self.diag,
                &self.unit.identifiers,
                cfg.get_function_token().line_num,
                *id,
                *ty,
            )?;
        }

        let mut w = RPOWorklist::new(cfg);
        (0..cfg.blocks().len()).for_each(|block| {
            w.push(block);
        });

        // Processing the Cfg in RPO order ensures that we see definitions of the
        // values before their use.
        while let Some(block_id) = w.pop() {
            let ops = cfg.remove_ops(block_id);
            let mut elaborated_ops = Vec::with_capacity(ops.len());
            for op in ops {
                elaborated_ops.push(self.analyze_op(op, &mut symbols, cfg.get_type(self.unit))?);
            }
            let last_op = elaborated_ops.last()?;
            if !last_op.is_terminator() {
                if cfg.get_type(self.unit).ret == Type::Void {
                    // Insert implicit return for void function.
                    let phantom_token = last_op.get_token();
                    elaborated_ops.push(Operation::Ret(phantom_token, None));
                } else {
                    last_op.get_token().error(
                        self.diag,
                        "Block terminator expected to be jump, br, or ret.",
                    );
                    return None;
                }
            }
            cfg.extend_block(block_id, elaborated_ops.iter());
        }
        Some(())
    }
}
