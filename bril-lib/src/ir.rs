use core::fmt::Display;
use std::collections::{hash_map::Entry, HashMap};
use utils::DiagnosticEmitter;

use crate::lexer::{Identifier, IdentifierTable, Location, Token, TokenValue};
use analysis::cfg::*;
use itertools::Itertools;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Type {
    Int,
    Bool,
    Void,
    Fn(usize),
}

impl Type {
    pub fn get_function_type(self, unit: &Unit) -> Option<&FunctionType> {
        match self {
            Type::Fn(id) => Some(&unit.function_types[id]),
            _ => None,
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Bool => write!(f, "bool"),
            Type::Void => write!(f, "void"),
            Type::Fn(_) => write!(f, "fn"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct FunctionType {
    pub ret: Type,
    pub formals: Vec<Type>,
}

#[derive(Clone, Debug, Copy, PartialEq, Eq, Hash)]
pub enum IdentifierType {
    Local,
    Global,
    Label,
}

#[derive(Clone, Copy, Debug)]
pub struct Variable {
    pub id: Identifier,
    pub ty: Type,
}

#[derive(Clone, Debug)]
pub enum Operation {
    BinaryOp {
        token: Token,
        result: Variable,
        lhs: Variable,
        rhs: Variable,
    },
    UnaryOp {
        token: Token,
        result: Variable,
        operand: Variable,
    },
    Jump(Token, Identifier),
    Branch {
        token: Token,
        cond: Variable,
        then: Identifier,
        els: Identifier,
    },
    Call {
        token: Token,
        callee: Variable,
        result: Option<Variable>,
        args: Vec<Variable>,
    },
    Ret(Token, Option<Variable>),
    Print(Token, Variable),
    Nop(Token),
    Const(Token, Variable),
}

impl Operation {
    pub fn get_token(&self) -> Token {
        // TODO: hoist token into a struct.
        use Operation::*;
        match self {
            BinaryOp { token, .. }
            | UnaryOp { token, .. }
            | Branch { token, .. }
            | Call { token, .. }
            | Jump(token, _)
            | Ret(token, _)
            | Print(token, _)
            | Nop(token)
            | Const(token, _) => *token,
        }
    }

    pub fn is_terminator(&self) -> bool {
        use Operation::*;
        matches!(self, Branch { .. } | Jump(_, _) | Ret(_, _))
    }
}

/// The last operation is a terminator:
/// * Branch
/// * Jump
/// * Ret
#[derive(Clone, Debug, Default)]
pub struct BasicBlock {
    operations: Vec<Operation>,
    succs: Vec<usize>,
    preds: Vec<usize>,
}

impl BasicBlock {
    pub fn new() -> Self {
        Self::default()
    }
}

impl CfgBlock for BasicBlock {
    type Operation = Operation;

    fn operations(&self) -> &[Self::Operation] {
        &self.operations
    }

    fn predecessors(&self) -> &[usize] {
        &self.preds
    }

    fn successors(&self) -> &[usize] {
        &self.succs
    }
}

#[derive(Clone, Debug)]
pub struct Cfg {
    basic_blocks: Vec<BasicBlock>,
    function: Token,
    ty: Type,
    formals: Vec<Variable>,
}

impl ControlFlowGraph for Cfg {
    type Block = BasicBlock;

    fn blocks(&self) -> &[Self::Block] {
        &self.basic_blocks
    }
}

impl MutableCfg for Cfg {
    fn new_block(&mut self) -> usize {
        self.basic_blocks.push(BasicBlock::new());
        self.basic_blocks.len() - 1
    }

    fn add_block(&mut self, block: <Self as ControlFlowGraph>::Block) -> usize {
        self.basic_blocks.push(block);
        self.basic_blocks.len() - 1
    }

    fn remove_block(&mut self, block: usize) -> <Self as ControlFlowGraph>::Block {
        self.basic_blocks.remove(block)
    }

    fn add_edge(&mut self, from: usize, to: usize) -> &mut Self {
        self.basic_blocks[from].succs.push(to);
        self.basic_blocks[to].preds.push(from);
        self
    }

    fn remove_edge(&mut self, from: usize, to: usize) -> &mut Self {
        self.basic_blocks[from].succs.retain(|b| *b != to);
        self.basic_blocks[to].succs.retain(|b| *b != from);
        self
    }
}

impl BlockMutableCfg for Cfg {
    fn extend_block<'cfg>(
        &'cfg mut self,
        block: usize,
        ops: impl Iterator<Item = &'cfg <<Self as ControlFlowGraph>::Block as CfgBlock>::Operation>,
    ) {
        self.basic_blocks[block].operations.extend(ops.cloned());
    }

    fn remove_ops(
        &mut self,
        block: usize,
    ) -> Vec<<<Self as ControlFlowGraph>::Block as CfgBlock>::Operation> {
        core::mem::take(&mut self.basic_blocks[block].operations)
    }
}

impl Cfg {
    pub fn new(function: Token, ty: Type, formals: Vec<Variable>) -> Self {
        Self {
            basic_blocks: Vec::default(),
            function,
            ty,
            formals,
        }
    }

    pub fn get_function(&self) -> Token {
        self.function
    }

    pub fn get_type<'ctx>(&self, unit: &'ctx Unit) -> &'ctx FunctionType {
        let Type::Fn(id) = self.ty
        else {
            panic!("Function type expected.");
        };
        &unit.function_types[id]
    }

    pub fn get_formals(&self) -> &[Variable] {
        &self.formals
    }
}

#[derive(Clone, Debug, Default)]
pub struct SymbolTable(HashMap<Identifier, Variable>);

impl SymbolTable {
    pub fn insert(
        &mut self,
        diag: &mut DiagnosticEmitter,
        ids: &IdentifierTable,
        loc: Location,
        var: Identifier,
        ty: Type,
    ) -> Option<()> {
        let existing = self.0.entry(var).or_insert(Variable { id: var, ty });
        if existing.ty != ty {
            diag.report(
                loc.0,
                &format!("at '{}'", ids.get_name(var)),
                &format!("Unexpected type '{}'. Expected '{}'.", ty, existing.ty),
            );
            return None;
        }
        Some(())
    }

    pub fn get(
        &self,
        diag: &mut DiagnosticEmitter,
        ids: &IdentifierTable,
        loc: Location,
        var: Identifier,
    ) -> Option<Variable> {
        match self.0.get(&var).copied() {
            None => {
                diag.report(
                    loc.0,
                    &format!("at '{}'", ids.get_name(var)),
                    "Undefined identifier.",
                );
                None
            }
            res => res,
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct LabelsToBlocks(HashMap<Identifier, usize>);

impl LabelsToBlocks {
    pub fn insert(
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

    pub fn get(
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

#[derive(Clone, Debug)]
pub struct Unit {
    pub functions: Vec<Cfg>,
    pub function_types: Vec<FunctionType>, // TODO: deduplicate.
    pub identifiers: IdentifierTable,
    pub globals: SymbolTable,
}

pub fn print_operation(op: &Operation, unit: &Unit) -> String {
    let get_name = |var: &Variable| unit.identifiers.get_name(var.id);
    let get_label_name = |&l: &Identifier| unit.identifiers.get_name(l);
    match op {
        Operation::BinaryOp {
            token,
            result,
            lhs,
            rhs,
        } => format!(
            "{}: {} = {} {} {};",
            get_name(result),
            result.ty,
            token.value,
            get_name(lhs),
            get_name(rhs)
        ),
        Operation::UnaryOp {
            token,
            result,
            operand,
        } => format!(
            "{}: {} = {} {};",
            get_name(result),
            result.ty,
            token.value,
            get_name(operand)
        ),
        Operation::Branch {
            cond, then, els, ..
        } => format!(
            "br {} {} {};",
            get_name(cond),
            get_label_name(then),
            get_label_name(els)
        ),
        Operation::Jump(_, id) => format!("jmp {};", get_label_name(id)),
        Operation::Const(tok, var) => {
            format!("{}: {} = const {};", get_name(var), var.ty, tok.value)
        }
        Operation::Call {
            callee,
            result: Some(result),
            args,
            ..
        } => format!(
            "{}: {} = call {} {};",
            get_name(result),
            result.ty,
            get_name(callee),
            args.iter().map(|v| get_name(v)).join(" ")
        ),
        Operation::Call {
            callee,
            result: None,
            args,
            ..
        } => format!(
            "call {} {};",
            get_name(callee),
            args.iter().map(|v| get_name(v)).join(" ")
        ),
        Operation::Print(_, v) => format!("print {};", get_name(v)),
        Operation::Nop(_) => "nop;".to_owned(),
        Operation::Ret(_, Some(v)) => format!("ret {};", get_name(v)),
        Operation::Ret(_, None) => "ret;".to_owned(),
    }
}

pub fn print_cfg(cfg: &Cfg, unit: &Unit) -> String {
    let TokenValue::Id(id) = cfg.function.value
    else {
        panic!("");
    };
    let name = format!("\"{}\"", &unit.identifiers.get_name(id));
    analysis::cfg::print(Some(&name), cfg, |op| print_operation(op, unit))
}

pub fn print(unit: &Unit) -> String {
    let mut result = String::new();
    for cfg in &unit.functions {
        result.push_str(&print_cfg(cfg, unit));
        result.push('\n');
    }
    result.pop();
    result
}
