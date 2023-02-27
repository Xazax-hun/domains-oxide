use core::fmt::Display;
use std::collections::HashMap;

use crate::lexer::{Identifier, IdentifierTable, Location, Token, TokenValue};
use analysis::cfg::*;
use itertools::Itertools;

#[derive(Clone, Debug)]
pub enum Type {
    Int,
    Bool,
    Void,
    Fn(usize),
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

#[derive(Clone, Debug)]
pub struct Variable {
    pub id: Identifier,
    pub ty: Type,
}

#[derive(Clone, Debug)]
pub struct BinaryOp {
    pub token: Token,
    pub result: Variable,
    pub lhs: Variable,
    pub rhs: Variable,
}

#[derive(Clone, Debug)]
pub struct UnaryOp {
    pub token: Token,
    pub result: Variable,
    pub operand: Variable,
}

#[derive(Clone, Debug)]
pub struct Call {
    pub location: Location,
    pub callee: Variable,
    pub result: Option<Variable>,
    pub args: Vec<Variable>,
}

#[derive(Clone, Debug)]
pub struct Target(pub usize);

#[derive(Clone, Debug)]
pub struct Branch {
    pub location: Location,
    pub cond: Variable,
    pub then: Target,
    pub els: Target,
}

#[derive(Clone, Debug)]
pub enum Operation {
    BinOp(BinaryOp),
    UnOp(UnaryOp),
    Jump(Location, Target),
    Br(Branch),
    Call(Call),
    Ret(Location, Variable),
    Print(Location, Variable),
    Nop(Location),
    Const(Token, Variable),
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
    ret_ty: Type,
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
    pub fn new(function: Token, ret_ty: Type, formals: Vec<Variable>) -> Self {
        Self {
            basic_blocks: Vec::default(),
            function,
            ret_ty,
            formals,
        }
    }

    pub fn get_function(&self) -> Token {
        self.function
    }

    pub fn get_return_type(&self) -> Type {
        self.ret_ty.clone()
    }

    pub fn get_formals(&self) -> &[Variable] {
        &self.formals
    }
}

pub type SymbolTable = HashMap<Identifier, Variable>;

#[derive(Clone, Debug)]
pub struct Unit {
    pub functions: Vec<Cfg>,
    pub function_types: Vec<FunctionType>,
    pub identifier_table: IdentifierTable,
    pub globals: SymbolTable,
}

pub fn print_operation(op: &Operation, unit: &Unit) -> String {
    let get_name = |var: &Variable| unit.identifier_table.get_name(var.id);
    match op {
        Operation::BinOp(BinaryOp {
            token,
            result,
            lhs,
            rhs,
        }) => format!(
            "{}: {} = {} {} {};",
            get_name(result),
            result.ty,
            token.value,
            get_name(lhs),
            get_name(rhs)
        ),
        Operation::UnOp(UnaryOp {
            token,
            result,
            operand,
        }) => format!(
            "{}: {} = {} {};",
            get_name(result),
            result.ty,
            token.value,
            get_name(operand)
        ),
        Operation::Br(Branch { cond, .. }) => format!("br {};", get_name(cond)),
        Operation::Jump(_, _) => "jmp;".to_owned(),
        Operation::Const(tok, var) => {
            format!("{}: {} = const {};", get_name(var), var.ty, tok.value)
        }
        Operation::Call(Call {
            callee,
            result: Some(result),
            args,
            ..
        }) => format!(
            "{}: {} = call {} {};",
            get_name(result),
            result.ty,
            get_name(callee),
            args.iter().map(|v| get_name(v)).join(" ")
        ),
        Operation::Call(Call {
            callee,
            result: None,
            args,
            ..
        }) => format!(
            "call {} {};",
            get_name(callee),
            args.iter().map(|v| get_name(v)).join(" ")
        ),
        Operation::Print(_, v) => format!("print {};", get_name(v)),
        Operation::Nop(_) => "nop;".to_owned(),
        Operation::Ret(_, v) => format!("ret {};", get_name(v)),
    }
}

pub fn print_cfg(cfg: &Cfg, unit: &Unit) -> String {
    let TokenValue::Id(id) = cfg.function.value
    else {
        panic!("");
    };
    let name = format!("\"{}\"", &unit.identifier_table.get_name(id));
    analysis::cfg::print(Some(&name), cfg, |op| print_operation(op, unit))
}

pub fn print(unit: &Unit) -> String {
    let mut result = String::new();
    for cfg in &unit.functions {
        result.push_str(&print_cfg(cfg, &unit));
        result.push('\n');
    }
    result.pop();
    result
}
