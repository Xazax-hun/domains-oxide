use core::fmt::Display;
use core::fmt::Write;
use std::collections::HashMap;
use utils::DiagnosticEmitter;

use crate::lexer::{Identifier, IdentifierTable, Location, Token, TokenValue};
use analysis::cfg::*;
use itertools::Itertools;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Type {
    Placeholder, // Used before semantic elaboration.
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
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Type::Placeholder => write!(f, "placeholder"),
            Type::Int => write!(f, "int"),
            Type::Bool => write!(f, "bool"),
            Type::Void => write!(f, "void"),
            Type::Fn(_) => write!(f, "fn"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FunctionType {
    pub ret: Type,
    pub formals: Vec<Type>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Variable {
    pub id: Identifier,
    pub ty: Type,
}

impl Variable {
    pub fn placeholder(id: Identifier) -> Self {
        Self {
            id,
            ty: Type::Placeholder,
        }
    }
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
        use Operation as O;
        match self {
            O::BinaryOp { token, .. }
            | O::UnaryOp { token, .. }
            | O::Branch { token, .. }
            | O::Call { token, .. }
            | O::Jump(token, _)
            | O::Ret(token, _)
            | O::Print(token, _)
            | O::Nop(token)
            | O::Const(token, _) => *token,
        }
    }

    pub fn is_terminator(&self) -> bool {
        use Operation as O;
        matches!(self, O::Branch { .. } | O::Jump(_, _) | O::Ret(_, _))
    }

    pub fn get_result(&self) -> Option<Variable> {
        use Operation as O;
        match self {
            O::BinaryOp { result, .. } | O::UnaryOp { result, .. } | O::Const(_, result) => {
                Some(*result)
            }
            O::Call { result, .. } => *result,
            _ => None,
        }
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

    pub fn get_function_token(&self) -> Token {
        self.function
    }

    pub fn get_function(&self) -> Identifier {
        let tok = self.get_function_token();
        let TokenValue::Global(id) = tok.value else {
            panic!("Unexpected function name.");
        };
        id
    }

    pub fn get_type<'ctx>(&self, unit: &'ctx Unit) -> &'ctx FunctionType {
        let Type::Fn(id) = self.ty else {
            panic!("Function type expected.");
        };
        &unit.function_types[id]
    }

    pub fn get_formals(&self) -> &[Variable] {
        &self.formals
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
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

#[derive(Clone, Debug)]
pub struct Unit {
    pub functions: Vec<Cfg>,
    pub identifiers: IdentifierTable,
    pub globals: SymbolTable,
    function_types: Vec<FunctionType>,
}

impl Unit {
    pub fn new(identifiers: IdentifierTable) -> Self {
        Self {
            functions: Vec::new(),
            identifiers,
            globals: SymbolTable::default(),
            function_types: Vec::new(),
        }
    }

    pub fn get_function(&self, func: Identifier) -> Option<&Cfg> {
        self.functions
            .iter()
            .find(|&cfg| cfg.get_function() == func)
    }

    pub fn add_function_type(&mut self, ty: FunctionType) -> Type {
        // TODO: make this more efficient
        if let Some(pos) = self
            .function_types
            .iter()
            .position(|existing| *existing == ty)
        {
            Type::Fn(pos)
        } else {
            self.function_types.push(ty);
            Type::Fn(self.function_types.len() - 1)
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Annotations {
    pub pre: HashMap<OpPos, Vec<String>>,
    pub post: HashMap<OpPos, Vec<String>>,
}

pub fn print_operation(pos: OpPos, op: &Operation, unit: &Unit, anns: &Annotations) -> String {
    let get_name = |var: &Variable| unit.identifiers.get_name(var.id);
    let get_label_name = |&l: &Identifier| unit.identifiers.get_name(l);
    let mut printed = String::new();
    if let Some(mut ann_list) = anns.pre.get(&pos).cloned() {
        if !ann_list.is_empty() {
            ann_list.sort_unstable();
            write!(printed, "/* {} */ ", ann_list.join(", ")).expect("");
        }
    }
    match op {
        Operation::BinaryOp {
            token,
            result,
            lhs,
            rhs,
        } => write!(
            printed,
            "{}: {} = {} {} {};",
            get_name(result),
            result.ty,
            token.value,
            get_name(lhs),
            get_name(rhs)
        )
        .expect(""),
        Operation::UnaryOp {
            token,
            result,
            operand,
        } => write!(
            printed,
            "{}: {} = {} {};",
            get_name(result),
            result.ty,
            token.value,
            get_name(operand)
        )
        .expect(""),
        Operation::Branch {
            cond, then, els, ..
        } => write!(
            printed,
            "br {} {} {};",
            get_name(cond),
            get_label_name(then),
            get_label_name(els)
        )
        .expect(""),
        Operation::Jump(_, id) => write!(printed, "jmp {};", get_label_name(id)).expect(""),
        Operation::Const(tok, var) => write!(
            printed,
            "{}: {} = const {};",
            get_name(var),
            var.ty,
            tok.value
        )
        .expect(""),
        Operation::Call {
            callee,
            result: Some(result),
            args,
            ..
        } => write!(
            printed,
            "{}: {} = call {} {};",
            get_name(result),
            result.ty,
            get_name(callee),
            args.iter().map(get_name).join(" ")
        )
        .expect(""),
        Operation::Call {
            callee,
            result: None,
            args,
            ..
        } => write!(
            printed,
            "call {} {};",
            get_name(callee),
            args.iter().map(get_name).join(" ")
        )
        .expect(""),
        Operation::Print(_, v) => write!(printed, "print {};", get_name(v)).expect(""),
        Operation::Nop(_) => printed.push_str("nop;"),
        Operation::Ret(_, Some(v)) => write!(printed, "ret {};", get_name(v)).expect(""),
        Operation::Ret(_, None) => printed.push_str("ret;"),
    };
    if let Some(mut ann_list) = anns.post.get(&pos).cloned() {
        if !ann_list.is_empty() {
            ann_list.sort_unstable();
            write!(printed, " /* {} */", ann_list.join(", ")).expect("");
        }
    }
    printed
}

pub fn print_cfg_dot(cfg: &Cfg, unit: &Unit) -> String {
    let id = cfg.get_function();
    let name = format!("\"{}\"", &unit.identifiers.get_name(id));
    let anns = Annotations::default();
    analysis::cfg::print(Some(&name), cfg, |pos, op| {
        print_operation(pos, op, unit, &anns)
    })
}

fn get_block_name<'unit>(unit: &'unit Unit, cfg: &Cfg, block_id: usize) -> Option<&'unit str> {
    let block = &cfg.blocks()[block_id];
    if let Some(pred_id) = block.predecessors().iter().next() {
        let pred = &cfg.blocks()[*pred_id];
        let terminator = pred.operations().last().unwrap();
        match terminator {
            Operation::Jump(_, label) => return Some(unit.identifiers.get_name(*label)),
            Operation::Branch { then, els, .. } => {
                let which_succ = pred
                    .successors()
                    .iter()
                    .position(|&b| b == block_id)
                    .unwrap();
                assert!(which_succ < 2);
                return Some(if which_succ == 0 {
                    unit.identifiers.get_name(*then)
                } else {
                    unit.identifiers.get_name(*els)
                });
            }
            _ => panic!("Unexpected terminator for predecessors.\n"),
        }
    }
    None
}

pub fn print_cfg(cfg: &Cfg, unit: &Unit, anns: &Annotations) -> String {
    let mut result = String::new();
    result.push_str(unit.identifiers.get_name(cfg.get_function()));
    let formals = cfg.get_formals();
    let ret_ty = cfg.get_type(unit).ret;
    if !formals.is_empty() || !matches!(ret_ty, Type::Void) {
        result.push('(');
        let mut formal_strs = formals.iter().map(|var| {
            let mut formal_str = String::new();
            formal_str.push_str(unit.identifiers.get_name(var.id));
            formal_str.push_str(": ");
            formal_str.push_str(&var.ty.to_string());
            formal_str
        });
        result.push_str(&formal_strs.join(", "));
        result.push_str("): ");
        result.push_str(&ret_ty.to_string());
    }
    result.push_str(" {\n");
    for (block_id, block) in cfg.blocks().iter().enumerate() {
        if let Some(name) = get_block_name(unit, cfg, block_id) {
            result.push_str(&format!("{name}:\n"));
        }
        for (op_id, op) in block.operations().iter().enumerate() {
            result.push_str(&format!(
                "  {}\n",
                print_operation(OpPos { block_id, op_id }, op, unit, anns)
            ));
        }
        result.push('\n');
    }
    result.pop();

    result.push_str("}\n");
    result
}

pub fn print_dot(unit: &Unit) -> String {
    let mut result = String::new();
    for cfg in &unit.functions {
        result.push_str(&print_cfg_dot(cfg, unit));
        result.push('\n');
    }
    result.pop();
    result
}

pub type AnnotationMap = HashMap<Identifier, Annotations>;

pub fn print(unit: &Unit, anns: &AnnotationMap) -> String {
    let mut result = String::new();
    let default = Annotations::default();
    for cfg in &unit.functions {
        let ann = anns.get(&cfg.get_function()).unwrap_or(&default);
        result.push_str(&print_cfg(cfg, unit, ann));
        result.push('\n');
    }
    result.pop();
    result
}
