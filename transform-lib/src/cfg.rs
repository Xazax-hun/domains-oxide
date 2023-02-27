use analysis::cfg::*;

use crate::ast::{self, *};

// TODO: Should this have a phantom lifetime to express
//       its dependency on the CFG?
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
struct CfgImpl<'ctx> {
    basic_blocks: Vec<BasicBlock>,
    context: &'ctx ASTContext,
}

#[derive(Clone, Debug)]
pub struct Cfg<'ctx>(CfgImpl<'ctx>);

impl<'ctx> ControlFlowGraph for CfgImpl<'ctx> {
    type Block = BasicBlock;

    fn blocks(&self) -> &[Self::Block] {
        &self.basic_blocks
    }
}

impl<'ctx> MutableCfg for CfgImpl<'ctx> {
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

impl<'ctx> BlockMutableCfg for CfgImpl<'ctx> {
    fn extend_block<'cfg>(
        &'cfg mut self,
        block: usize,
        ops: impl Iterator<Item = &'cfg <<Self as ControlFlowGraph>::Block as CfgBlock>::Operation>,
    ) {
        self.basic_blocks[block].operations.extend(ops);
    }

    fn remove_ops(
        &mut self,
        block: usize,
    ) -> Vec<<<Self as ControlFlowGraph>::Block as CfgBlock>::Operation> {
        core::mem::take(&mut self.basic_blocks[block].operations)
    }
}

impl<'ctx> ControlFlowGraph for Cfg<'ctx> {
    type Block = BasicBlock;

    fn blocks(&self) -> &[Self::Block] {
        self.0.blocks()
    }
}

impl<'ctx> Cfg<'ctx> {
    pub fn new(ctx: &'ctx ASTContext) -> Self {
        let mut cfg = Cfg(CfgImpl {
            basic_blocks: Vec::default(),
            context: ctx,
        });
        let root = ctx.get_root();
        let start_block = cfg.0.new_block();
        cfg.0.add_ast_node(start_block, root, ctx);
        cfg
    }

    pub fn context(&self) -> &'ctx ASTContext {
        self.0.context
    }

    fn empty(ctx: &'ctx ASTContext) -> Self {
        Cfg(CfgImpl {
            basic_blocks: Vec::default(),
            context: ctx,
        })
    }
}

impl<'ctx> CfgImpl<'ctx> {
    fn add_ast_node(&mut self, mut current_block: usize, n: Node, ctx: &ASTContext) -> usize {
        match (n, ctx.node_to_ref(n)) {
            (Node::Operation(op), _) => {
                self.basic_blocks[current_block].operations.push(op);
                current_block
            }
            (_, NodeRef::Sequence(seq_ref)) => {
                for child in &seq_ref.nodes {
                    current_block = self.add_ast_node(current_block, *child, ctx);
                }
                current_block
            }
            (_, NodeRef::Branch(branch)) => {
                let lhs_block = self.new_block();
                let rhs_block = self.new_block();
                let branch_pred = current_block;
                let lhs_after = self.add_ast_node(lhs_block, branch.lhs, ctx);
                let rhs_after = self.add_ast_node(rhs_block, branch.rhs, ctx);
                self.add_edge(branch_pred, lhs_block);
                self.add_edge(branch_pred, rhs_block);
                let after_branch = self.new_block();
                self.add_edge(lhs_after, after_branch);
                self.add_edge(rhs_after, after_branch);
                after_branch
            }
            (_, NodeRef::Loop(loop_)) => {
                let body_begin = self.new_block();
                self.add_edge(current_block, body_begin);
                let body_end = self.add_ast_node(body_begin, loop_.body, ctx);
                let after_body = self.new_block();
                self.add_edge(body_end, body_begin);
                self.add_edge(body_end, after_body);
                after_body
            }
            _ => panic!(),
        }
    }
}

pub fn print(cfg: &Cfg, ctx: &ASTContext) -> String {
    let anns = Annotations::new();
    analysis::cfg::print(None, cfg, |&op| ast::print(Node::Operation(op), ctx, &anns))
}

pub fn reverse<'ctx>(cfg: &Cfg<'ctx>) -> Cfg<'ctx> {
    let mut reversed = Cfg::empty(cfg.context());
    analysis::cfg::reverse_in_place(&cfg.0, &mut reversed.0);
    reversed
}
