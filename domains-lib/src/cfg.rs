use analysis::cfg::*;

use crate::ast::*;

use std::fmt::Write;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Operation {
    Init(u32),
    Translation(u32),
    Rotation(u32),
}

impl From<&Operation> for Node {
    fn from(op: &Operation) -> Self {
        match *op {
            Operation::Init(id) => Node::Init(id),
            Operation::Translation(id) => Node::Translation(id),
            Operation::Rotation(id) => Node::Rotation(id),
        }
    }
}

// TODO: Should this have a phantom lifetime to express
//       its dependency on the CFG?
pub struct BasicBlock {
    operations: Vec<Operation>,
    succs: Vec<usize>,
    preds: Vec<usize>,
}

impl BasicBlock {
    pub fn new() -> Self {
        Self {
            operations: Vec::new(),
            succs: Vec::new(),
            preds: Vec::new(),
        }
    }
}

impl Default for BasicBlock {
    fn default() -> Self {
        Self::new()
    }
}

impl CfgBlock for BasicBlock {
    type Element = Operation;

    fn operations(&self) -> &[Self::Element] {
        &self.operations
    }

    fn predecessors(&self) -> Vec<usize> {
        self.preds.clone()
    }

    fn successors(&self) -> Vec<usize> {
        self.succs.clone()
    }
}

// TODO: Either introduce phantom lifetime to make the connection to ASTContext
//       explicit, or make it own the AST Context.
pub struct Cfg {
    basic_blocks: Vec<BasicBlock>,
}

impl ControlFlowGraph<BasicBlock> for Cfg {
    type Block = BasicBlock;

    fn blocks(&self) -> &[Self::Block] {
        &self.basic_blocks
    }
}

impl Cfg {
    pub fn new(ctx: &ASTContext) -> Self {
        let mut cfg = Cfg {
            basic_blocks: Vec::new(),
        };
        let start_block = cfg.new_block();
        let root = ctx.get_root();
        cfg.add_ast_node(start_block, root, ctx);
        cfg
    }

    fn new_block(&mut self) -> usize {
        self.basic_blocks.push(BasicBlock::new());
        self.basic_blocks.len() - 1
    }

    fn add_edge(&mut self, from: usize, to: usize) -> &mut Self {
        self.basic_blocks[from].succs.push(to);
        self.basic_blocks[to].preds.push(from);
        // TODO: assertion about no duplicates in succs/preds.
        self
    }

    fn add_ast_node(&mut self, mut current_block: usize, n: Node, ctx: &ASTContext) -> usize {
        match (n, ctx.node_to_ref(n)) {
            (Node::Init(init), _) => {
                self.basic_blocks[current_block]
                    .operations
                    .push(Operation::Init(init));
                current_block
            }
            (Node::Translation(trans), _) => {
                self.basic_blocks[current_block]
                    .operations
                    .push(Operation::Translation(trans));
                current_block
            }
            (Node::Rotation(rot), _) => {
                self.basic_blocks[current_block]
                    .operations
                    .push(Operation::Rotation(rot));
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
    let mut output = "digraph CFG {\n".to_owned();
    let anns = Annotations::new();
    for (counter, block) in cfg.basic_blocks.iter().enumerate() {
        write!(output, "  Node_{}[label=\"", counter).unwrap();
        for op in block.operations() {
            write!(output, "{}\\n", crate::ast::print(op.into(), ctx, &anns)).unwrap();
        }
        output.push_str("\"]\n");
    }
    output.push('\n');
    for (counter, block) in cfg.basic_blocks.iter().enumerate() {
        for next in block.successors() {
            writeln!(output, "  Node_{} -> Node_{}", counter, next).unwrap();
        }
    }
    output.push_str("}\n");
    output
}
