use analysis::cfg::*;

use crate::ast::*;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Operation {
    Init(u32),
    Translation(u32),
    Rotation(u32),
}

impl Into<Node> for &Operation {
    fn into(self) -> Node {
        match self {
            Operation::Init(id) => Node::Init(*id),
            Operation::Translation(id) => Node::Translation(*id),
            Operation::Rotation(id) => Node::Rotation(*id),
        }
    }
}

pub struct BasicBlock {
    operations: Vec<Operation>,
    succs: Vec<usize>,
    preds: Vec<usize>,
}

impl BasicBlock {
    pub fn new() -> Self {
        BasicBlock {
            operations: Vec::new(),
            succs: Vec::new(),
            preds: Vec::new(),
        }
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
        let node_ref = ctx.node_to_ref(n);
        match (n, node_ref) {
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
                    current_block = self.add_ast_node(current_block, child.clone(), ctx);
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
    let mut output = String::new();
    output.push_str("digraph CFG {\n");
    let anns = Annotations::new();
    let mut counter = 0;
    for block in &cfg.basic_blocks {
        output.push_str(&format!("  Node_{}[label=\"", counter));
        for op in block.operations() {
            output.push_str(&format!("{}\\n", crate::ast::print(op.into(), ctx, &anns)));
        }
        output.push_str("\"]\n");
        counter += 1;
    }
    output.push('\n');
    let mut counter = 0;
    for block in &cfg.basic_blocks {
        for next in block.successors() {
            output.push_str(&format!("  Node_{} -> Node_{}\n", counter, next))
        }
        counter += 1;
    }
    output.push_str("}\n");
    output
}
