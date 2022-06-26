pub trait CfgBlock {
    type Element;

    fn operations(&self) -> &[Self::Element];
    fn successors(&self) -> Vec<usize>;
    fn predecessors(&self) -> Vec<usize>;
}

pub trait ControlFlowGraph<Block: CfgBlock> {
    type Block;
    fn blocks(&self) -> &[Block];
}
