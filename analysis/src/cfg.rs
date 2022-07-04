pub trait CfgBlock {
    type Element;

    fn operations(&self) -> &[Self::Element];
    fn successors(&self) -> &[usize];
    fn predecessors(&self) -> &[usize];
}

pub trait ControlFlowGraph<Block: CfgBlock> {
    type Block;
    fn blocks(&self) -> &[Block];
}
