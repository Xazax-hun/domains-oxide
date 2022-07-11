use std::fmt::Write;

pub trait CfgBlock {
    type Element;

    fn operations(&self) -> &[Self::Element];
    fn successors(&self) -> &[usize];
    fn predecessors(&self) -> &[usize];
}

pub trait ControlFlowGraph {
    type Block: CfgBlock;
    fn blocks(&self) -> &[Self::Block];
}

pub fn print<Cfg, OpPrinter>(cfg: &Cfg, printer: OpPrinter) -> String
where
    Cfg: ControlFlowGraph,
    OpPrinter: Fn(&<<Cfg as ControlFlowGraph>::Block as CfgBlock>::Element) -> String,
{
    let mut output = "digraph CFG {\n".to_owned();
    for (counter, block) in cfg.blocks().iter().enumerate() {
        write!(output, "  Node_{}[label=\"", counter).unwrap();
        let text: Vec<_> = block.operations().iter().map(&printer).collect();
        output.push_str(&text.join("\\n"));
        output.push_str("\"]\n");
    }
    output.push('\n');
    for (counter, block) in cfg.blocks().iter().enumerate() {
        for next in block.successors() {
            writeln!(output, "  Node_{} -> Node_{}", counter, next).unwrap();
        }
    }
    output.push_str("}\n");
    output
}
