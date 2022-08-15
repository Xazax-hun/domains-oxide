use std::collections::HashSet;
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

#[derive(PartialEq, Eq, Clone, Copy)]
enum Color {
    White,
    Gray,
    Black,
}

pub fn get_back_edges<Cfg>(cfg: &Cfg) -> HashSet<(usize, usize)>
where
    Cfg: ControlFlowGraph,
{
    let mut color = vec![Color::White; cfg.blocks().len()];
    let mut processing = Vec::new();
    let mut back_edges = HashSet::new();

    processing.push(0usize);

    while !processing.is_empty() {
        let current = processing.pop().unwrap();
        match color[current] {
            Color::White => {
                color[current] = Color::Gray;

                // Edges pointing to a node that we visited once (Grey) but have not
                // finished processing its successor subgraph are the back edges.
                let (mut back_succs, mut fwd_succs) = (Vec::new(), Vec::new());

                for succ in cfg.blocks()[current].successors() {
                    // Gray successors are back edges. Black successors are already processed
                    // on a different path. We only need continue processing unexplored (White)
                    // nodes.
                    match color[*succ] {
                        Color::White => fwd_succs.push(*succ),
                        Color::Gray => back_succs.push(*succ),
                        Color::Black => continue,
                    }
                }

                // When the node popped the second time, we are done processing all
                // of the reachable subgraph from the node. We can turn its color to Black.
                processing.push(current);
                processing.extend(fwd_succs);

                let new_back_edges: HashSet<(usize, usize)> =
                    back_succs.iter().map(|succ| (current, *succ)).collect();
                back_edges.extend(new_back_edges);
            }
            Color::Gray => color[current] = Color::Black,
            Color::Black => continue,
        }
    }

    back_edges
}
