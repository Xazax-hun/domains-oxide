use priority_queue::PriorityQueue;
use std::collections::HashSet;
use std::fmt::Write;

pub trait CfgBlock {
    type Operation;

    fn operations(&self) -> &[Self::Operation];
    fn successors(&self) -> &[usize];
    fn predecessors(&self) -> &[usize];
}

pub trait ControlFlowGraph {
    type Block: CfgBlock;
    fn blocks(&self) -> &[Self::Block];
}

pub trait MutableCfg: ControlFlowGraph + Default {
    fn add_edge(&mut self, from: usize, to: usize) -> &mut Self;
    fn remove_edge(&mut self, from: usize, to: usize) -> &mut Self;
    fn new_block(&mut self) -> usize;
    fn add_block(&mut self, block: <Self as ControlFlowGraph>::Block) -> usize;
    fn remove_block(&mut self, block: usize) -> <Self as ControlFlowGraph>::Block;
}

pub trait BlockMutableCfg: MutableCfg {
    fn extend_block<'cfg>(
        &'cfg mut self,
        block: usize,
        ops: impl Iterator<Item = &'cfg <<Self as ControlFlowGraph>::Block as CfgBlock>::Operation>,
    );
    fn remove_ops(
        &mut self,
        block: usize,
    ) -> Vec<<<Self as ControlFlowGraph>::Block as CfgBlock>::Operation>;
}

pub fn reverse<Cfg: BlockMutableCfg>(cfg: &Cfg) -> Cfg {
    let mut reversed = Cfg::default();
    let block_num = cfg.blocks().len();
    for block in cfg.blocks().iter().rev() {
        let new_block = reversed.new_block();
        reversed.extend_block(new_block, block.operations().iter().rev());
    }
    for (id, block) in cfg.blocks().iter().rev().enumerate() {
        for pred in block.predecessors() {
            reversed.add_edge(id, block_num - 1 - pred);
        }
    }
    reversed
}

pub fn print<Cfg, OpPrinter>(cfg: &Cfg, printer: OpPrinter) -> String
where
    Cfg: ControlFlowGraph,
    OpPrinter: Fn(&<<Cfg as ControlFlowGraph>::Block as CfgBlock>::Operation) -> String,
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

pub struct RPOWorklist<'a, Cfg: ControlFlowGraph> {
    queue: PriorityQueue<usize, usize>,
    rpo_order: Vec<usize>,
    cfg: &'a Cfg,
}

impl<'cfg, Cfg: ControlFlowGraph> RPOWorklist<'cfg, Cfg> {
    pub fn new(cfg: &'cfg Cfg) -> Self {
        // TODO: look into deduplicating this and get_back_edges
        let mut counter = 0usize;
        let mut color = vec![Color::White; cfg.blocks().len()];
        let mut processing = Vec::new();
        let mut worklist = Self {
            queue: PriorityQueue::<usize, usize>::new(),
            rpo_order: vec![0; cfg.blocks().len()],
            cfg,
        };

        processing.push(0usize);

        while !processing.is_empty() {
            let current = processing.pop().unwrap();
            match color[current] {
                Color::White => {
                    color[current] = Color::Gray;
                    processing.push(current);

                    for succ in cfg.blocks()[current].successors() {
                        if color[*succ] == Color::White {
                            processing.push(*succ);
                        }
                    }
                }
                Color::Gray => {
                    color[current] = Color::Black;
                    worklist.rpo_order[current] = counter;
                    counter += 1;
                }
                Color::Black => continue,
            }
        }

        worklist
    }

    pub fn push(&mut self, block: usize) {
        self.queue.push(block, self.rpo_order[block]);
    }

    pub fn push_successors(&mut self, block: usize) {
        for succ in self.cfg.blocks()[block].successors() {
            self.push(*succ);
        }
    }

    pub fn pop(&mut self) -> Option<usize> {
        self.queue.pop().map(|x| x.0)
    }

    pub fn get_rpo_order(&self, block: usize) -> usize {
        self.rpo_order.len() - 1 - self.rpo_order[block]
    }
}
