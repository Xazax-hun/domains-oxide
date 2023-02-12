use core::fmt::Write;
use priority_queue::PriorityQueue;
use std::collections::HashSet;

/// Trait for an immutable basic block of a control flow graph.
pub trait CfgBlock {
    type Operation;

    /// The operations in the basic block are always
    /// executed in the same order as they appear in
    /// the result of this method, the terminator is
    /// the last operation.
    fn operations(&self) -> &[Self::Operation];

    /// Returns the unique identifiers of successors.
    /// These identifiers can be used to index into
    /// the result of [ControlFlowGraph::blocks].
    fn successors(&self) -> &[usize];

    /// Returns the unique identifiers of predecessors.
    /// These identifiers can be used to index into
    /// the result of [ControlFlowGraph::blocks].
    fn predecessors(&self) -> &[usize];
}

/// Trait for an immutable control flow graph. It has a list
/// of basic blocks containing sequentially executed code, and
/// directed edges between these blocks.
///
/// Use [RPOWorklist] to traverse the graph.
pub trait ControlFlowGraph {
    type Block: CfgBlock;

    /// Returns the list of basic blocks in the control flow graph.
    /// The first element of the result is the entry block of the
    /// graph.
    fn blocks(&self) -> &[Self::Block];
}

/// Trait for a control flow graph where the shape of the graph
/// can be edited.
pub trait MutableCfg: ControlFlowGraph {
    fn add_edge(&mut self, from: usize, to: usize) -> &mut Self;
    fn remove_edge(&mut self, from: usize, to: usize) -> &mut Self;
    fn new_block(&mut self) -> usize;
    fn add_block(&mut self, block: <Self as ControlFlowGraph>::Block) -> usize;
    fn remove_block(&mut self, block: usize) -> <Self as ControlFlowGraph>::Block;

    fn add_edges(&mut self, edges: &[(usize, usize)]) -> &mut Self {
        for &(from, to) in edges {
            self.add_edge(from, to);
        }
        self
    }

    fn remove_edges(&mut self, edges: &[(usize, usize)]) -> &mut Self {
        for &(from, to) in edges {
            self.remove_edge(from, to);
        }
        self
    }
}

/// Trait for a control flow graph where both the shape and the individual
/// operations can be edited. It is useful to do transformations on the
/// control flow graph like reversing it, or removing unreachable nodes.
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

/// Reverse a mutable control flow graph.
pub fn reverse<Cfg: BlockMutableCfg + Default>(cfg: &Cfg) -> Cfg {
    let mut reversed = Cfg::default();
    reverse_in_place(cfg, &mut reversed);
    reversed
}

/// Reverse a mutable control flow graph in place.
pub fn reverse_in_place<Cfg: BlockMutableCfg>(cfg: &Cfg, empty: &mut Cfg) {
    let block_num = cfg.blocks().len();
    for block in cfg.blocks().iter().rev() {
        let new_block = empty.new_block();
        empty.extend_block(new_block, block.operations().iter().rev());
    }
    for (id, block) in cfg.blocks().iter().rev().enumerate() {
        for pred in block.predecessors() {
            empty.add_edge(id, block_num - 1 - pred);
        }
    }
}

// TODO: add other helpers like cleaning up unreachable nodes.

/// Print the control flow graph in dot language that can be rendered
/// as a picture using graphviz. The `printer` is responsible for
/// rendering the individual operations and it has to do its own
/// dot escaping.
pub fn print<Cfg, OpPrinter>(cfg: &Cfg, printer: OpPrinter) -> String
where
    Cfg: ControlFlowGraph,
    OpPrinter: Fn(&<<Cfg as ControlFlowGraph>::Block as CfgBlock>::Operation) -> String,
{
    let mut output = "digraph CFG {\n".to_owned();
    for (counter, block) in cfg.blocks().iter().enumerate() {
        write!(output, "  Node_{counter}[label=\"").unwrap();
        // TODO: add escaping.
        let text: Vec<_> = block.operations().iter().map(&printer).collect();
        output.push_str(&text.join("\\n"));
        output.push_str("\"]\n");
    }
    output.push('\n');
    for (counter, block) in cfg.blocks().iter().enumerate() {
        for next in block.successors() {
            writeln!(output, "  Node_{counter} -> Node_{next}").unwrap();
        }
    }
    output.push_str("}\n");
    output
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
enum Color {
    White,
    Gray,
    Black,
}

/// In structured programming all back edges need to point to a loop header
/// node that dominates all the nodes inside the loop. This function will
/// get the back edges from a structured program.
pub fn get_back_edges<Cfg>(cfg: &Cfg) -> HashSet<(usize, usize)>
where
    Cfg: ControlFlowGraph,
{
    let mut color = vec![Color::White; cfg.blocks().len()];
    let mut processing = Vec::new();
    let mut back_edges = HashSet::new();

    processing.push(0_usize);

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

/// A worklist to visit the nodes of a control flow graph in
/// reverse post-order. The worklist can contain each node
/// at most once at a given time.
#[derive(Clone, Debug)]
pub struct RPOWorklist<'cfg, Cfg: ControlFlowGraph> {
    queue: PriorityQueue<usize, usize>,
    rpo_order: Vec<usize>,
    cfg: &'cfg Cfg,
}

impl<'cfg, Cfg: ControlFlowGraph> RPOWorklist<'cfg, Cfg> {
    /// Creates a new RPOWorklist by computing the reverse-post order.
    /// It is more efficient to clear a RPOWorklist and reuse it for
    /// another traversal than creating it from scratch.
    pub fn new(cfg: &'cfg Cfg) -> Self {
        // TODO: look into deduplicating this and get_back_edges
        let mut counter = 0_usize;
        let mut color = vec![Color::White; cfg.blocks().len()];
        let mut processing = Vec::new();
        let mut worklist = Self {
            queue: PriorityQueue::<usize, usize>::new(),
            rpo_order: vec![0; cfg.blocks().len()],
            cfg,
        };

        processing.push(0_usize);

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

    /// Push a new node into the worklist. Returns true if it was
    /// successful, false if the node was already in the worklist.
    pub fn push(&mut self, block: usize) -> bool {
        self.queue.push(block, self.rpo_order[block]).is_none()
    }

    /// Push all the successors of a node into the worklist.
    pub fn push_successors(&mut self, block: usize) {
        for succ in self.cfg.blocks()[block].successors() {
            self.push(*succ);
        }
    }

    /// Pop a node from the worklist, returns None when the worklist
    /// was empty.
    pub fn pop(&mut self) -> Option<usize> {
        self.queue.pop().map(|x| x.0)
    }

    /// Returns the position of a node in the reverse post-order.
    /// The lower the returned value, the earlier a node would
    /// be visited.
    pub fn get_rpo_order(&self, block: usize) -> usize {
        self.rpo_order.len() - 1 - self.rpo_order[block]
    }

    /// Removes all the nodes from the worklist.
    pub fn clear(&mut self) {
        self.queue.clear()
    }
}
