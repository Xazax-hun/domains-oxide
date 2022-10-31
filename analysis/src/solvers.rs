use super::cfg::*;
use super::domains::*;

pub struct SolveMonotone {
    pub node_limit: usize,
}

impl Default for SolveMonotone {
    fn default() -> Self {
        Self { node_limit: 20 }
    }
}

impl SolveMonotone {
    pub fn transfer_blocks<Cfg, D, F>(self, cfg: &Cfg, transfer: &mut F) -> Vec<D>
    where
        Cfg: ControlFlowGraph,
        D: Domain,
        F: FnMut(usize, &<Cfg as ControlFlowGraph>::Block, &D) -> D,
    {
        let limit = self.node_limit * cfg.blocks().len();
        let mut processed_nodes = 0usize;
        let mut post_states = vec![D::bottom(); cfg.blocks().len()];
        let mut visited = vec![false; cfg.blocks().len()];

        let mut worklist = RPOWorklist::new(cfg);
        worklist.push(0usize);
        while let Some(current) = worklist.pop() {
            if limit > 0 && processed_nodes >= limit {
                return Vec::new();
            }

            let mut pre_state = D::bottom();
            for pred in cfg.blocks()[current].predecessors() {
                pre_state = pre_state.join(&post_states[*pred]);
            }
            let post_state = transfer(current, &cfg.blocks()[current], &pre_state);

            processed_nodes += 1;
            if visited[current] && post_states[current] == post_state {
                continue;
            }

            visited[current] = true;
            post_states[current] = post_state;
            worklist.push_successors(current);
        }

        post_states
    }

    pub fn transfer_operations<Cfg, D, F>(self, cfg: &Cfg, transfer: &mut F) -> Vec<D>
    where
        Cfg: ControlFlowGraph,
        D: Domain,
        F: FnMut(&<<Cfg as ControlFlowGraph>::Block as CfgBlock>::Element, &D) -> D,
    {
        self.transfer_blocks(cfg, &mut |_, block, dom: &D| {
            let mut post_state = dom.clone();
            for op in block.operations() {
                post_state = transfer(op, &post_state);
            }
            post_state
        })
    }
}
