use super::cfg::{CfgBlock, ControlFlowGraph, RPOWorklist};
use super::domains::JoinSemiLattice;

pub struct SolveMonotone {
    pub node_limit: usize,
}

impl Default for SolveMonotone {
    fn default() -> Self {
        Self { node_limit: 20 }
    }
}

impl SolveMonotone {
    pub fn transfer_blocks_in_place<Cfg, D, F>(
        self,
        cfg: &Cfg,
        lat_ctx: &D::LatticeContext,
        post_states: &mut Vec<D>,
        transfer: &mut F,
    ) where
        Cfg: ControlFlowGraph,
        D: JoinSemiLattice,
        F: FnMut(usize, &Cfg, &D::LatticeContext, &D) -> D,
    {
        let limit = self.node_limit * cfg.blocks().len();
        let mut visited = vec![false; cfg.blocks().len()];
        let mut worklist = RPOWorklist::new(cfg);

        // Process first node. It is hoisted, so the input state can be other than
        // the bottom value.
        let first_state = post_states[0].clone();
        post_states[0] = transfer(0, cfg, lat_ctx, &first_state);
        visited[0] = true;
        worklist.push_successors(0);

        let mut processed_nodes = 1_usize;
        while let Some(current) = worklist.pop() {
            if limit > 0 && processed_nodes >= limit {
                post_states.clear();
                return;
            }

            let mut pre_state = D::bottom(lat_ctx);
            for pred in cfg.blocks()[current].predecessors() {
                pre_state = pre_state.join(&post_states[*pred]);
            }
            let post_state = transfer(current, cfg, lat_ctx, &pre_state);

            processed_nodes += 1;
            if visited[current] && post_states[current] == post_state {
                continue;
            }

            visited[current] = true;
            post_states[current] = post_state;
            worklist.push_successors(current);
        }
    }

    pub fn transfer_blocks<Cfg, D, F>(
        self,
        cfg: &Cfg,
        lat_ctx: &D::LatticeContext,
        transfer: &mut F,
    ) -> Vec<D>
    where
        Cfg: ControlFlowGraph,
        D: JoinSemiLattice,
        F: FnMut(usize, &Cfg, &D::LatticeContext, &D) -> D,
    {
        let mut post_states = vec![D::bottom(lat_ctx); cfg.blocks().len()];
        self.transfer_blocks_in_place(cfg, lat_ctx, &mut post_states, transfer);
        post_states
    }

    pub fn transfer_operations_in_place<Cfg, D, F>(
        self,
        cfg: &Cfg,
        lat_ctx: &D::LatticeContext,
        post_states: &mut Vec<D>,
        transfer: &mut F,
    ) where
        Cfg: ControlFlowGraph,
        D: JoinSemiLattice,
        F: FnMut(
            &<<Cfg as ControlFlowGraph>::Block as CfgBlock>::Operation,
            &Cfg,
            &D::LatticeContext,
            &D,
        ) -> D,
    {
        self.transfer_blocks_in_place(
            cfg,
            lat_ctx,
            post_states,
            &mut |current, cfg, lat_ctx, dom: &D| {
                let mut post_state = dom.clone();
                for op in cfg.blocks()[current].operations() {
                    post_state = transfer(op, cfg, lat_ctx, &post_state);
                }
                post_state
            },
        );
    }

    pub fn transfer_operations<Cfg, D, F>(
        self,
        cfg: &Cfg,
        lat_ctx: &D::LatticeContext,
        transfer: &mut F,
    ) -> Vec<D>
    where
        Cfg: ControlFlowGraph,
        D: JoinSemiLattice,
        F: FnMut(
            &<<Cfg as ControlFlowGraph>::Block as CfgBlock>::Operation,
            &Cfg,
            &D::LatticeContext,
            &D,
        ) -> D,
    {
        self.transfer_blocks(cfg, lat_ctx, &mut |current, cfg, lat_ctx, dom: &D| {
            let mut post_state = dom.clone();
            for op in cfg.blocks()[current].operations() {
                post_state = transfer(op, cfg, lat_ctx, &post_state);
            }
            post_state
        })
    }
}
