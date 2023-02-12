use std::collections::HashSet;

use super::cfg::{get_back_edges, CfgBlock, ControlFlowGraph, RPOWorklist};
use super::domains::JoinSemiLattice;

/// A basic solver for monotonic transfer functions. It is also doing
/// widening on loop heads. The solver is using a worklist that visits
/// the queued nodes in reverse post-order.
///
/// Requirements:
/// * All the back edges must target the loop head (node dominating every
///   node within the loop.)
#[derive(Clone, Debug)]
pub struct SolveMonotone {
    /// Set the approximate iteration limit per node. If the limit is reached
    /// (the analysis did not converge in the permitted number of steps),
    /// the solver terminates without a result.
    pub node_limit: usize,
}

impl Default for SolveMonotone {
    fn default() -> Self {
        Self { node_limit: 20 }
    }
}

impl SolveMonotone {
    /// Run the solver on a CFG mutating the analysis states in place. The
    /// `post_states` vector is cleared when the analysis did not converge.
    ///
    /// # Arguments
    ///
    /// * `post_states` - The analysis state after each CFG block.
    /// * `transfer` - Function to apply the effects of a block to the state.
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
        // Loop header dominates the whole loop, every back edge should point to a
        // loop header.
        let loop_heads: HashSet<_> = get_back_edges(cfg)
            .iter()
            .map(|&(_, target)| target)
            .collect();

        // Process first node. It is hoisted, so the input state can be other than
        // the bottom value.
        let first_state = post_states[0].clone();
        post_states[0] = transfer(0, cfg, lat_ctx, &first_state);

        let node_num = cfg.blocks().len();
        let mut visited = vec![false; node_num];
        visited[0] = true;

        let mut worklist = RPOWorklist::new(cfg);
        worklist.push_successors(0);

        let limit = self.node_limit * node_num;
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
            let mut post_state = transfer(current, cfg, lat_ctx, &pre_state);

            if loop_heads.contains(&current) {
                post_state = post_state.widen(&post_states[current], processed_nodes / node_num);
            }

            processed_nodes += 1;
            if visited[current] && post_states[current] == post_state {
                continue;
            }

            visited[current] = true;
            post_states[current] = post_state;
            worklist.push_successors(current);
        }
    }

    /// Run the solver on a CFG returning the analysis states at the end of
    /// each basic block. Returns an empty vector when the analysis did not
    /// converge.
    ///
    /// # Arguments
    ///
    /// * `post_states` - The analysis state after each CFG block.
    /// * `transfer` - Function to apply the effects of a block to the state.
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

    /// Run the solver on a CFG mutating the analysis states in place. The
    /// `post_states` vector is cleared when the analysis did not converge.
    /// This method can be used as a second pass (after the first pass
    /// converged), to collect per-operation analysis states from the
    /// per-block states. Alternatively, a second pass can also be used
    /// to report errors in a program analysis tool.
    ///
    /// # Arguments
    ///
    /// * `post_states` - The analysis state after each CFG block.
    /// * `transfer` - Function to apply the effects of an operation to the state.
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

    /// Run the solver on a CFG returning the analysis states at the end of
    /// each basic block. Returns an empty vector when the analysis did not
    /// converge.
    ///
    /// # Arguments
    ///
    /// * `post_states` - The analysis state after each CFG block.
    /// * `transfer` - Function to apply the effects of an operation to the state.
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
