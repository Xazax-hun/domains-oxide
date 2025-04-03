use core::marker::PhantomData;
use std::collections::HashSet;

use super::cfg::{CfgBlock, ControlFlowGraph, OpPos, RPOWorklist, get_back_edges};
use super::domains::JoinSemiLattice;

/// Transfer functions need to implement this trait and define either
/// [`TransferFunction::block`] or [`TransferFunction::operation`].
/// For the most common cases creating [`OpTransfer`] or [`BlockTransfer`]
/// from a closure should be sufficient.
pub trait TransferFunction<Cfg, D>
where
    Cfg: ControlFlowGraph,
    D: JoinSemiLattice,
{
    /// Optional function to apply the effects of traversing an edge. The most common
    /// use case is to implement conditional jumps or statements.
    fn edge(
        &mut self,
        _from: usize,
        _to: usize,
        _cfg: &Cfg,
        _ctx: &D::LatticeContext,
        pre_state: &D,
    ) -> Option<D> {
        Some(pre_state.clone())
    }

    /// Apply the effects of a block to the analysis state. In case [`TransferFunction::operation`]
    /// is implemented, the default implementation should be sufficient.
    fn block(&mut self, block_id: usize, cfg: &Cfg, ctx: &D::LatticeContext, pre_state: &D) -> D {
        let mut post_state = pre_state.clone();
        for (op_id, op) in cfg.blocks()[block_id].operations().iter().enumerate() {
            post_state = self.operation(OpPos { block_id, op_id }, op, cfg, ctx, &post_state);
        }
        post_state
    }

    /// Apply the effects of an operation to the analysis state. In case [`TransferFunction::block`]
    /// is implemented, the default implementation should be sufficient.
    fn operation(
        &mut self,
        _pos: OpPos,
        _op: &<<Cfg as ControlFlowGraph>::Block as CfgBlock>::Operation,
        _cfg: &Cfg,
        _ctx: &D::LatticeContext,
        pre_state: &D,
    ) -> D {
        pre_state.clone()
    }
}

/// A transfer function that never modifies the analysis state. It can be
/// useful to use together with other utilities that can wrap/combine/transform
/// transfer functions.
pub struct NoOpTransfer;

impl<Cfg, D> TransferFunction<Cfg, D> for NoOpTransfer
where
    Cfg: ControlFlowGraph,
    D: JoinSemiLattice,
{
}

/// Small utility so users do not need to create a new struct for every
/// transfer function for operations.
pub struct OpTransfer<F, Cfg, D>
where
    Cfg: ControlFlowGraph,
    D: JoinSemiLattice,
    F: FnMut(
        OpPos,
        &<<Cfg as ControlFlowGraph>::Block as CfgBlock>::Operation,
        &Cfg,
        &D::LatticeContext,
        &D,
    ) -> D,
{
    func: F,
    phantom: PhantomData<(Cfg, D)>,
}

impl<F, Cfg, D> TransferFunction<Cfg, D> for OpTransfer<F, Cfg, D>
where
    Cfg: ControlFlowGraph,
    D: JoinSemiLattice,
    F: FnMut(
        OpPos,
        &<<Cfg as ControlFlowGraph>::Block as CfgBlock>::Operation,
        &Cfg,
        &D::LatticeContext,
        &D,
    ) -> D,
{
    fn operation(
        &mut self,
        pos: OpPos,
        op: &<<Cfg as ControlFlowGraph>::Block as CfgBlock>::Operation,
        cfg: &Cfg,
        ctx: &<D as JoinSemiLattice>::LatticeContext,
        pre_state: &D,
    ) -> D {
        (self.func)(pos, op, cfg, ctx, pre_state)
    }
}

impl<F, Cfg, D> OpTransfer<F, Cfg, D>
where
    Cfg: ControlFlowGraph,
    D: JoinSemiLattice,
    F: FnMut(
        OpPos,
        &<<Cfg as ControlFlowGraph>::Block as CfgBlock>::Operation,
        &Cfg,
        &D::LatticeContext,
        &D,
    ) -> D,
{
    /// Create a new transfer function for operations from a closure or function.
    pub fn new(func: F) -> Self {
        Self {
            func,
            phantom: PhantomData,
        }
    }
}

/// Small utility so users do not need to create a new struct for every
/// transfer function for blocks.
pub struct BlockTransfer<F, Cfg, D>
where
    Cfg: ControlFlowGraph,
    D: JoinSemiLattice,
    F: FnMut(usize, &Cfg, &D::LatticeContext, &D) -> D,
{
    func: F,
    phantom: PhantomData<(Cfg, D)>,
}

impl<F, Cfg, D> TransferFunction<Cfg, D> for BlockTransfer<F, Cfg, D>
where
    Cfg: ControlFlowGraph,
    D: JoinSemiLattice,
    F: FnMut(usize, &Cfg, &D::LatticeContext, &D) -> D,
{
    fn block(
        &mut self,
        block_id: usize,
        cfg: &Cfg,
        ctx: &<D as JoinSemiLattice>::LatticeContext,
        pre_state: &D,
    ) -> D {
        (self.func)(block_id, cfg, ctx, pre_state)
    }
}

impl<F, Cfg, D> BlockTransfer<F, Cfg, D>
where
    Cfg: ControlFlowGraph,
    D: JoinSemiLattice,
    F: FnMut(usize, &Cfg, &D::LatticeContext, &D) -> D,
{
    /// Create a new transfer function for blocks from a closure or function.
    pub fn new(func: F) -> Self {
        Self {
            func,
            phantom: PhantomData,
        }
    }
}

/// A basic solver for monotonic transfer functions. It is also doing
/// widening on loop heads. The solver is using a worklist that visits
/// the queued nodes in reverse post-order.
///
/// Requirements:
/// * All the back edges must target the loop head (node dominating every
///   node within the loop.)
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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
    /// return value is false when the analysis did not converge.
    ///
    /// # Arguments
    ///
    /// * `post_states` - The analysis state after each CFG block.
    /// * `transfer` - Function to apply the effects of blocks, edges, operations.
    pub fn solve_in_place<Cfg, D, F>(
        self,
        cfg: &Cfg,
        lat_ctx: &D::LatticeContext,
        post_states: &mut [D],
        transfer: &mut F,
    ) -> bool
    where
        Cfg: ControlFlowGraph,
        D: JoinSemiLattice,
        F: TransferFunction<Cfg, D>,
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
        post_states[0] = transfer.block(0, cfg, lat_ctx, &first_state);

        let node_num = cfg.blocks().len();
        let mut visited = vec![false; node_num];
        visited[0] = true;

        let mut worklist = RPOWorklist::new(cfg);
        worklist.push_successors(0, cfg);

        let limit = self.node_limit * node_num;
        let mut processed_nodes = 1_usize;
        while let Some(current) = worklist.pop() {
            if limit > 0 && processed_nodes >= limit {
                return false;
            }

            let mut pre_state = D::bottom(lat_ctx);
            for &pred in cfg.blocks()[current].predecessors() {
                if let Some(transferred) =
                    transfer.edge(pred, current, cfg, lat_ctx, &post_states[pred])
                {
                    pre_state = pre_state.join(&transferred, lat_ctx);
                }
            }
            let mut post_state = transfer.block(current, cfg, lat_ctx, &pre_state);

            if loop_heads.contains(&current) {
                post_state =
                    post_state.widen(&post_states[current], lat_ctx, processed_nodes / node_num);
            }

            processed_nodes += 1;
            if visited[current] && post_states[current] == post_state {
                continue;
            }

            visited[current] = true;
            post_states[current] = post_state;
            worklist.push_successors(current, cfg);
        }
        true
    }

    /// Run the solver on a CFG returning the analysis states at the end of
    /// each basic block. Returns an empty vector when the analysis did not
    /// converge.
    ///
    /// # Arguments
    ///
    /// * `seed` - The initial program state for the start node. This often has
    ///   the initial abstract values for the formal parameters of a function.
    /// * `post_states` - The analysis state after each CFG block.
    /// * `transfer` - Function to apply the effects of blocks, edges, operations.
    pub fn solve<Cfg, D, F>(
        self,
        cfg: &Cfg,
        seed: D,
        lat_ctx: &D::LatticeContext,
        transfer: &mut F,
    ) -> Vec<D>
    where
        Cfg: ControlFlowGraph,
        D: JoinSemiLattice,
        F: TransferFunction<Cfg, D>,
    {
        let mut post_states = vec![D::bottom(lat_ctx); cfg.blocks().len()];
        post_states[0] = seed;
        if self.solve_in_place(cfg, lat_ctx, &mut post_states, transfer) {
            post_states
        } else {
            vec![]
        }
    }
}
