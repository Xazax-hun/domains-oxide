pub mod sign_analysis;

use analysis::{
    cfg::{CfgBlock, ControlFlowGraph},
    domains::JoinSemiLattice,
    solvers::SolveMonotone,
};

use crate::{
    ast::{Annotations, Node},
    cfg::Cfg,
};

pub fn annotations_from_forward_analysis_results<D, F>(
    cfg: &Cfg,
    lat_ctx: &D::LatticeContext,
    transfer: &mut F,
    result: &[D],
) -> Annotations
where
    D: JoinSemiLattice,
    F: FnMut(&<<Cfg as ControlFlowGraph>::Block as CfgBlock>::Operation, &Cfg, &D) -> D,
{
    let mut anns = Annotations::new();
    let mut states = Vec::from(result);
    let solver = SolveMonotone::default();
    solver.transfer_operations_in_place(cfg, lat_ctx, &mut states, &mut |op, cfg, pre_state| {
        let post_state = transfer(op, cfg, pre_state);
        anns.post_annotations
            .entry(Node::Operation(*op))
            .or_default()
            .push(post_state.to_string());
        post_state
    });
    anns
}

#[cfg(test)]
mod sign_analysis_test;
