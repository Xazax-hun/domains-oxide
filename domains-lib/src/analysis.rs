pub mod sign_analysis;

use analysis::{
    cfg::{CfgBlock, ControlFlowGraph},
    domains::JoinSemiLattice,
    solvers::SolveMonotone,
};

use std::collections::HashMap;

use crate::{
    ast::{Annotations, Node},
    cfg::Cfg,
};

// TODO: maybe a Trait object instead?
struct AnalysisDescription {
    annotate: fn(&Cfg) -> Annotations,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Analyses {
    Sign,
}

lazy_static! {
    static ref FORWARD_ANALYSES: HashMap<Analyses, AnalysisDescription> = {
        let mut m = HashMap::new();
        m.insert(
            Analyses::Sign,
            AnalysisDescription {
                annotate: |cfg| {
                    let results = sign_analysis::get_sign_analysis(cfg);
                    sign_analysis::sign_analysis_results_to_annotations(cfg, &results)
                },
            },
        );
        m
    };
}

pub fn get_analysis_results(analysis: Analyses, cfg: &Cfg) -> Annotations {
    if let Some(desc) = FORWARD_ANALYSES.get(&analysis) {
        return (desc.annotate)(cfg);
    }
    panic!("Unimplemented analysis!")
}

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
        let entry = anns
            .post_annotations
            .entry(Node::Operation(*op))
            .or_default();
        // The solver visits loop heads twice when used for post-processing, we need the
        // annotations only once.
        if entry.is_empty() {
            entry.push(post_state.to_string());
        }
        post_state
    });
    anns
}

#[cfg(test)]
mod sign_analysis_test;
