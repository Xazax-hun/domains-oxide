pub mod interval_analysis;
pub mod reachability_analysis;
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

pub trait Analysis: Sync {
    fn annotate(&self, cfg: &Cfg) -> Annotations;
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Analyses {
    Sign,
    Interval,
    PastOperations,
    FutureOperations,
}

lazy_static! {
    static ref FORWARD_ANALYSES: HashMap<Analyses, Box<dyn Analysis>> = {
        let mut m = HashMap::<Analyses, Box<dyn Analysis>>::new();
        m.insert(Analyses::Sign, Box::new(sign_analysis::SignAnalysis));
        m.insert(
            Analyses::Interval,
            Box::new(interval_analysis::IntervalAnalysis),
        );
        m.insert(
            Analyses::PastOperations,
            Box::new(reachability_analysis::PastOperations),
        );
        m.insert(
            Analyses::FutureOperations,
            Box::new(reachability_analysis::FutureOperations),
        );
        m
    };
}

pub fn get_analysis_results(analysis: Analyses, cfg: &Cfg) -> Annotations {
    if let Some(analysis) = FORWARD_ANALYSES.get(&analysis) {
        return analysis.annotate(cfg);
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
    F: FnMut(
        &<<Cfg as ControlFlowGraph>::Block as CfgBlock>::Operation,
        &Cfg,
        &D::LatticeContext,
        &D,
    ) -> D,
{
    annotations_from_analysis_results(cfg, lat_ctx, transfer, result, &mut |anns| {
        &mut anns.post_annotations
    })
}

pub fn annotations_from_backward_analysis_results<D, F>(
    cfg: &Cfg,
    lat_ctx: &D::LatticeContext,
    transfer: &mut F,
    result: &[D],
) -> Annotations
where
    D: JoinSemiLattice,
    F: FnMut(
        &<<Cfg as ControlFlowGraph>::Block as CfgBlock>::Operation,
        &Cfg,
        &D::LatticeContext,
        &D,
    ) -> D,
{
    annotations_from_analysis_results(cfg, lat_ctx, transfer, result, &mut |anns| {
        &mut anns.pre_annotations
    })
}

fn annotations_from_analysis_results<D, F, G>(
    cfg: &Cfg,
    lat_ctx: &D::LatticeContext,
    transfer: &mut F,
    result: &[D],
    selector: &mut G,
) -> Annotations
where
    D: JoinSemiLattice,
    F: FnMut(
        &<<Cfg as ControlFlowGraph>::Block as CfgBlock>::Operation,
        &Cfg,
        &D::LatticeContext,
        &D,
    ) -> D,
    G: FnMut(&mut Annotations) -> &mut HashMap<Node, Vec<String>>,
{
    let mut anns = Annotations::new();
    let mut states = Vec::from(result);
    if states.is_empty() {
        return anns;
    }

    // TODO: we don't want to start with the end-state of the first block,
    //       that is incorrect. But starting with bottom can also be incorrect
    //       in case the start node is a loop header. We should probably have
    //       a dedicated start node in the CFG that is never a loop header.
    states[0] = D::bottom(lat_ctx);

    let solver = SolveMonotone::default();
    solver.transfer_operations_in_place(
        cfg,
        lat_ctx,
        &mut states,
        &mut |op, cfg, lat_ctx, pre_state| {
            let post_state = transfer(op, cfg, lat_ctx, pre_state);
            let entry = selector(&mut anns).entry(Node::Operation(*op)).or_default();
            // The solver visits loop heads twice when used for post-processing, we need the
            // annotations only once.
            if entry.is_empty() {
                entry.push(post_state.to_string());
            }
            post_state
        },
    );
    anns
}

#[cfg(test)]
mod sign_analysis_tests;

#[cfg(test)]
mod interval_analysis_tests;

#[cfg(test)]
mod reachability_analysis_tests;

#[cfg(test)]
mod test_utils {
    use super::*;
    use crate::{
        ast::print,
        cfg::Cfg,
        cfg_tests::{parse_string, ParseResult},
    };

    pub fn check_expected_results<A: Analysis>(analysis: A, source: &str, expected: &str) {
        let ParseResult { output, ctx } = parse_string(source).unwrap();
        let cfg = Cfg::new(&ctx);
        let anns = analysis.annotate(&cfg);
        assert!(output.is_empty());
        assert_eq!(expected, print(ctx.get_root(), &ctx, &anns));
    }
}
