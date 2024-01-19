pub mod interval_analysis;
pub mod reachability_analysis;
pub mod sign_analysis;

use analysis::{
    domains::JoinSemiLattice,
    solvers::{OpTransfer, SolveMonotone, TransferFunction},
};
use utils::Polygon;

use std::collections::HashMap;
use std::sync::OnceLock;

use crate::{
    ast::{Annotations, Node},
    cfg::Cfg,
    render::RenderableDomain,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AnalysisResult {
    pub annotations: Annotations,
    pub covered: Vec<Polygon>,
}

pub trait Analysis: Send + Sync {
    fn analyze(&self, cfg: &Cfg) -> AnalysisResult;
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Analyses {
    Sign,
    Interval,
    PastOperations,
    FutureOperations,
}

static ANALYSES: OnceLock<HashMap<Analyses, Box<dyn Analysis>>> = OnceLock::new();
fn init_analyses() -> HashMap<Analyses, Box<dyn Analysis>> {
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
}

pub fn get_analysis_results(analysis: Analyses, cfg: &Cfg) -> AnalysisResult {
    let analysis = ANALYSES
        .get_or_init(init_analyses)
        .get(&analysis)
        .expect("Unimplemented analysis!");
    analysis.analyze(cfg)
}

pub fn annotations_from_forward_analysis_results<'ctx, D: JoinSemiLattice>(
    cfg: &Cfg<'ctx>,
    lat_ctx: &D::LatticeContext,
    transfer: &mut impl TransferFunction<Cfg<'ctx>, D>,
    result: &[D],
) -> Annotations {
    annotations_from_analysis_results(cfg, lat_ctx, transfer, result, &mut |anns| &mut anns.post)
}

pub fn annotations_from_backward_analysis_results<'ctx, D: JoinSemiLattice>(
    cfg: &Cfg<'ctx>,
    lat_ctx: &D::LatticeContext,
    transfer: &mut impl TransferFunction<Cfg<'ctx>, D>,
    result: &[D],
) -> Annotations {
    annotations_from_analysis_results(cfg, lat_ctx, transfer, result, &mut |anns| &mut anns.pre)
}

fn annotations_from_analysis_results<'ctx, D, G>(
    cfg: &Cfg<'ctx>,
    lat_ctx: &D::LatticeContext,
    transfer: &mut impl TransferFunction<Cfg<'ctx>, D>,
    result: &[D],
    selector: &mut G,
) -> Annotations
where
    D: JoinSemiLattice,
    G: FnMut(&mut Annotations) -> &mut HashMap<Node, Vec<String>>,
{
    let mut anns = Annotations::new();
    let mut states: Box<[D]> = Box::from(result);
    if states.is_empty() {
        return anns;
    }

    // TODO: we don't want to start with the end-state of the first block,
    //       that is incorrect. But starting with bottom can also be incorrect
    //       in case the start node is a loop header. We should probably have
    //       a dedicated start node in the CFG that is never a loop header.
    states[0] = D::bottom(lat_ctx);

    let solver = SolveMonotone::default();
    solver.solve_in_place(
        cfg,
        lat_ctx,
        &mut states,
        &mut OpTransfer::new(|pos, op, cfg, lat_ctx, pre_state| {
            let post_state = transfer.operation(pos, op, cfg, lat_ctx, pre_state);
            let entry = selector(&mut anns).entry(Node::Operation(*op)).or_default();
            // The solver visits loop heads twice when used for post-processing, we need the
            // annotations only once.
            if entry.is_empty() {
                entry.push(format!("{post_state:?}"));
            }
            post_state
        }),
    );
    anns
}

pub fn covered_area_from_analysis_results<'ctx, D>(
    cfg: &Cfg<'ctx>,
    lat_ctx: &D::LatticeContext,
    transfer: &mut impl TransferFunction<Cfg<'ctx>, D>,
    result: &[D],
) -> Vec<Polygon>
where
    D: JoinSemiLattice + RenderableDomain,
{
    let mut polys = Vec::new();
    let mut states: Box<[D]> = Box::from(result);
    if states.is_empty() {
        return polys;
    }

    // TODO: see note in annotations_from_analysis_results
    states[0] = D::bottom(lat_ctx);

    let solver = SolveMonotone::default();
    solver.solve_in_place(
        cfg,
        lat_ctx,
        &mut states,
        &mut OpTransfer::new(|pos, op, cfg, lat_ctx, pre_state| {
            let post_state = transfer.operation(pos, op, cfg, lat_ctx, pre_state);
            polys.extend(post_state.render().iter().cloned());
            post_state
        }),
    );
    polys
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
    use crate::{ast::print, cfg::Cfg, parser_tests::parse_string};

    pub fn check_expected_results(analysis: impl Analysis, source: &str, expected: &str) {
        let ctx = parse_string(source).unwrap();
        let cfg = Cfg::new(&ctx);
        let anns = analysis.analyze(&cfg).annotations;
        assert_eq!(expected, print(ctx.get_root(), &ctx, &anns));
    }
}
