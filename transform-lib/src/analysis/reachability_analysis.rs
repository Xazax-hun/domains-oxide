#![allow(clippy::explicit_auto_deref)] // False positive with LAT_CTX

use std::collections::HashSet;
use std::sync::LazyLock;

use analysis::cfg::OpPos;
use analysis::domains::{JoinSemiLattice, PowerSet, PowerSetTop};
use analysis::solvers::{OpTransfer, SolveMonotone};

use crate::analysis::annotations_from_backward_analysis_results;
use crate::analysis::annotations_from_forward_analysis_results;
use crate::analysis::AnalysisResult;
use crate::ast::Operation;
use crate::cfg::{reverse, Cfg};

use super::Analysis;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum OpKind {
    Init,
    Translation,
    Rotation,
}

type OperationKindsDomain = PowerSet<OpKind>;

static LAT_CTX: LazyLock<PowerSetTop<OpKind>> = LazyLock::new(|| {
    PowerSetTop(PowerSet::<OpKind>(HashSet::from([
        OpKind::Init,
        OpKind::Translation,
        OpKind::Rotation,
    ])))
});

pub fn collect_operation_kind(
    _pos: OpPos,
    &op: &Operation,
    _cfg: &Cfg,
    _: &PowerSetTop<OpKind>,
    pre_state: &OperationKindsDomain,
) -> OperationKindsDomain {
    let mut result = pre_state.clone();
    match op {
        Operation::Init(_) => result.insert(OpKind::Init),
        Operation::Translation(_) => result.insert(OpKind::Translation),
        Operation::Rotation(_) => result.insert(OpKind::Rotation),
    };
    result
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PastOperations;

impl PastOperations {
    pub fn get_results(cfg: &Cfg) -> Vec<OperationKindsDomain> {
        let solver = SolveMonotone::default();
        let lat_ctx = &*LAT_CTX;
        let seed = OperationKindsDomain::bottom(lat_ctx);
        solver.solve(
            cfg,
            seed,
            lat_ctx,
            &mut OpTransfer::new(collect_operation_kind),
        )
    }
}

impl Analysis for PastOperations {
    fn analyze(&self, cfg: &Cfg) -> AnalysisResult {
        let results = Self::get_results(cfg);
        let lat_ctx = &*LAT_CTX;
        AnalysisResult {
            annotations: annotations_from_forward_analysis_results(
                cfg,
                lat_ctx,
                &mut OpTransfer::new(collect_operation_kind),
                &results,
            ),
            covered: Vec::new(),
        }
    }
}

#[derive(Debug)]
pub struct FutureOperations;

impl FutureOperations {
    pub fn get_results(cfg: &Cfg) -> Vec<OperationKindsDomain> {
        let reversed = reverse(cfg);
        Self::get_results_impl(&reversed)
    }

    fn get_results_impl(cfg: &Cfg) -> Vec<OperationKindsDomain> {
        let solver = SolveMonotone::default();
        let lat_ctx = &*LAT_CTX;
        let seed = OperationKindsDomain::bottom(lat_ctx);
        solver.solve(
            cfg,
            seed,
            lat_ctx,
            &mut OpTransfer::new(collect_operation_kind),
        )
    }
}

impl Analysis for FutureOperations {
    fn analyze(&self, cfg: &Cfg) -> AnalysisResult {
        let reversed = reverse(cfg);
        let results = Self::get_results_impl(&reversed);
        let lat_ctx = &*LAT_CTX;
        AnalysisResult {
            annotations: annotations_from_backward_analysis_results(
                &reversed,
                lat_ctx,
                &mut OpTransfer::new(collect_operation_kind),
                &results,
            ),
            covered: Vec::new(),
        }
    }
}
