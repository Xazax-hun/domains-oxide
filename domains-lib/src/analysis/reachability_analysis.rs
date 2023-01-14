#![allow(clippy::explicit_auto_deref)] // False positive with LAT_CTX

use std::collections::HashSet;

use analysis::domains::{PowerSetDomain, PowerSetTop};
use analysis::solvers::SolveMonotone;

use crate::analysis::annotations_from_backward_analysis_results;
use crate::analysis::annotations_from_forward_analysis_results;
use crate::analysis::AnalysisResult;
use crate::ast::Operation;
use crate::cfg::{reverse, Cfg};

use super::Analysis;

type OperationKindsDomain = PowerSetDomain<&'static str>;

lazy_static! {
    static ref LAT_CTX: PowerSetTop<&'static str> =
        PowerSetTop(PowerSetDomain::<&'static str>(HashSet::from([
            "Init",
            "Translation",
            "Rotation",
        ])));
}

pub fn collect_operation_kind(
    &op: &Operation,
    _cfg: &Cfg,
    _: &PowerSetTop<&'static str>,
    pre_state: &OperationKindsDomain,
) -> OperationKindsDomain {
    let mut result = pre_state.clone();
    match op {
        Operation::Init(_) => result.0.insert("Init"),
        Operation::Translation(_) => result.0.insert("Translation"),
        Operation::Rotation(_) => result.0.insert("Rotation"),
    };
    result
}

pub struct PastOperations;

impl PastOperations {
    pub fn get_results(cfg: &Cfg) -> Vec<OperationKindsDomain> {
        let solver = SolveMonotone::default();
        solver.transfer_operations(cfg, &*LAT_CTX, &mut collect_operation_kind)
    }
}

impl Analysis for PastOperations {
    fn analyze(&self, cfg: &Cfg) -> AnalysisResult {
        let results = PastOperations::get_results(cfg);
        AnalysisResult {
            annotations: annotations_from_forward_analysis_results(
                cfg,
                &*LAT_CTX,
                &mut collect_operation_kind,
                &results,
            ),
            covered: Vec::new(),
        }
    }
}

pub struct FutureOperations;

impl FutureOperations {
    pub fn get_results(cfg: &Cfg) -> Vec<OperationKindsDomain> {
        let reversed = reverse(cfg);
        Self::get_results_impl(&reversed)
    }

    fn get_results_impl(cfg: &Cfg) -> Vec<OperationKindsDomain> {
        let solver = SolveMonotone::default();
        solver.transfer_operations(cfg, &*LAT_CTX, &mut collect_operation_kind)
    }
}

impl Analysis for FutureOperations {
    fn analyze(&self, cfg: &Cfg) -> AnalysisResult {
        let reversed = reverse(cfg);
        let results = FutureOperations::get_results_impl(&reversed);
        AnalysisResult {
            annotations: annotations_from_backward_analysis_results(
                &reversed,
                &*LAT_CTX,
                &mut collect_operation_kind,
                &results,
            ),
            covered: Vec::new(),
        }
    }
}
