use std::sync::OnceLock;
use std::{collections::HashMap, marker::PhantomData};

use analysis::{
    cfg::OpPos,
    domains::{JoinSemiLattice, Map},
    solvers::TransferFunction,
};

use crate::{
    ir::{AnnotationMap, Annotations, Cfg, Operation, Unit},
    lexer::Identifier,
};

pub mod congruence_analysis;
pub mod interval_analysis;
pub mod sign_analysis;

pub trait Analysis: Send + Sync {
    fn analyze(&self, cfg: &Cfg, unit: &Unit) -> Annotations;
    fn analyze_all(&self, unit: &Unit) -> AnnotationMap {
        let mut result = HashMap::new();
        for cfg in &unit.functions {
            let anns = self.analyze(cfg, unit);
            result.insert(cfg.get_function(), anns);
        }
        result
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Analyses {
    Sign,
    Interval,
    UnrolledInterval,
    Congruence,
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
        Analyses::UnrolledInterval,
        Box::new(interval_analysis::UnrolledIntervalAnalysis(3)),
    );
    m.insert(
        Analyses::Congruence,
        Box::new(congruence_analysis::CongruenceAnalysis),
    );
    m
}

pub fn get_analysis_results(analysis: Analyses, unit: &Unit) -> AnnotationMap {
    let analysis = ANALYSES
        .get_or_init(init_analyses)
        .get(&analysis)
        .expect("Unimplemented analysis!");
    analysis.analyze_all(unit)
}

pub struct TransferLogger<'unit, D, Transfer> {
    unit: &'unit Unit,
    anns: Annotations,
    transfer: Transfer,
    d: PhantomData<D>,
}

impl<'unit, D, EnvD, Transfer> TransferFunction<Cfg, EnvD> for TransferLogger<'unit, D, Transfer>
where
    D: JoinSemiLattice,
    EnvD: AsRef<Map<Identifier, D>> + JoinSemiLattice,
    Transfer: TransferFunction<Cfg, EnvD>,
{
    fn operation(
        &mut self,
        pos: OpPos,
        op: &Operation,
        cfg: &Cfg,
        ctx: &<EnvD as JoinSemiLattice>::LatticeContext,
        pre_state: &EnvD,
    ) -> EnvD {
        let post_state = self.transfer.operation(pos, op, cfg, ctx, pre_state);
        let changed_values = post_state.as_ref().changed_values(pre_state.as_ref());
        self.annotate(op, post_state.as_ref(), changed_values, pos);
        post_state
    }

    fn edge(
        &mut self,
        from: usize,
        to: usize,
        cfg: &Cfg,
        ctx: &<EnvD as JoinSemiLattice>::LatticeContext,
        pre_state: &EnvD,
    ) -> Option<EnvD> {
        self.transfer.edge(from, to, cfg, ctx, pre_state)
    }
}

impl<'unit, D, Transfer> TransferLogger<'unit, D, Transfer>
where
    D: JoinSemiLattice,
{
    pub fn new(unit: &'unit Unit, transfer: Transfer) -> Self {
        Self {
            unit,
            anns: Annotations::default(),
            transfer,
            d: PhantomData,
        }
    }

    pub fn get_annotations(self) -> Annotations {
        self.anns
    }

    fn annotate(
        &mut self,
        op: &Operation,
        post_state: &Map<Identifier, D>,
        mut changed_values: Map<Identifier, D>,
        pos: OpPos,
    ) {
        if let Some(result) = op.get_result() {
            if let Some(val) = post_state.get(&result.id) {
                changed_values.insert(result.id, val.clone());
            }
        }
        if !changed_values.is_empty() {
            let printed: Vec<_> = changed_values
                .iter()
                .map(|(id, val)| format!("{}: {:?}", self.unit.identifiers.get_name(*id), val))
                .collect();
            self.anns.post.insert(pos, printed);
        }
    }
}

#[cfg(test)]
mod sign_analysis_tests;

#[cfg(test)]
mod interval_analysis_tests;

#[cfg(test)]
mod congruence_analysis_tests;

#[cfg(test)]
mod test_utils {
    use super::*;
    use crate::{ir::print, parser_tests::parse_string};

    pub fn check_expected_results(analysis: impl Analysis, source: &str, expected: &str) {
        let unit = parse_string(source).unwrap();
        let anns = analysis.analyze_all(&unit);
        assert_eq!(expected, print(&unit, &anns));
    }
}
