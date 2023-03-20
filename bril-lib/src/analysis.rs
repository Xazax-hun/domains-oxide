use std::{collections::HashMap, marker::PhantomData};

use analysis::{
    cfg::{CfgBlock, ControlFlowGraph, OpPos},
    domains::{JoinSemiLattice, Map},
    solvers::TransferFunction,
};

use crate::{
    ir::{AnnotationMap, Annotations, Cfg, Unit},
    lexer::Identifier,
};

pub mod sign_analysis;

pub trait Analysis: Sync {
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
}

lazy_static! {
    static ref ANALYSES: HashMap<Analyses, Box<dyn Analysis>> = {
        let mut m = HashMap::<Analyses, Box<dyn Analysis>>::new();
        m.insert(Analyses::Sign, Box::new(sign_analysis::SignAnalysis));
        m
    };
}

pub fn get_analysis_results(analysis: Analyses, unit: &Unit) -> AnnotationMap {
    let analysis = ANALYSES.get(&analysis).expect("Unimplemented analysis!");
    analysis.analyze_all(unit)
}

pub struct TransferLogger<'unit, D, Transfer>
where
    D: JoinSemiLattice,
    Transfer: TransferFunction<Cfg, Map<Identifier, D>>,
{
    unit: &'unit Unit,
    anns: Annotations,
    transfer: Transfer,
    d: PhantomData<D>,
}

impl<'unit, D, Transfer> TransferFunction<Cfg, Map<Identifier, D>>
    for TransferLogger<'unit, D, Transfer>
where
    D: JoinSemiLattice,
    Transfer: TransferFunction<Cfg, Map<Identifier, D>>,
{
    fn operation(
        &mut self,
        pos: OpPos,
        op: &<<Cfg as ControlFlowGraph>::Block as CfgBlock>::Operation,
        cfg: &Cfg,
        ctx: &<Map<Identifier, D> as JoinSemiLattice>::LatticeContext,
        pre_state: &Map<Identifier, D>,
    ) -> Map<Identifier, D> {
        // TODO: somehow print values that are changed during the join for pre states.
        let post_state = self.transfer.operation(pos, op, cfg, ctx, pre_state);
        let changed_values = post_state.changed_values(pre_state);
        if !changed_values.is_empty() {
            let printed: Vec<_> = changed_values
                .iter()
                .map(|(id, val)| format!("{}: {:?}", self.unit.identifiers.get_name(*id), val))
                .collect();
            self.anns.post.insert(pos, printed);
        }
        post_state
    }

    fn edge(
            &mut self,
            from: usize,
            to: usize,
            cfg: &Cfg,
            ctx: &<Map<Identifier, D> as JoinSemiLattice>::LatticeContext,
            pre_state: &Map<Identifier, D>,
        ) -> Option<Map<Identifier, D>> {
        self.transfer.edge(from, to, cfg, ctx, pre_state)
    }
}

impl<'unit, D, Transfer> TransferLogger<'unit, D, Transfer>
where
    D: JoinSemiLattice,
    Transfer: TransferFunction<Cfg, Map<Identifier, D>>,
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
}

#[cfg(test)]
mod sign_analysis_tests;

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
