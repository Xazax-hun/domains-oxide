use std::collections::HashMap;

use crate::ir::{AnnotationMap, Annotations, Cfg, Unit};

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
