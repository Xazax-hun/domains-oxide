use analysis::cfg::OpPos;
use analysis::domains::{JoinSemiLatticeNoContext, Sign, Vec2Domain};
use analysis::solvers::{SolveMonotone, TransferFunction};

use utils::Vec2;

use crate::analysis::AnalysisResult;
use crate::analysis::annotations_from_forward_analysis_results;
use crate::analysis::covered_area_from_analysis_results;
use crate::ast::{NodeRef, Operation};
use crate::cfg::Cfg;

use super::Analysis;

type Vec2Sign = Vec2Domain<Sign>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SignAnalysis;

impl TransferFunction<Cfg<'_>, Vec2Sign> for SignAnalysis {
    fn operation(
        &mut self,
        _: OpPos,
        &op: &Operation,
        cfg: &Cfg,
        &(): &(),
        pre_state: &Vec2Sign,
    ) -> Vec2Sign {
        use Sign::*;
        let ctx = cfg.context();
        match ctx.op_to_ref(op) {
            NodeRef::Init(init) => {
                let bot_left = Vec2::from(&init.bottom_left);
                let width = i64::from(init.size.x.value.to_num());
                let x_sign = if bot_left.x > 0 {
                    Positive
                } else if bot_left.x + width < 0 {
                    Negative
                } else if bot_left.x == 0 && width == 0 {
                    Zero
                } else {
                    Top
                };

                let height = i64::from(init.size.y.value.to_num());
                let y_sign = if bot_left.y > 0 {
                    Positive
                } else if bot_left.y + height < 0 {
                    Negative
                } else if bot_left.y == 0 && height == 0 {
                    Zero
                } else {
                    Top
                };
                Vec2Domain {
                    x: x_sign,
                    y: y_sign,
                }
            }
            NodeRef::Translation(trans) => Vec2Domain {
                x: pre_state.x + Sign::from(trans.vector.x.value.to_num()),
                y: pre_state.y + Sign::from(trans.vector.y.value.to_num()),
            },
            NodeRef::Rotation(rot) => {
                let deg = rot.deg.value.to_num() % 360;
                if deg == 0 {
                    return pre_state.clone();
                }

                if Vec2::from(&rot.origin) == (Vec2 { x: 0, y: 0 }) {
                    if deg == 270 {
                        return Vec2Domain {
                            x: pre_state.y,
                            y: -pre_state.x,
                        };
                    }

                    if deg == 180 {
                        return Vec2Domain {
                            x: -pre_state.x,
                            y: -pre_state.y,
                        };
                    }

                    if deg == 90 {
                        return Vec2Domain {
                            x: -pre_state.y,
                            y: pre_state.x,
                        };
                    }
                }

                Vec2Domain { x: Top, y: Top }
            }
            _ => panic!("Unexpected operation."),
        }
    }
}

impl SignAnalysis {
    pub fn get_results(cfg: &Cfg) -> Vec<Vec2Sign> {
        let solver = SolveMonotone::default();
        let seed = Vec2Sign::bottom_();
        solver.solve(cfg, seed, &(), &mut Self)
    }
}

impl Analysis for SignAnalysis {
    fn analyze(&self, cfg: &Cfg) -> AnalysisResult {
        let results = Self::get_results(cfg);
        AnalysisResult {
            annotations: annotations_from_forward_analysis_results(cfg, &(), &mut Self, &results),
            covered: covered_area_from_analysis_results(cfg, &(), &mut Self, &results),
        }
    }
}
