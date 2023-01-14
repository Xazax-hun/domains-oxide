use analysis::domains::{SignDomain, Vec2Domain};
use analysis::solvers::SolveMonotone;

use utils::Vec2;

use crate::analysis::annotations_from_forward_analysis_results;
use crate::ast::{Annotations, NodeRef, Operation};
use crate::cfg::Cfg;

use super::Analysis;

type Vec2Sign = Vec2Domain<SignDomain>;

pub struct SignAnalysis;

impl SignAnalysis {
    pub fn get_results(cfg: &Cfg) -> Vec<Vec2Sign> {
        let solver = SolveMonotone::default();
        solver.transfer_operations(cfg, &(), &mut SignAnalysis::transfer)
    }

    pub fn transfer(&op: &Operation, cfg: &Cfg, _: &(), pre_state: &Vec2Sign) -> Vec2Sign {
        let ctx = cfg.context();
        match ctx.op_to_ref(op) {
            NodeRef::Init(init) => {
                let bot_left = Vec2::from(&init.bottom_left);
                let width = init.size.x.value.to_num();
                let x_sign = if bot_left.x > 0 {
                    SignDomain::Positive
                } else if bot_left.x + width < 0 {
                    SignDomain::Negative
                } else if bot_left.x == 0 && width == 0 {
                    SignDomain::Zero
                } else {
                    SignDomain::Top
                };

                let height = init.size.y.value.to_num();
                let y_sign = if bot_left.y > 0 {
                    SignDomain::Positive
                } else if bot_left.y + height < 0 {
                    SignDomain::Negative
                } else if bot_left.y == 0 && height == 0 {
                    SignDomain::Zero
                } else {
                    SignDomain::Top
                };
                Vec2Domain {
                    x: x_sign,
                    y: y_sign,
                }
            }
            NodeRef::Translation(trans) => Vec2Domain {
                x: pre_state.x + SignDomain::from(trans.vector.x.value.to_num()),
                y: pre_state.y + SignDomain::from(trans.vector.y.value.to_num()),
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

                Vec2Domain {
                    x: SignDomain::Top,
                    y: SignDomain::Top,
                }
            }
            _ => panic!("Unexpected operation."),
        }
    }
}

impl Analysis for SignAnalysis {
    fn annotate(&self, cfg: &Cfg) -> Annotations {
        let results = SignAnalysis::get_results(cfg);
        annotations_from_forward_analysis_results(cfg, &(), &mut SignAnalysis::transfer, &results)
    }
}
