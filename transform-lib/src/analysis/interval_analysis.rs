use analysis::cfg::OpPos;
use analysis::domains::{self, Interval, JoinSemiLattice, Lattice, Vec2Domain};
use analysis::solvers::{SolveMonotone, TransferFunction};

use utils::Vec2;

use crate::analysis::annotations_from_forward_analysis_results;
use crate::analysis::covered_area_from_analysis_results;
use crate::analysis::{Analysis, AnalysisResult};
use crate::ast::{NodeRef, Operation};
use crate::cfg::Cfg;
use crate::eval::rotate;

type Vec2Interval = Vec2Domain<Interval>;

#[derive(Debug)]
pub struct IntervalAnalysis;

impl<'ctx> TransferFunction<Cfg<'ctx>, Vec2Interval> for IntervalAnalysis {
    fn operation(
        &mut self,
        _pos: OpPos,
        &op: &Operation,
        cfg: &Cfg,
        lat_ctx: &(),
        pre_state: &Vec2Interval,
    ) -> Vec2Interval {
        let ctx = cfg.context();
        match ctx.op_to_ref(op) {
            NodeRef::Init(init) => {
                let bot_left = Vec2::from(&init.bottom_left);
                let size = Vec2::from(&init.size);
                Vec2Domain {
                    x: Interval {
                        min: bot_left.x,
                        max: (bot_left.x + size.x),
                    },
                    y: Interval {
                        min: bot_left.y,
                        max: (bot_left.y + size.y),
                    },
                }
            }
            NodeRef::Translation(trans) => Vec2Domain {
                x: pre_state.x + Interval::from(i64::from(trans.vector.x.value.to_num())),
                y: pre_state.y + Interval::from(i64::from(trans.vector.y.value.to_num())),
            },
            NodeRef::Rotation(rot) => {
                let degree = rot.deg.value.to_num() % 360;
                if degree == 0 {
                    return pre_state.clone();
                }

                // When the rotation is the multiple of 90 degrees, we can easily handle
                // the rotation and it will not interfere with the infinite bounds.
                // First translate the state so the rotation's center is at the origo.
                // Then do the rotation as if inf and -inf were just regular numbers.
                // Then undo the translation.
                let origin = Vec2::from(&rot.origin);
                let to_rotate = Vec2Domain {
                    x: pre_state.x + Interval::from(-origin.x),
                    y: pre_state.y + Interval::from(-origin.y),
                };
                if degree == 270 {
                    return Vec2Domain {
                        x: to_rotate.y + Interval::from(origin.x),
                        y: -to_rotate.x + Interval::from(origin.y),
                    };
                }
                if degree == 180 {
                    return Vec2Domain {
                        x: -to_rotate.x + Interval::from(origin.x),
                        y: -to_rotate.y + Interval::from(origin.y),
                    };
                }
                if degree == 90 {
                    return Vec2Domain {
                        x: -to_rotate.y + Interval::from(origin.x),
                        y: to_rotate.x + Interval::from(origin.y),
                    };
                }

                // TODO: when only some of the bounds are infinite, we might be able to
                //       preserve some information.
                if pre_state.x.max == domains::INF
                    || pre_state.x.min == domains::NEG_INF
                    || pre_state.y.max == domains::INF
                    || pre_state.y.min == domains::NEG_INF
                {
                    return Vec2Domain::top(lat_ctx);
                }

                // The two intervals describe a rectangle aligned with the axes:
                //
                //  +--------+
                //  |        |
                //  |        |
                //  +--------+
                //
                // To get the rotated region, we rotate all the corners and calculate
                // the bounding box of the result:
                //
                //  +---------+
                //  |   /¯--__|
                //  |  /     /|
                //  | /     / |
                //  |¯---__/  |
                //  +---------+
                //
                let mut corners = [
                    Vec2 {
                        x: pre_state.x.min,
                        y: pre_state.y.min,
                    },
                    Vec2 {
                        x: pre_state.x.min,
                        y: pre_state.y.max,
                    },
                    Vec2 {
                        x: pre_state.x.max,
                        y: pre_state.y.min,
                    },
                    Vec2 {
                        x: pre_state.x.max,
                        y: pre_state.y.max,
                    },
                ];
                for corner in &mut corners {
                    *corner = rotate(*corner, origin, degree);
                }

                Vec2Domain {
                    x: Interval {
                        min: corners.iter().min_by_key(|&x| x.x).unwrap().x,
                        max: corners.iter().max_by_key(|&x| x.x).unwrap().x,
                    },
                    y: Interval {
                        min: corners.iter().min_by_key(|&x| x.y).unwrap().y,
                        max: corners.iter().max_by_key(|&x| x.y).unwrap().y,
                    },
                }
            }
            _ => panic!("Unexpected operation."),
        }
    }
}

impl IntervalAnalysis {
    pub fn get_results(cfg: &Cfg) -> Vec<Vec2Interval> {
        let solver = SolveMonotone::default();
        let seed = Vec2Interval::bottom(&());
        solver.solve(cfg, seed, &(), &mut IntervalAnalysis)
    }
}

impl Analysis for IntervalAnalysis {
    fn analyze(&self, cfg: &Cfg) -> AnalysisResult {
        let results = Self::get_results(cfg);
        let annotations =
            annotations_from_forward_analysis_results(cfg, &(), &mut IntervalAnalysis, &results);
        let covered = covered_area_from_analysis_results(cfg, &(), &mut IntervalAnalysis, &results);
        AnalysisResult {
            annotations,
            covered,
        }
    }
}
