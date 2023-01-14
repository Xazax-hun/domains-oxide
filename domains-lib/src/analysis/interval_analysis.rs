use analysis::domains::{self, IntervalDomain, Lattice, Vec2Domain};
use analysis::solvers::SolveMonotone;

use utils::Vec2;

use crate::analysis::annotations_from_forward_analysis_results;
use crate::ast::{Annotations, NodeRef, Operation};
use crate::cfg::Cfg;
use crate::eval::rotate;

use super::Analysis;

type Vec2Interval = Vec2Domain<IntervalDomain>;

pub struct IntervalAnalysis;

impl IntervalAnalysis {
    pub fn get_results(cfg: &Cfg) -> Vec<Vec2Interval> {
        let solver = SolveMonotone::default();
        solver.transfer_operations(cfg, &(), &mut IntervalAnalysis::transfer)
    }

    pub fn transfer(
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
                    x: IntervalDomain {
                        min: bot_left.x.into(),
                        max: (bot_left.x + size.x).into(),
                    },
                    y: IntervalDomain {
                        min: bot_left.y.into(),
                        max: (bot_left.y + size.y).into(),
                    },
                }
            }
            NodeRef::Translation(trans) => Vec2Domain {
                x: pre_state.x + IntervalDomain::from(trans.vector.x.value.to_num()),
                y: pre_state.y + IntervalDomain::from(trans.vector.y.value.to_num()),
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
                    x: pre_state.x + IntervalDomain::from(-origin.x),
                    y: pre_state.y + IntervalDomain::from(-origin.y),
                };
                if degree == 270 {
                    return Vec2Domain {
                        x: to_rotate.y + IntervalDomain::from(origin.x),
                        y: -to_rotate.x + IntervalDomain::from(origin.y),
                    };
                }
                if degree == 180 {
                    return Vec2Domain {
                        x: -to_rotate.x + IntervalDomain::from(origin.x),
                        y: -to_rotate.y + IntervalDomain::from(origin.y),
                    };
                }
                if degree == 90 {
                    return Vec2Domain {
                        x: -to_rotate.y + IntervalDomain::from(origin.x),
                        y: to_rotate.x + IntervalDomain::from(origin.y),
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
                        x: pre_state.x.min as i32,
                        y: pre_state.y.min as i32,
                    },
                    Vec2 {
                        x: pre_state.x.min as i32,
                        y: pre_state.y.max as i32,
                    },
                    Vec2 {
                        x: pre_state.x.max as i32,
                        y: pre_state.y.min as i32,
                    },
                    Vec2 {
                        x: pre_state.x.max as i32,
                        y: pre_state.y.max as i32,
                    },
                ];
                for corner in &mut corners {
                    *corner = rotate(*corner, origin, degree);
                }

                Vec2Domain {
                    x: IntervalDomain {
                        min: corners.iter().min_by_key(|&x| x.x).unwrap().x.into(),
                        max: corners.iter().max_by_key(|&x| x.x).unwrap().x.into(),
                    },
                    y: IntervalDomain {
                        min: corners.iter().min_by_key(|&x| x.y).unwrap().y.into(),
                        max: corners.iter().max_by_key(|&x| x.y).unwrap().y.into(),
                    },
                }
            }
            _ => panic!("Unexpected operation."),
        }
    }
}

impl Analysis for IntervalAnalysis {
    fn annotate(&self, cfg: &Cfg) -> Annotations {
        let results = IntervalAnalysis::get_results(cfg);
        annotations_from_forward_analysis_results(
            cfg,
            &(),
            &mut IntervalAnalysis::transfer,
            &results,
        )
    }
}
