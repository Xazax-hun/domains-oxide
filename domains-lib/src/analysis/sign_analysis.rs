use analysis::domains::SignDomain;
use analysis::domains::Vec2Domain;
use analysis::solvers::SolveMonotone;

use crate::ast::{NodeRef, Operation};
use crate::cfg::Cfg;

type Vec2Sign = Vec2Domain<SignDomain>;

fn sing_transfer(&op: &Operation, cfg: &Cfg, pre_state: &Vec2Sign) -> Vec2Sign {
    let ctx = cfg.context();
    match ctx.op_to_ref(op) {
        NodeRef::Init(init) => {
            let bot_left_x = init.bottom_left.x.value.to_num();
            let width = init.size.x.value.to_num();
            let x_sign = if bot_left_x > 0 {
                SignDomain::Positive
            } else if bot_left_x + width < 0 {
                SignDomain::Negative
            } else if bot_left_x == 0 && width == 0 {
                SignDomain::Zero
            } else {
                SignDomain::Top
            };

            let bot_left_y = init.bottom_left.y.value.to_num();
            let height = init.size.y.value.to_num();
            let y_sign = if bot_left_y > 0 {
                SignDomain::Positive
            } else if bot_left_y + height < 0 {
                SignDomain::Negative
            } else if bot_left_y == 0 && height == 0 {
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

            if rot.origin.x.value.to_num() == 0 && rot.origin.y.value.to_num() == 0 {
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

pub fn get_sign_analysis(cfg: &Cfg) -> Vec<Vec2Sign> {
    let solver = SolveMonotone::default();
    solver.transfer_operations(cfg, &(), &mut sing_transfer)
}
