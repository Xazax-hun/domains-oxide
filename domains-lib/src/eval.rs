use std::collections::HashMap;
use std::collections::HashSet;

use analysis::cfg::CfgBlock;
use analysis::cfg::ControlFlowGraph;
use rand::prelude::ThreadRng;
use rand::Rng;
use utils::Vec2;

use crate::ast::*;
use crate::cfg::Cfg;

pub struct Step {
    pub pos: Vec2,
    pub op: Operation,
}

pub type Walk = Vec<Step>;

/// A random walk in the CFG.
///
/// # Arguments
///
/// * `loopiness` - Bias towards taking a back edge in the CFG during the walk.
/// 1 means that back edges have the same chance of being picked as regular edges.
/// n means back edges are n times more likely than regular edges.
pub fn create_random_walk(cfg: &Cfg, ctx: &ASTContext, loopiness: u32) -> Walk {
    let mut w = Vec::new();
    if !matches!(
        cfg.blocks().first().unwrap().operations().first().unwrap(),
        Operation::Init(_)
    ) {
        return w; // TODO: add error message.
    }

    let mut rng = rand::thread_rng();

    // TODO: we should hoist this, so we do not recalculate for all the walks.
    let mut back_edges = analysis::cfg::get_back_edges(cfg);

    let mut current = 0;
    loop {
        for &op in cfg.blocks()[current].operations() {
            let step = match ctx.op_to_ref(op) {
                NodeRef::Init(init) => {
                    let from_x = init.bottom_left.x.value.to_num();
                    let from_y = init.bottom_left.y.value.to_num();
                    let to_x = from_x + init.size.x.value.to_num();
                    let to_y = from_y + init.size.y.value.to_num();
                    Step {
                        pos: Vec2 {
                            x: rng.gen_range(from_x..=to_x).into(),
                            y: rng.gen_range(from_y..=to_y).into(),
                        },
                        op,
                    }
                }
                NodeRef::Translation(trans) => {
                    let prev = w.last().unwrap().pos;
                    Step {
                        pos: prev
                            + Vec2 {
                                x: trans.vector.x.value.to_num().into(),
                                y: trans.vector.y.value.to_num().into(),
                            },
                        op,
                    }
                }
                NodeRef::Rotation(rot) => {
                    let origin = Vec2 {
                        x: rot.origin.x.value.to_num().into(),
                        y: rot.origin.y.value.to_num().into(),
                    };
                    let to_rotate = w.last().unwrap().pos;
                    Step {
                        pos: rotate(to_rotate, origin, rot.deg.value.to_num()),
                        op,
                    }
                }
                _ => panic!(),
            };
            w.push(step);
        }
        if cfg.blocks()[current].successors().is_empty() {
            break;
        }
        current = get_next_block(&mut rng, cfg, current, &mut back_edges, loopiness);
    }

    w
}

pub fn rotate(mut to_rotate: Vec2, origin: Vec2, deg: i32) -> Vec2 {
    to_rotate -= origin;
    let rad = to_rad(deg);
    let mut rotated = Vec2 {
        x: ((to_rotate.x as f64) * rad.cos() - (to_rotate.y as f64) * rad.sin()).round() as i64,
        y: ((to_rotate.y as f64) * rad.cos() + (to_rotate.x as f64) * rad.sin()).round() as i64,
    };
    rotated += origin;
    rotated
}

pub fn annotate_with_walks(walks: &[Walk]) -> Annotations {
    let mut collected_steps: HashMap<Operation, Vec<Vec2>> = HashMap::new();

    for walk in walks {
        walk.iter()
            .for_each(|step| collected_steps.entry(step.op).or_default().push(step.pos));
    }

    fn print_set(v: &[Vec2]) -> String {
        let mut result = "{".to_owned();

        let positions: Vec<String> = v
            .iter()
            .map(|v| format!("{{x: {}, y: {}}}", v.x, v.y))
            .collect();

        result.push_str(&positions.join(", "));
        result.push('}');

        result
    }

    let mut anns = Annotations::new();
    for (&op, pos) in &collected_steps {
        anns.post_annotations
            .insert(Node::Operation(op), vec![print_set(pos)]);
    }

    anns
}

fn to_rad(deg: i32) -> f64 {
    f64::from(deg) / 180_f64 * core::f64::consts::PI
}

fn get_next_block(
    rng: &mut ThreadRng,
    cfg: &Cfg,
    current: usize,
    back_edges: &mut HashSet<(usize, usize)>,
    loopiness: u32,
) -> usize {
    let succs = cfg.blocks()[current].successors();
    let (back_edges, regular_edges): (Vec<usize>, Vec<usize>) = succs
        .iter()
        .copied()
        .partition(|succ| back_edges.contains(&(current, *succ)));
    let max = regular_edges.len() + back_edges.len() * (loopiness as usize);
    let which_edge_kind = rng.gen_range(1..=max);

    if which_edge_kind <= regular_edges.len() {
        let next = rng.gen_range(0..regular_edges.len());
        return regular_edges[next];
    }

    let next = rng.gen_range(0..back_edges.len());
    back_edges[next]
}
