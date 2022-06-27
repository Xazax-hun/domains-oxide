use std::collections::HashMap;
use std::collections::HashSet;

use analysis::cfg::CfgBlock;
use analysis::cfg::ControlFlowGraph;
use rand::prelude::ThreadRng;
use rand::Rng;
use utils::Vec2;

use crate::ast::*;
use crate::cfg::*;

pub struct Step {
    pub pos: Vec2,
    pub op: Operation,
}

pub type Walk = Vec<Step>;

pub fn create_random_walk(cfg: &Cfg, ctx: &ASTContext, loopiness: u32) -> Walk {
    let mut w = Vec::new();
    if !matches!(
        cfg.blocks().first().unwrap().operations().first().unwrap(),
        Operation::Init(_)
    ) {
        return w; // TODO: add error message.
    }

    let mut rng = rand::thread_rng();

    // Discover what edges are back edges as we go to correctly
    // account for loopiness when choosing the next block.
    let mut visited: HashSet<usize> = HashSet::new();
    let mut back_edges: HashSet<(usize, usize)> = HashSet::new();

    let mut current = 0;
    loop {
        for op in cfg.blocks()[current].operations() {
            let step = match ctx.node_to_ref(op.into()) {
                NodeRef::Init(init) => {
                    let from_x = init.bottom_left.x.value.to_num();
                    let from_y = init.bottom_left.y.value.to_num();
                    let to_x = from_x + init.size.x.value.to_num();
                    let to_y = from_y + init.size.y.value.to_num();
                    Step {
                        pos: Vec2 {
                            x: rng.gen_range(from_x..=to_x),
                            y: rng.gen_range(from_y..=to_y),
                        },
                        op: *op,
                    }
                }
                NodeRef::Translation(trans) => {
                    let prev = w.last().unwrap().pos;
                    Step {
                        pos: prev
                            + Vec2 {
                                x: trans.vector.x.value.to_num(),
                                y: trans.vector.y.value.to_num(),
                            },
                        op: *op,
                    }
                }
                NodeRef::Rotation(rot) => {
                    let origin = Vec2 {
                        x: rot.origin.x.value.to_num(),
                        y: rot.origin.y.value.to_num(),
                    };
                    let to_rotate = w.last().unwrap().pos;
                    Step {
                        pos: rotate(to_rotate, origin, rot.deg.value.to_num()),
                        op: *op,
                    }
                }
                _ => panic!(),
            };
            w.push(step);
        }
        if cfg.blocks()[current].successors().is_empty() {
            break;
        }
        visited.insert(current);
        back_edges.extend(detect_back_edges(cfg, current, &visited));
        current = get_next_block(&mut rng, cfg, current, &mut back_edges, loopiness);
    }

    w
}
pub fn rotate(mut to_rotate: Vec2, origin: Vec2, deg: i32) -> Vec2 {
    to_rotate -= origin;
    let rad = to_rad(deg);
    let mut rotated = Vec2 {
        x: (f64::from(to_rotate.x) * rad.cos() - f64::from(to_rotate.y) * rad.sin()).round() as i32,
        y: (f64::from(to_rotate.y) * rad.cos() + f64::from(to_rotate.x) * rad.sin()).round() as i32,
    };
    rotated += origin;
    rotated
}

pub fn annotate_with_walks(walks: &[Walk]) -> Annotations {
    let mut collected_steps: HashMap<Operation, Vec<Vec2>> = HashMap::new();

    for walk in walks {
        for step in walk {
            let steps_at = collected_steps.entry(step.op).or_insert(Vec::new());
            steps_at.push(step.pos);
        }
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
    for (op, pos) in &collected_steps {
        anns.post_annotations
            .insert(op.into(), vec![print_set(pos)]);
    }

    anns
}

fn to_rad(deg: i32) -> f64 {
    deg as f64 / 180f64 * std::f64::consts::PI
}

fn detect_back_edges(
    cfg: &Cfg,
    current: usize,
    visited: &HashSet<usize>,
) -> HashSet<(usize, usize)> {
    // Only do back-edge detection when we first encounter a node.
    if visited.contains(&current) {
        return HashSet::new();
    }

    let mut result: HashSet<(usize, usize)> = HashSet::new();
    for succ in cfg.blocks()[current].successors() {
        if visited.contains(&succ) {
            result.insert((current, succ));
        }
    }

    result
}

fn get_next_block(
    rng: &mut ThreadRng,
    cfg: &Cfg,
    current: usize,
    back_edges: &mut HashSet<(usize, usize)>,
    loopiness: u32,
) -> usize {
    let succs = cfg.blocks()[current].successors();
    let (regular_edges, back_edges): (Vec<usize>, Vec<usize>) = succs
        .into_iter()
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
