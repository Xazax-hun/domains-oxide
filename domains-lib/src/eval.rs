use std::collections::HashMap;
use std::collections::HashSet;

use analysis::cfg::CfgBlock;
use analysis::cfg::ControlFlowGraph;
use rand::prelude::ThreadRng;
use rand::Rng;
use utils::Vec2;

use crate::ast::*;
use crate::cfg::Cfg;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Step {
    pub pos: Vec2,
    pub op: Operation,
}

pub type Walk = Vec<Step>;

/// Create a list of random walks in the Cfg.
///
/// # Arguments
///
/// * `loopiness` - Bias towards taking a back edge in the CFG during the walk.
/// 1 means that back edges have the same chance of being picked as regular edges.
/// n means back edges are n times more likely than regular edges.
pub fn create_random_walks(cfg: &Cfg, ctx: &ASTContext, loopiness: u32, num: usize) -> Vec<Walk> {
    if !matches!(
        cfg.blocks().first().unwrap().operations().first().unwrap(),
        Operation::Init(_)
    ) {
        return Vec::new(); // TODO: add error message.
    }

    let mut rng = rand::thread_rng();
    let back_edges = analysis::cfg::get_back_edges(cfg);
    let mut result = Vec::new();
    result.reserve(num);

    for _ in 1..=num {
        result.push(create_random_walk_impl(
            &mut rng,
            cfg,
            &back_edges,
            ctx,
            loopiness,
        ));
    }
    result
}

/// A random walk in the CFG.
///
/// # Arguments
///
/// * `loopiness` - Bias towards taking a back edge in the CFG during the walk.
/// 1 means that back edges have the same chance of being picked as regular edges.
/// n means back edges are n times more likely than regular edges.
pub fn create_random_walk(cfg: &Cfg, ctx: &ASTContext, loopiness: u32) -> Walk {
    if !matches!(
        cfg.blocks().first().unwrap().operations().first().unwrap(),
        Operation::Init(_)
    ) {
        return Vec::new(); // TODO: add error message.
    }

    let mut rng = rand::thread_rng();
    let back_edges = analysis::cfg::get_back_edges(cfg);

    create_random_walk_impl(&mut rng, cfg, &back_edges, ctx, loopiness)
}

fn create_random_walk_impl(
    rng: &mut ThreadRng,
    cfg: &Cfg,
    back_edges: &HashSet<(usize, usize)>,
    ctx: &ASTContext,
    loopiness: u32,
) -> Walk {
    let mut w: Vec<Step> = Vec::new();
    let mut current = 0;
    loop {
        for &op in cfg.blocks()[current].operations() {
            let step = match ctx.op_to_ref(op) {
                NodeRef::Init(init) => {
                    let from = Vec2::from(&init.bottom_left);
                    let to = from + Vec2::from(&init.size);
                    Step {
                        pos: Vec2 {
                            x: rng.gen_range(from.x..=to.x),
                            y: rng.gen_range(from.y..=to.y),
                        },
                        op,
                    }
                }
                NodeRef::Translation(trans) => {
                    let prev = w.last().unwrap().pos;
                    Step {
                        pos: prev + Vec2::from(&trans.vector),
                        op,
                    }
                }
                NodeRef::Rotation(rot) => {
                    let origin = Vec2::from(&rot.origin);
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
        current = get_next_block(rng, cfg, current, back_edges, loopiness);
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
    back_edges: &HashSet<(usize, usize)>,
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
