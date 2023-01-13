use super::cfg::{BlockMutableCfg, ControlFlowGraph, reverse};
use super::domains::*;
use super::solvers::SolveMonotone;

pub fn calculate_dominators<Cfg: ControlFlowGraph>(
    cfg: &Cfg,
    node_limit: usize,
) -> Vec<Flipped<BitSetDomain>> {
    let node_num = cfg.blocks().len();
    let ctx = BitSetTop(node_num);
    let mut states = Vec::new();
    states.push(Flipped(BitSetDomain::from(&ctx, &[0])));
    for _ in 1..node_num {
        states.push(Flipped::<BitSetDomain>::bottom(&ctx));
    }
    let solver = SolveMonotone { node_limit };
    solver.transfer_blocks_in_place(
        cfg,
        &ctx,
        &mut states,
        &mut |id: usize,
              _block: &<Cfg as ControlFlowGraph>::Block,
              preds_merged: &Flipped<BitSetDomain>| {
            let mut result = Flipped(BitSetDomain::from(&ctx, &[id]));
            result = result.meet(preds_merged);
            result
        },
    );
    states
}

pub fn calculate_post_dominators<Cfg: BlockMutableCfg>(
    cfg: &Cfg,
    node_limit: usize,
) -> Vec<Flipped<BitSetDomain>> {
    let reversed = reverse(cfg);
    let mut result = calculate_dominators(&reversed, node_limit);
    let node_num = cfg.blocks().len();
    let ctx = BitSetTop(node_num);
    // TODO: make a more general utility to reverse the results.
    result.reverse();
    result
        .iter()
        .map(|dom| {
            let mut result = Flipped::<BitSetDomain>::top(&ctx);
            for item in dom.0 .0.ones() {
                result.0 .0.insert(node_num - 1 - item);
            }
            result
        })
        .collect()
}
