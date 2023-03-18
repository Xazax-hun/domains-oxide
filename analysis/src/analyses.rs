use super::cfg::{reverse, BlockMutableCfg, ControlFlowGraph};
use super::domains::*;
use super::solvers::SolveMonotone;

pub fn calculate_dominators(
    cfg: &impl ControlFlowGraph,
    node_limit: usize,
) -> Vec<Flipped<BitSetDomain>> {
    let node_num = cfg.blocks().len();
    let ctx = BitSetTop(node_num);
    let solver = SolveMonotone { node_limit };
    solver.transfer_blocks(
        cfg,
        Flipped(BitSetDomain::from(&ctx, &[0])),
        &ctx,
        &mut |id, _cfg, _lat_ctx, preds_merged| {
            let mut result = Flipped(BitSetDomain::from(&ctx, &[id]));
            result = result.meet(preds_merged, &ctx);
            result
        },
    )
}

pub fn calculate_post_dominators<Cfg: BlockMutableCfg + Default>(
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

// TODO: add a proper dominator tree type:
//  * It should be traversable
//  * It should answer queries efficiently
// TODO: add other utilities like calculating control dependence.
