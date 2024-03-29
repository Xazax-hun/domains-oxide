use crate::analyses::calculate_dominators;
use crate::analyses::calculate_post_dominators;
use crate::cfg::ControlFlowGraph;
use crate::domains::{BitSet, BitSetTop};

use super::cfg::MutableCfg;
use super::cfg_tests::TestCfg;

#[test]
fn test_dominators() {
    //     0
    //    / \
    //   1   2
    //   |   |
    //   |   3
    //    \ /
    //     4
    let mut cfg = TestCfg::new(5);
    cfg.add_edges(&[(0, 1), (0, 2), (1, 4), (2, 3), (3, 4)]);
    let ctx = BitSetTop(cfg.blocks().len());

    let dominators = calculate_dominators(&cfg, 20);
    assert_eq!(dominators[0].0, BitSet::from(&ctx, &[0]));
    assert_eq!(dominators[1].0, BitSet::from(&ctx, &[0, 1]));
    assert_eq!(dominators[2].0, BitSet::from(&ctx, &[0, 2]));
    assert_eq!(dominators[3].0, BitSet::from(&ctx, &[0, 2, 3]));
    assert_eq!(dominators[4].0, BitSet::from(&ctx, &[0, 4]));
}

#[test]
fn test_post_dominators() {
    //     0
    //    / \
    //   1   2
    //   |   |
    //   |   3
    //    \ /
    //     4
    let mut cfg = TestCfg::new(5);
    cfg.add_edges(&[(0, 1), (0, 2), (1, 4), (2, 3), (3, 4)]);
    let ctx = BitSetTop(cfg.blocks().len());

    let post_dominators = calculate_post_dominators(&cfg, 20);
    assert_eq!(post_dominators[0].0, BitSet::from(&ctx, &[0, 4]));
    assert_eq!(post_dominators[1].0, BitSet::from(&ctx, &[1, 4]));
    assert_eq!(post_dominators[2].0, BitSet::from(&ctx, &[2, 3, 4]));
    assert_eq!(post_dominators[3].0, BitSet::from(&ctx, &[3, 4]));
    assert_eq!(post_dominators[4].0, BitSet::from(&ctx, &[4]));
}
