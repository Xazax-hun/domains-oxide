use std::collections::HashSet;
use std::usize;

use super::cfg::*;
use super::solvers::*;

#[derive(Default, Clone)]
pub struct TestBasicBlock {
    succs: Vec<usize>,
    preds: Vec<usize>,
}

impl CfgBlock for TestBasicBlock {
    type Operation = usize;

    fn operations(&self) -> &[Self::Operation] {
        &[]
    }

    fn predecessors(&self) -> &[usize] {
        &self.preds
    }

    fn successors(&self) -> &[usize] {
        &self.succs
    }
}

#[derive(Default)]
pub struct TestCfg {
    basic_blocks: Vec<TestBasicBlock>,
}

impl ControlFlowGraph for TestCfg {
    type Block = TestBasicBlock;

    fn blocks(&self) -> &[Self::Block] {
        &self.basic_blocks
    }
}

impl MutableCfg for TestCfg {
    fn new_block(&mut self) -> usize {
        self.basic_blocks.push(TestBasicBlock::default());
        self.basic_blocks.len()
    }

    fn add_block(&mut self, block: <Self as ControlFlowGraph>::Block) -> usize {
        self.basic_blocks.push(block);
        self.basic_blocks.len() - 1
    }

    fn remove_block(&mut self, block: usize) -> TestBasicBlock {
        self.basic_blocks.remove(block)
    }

    fn add_edge(&mut self, from: usize, to: usize) -> &mut Self {
        self.basic_blocks[from].succs.push(to);
        self.basic_blocks[to].preds.push(from);
        self
    }

    fn remove_edge(&mut self, from: usize, to: usize) -> &mut Self {
        self.basic_blocks[from].succs.retain(|b| *b != to);
        self.basic_blocks[to].succs.retain(|b| *b != from);
        self
    }
}

impl BlockMutableCfg for TestCfg {
    fn extend_block<'cfg>(
        &'cfg mut self,
        _block: usize,
        _ops: impl Iterator<Item = &'cfg <<Self as ControlFlowGraph>::Block as CfgBlock>::Operation>,
    ) {
    }

    fn remove_ops(
        &mut self,
        _block: usize,
    ) -> Vec<<<Self as ControlFlowGraph>::Block as CfgBlock>::Operation> {
        Vec::default()
    }
}

impl TestCfg {
    pub fn new(size: usize) -> Self {
        Self {
            basic_blocks: vec![TestBasicBlock::default(); size],
        }
    }
}

#[test]
fn test_cfg_print() {
    //     0
    //    / \
    //   1   2
    //   |   |
    //   |   3
    //    \ /
    //     4
    let mut cfg = TestCfg::new(5);
    cfg.add_edge(0, 1)
        .add_edge(0, 2)
        .add_edge(1, 4)
        .add_edge(2, 3)
        .add_edge(3, 4);

    let printed = print(&cfg, |_| "".to_owned());
    let expected = r#"digraph CFG {
  Node_0[label=""]
  Node_1[label=""]
  Node_2[label=""]
  Node_3[label=""]
  Node_4[label=""]

  Node_0 -> Node_1
  Node_0 -> Node_2
  Node_1 -> Node_4
  Node_2 -> Node_3
  Node_3 -> Node_4
}
"#;
    assert_eq!(printed, expected);
}

#[test]
fn test_cfg_reverse() {
    //     0
    //    / \
    //   1   2
    //   \   /
    //     3
    //     |
    //     4
    let mut cfg = TestCfg::new(5);
    cfg.add_edge(0, 1)
        .add_edge(0, 2)
        .add_edge(1, 3)
        .add_edge(2, 3)
        .add_edge(3, 4);

    //     0
    //     |
    //     1
    //    / \
    //   2   3
    //   \   /
    //     4
    let reversed = reverse(&cfg);
    let printed = print(&reversed, |_| "".to_owned());
    let expected = r#"digraph CFG {
  Node_0[label=""]
  Node_1[label=""]
  Node_2[label=""]
  Node_3[label=""]
  Node_4[label=""]

  Node_0 -> Node_1
  Node_1 -> Node_3
  Node_1 -> Node_2
  Node_2 -> Node_4
  Node_3 -> Node_4
}
"#;
    assert_eq!(printed, expected);
}

#[test]
fn test_rpo_order() {
    //     0
    //    / \
    //   1   2
    //   |   |
    //   |   3
    //    \ /
    //     4
    let mut cfg = TestCfg::new(5);
    cfg.add_edge(0, 1)
        .add_edge(0, 2)
        .add_edge(1, 4)
        .add_edge(2, 3)
        .add_edge(3, 4);

    let mut worklist = RPOWorklist::new(&cfg);
    assert_eq!(worklist.get_rpo_order(0), 0);
    assert_eq!(worklist.get_rpo_order(1), 1);
    assert_eq!(worklist.get_rpo_order(2), 2);
    assert_eq!(worklist.get_rpo_order(3), 3);
    assert_eq!(worklist.get_rpo_order(4), 4);

    worklist.push(1);
    worklist.push(4);
    let (succ_1, succ_2) = (worklist.pop().unwrap(), worklist.pop().unwrap());
    assert_eq!((succ_1, succ_2), (1, 4));
    assert_eq!(worklist.pop(), None);
}

#[test]
fn test_rpo_order_mirrored() {
    //     0
    //    / \
    //   2   1
    //   |   |
    //   3   |
    //    \ /
    //     4
    let mut cfg = TestCfg::new(5);
    cfg.add_edge(0, 2)
        .add_edge(0, 1)
        .add_edge(1, 4)
        .add_edge(2, 3)
        .add_edge(3, 4);

    let worklist = RPOWorklist::new(&cfg);
    assert_eq!(worklist.get_rpo_order(0), 0);
    assert_eq!(worklist.get_rpo_order(2), 1);
    assert_eq!(worklist.get_rpo_order(3), 2);
    assert_eq!(worklist.get_rpo_order(1), 3);
    assert_eq!(worklist.get_rpo_order(4), 4);
}

#[test]
fn test_rpo_order_with_back_edges() {
    //      0  <----
    //     / \   | |
    //    1   2--| |
    //    |   |    |
    //    |   3----|
    //     \ /
    //      4
    let mut cfg = TestCfg::new(5);
    cfg.add_edge(0, 1)
        .add_edge(0, 2)
        .add_edge(1, 4)
        .add_edge(2, 3)
        .add_edge(2, 0)
        .add_edge(3, 4)
        .add_edge(3, 0);

    let worklist = RPOWorklist::new(&cfg);
    assert_eq!(worklist.get_rpo_order(0), 0);
    assert_eq!(worklist.get_rpo_order(1), 1);
    assert_eq!(worklist.get_rpo_order(2), 2);
    assert_eq!(worklist.get_rpo_order(3), 3);
    assert_eq!(worklist.get_rpo_order(4), 4);
}

#[test]
fn test_rpo_order_with_back_edges_2() {
    //      0  <----
    //     / \   | |
    // -->1   2--| |
    // |  |   |    |
    // |  |   3----|
    // |   \ /
    // |----4
    let mut cfg = TestCfg::new(5);
    cfg.add_edge(0, 1)
        .add_edge(0, 2)
        .add_edge(1, 4)
        .add_edge(2, 3)
        .add_edge(2, 0)
        .add_edge(3, 4)
        .add_edge(3, 0)
        .add_edge(4, 1);

    let worklist = RPOWorklist::new(&cfg);
    // TODO: is this actually the order we want?
    //       would we want to visit 1 earlier?
    //       It turns out while visiting 1 earlier would be nice,
    //       this control flow rarely happens in the real world.
    //       In most real world loops back edges will go to a loop header
    //       header that dominates all the nodes in the loop.
    assert_eq!(worklist.get_rpo_order(0), 0);
    assert_eq!(worklist.get_rpo_order(2), 1);
    assert_eq!(worklist.get_rpo_order(3), 2);
    assert_eq!(worklist.get_rpo_order(4), 3);
    assert_eq!(worklist.get_rpo_order(1), 4);
}

#[test]
fn test_get_back_edges() {
    //      0  <----
    //     / \   | |
    // -->1   2--| |
    // |  |   |    |
    // |  |   3----|
    // |   \ /
    // |----4
    let mut cfg = TestCfg::new(5);
    cfg.add_edge(0, 1)
        .add_edge(0, 2)
        .add_edge(1, 4)
        .add_edge(2, 3)
        .add_edge(2, 0)
        .add_edge(3, 4)
        .add_edge(3, 0)
        .add_edge(4, 1);

    let edges = get_back_edges(&cfg);
    assert_eq!(edges.len(), 3);
    assert!(edges.contains(&(2usize, 0usize)));
    assert!(edges.contains(&(3usize, 0usize)));
    // One might expect (4,1) but (1,4) is also a valid answer
    // according to one of the traversal orders. See the comment
    // in test_rpo_order_with_back_edges_2 why we don't care about
    // this case too much.
    assert!(edges.contains(&(1usize, 4usize)));
}

#[test]
fn basic_solver_visit_nodes() {
    //     0
    //    / \
    //   2   1
    //   |   |
    //   3   |
    //    \ /
    //     4
    let mut cfg = TestCfg::new(5);
    cfg.add_edge(0, 2)
        .add_edge(0, 1)
        .add_edge(1, 4)
        .add_edge(2, 3)
        .add_edge(3, 4);

    let mut visited = Vec::new();

    let solver = SolveMonotone::default();
    let result = solver.transfer_blocks(&cfg, &(), &mut |id, _, _, &dom: &()| {
        visited.push(id);
        dom
    });

    assert_eq!(result.len(), cfg.blocks().len());
    assert_eq!(visited.len(), 5);
    assert_eq!(
        visited.iter().collect::<HashSet<_>>(),
        vec![0, 1, 2, 3, 4].iter().collect::<HashSet<_>>()
    );
}
