use super::cfg::*;
use super::domains::*;

pub fn solve_monotone_framework<Cfg, D, F, const NODELIMIT: usize>(
    cfg: &Cfg,
    transfer: &mut F,
) -> Vec<D>
where
    Cfg: ControlFlowGraph,
    D: Domain,
    F: FnMut(&<<Cfg as ControlFlowGraph>::Block as CfgBlock>::Element, &D) -> D,
{
    let limit = NODELIMIT * cfg.blocks().len();
    let mut processed_nodes = 0usize;
    let mut post_states = vec![D::bottom(); cfg.blocks().len()];
    let mut visited = vec![false; cfg.blocks().len()];

    let mut worklist = RPOWorklist::new(cfg);
    worklist.push(0usize);
    while let Some(current) = worklist.pop() {
        if limit > 0 && processed_nodes >= limit {
            return Vec::new();
        }

        let mut pre_state = D::bottom();
        for pred in cfg.blocks()[current].predecessors() {
            pre_state = pre_state.join(&post_states[*pred]);
        }
        let mut post_state = pre_state;
        for element in cfg.blocks()[current].operations() {
            post_state = transfer(element, &post_state);
        }

        processed_nodes += 1;
        if visited[current] && post_states[current] == post_state {
            continue;
        }

        visited[current] = true;
        post_states[current] = post_state;
        worklist.push_successors(current);
    }

    post_states
}
