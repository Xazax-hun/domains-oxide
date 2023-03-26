use std::collections::HashSet;

use analysis::{
    domains::{Congruence, JoinSemiLattice, Lattice, Map, MapCtx},
    solvers::{SolveMonotone, TransferFunction}, cfg::{OpPos, ControlFlowGraph, CfgBlock},
};

use crate::{
    ir::{Annotations, Cfg, Unit, Variable, Operation},
    lexer::{Identifier, TokenValue, Token},
};

use super::{Analysis, TransferLogger};

type CongruenceEnv = Map<Identifier, Congruence>;
type CongruenceCtx = MapCtx<Identifier, Congruence>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CongruenceAnalysis;

impl CongruenceAnalysis {
   fn transfer_binary_op(token: Token, lhs: Congruence, rhs: Congruence) -> Congruence {
        match token.value {
            TokenValue::Add => lhs + rhs,
            TokenValue::Mul => lhs * rhs,
            TokenValue::Sub => lhs - rhs,
            TokenValue::Div => Congruence::top(&()),
            TokenValue::Mod => Congruence::top(&()),
            TokenValue::Equal => Congruence::top(&()),
            TokenValue::And => Congruence::top(&()),
            TokenValue::Or => Congruence::top(&()),
            TokenValue::LessThan => Congruence::top(&()),
            TokenValue::GreaterThan => Congruence::top(&()),
            TokenValue::LessThanOrEq => Congruence::top(&()),
            TokenValue::GreaterThanOrEq => Congruence::top(&()),
            _ => {
                panic!("Unexpected binary operator.")
            }
        }
    }

    fn transfer_unary_op(token: Token, operand: Congruence) -> Congruence {
        match token.value {
            TokenValue::Not => Congruence::top(&()),
            TokenValue::Identity => operand,
            _ => {
                panic!("Unexpected unary operator.");
            }
        }
    }
}

impl TransferFunction<Cfg, CongruenceEnv> for CongruenceAnalysis {
    fn operation(
        &mut self,
        _pos: OpPos,
        op: &Operation,
        _cfg: &Cfg,
        ctx: &CongruenceCtx,
        pre_state: &CongruenceEnv,
    ) -> CongruenceEnv {
        match op {
            Operation::BinaryOp {
                token,
                result,
                lhs,
                rhs,
            } => {
                let lhs = *pre_state.get(&lhs.id).unwrap_or(&Congruence::bottom(&ctx.1));
                let rhs = *pre_state.get(&rhs.id).unwrap_or(&Congruence::bottom(&ctx.1));
                let result_sign = Self::transfer_binary_op(*token, lhs, rhs);
                let mut new_state = pre_state.clone();
                new_state.insert(result.id, result_sign);
                new_state
            }
            Operation::UnaryOp {
                token,
                result,
                operand,
            } => {
                let operand = *pre_state
                    .get(&operand.id)
                    .unwrap_or(&Congruence::bottom(&ctx.1));
                let result_sign = Self::transfer_unary_op(*token, operand);
                let mut new_state = pre_state.clone();
                new_state.insert(result.id, result_sign);
                new_state
            }
            Operation::Const(token, result) => {
                let mut new_state = pre_state.clone();
                let val = match token.value {
                    TokenValue::Integer(i) => Congruence::from(i64::from(i), 0),
                    TokenValue::True => Congruence::from(1, 0),
                    TokenValue::False => Congruence::from(0, 0),
                    _ => panic!("Unexpected token."),
                };
                new_state.insert(result.id, val);
                new_state
            }
            Operation::Jump(_, _)
            | Operation::Branch { .. }
            | Operation::Call { .. }
            | Operation::Ret(_, _)
            | Operation::Print(_, _)
            | Operation::Nop(_) => pre_state.clone(),
        }
    }

    fn edge(
        &mut self,
        from: usize,
        to: usize,
        cfg: &Cfg,
        ctx: &CongruenceCtx,
        pre_state: &CongruenceEnv,
    ) -> Option<CongruenceEnv> {
        let is_true_branch = *cfg.blocks()[from].successors().first().unwrap() == to;
        let last_op = cfg.blocks()[from].operations().last().unwrap();
        match last_op {
            Operation::Branch { cond, .. } => {
                let cond_range = *pre_state.get(&cond.id).unwrap_or(&Congruence::top(&ctx.1));
                if cond_range == Congruence::from(0, 0) && is_true_branch {
                    return None;
                }
                if cond_range == Congruence::from(1, 0) && !is_true_branch {
                    return None;
                }
                Some(pre_state.clone())
            }
            _ => Some(pre_state.clone()),
        }
    }
}

impl Analysis for CongruenceAnalysis {
    fn analyze(&self, cfg: &Cfg, unit: &Unit) -> Annotations {
        let solver = SolveMonotone::default();

        let ctx = MapCtx(HashSet::new(), ());
        let mut seed = CongruenceEnv::bottom(&ctx);
        // Values for the formal parameters.
        for Variable { id, .. } in cfg.get_formals() {
            seed.insert(*id, Congruence::top(&()));
        }

        let mut transfer = TransferLogger::new(unit, CongruenceAnalysis);
        solver.solve(cfg, seed, &ctx, &mut transfer);
        transfer.get_annotations()
    }
}
