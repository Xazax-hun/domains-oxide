use std::{cmp::Ordering, collections::HashSet};

use crate::{
    ir::{Annotations, Cfg, Operation, Type, Unit, Variable},
    lexer::{Identifier, Token, TokenValue},
};
use analysis::{
    cfg::{CfgBlock, ControlFlowGraph},
    domains::{
        BOOL_RANGE, FALSE_RANGE, Interval, JoinSemiLattice, LatticeNoContext, Map, MapCtx,
        TRUE_RANGE, UnrollWiden,
    },
    solvers::{SolveMonotone, TransferFunction},
};

use super::{Analysis, TransferLogger};

type IntervalEnv = Map<Identifier, Interval>;
type IntervalCtx = MapCtx<Identifier, Interval>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IntervalAnalysis;

impl IntervalAnalysis {
    fn transfer_binary_op(token: Token, lhs: Interval, rhs: Interval) -> Interval {
        match token.value {
            TokenValue::Add => lhs + rhs,
            TokenValue::Mul => lhs * rhs,
            TokenValue::Sub => lhs - rhs,
            TokenValue::Div => Interval::top_(),
            TokenValue::Mod => lhs % rhs,
            TokenValue::Equal => lhs.equals(rhs),
            TokenValue::And => lhs.logical_and(rhs),
            TokenValue::Or => lhs.logical_or(rhs),
            TokenValue::LessThan => match lhs.strict_cmp(rhs) {
                Some(Ordering::Less) => TRUE_RANGE,
                Some(_) => FALSE_RANGE,
                _ => BOOL_RANGE,
            },
            TokenValue::GreaterThan => match lhs.strict_cmp(rhs) {
                Some(Ordering::Greater) => TRUE_RANGE,
                Some(_) => FALSE_RANGE,
                _ => BOOL_RANGE,
            },
            TokenValue::LessThanOrEq => match lhs.weak_cmp(rhs) {
                Some(Ordering::Greater) => FALSE_RANGE,
                Some(_) => TRUE_RANGE,
                _ => BOOL_RANGE,
            },
            TokenValue::GreaterThanOrEq => match lhs.weak_cmp(rhs) {
                Some(Ordering::Less) => FALSE_RANGE,
                Some(_) => TRUE_RANGE,
                _ => BOOL_RANGE,
            },
            _ => {
                panic!("Unexpected binary operator.")
            }
        }
    }

    fn transfer_unary_op(token: Token, operand: Interval) -> Interval {
        match token.value {
            TokenValue::Not => operand.logical_not(),
            TokenValue::Identity => operand,
            _ => {
                panic!("Unexpected unary operator.");
            }
        }
    }
}

impl TransferFunction<Cfg, IntervalEnv> for IntervalAnalysis {
    fn operation(
        &mut self,
        _pos: analysis::cfg::OpPos,
        op: &Operation,
        _cfg: &Cfg,
        ctx: &IntervalCtx,
        pre_state: &IntervalEnv,
    ) -> IntervalEnv {
        match op {
            Operation::BinaryOp {
                token,
                result,
                lhs,
                rhs,
            } => {
                let lhs = pre_state.get_or_bottom(&lhs.id, ctx);
                let rhs = pre_state.get_or_bottom(&rhs.id, ctx);
                let result_range = Self::transfer_binary_op(*token, lhs, rhs);
                let mut new_state = pre_state.clone();
                new_state.insert(result.id, result_range);
                new_state
            }
            Operation::UnaryOp {
                token,
                result,
                operand,
            } => {
                let operand = pre_state.get_or_bottom(&operand.id, ctx);
                let result_range = Self::transfer_unary_op(*token, operand);
                let mut new_state = pre_state.clone();
                new_state.insert(result.id, result_range);
                new_state
            }
            Operation::Const(token, result) => {
                let mut new_state = pre_state.clone();
                let val = match token.value {
                    TokenValue::Integer(i) => Interval::from(i64::from(i)),
                    TokenValue::True => Interval::from(1),
                    TokenValue::False => Interval::from(0),
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
        ctx: &IntervalCtx,
        pre_state: &IntervalEnv,
    ) -> Option<IntervalEnv> {
        let is_true_branch = *cfg.blocks()[from].successors().first().unwrap() == to;
        let last_op = cfg.blocks()[from].operations().last().unwrap();
        match last_op {
            Operation::Branch { cond, .. } => {
                let cond_range = pre_state.get_or_top(&cond.id, ctx);
                if cond_range == FALSE_RANGE && is_true_branch {
                    return None;
                }
                if cond_range == TRUE_RANGE && !is_true_branch {
                    return None;
                }
                Some(pre_state.clone())
            }
            _ => Some(pre_state.clone()),
        }
    }
}

impl Analysis for IntervalAnalysis {
    fn analyze(&self, cfg: &Cfg, unit: &Unit) -> Annotations {
        let solver = SolveMonotone::default();

        let ctx = MapCtx(HashSet::new(), ());
        let mut seed = IntervalEnv::bottom(&ctx);
        // Values for the formal parameters.
        for Variable { id, ty } in cfg.get_formals() {
            seed.insert(
                *id,
                match ty {
                    Type::Bool => BOOL_RANGE,
                    _ => Interval::top_(),
                },
            );
        }

        let mut transfer = TransferLogger::new(unit, Self);
        solver.solve(cfg, seed, &ctx, &mut transfer);
        transfer.get_annotations()
    }
}

type UnrolledIntervalEnv = UnrollWiden<Map<Identifier, Interval>>;
type UnrolledIntervalCtx = (usize, MapCtx<Identifier, Interval>);

// TODO: remove boilerplate.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct UnrolledIntervalAnalysis(pub usize);
impl TransferFunction<Cfg, UnrolledIntervalEnv> for UnrolledIntervalAnalysis {
    fn operation(
        &mut self,
        pos: analysis::cfg::OpPos,
        op: &Operation,
        cfg: &Cfg,
        ctx: &UnrolledIntervalCtx,
        pre_state: &UnrolledIntervalEnv,
    ) -> UnrolledIntervalEnv {
        UnrollWiden(IntervalAnalysis.operation(pos, op, cfg, &ctx.1, &pre_state.0))
    }

    fn edge(
        &mut self,
        from: usize,
        to: usize,
        cfg: &Cfg,
        ctx: &UnrolledIntervalCtx,
        pre_state: &UnrolledIntervalEnv,
    ) -> Option<UnrolledIntervalEnv> {
        IntervalAnalysis
            .edge(from, to, cfg, &ctx.1, &pre_state.0)
            .map(UnrollWiden)
    }
}

impl Analysis for UnrolledIntervalAnalysis {
    fn analyze(&self, cfg: &Cfg, unit: &Unit) -> Annotations {
        let solver = SolveMonotone::default();

        let ctx = (self.0, MapCtx::for_join_semi_lattice());
        let mut seed = UnrolledIntervalEnv::bottom(&ctx);
        // Values for the formal parameters.
        for Variable { id, ty } in cfg.get_formals() {
            seed.insert(
                *id,
                match ty {
                    Type::Bool => BOOL_RANGE,
                    _ => Interval::top_(),
                },
            );
        }

        let mut transfer = TransferLogger::new(unit, Self(2));
        solver.solve(cfg, seed, &ctx, &mut transfer);
        transfer.get_annotations()
    }
}
