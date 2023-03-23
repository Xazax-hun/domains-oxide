use std::{cmp::Ordering, collections::HashSet};

use crate::{
    ir::{Annotations, Cfg, Operation, Type, Unit, Variable},
    lexer::{Identifier, Token, TokenValue},
};
use analysis::{
    domains::{
        IntervalDomain, JoinSemiLattice, Lattice, Map, MapCtx, BOOL_RANGE, FALSE_RANGE, TRUE_RANGE,
    },
    solvers::{SolveMonotone, TransferFunction},
};

use super::{Analysis, TransferLogger};

type IntervalEnv = Map<Identifier, IntervalDomain>;
type IntervalCtx = MapCtx<Identifier, IntervalDomain>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IntervalAnalysis;

impl IntervalAnalysis {
    fn transfer_binary_op(
        token: Token,
        lhs: IntervalDomain,
        rhs: IntervalDomain,
    ) -> IntervalDomain {
        match token.value {
            TokenValue::Add => lhs + rhs,
            TokenValue::Mul => lhs * rhs,
            TokenValue::Sub => lhs - rhs,
            TokenValue::Div => IntervalDomain::top(&()),
            TokenValue::Mod => IntervalDomain::top(&()),
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
            TokenValue::LessThanOrEq => match lhs.strict_cmp(rhs) {
                Some(Ordering::Greater) => FALSE_RANGE,
                Some(_) => TRUE_RANGE,
                _ => BOOL_RANGE,
            },
            TokenValue::GreaterThanOrEq => match lhs.strict_cmp(rhs) {
                Some(Ordering::Less) => FALSE_RANGE,
                Some(_) => TRUE_RANGE,
                _ => BOOL_RANGE,
            },
            _ => {
                panic!("Unexpected binary operator.")
            }
        }
    }

    fn transfer_unary_op(token: Token, operand: IntervalDomain) -> IntervalDomain {
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
                let lhs = *pre_state
                    .get(&lhs.id)
                    .unwrap_or(&IntervalDomain::bottom(&ctx.1));
                let rhs = *pre_state
                    .get(&rhs.id)
                    .unwrap_or(&IntervalDomain::bottom(&ctx.1));
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
                    .unwrap_or(&IntervalDomain::bottom(&ctx.1));
                let result_sign = Self::transfer_unary_op(*token, operand);
                let mut new_state = pre_state.clone();
                new_state.insert(result.id, result_sign);
                new_state
            }
            Operation::Const(token, result) => {
                let mut new_state = pre_state.clone();
                let val = match token.value {
                    TokenValue::Integer(i) => IntervalDomain::from(i64::from(i)),
                    TokenValue::True => IntervalDomain::from(1),
                    TokenValue::False => IntervalDomain::from(0),
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
                    _ => IntervalDomain::top(&()),
                },
            );
        }

        let mut transfer = TransferLogger::new(unit, IntervalAnalysis);
        solver.solve(cfg, seed, &ctx, &mut transfer);
        transfer.get_annotations()
    }
}
