use core::cmp::Ordering;

use analysis::{
    cfg::{CfgBlock, ControlFlowGraph, OpPos},
    domains::{JoinSemiLattice, Map, MapCtx, Sign},
    solvers::{SolveMonotone, TransferFunction},
};

use crate::{
    ir::{Annotations, Cfg, Operation, Type, Unit, Variable},
    lexer::{Identifier, Token, TokenValue},
};

use super::{Analysis, TransferLogger};

type SignEnv = Map<Identifier, Sign>;
type SignCtx = MapCtx<Identifier, Sign>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SignAnalysis;

impl SignAnalysis {
    fn transfer_binary_op(token: Token, lhs: Sign, rhs: Sign, op_type: Type) -> Sign {
        match token.value {
            TokenValue::Add => lhs + rhs,
            TokenValue::Mul => lhs * rhs,
            TokenValue::Sub => lhs - rhs,
            TokenValue::Div => lhs / rhs,
            TokenValue::Mod => lhs % rhs,
            TokenValue::Equal => lhs.logical_eq(rhs, op_type == Type::Bool),
            TokenValue::And => lhs.logical_and(rhs),
            TokenValue::Or => lhs.logical_or(rhs),
            TokenValue::LessThan => match lhs.strict_cmp(rhs) {
                Some(Ordering::Less) => Sign::Positive,
                Some(_) => Sign::Zero,
                _ => Sign::NonNeg,
            },
            TokenValue::GreaterThan => match lhs.strict_cmp(rhs) {
                Some(Ordering::Greater) => Sign::Positive,
                Some(_) => Sign::Zero,
                _ => Sign::NonNeg,
            },
            TokenValue::LessThanOrEq => match lhs.weak_cmp(rhs) {
                Some(Ordering::Less) => Sign::Positive,
                Some(_) => Sign::Zero,
                _ => Sign::NonNeg,
            },
            TokenValue::GreaterThanOrEq => match lhs.weak_cmp(rhs) {
                Some(Ordering::Greater) => Sign::Positive,
                Some(_) => Sign::Zero,
                _ => Sign::NonNeg,
            },
            _ => {
                panic!("Unexpected binary operator.")
            }
        }
    }

    fn transfer_unary_op(token: Token, operand: Sign) -> Sign {
        match token.value {
            TokenValue::Not => operand.logical_not(),
            TokenValue::Identity => operand,
            _ => {
                panic!("Unexpected unary operator.");
            }
        }
    }
}

impl TransferFunction<Cfg, SignEnv> for SignAnalysis {
    fn operation(
        &mut self,
        _pos: OpPos,
        op: &Operation,
        _cfg: &Cfg,
        ctx: &SignCtx,
        pre_state: &SignEnv,
    ) -> SignEnv {
        match op {
            Operation::BinaryOp {
                token,
                result,
                lhs,
                rhs,
            } => {
                let op_type = lhs.ty;
                let lhs = pre_state.get_or_bottom(&lhs.id, ctx);
                let rhs = pre_state.get_or_bottom(&rhs.id, ctx);
                let result_sign = Self::transfer_binary_op(*token, lhs, rhs, op_type);
                let mut new_state = pre_state.clone();
                new_state.insert(result.id, result_sign);
                new_state
            }
            Operation::UnaryOp {
                token,
                result,
                operand,
            } => {
                let operand = pre_state.get_or_bottom(&operand.id, ctx);
                let result_sign = Self::transfer_unary_op(*token, operand);
                let mut new_state = pre_state.clone();
                new_state.insert(result.id, result_sign);
                new_state
            }
            Operation::Const(token, result) => {
                let mut new_state = pre_state.clone();
                let val = match token.value {
                    TokenValue::Integer(i) => Sign::from(i),
                    TokenValue::True => Sign::Positive,
                    TokenValue::False => Sign::Zero,
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
        ctx: &SignCtx,
        pre_state: &SignEnv,
    ) -> Option<SignEnv> {
        let is_true_branch = *cfg.blocks()[from].successors().first().unwrap() == to;
        let last_op = cfg.blocks()[from].operations().last().unwrap();
        match last_op {
            Operation::Branch { cond, .. } => match pre_state.get_or_top(&cond.id, ctx) {
                Sign::Zero if is_true_branch => None,
                Sign::Positive if !is_true_branch => None,
                _ => Some(pre_state.clone()),
            },
            _ => Some(pre_state.clone()),
        }
    }
}

impl Analysis for SignAnalysis {
    fn analyze(&self, cfg: &Cfg, unit: &Unit) -> Annotations {
        let solver = SolveMonotone::default();

        let ctx = MapCtx::for_join_semi_lattice();
        let mut seed = SignEnv::bottom(&ctx);
        // Values for the formal parameters.
        for Variable { id, ty } in cfg.get_formals() {
            seed.insert(
                *id,
                match ty {
                    Type::Bool => Sign::NonNeg,
                    _ => Sign::Top,
                },
            );
        }

        let mut transfer = TransferLogger::new(unit, SignAnalysis);
        solver.solve(cfg, seed, &ctx, &mut transfer);
        transfer.get_annotations()
    }
}
