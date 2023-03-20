use std::{cmp::Ordering, collections::HashSet};

use analysis::{
    cfg::{CfgBlock, ControlFlowGraph, OpPos},
    domains::{JoinSemiLattice, Map, MapCtx, SignDomain},
    solvers::{SolveMonotone, TransferFunction},
};

use crate::{
    ir::{Annotations, Cfg, Operation, Type, Unit, Variable},
    lexer::{Identifier, Token, TokenValue},
};

use super::Analysis;

type SignEnv = Map<Identifier, SignDomain>;
type SignCtx = MapCtx<Identifier, SignDomain>;

#[derive(Debug)]
pub struct SignAnalysis;

impl SignAnalysis {
    fn transfer_binary_op(
        token: Token,
        lhs: SignDomain,
        rhs: SignDomain,
        op_type: Type,
    ) -> SignDomain {
        match token.value {
            TokenValue::Add => lhs + rhs,
            TokenValue::Mul => lhs * rhs,
            TokenValue::Sub => lhs - rhs,
            TokenValue::Div => lhs / rhs,
            TokenValue::Equal => lhs.logical_eq(rhs, op_type == Type::Bool),
            TokenValue::And => lhs.logical_and(rhs),
            TokenValue::Or => lhs.logical_or(rhs),
            TokenValue::LessThan => match lhs.strict_cmp(rhs) {
                Some(Ordering::Less) => SignDomain::Positive,
                Some(_) => SignDomain::Zero,
                _ => SignDomain::NonNeg,
            },
            TokenValue::GreaterThan => match lhs.strict_cmp(rhs) {
                Some(Ordering::Greater) => SignDomain::Positive,
                Some(_) => SignDomain::Zero,
                _ => SignDomain::NonNeg,
            },
            TokenValue::LessThanOrEq => match lhs.weak_cmp(rhs) {
                Some(Ordering::Less) => SignDomain::Positive,
                Some(_) => SignDomain::Zero,
                _ => SignDomain::NonNeg,
            },
            TokenValue::GreaterThanOrEq => match lhs.weak_cmp(rhs) {
                Some(Ordering::Greater) => SignDomain::Positive,
                Some(_) => SignDomain::Zero,
                _ => SignDomain::NonNeg,
            },
            _ => {
                panic!("Unexpected binary operator.")
            }
        }
    }

    fn transfer_unary_op(token: Token, operand: SignDomain) -> SignDomain {
        match token.value {
            TokenValue::Not => SignDomain::NonNeg, // TODO: more precision.
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
        _: &SignCtx,
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
                let lhs = *pre_state.get(&lhs.id).unwrap_or(&SignDomain::Top);
                let rhs = *pre_state.get(&rhs.id).unwrap_or(&SignDomain::Top);
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
                let operand = *pre_state.get(&operand.id).unwrap_or(&SignDomain::Top);
                let result_sign = Self::transfer_unary_op(*token, operand);
                let mut new_state = pre_state.clone();
                new_state.insert(result.id, result_sign);
                new_state
            }
            Operation::Const(token, result) => {
                let mut new_state = pre_state.clone();
                let val = match token.value {
                    TokenValue::Integer(i) => SignDomain::from(i),
                    TokenValue::True => SignDomain::Positive,
                    TokenValue::False => SignDomain::Zero,
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
        _: &SignCtx,
        pre_state: &SignEnv,
    ) -> Option<SignEnv> {
        let is_true_branch = *cfg.blocks()[from].successors().first().unwrap() == to;
        let last_op = cfg.blocks()[from].operations().last().unwrap();
        match last_op {
            Operation::Branch { cond, .. } => {
                match *pre_state.get(&cond.id).unwrap_or(&SignDomain::Top) {
                    SignDomain::Zero if is_true_branch => None,
                    SignDomain::Positive if !is_true_branch => None,
                    _ => Some(pre_state.clone()),
                }
            }
            _ => Some(pre_state.clone()),
        }
    }
}

impl Analysis for SignAnalysis {
    fn analyze(&self, cfg: &Cfg, unit: &Unit) -> Annotations {
        let solver = SolveMonotone::default();

        let ctx = MapCtx(HashSet::new(), ());
        let mut seed = SignEnv::bottom(&ctx);
        // Values for the formal parameters.
        for Variable { id, ty } in cfg.get_formals() {
            seed.insert(
                *id,
                match ty {
                    Type::Bool => SignDomain::NonNeg,
                    _ => SignDomain::Top,
                },
            );
        }

        let states = solver.solve(cfg, seed, &ctx, &mut SignAnalysis);
        let mut anns = Annotations::default();
        // TODO: collect annotations for each operation?
        //       should annotations for operations be incremental? (Only emit changed values.)
        for (block_id, state) in states.iter().enumerate() {
            let block = &cfg.blocks()[block_id];
            let pos = OpPos {
                block_id,
                op_id: block.operations().len() - 1,
            };
            let printed: Vec<_> = state
                .iter()
                .map(|(id, val)| format!("{}: {:?}", unit.identifiers.get_name(*id), val))
                .collect();
            anns.post.insert(pos, printed);
        }
        anns
    }
}
