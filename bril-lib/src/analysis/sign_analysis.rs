use std::collections::HashSet;

use analysis::{
    cfg::{CfgBlock, ControlFlowGraph, OpPos},
    domains::{Map, MapCtx, SignDomain},
    solvers::SolveMonotone,
};

use crate::{
    ir::{Annotations, Cfg, Operation, Unit},
    lexer::{Identifier, Token, TokenValue},
};

use super::Analysis;

type SignEnv = Map<Identifier, SignDomain>;
type SignCtx = MapCtx<Identifier, SignDomain>;

#[derive(Debug)]
pub struct SignAnalysis;

impl SignAnalysis {
    pub fn transfer(op: &Operation, _cfg: &Cfg, _: &SignCtx, pre_state: &SignEnv) -> SignEnv {
        match op {
            Operation::BinaryOp {
                token,
                result,
                lhs,
                rhs,
            } => {
                let lhs = *pre_state.get(&lhs.id).unwrap_or(&SignDomain::Top);
                let rhs = *pre_state.get(&rhs.id).unwrap_or(&SignDomain::Top);
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

    fn transfer_binary_op(token: Token, lhs: SignDomain, rhs: SignDomain) -> SignDomain {
        match token.value {
            TokenValue::Add => lhs + rhs,
            TokenValue::Mul => lhs * rhs,
            TokenValue::Sub => lhs - rhs,
            TokenValue::Div => lhs / rhs,
            // TODO: more precision for logical ops.
            TokenValue::Equal => SignDomain::NonNeg,
            TokenValue::LessThan => SignDomain::NonNeg,
            TokenValue::GreaterThan => SignDomain::NonNeg,
            TokenValue::LessThanOrEq => SignDomain::NonNeg,
            TokenValue::GreaterThanOrEq => SignDomain::NonNeg,
            TokenValue::And => SignDomain::NonNeg,
            TokenValue::Or => SignDomain::NonNeg,
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

impl Analysis for SignAnalysis {
    fn analyze(&self, cfg: &Cfg, unit: &Unit) -> Annotations {
        let solver = SolveMonotone::default();
        let ctx = MapCtx(HashSet::new(), ());
        let states = solver.transfer_operations(cfg, &ctx, &mut SignAnalysis::transfer);
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
