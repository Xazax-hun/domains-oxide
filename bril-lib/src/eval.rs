use core::fmt::Display;
use std::collections::HashMap;

use analysis::cfg::{CfgBlock, ControlFlowGraph};
use utils::DiagnosticEmitter;

use crate::{
    ir::*,
    lexer::{Identifier, Token, TokenValue},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Value {
    I(i32),
    B(bool),
    Unit,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::I(i) => write!(f, "{i}"),
            Value::B(b) => write!(f, "{b}"),
            Value::Unit => write!(f, "()"),
        }
    }
}

impl Value {
    pub fn as_int(self, tok: Token, diag: &mut DiagnosticEmitter) -> Option<i32> {
        if let Value::I(i) = self {
            Some(i)
        } else {
            tok.error(diag, "Integer expected.");
            None
        }
    }

    pub fn as_bool(self, tok: Token, diag: &mut DiagnosticEmitter) -> Option<bool> {
        if let Value::B(b) = self {
            Some(b)
        } else {
            tok.error(diag, "Bool expected.");
            None
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct Environment {
    locals: HashMap<Identifier, Value>,
}

impl Environment {
    pub fn set_local(&mut self, id: Identifier, val: Value) {
        self.locals.insert(id, val);
    }

    pub fn lookup(
        &self,
        id: Identifier,
        diag: &mut DiagnosticEmitter,
        unit: &Unit,
    ) -> Option<Value> {
        if let Some(&val) = self.locals.get(&id) {
            Some(val)
        } else {
            diag.err_ln(&format!(
                "Undefined value '{}'.",
                unit.identifiers.get_name(id)
            ));
            None
        }
    }
}

pub struct Interpreter<'u> {
    unit: &'u Unit,
    diag: &'u mut DiagnosticEmitter,
    env: Environment,
}

impl<'u> Interpreter<'u> {
    pub fn new(unit: &'u Unit, diag: &'u mut DiagnosticEmitter) -> Self {
        Self {
            unit,
            diag,
            env: Environment::default(),
        }
    }

    pub fn eval_main(&mut self, args: &[Value]) -> Option<Value> {
        let Some(func) = self.unit.identifiers.lookup("@main")
        else {
            self.diag.err_ln("'@main' function not found.");
            return None;
        };
        let main = self.unit.get_function(func)?;
        let main_token = main.get_function_token();
        let formals = main.get_formals();
        if formals.len() != args.len() {
            main_token.error(
                self.diag,
                &format!(
                    "{} arguments expected; '{}' given.",
                    formals.len(),
                    args.len()
                ),
            );
            return None;
        }
        for (formal, val) in formals.iter().zip(args) {
            match formal.ty {
                Type::Int => {
                    val.as_int(main_token, self.diag)?;
                }
                Type::Bool => {
                    val.as_bool(main_token, self.diag)?;
                }
                Type::Void | Type::Fn(_) => panic!("Unexpected type."),
            };
            self.env.set_local(formal.id, *val);
        }

        self.eval_func(main)
    }

    pub fn eval_func(&mut self, cfg: &Cfg) -> Option<Value> {
        for formal in cfg.get_formals() {
            if !self.env.locals.contains_key(&formal.id) {
                cfg.get_function_token().error(
                    self.diag,
                    &format!(
                        "No value set for formal '{}'.",
                        self.unit.identifiers.get_name(formal.id)
                    ),
                );
                return None;
            }
        }

        let mut current_block = 0;
        loop {
            let block = &cfg.blocks()[current_block];
            for op in block.operations() {
                match op {
                    Operation::BinaryOp {
                        token,
                        result,
                        lhs,
                        rhs,
                    } => {
                        let result_val = self.eval_binary_op(*token, *lhs, *rhs)?;
                        self.env.set_local(result.id, result_val);
                    }
                    Operation::UnaryOp {
                        token,
                        result,
                        operand,
                    } => {
                        let result_val = self.eval_unary_op(*token, *operand)?;
                        self.env.set_local(result.id, result_val);
                    }
                    Operation::Jump(_, _) => {
                        assert_eq!(1, block.successors().len());
                        current_block = *block.successors().first()?;
                        break;
                    }
                    Operation::Branch { token, cond, .. } => {
                        assert_eq!(2, block.successors().len());
                        match self.env.lookup(cond.id, self.diag, self.unit)? {
                            Value::B(true) => current_block = block.successors()[0],
                            Value::B(false) => current_block = block.successors()[1],
                            Value::I(_) | Value::Unit => {
                                token.error(
                                    self.diag,
                                    &format!(
                                        "Unexpected value for '{}'.",
                                        self.unit.identifiers.get_name(cond.id)
                                    ),
                                );
                            }
                        }
                        break;
                    }
                    Operation::Call {
                        token,
                        callee,
                        result,
                        args,
                        ..
                    } => {
                        let Some(cfg) = self.unit.get_function(callee.id)
                        else {
                            token.error(self.diag, &format!("Function '{}' not found.", self.unit.identifiers.get_name(callee.id)));
                            return None;
                        };
                        let mut vals = Vec::new();
                        for arg in args {
                            vals.push(self.env.lookup(arg.id, self.diag, self.unit)?);
                        }
                        let mut sub_interp = Interpreter::new(self.unit, self.diag);
                        for (arg, val) in cfg.get_formals().iter().zip(vals) {
                            sub_interp.env.set_local(arg.id, val);
                        }
                        let returned = sub_interp.eval_func(cfg);
                        if let Some(res) = result {
                            let Some(returned_unwrapped) = returned
                            else {
                                token.error(self.diag, "Function failed to return a value.");
                                return None;
                            };
                            self.env.set_local(res.id, returned_unwrapped);
                        }
                    }
                    Operation::Ret(_, res) => {
                        return match res {
                            Some(var) => self.env.lookup(var.id, self.diag, self.unit),
                            None => Some(Value::Unit),
                        };
                    }
                    Operation::Print(_, var) => {
                        let val = self.env.lookup(var.id, self.diag, self.unit)?;
                        self.diag.out(&format!("{val}\n"));
                    }
                    Operation::Nop(_) => continue,
                    Operation::Const(tok, res) => {
                        let val = match tok.value {
                            TokenValue::Integer(i) => Value::I(i),
                            TokenValue::True => Value::B(true),
                            TokenValue::False => Value::B(false),
                            _ => panic!("Unexpected token."),
                        };
                        self.env.set_local(res.id, val);
                    }
                };
            }
        }
    }

    fn eval_binary_op(&mut self, token: Token, lhs: Variable, rhs: Variable) -> Option<Value> {
        let lhs_val = self.env.lookup(lhs.id, self.diag, self.unit)?;
        let rhs_val = self.env.lookup(rhs.id, self.diag, self.unit)?;
        match token.value {
            TokenValue::Add => Some(Value::I(
                lhs_val.as_int(token, self.diag)? + rhs_val.as_int(token, self.diag)?,
            )),
            TokenValue::Mul => Some(Value::I(
                lhs_val.as_int(token, self.diag)? * rhs_val.as_int(token, self.diag)?,
            )),
            TokenValue::Sub => Some(Value::I(
                lhs_val.as_int(token, self.diag)? - rhs_val.as_int(token, self.diag)?,
            )),
            TokenValue::Div => {
                let rhs_val = rhs_val.as_int(token, self.diag)?;
                if rhs_val == 0 {
                    token.error(self.diag, "Division by zero.");
                    return None;
                }
                Some(Value::I(lhs_val.as_int(token, self.diag)? / rhs_val))
            }
            TokenValue::Equal => match lhs_val {
                Value::I(lhs_val) => Some(Value::B(lhs_val == rhs_val.as_int(token, self.diag)?)),
                Value::B(lhs_val) => Some(Value::B(lhs_val == rhs_val.as_bool(token, self.diag)?)),
                _ => {
                    token.error(self.diag, "Unexpected unit.");
                    None
                }
            },
            TokenValue::LessThan => Some(Value::B(
                lhs_val.as_int(token, self.diag)? < rhs_val.as_int(token, self.diag)?,
            )),
            TokenValue::GreaterThan => Some(Value::B(
                lhs_val.as_int(token, self.diag)? > rhs_val.as_int(token, self.diag)?,
            )),
            TokenValue::LessThanOrEq => Some(Value::B(
                lhs_val.as_int(token, self.diag)? <= rhs_val.as_int(token, self.diag)?,
            )),
            TokenValue::GreaterThanOrEq => Some(Value::B(
                lhs_val.as_int(token, self.diag)? >= rhs_val.as_int(token, self.diag)?,
            )),
            TokenValue::And => Some(Value::B(
                lhs_val.as_bool(token, self.diag)? && rhs_val.as_bool(token, self.diag)?,
            )),
            TokenValue::Or => Some(Value::B(
                lhs_val.as_bool(token, self.diag)? || rhs_val.as_bool(token, self.diag)?,
            )),
            _ => {
                token.error(self.diag, "Unexpected binary operator.");
                None
            }
        }
    }

    fn eval_unary_op(&mut self, token: Token, operand: Variable) -> Option<Value> {
        let operand = self.env.lookup(operand.id, self.diag, self.unit)?;
        match token.value {
            TokenValue::Not => Some(Value::B(!operand.as_bool(token, self.diag)?)),
            TokenValue::Identity => Some(operand),
            _ => {
                token.error(self.diag, "Unexpected unary operator.");
                None
            }
        }
    }
}
