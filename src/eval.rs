use std::collections::HashMap;

use crate::ast::{BinOp, Const, Expr, Type, UnOp, VarOp};

type Record = HashMap<i64, Value>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Number(i64),
    Bool(bool),
    Null(Type),
    Record(Record),
}

impl From<&Const> for Value {
    fn from(c: &Const) -> Self {
        match c {
            Const::Number(n) => Value::Number(*n),
            Const::Bool(b) => Value::Bool(*b),
            Const::Null(ty) => Value::Null(*ty),
        }
    }
}

impl From<&Value> for Type {
    fn from(v: &Value) -> Self {
        use Value::*;
        match v {
            Number(_) => Type::Number,
            Bool(_) => Type::Bool,
            Null(ty) => *ty,
            Record(_) => Type::Record,
        }
    }
}

#[derive(Debug, Clone)]
pub enum EvalError {
    UnboundVariable(usize),
    TypeError {
        value: Value,
        expected: Type,
        got: Type,
    },
    DivideByZero,
    NoSuchField {
        record: Record,
        field: i64,
    },
}

impl EvalError {
    fn type_error(value: Value, expected: Type) -> Self {
        let got = Type::from(&value);
        EvalError::TypeError {
            value,
            expected,
            got,
        }
    }
}

pub type Env = Vec<Value>;

impl Expr {
    pub fn eval(&self, env: &Env) -> Result<Value, EvalError> {
        match self {
            Expr::Var(v) => env
                .get(*v)
                .ok_or_else(|| EvalError::UnboundVariable(*v))
                .cloned(),
            Expr::Const(c) => Ok(Value::from(c)),
            Expr::UnOp(op, arg) => op.eval(arg.eval(env)?),
            Expr::BinOp(op, lhs, rhs) => op.eval(lhs.eval(env)?, rhs.eval(env)?),
            Expr::VarOp(op, args) => op.eval(args, env),
            Expr::If(e_cond, e_then, e_else) => match e_cond.eval(env)? {
                Value::Bool(b) => {
                    if b {
                        e_then.eval(env)
                    } else {
                        e_else.eval(env)
                    }
                }
                value => Err(EvalError::type_error(value, Type::Bool)),
            },
        }
    }
}

impl UnOp {
    pub fn eval(&self, arg: Value) -> Result<Value, EvalError> {
        match self {
            UnOp::Not => match arg {
                Value::Bool(b) => Ok(Value::Bool(!b)),
                value => Err(EvalError::type_error(value, Type::Bool)),
            },
            UnOp::Negate => match arg {
                Value::Number(n) => Ok(Value::Number(-n)),
                value => Err(EvalError::type_error(value, Type::Number)),
            },
            UnOp::IsNull => match arg {
                Value::Null(_) => Ok(Value::Bool(true)),
                _ => Ok(Value::Bool(false)),
            },
        }
    }
}

impl BinOp {
    fn eval(&self, lhs: Value, rhs: Value) -> Result<Value, EvalError> {
        match self {
            BinOp::Plus => match (lhs, rhs) {
                (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1 + n2)),
                (Value::Number(_), value) | (value, _) => {
                    Err(EvalError::type_error(value, Type::Number))
                }
            },
            BinOp::Minus => match (lhs, rhs) {
                (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1 - n2)),
                (Value::Number(_), value) | (value, _) => {
                    Err(EvalError::type_error(value, Type::Number))
                }
            },
            BinOp::Times => match (lhs, rhs) {
                (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1 * n2)),
                (Value::Number(_), value) | (value, _) => {
                    Err(EvalError::type_error(value, Type::Number))
                }
            },
            BinOp::Div => match (lhs, rhs) {
                (Value::Number(_), Value::Number(0)) => Err(EvalError::DivideByZero),
                (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1 / n2)),
                (Value::Number(_), value) | (value, _) => {
                    Err(EvalError::type_error(value, Type::Number))
                }
            },
            BinOp::Eq => Ok(Value::Bool(lhs == rhs)),
            BinOp::RecordGet => match (lhs, rhs) {
                (Value::Record(record), Value::Number(field)) => record
                    .get(&field)
                    .ok_or_else(|| EvalError::NoSuchField {
                        record: record.clone(),
                        field,
                    })
                    .cloned(),
                (Value::Record(_), value) => Err(EvalError::type_error(value, Type::Number)),
                (value, _) => Err(EvalError::type_error(value, Type::Record)),
            },
            BinOp::RecordUnion => match (lhs, rhs) {
                (Value::Record(lhs), Value::Record(rhs)) => {
                    let mut map = lhs;
                    map.extend(rhs);
                    Ok(Value::Record(map))
                }
                (Value::Record(_), value) | (value, _) => {
                    Err(EvalError::type_error(value, Type::Record))
                }
            },
            BinOp::RecordContains => Ok(Value::Bool(record_contains_record(&lhs, &rhs))),
        }
    }
}

/// Does the `lhs` contain the `rhs`?
/// Meant to mimic the `@>` jsonb contains operator from PostgreSQL https://www.postgresql.org/docs/current/datatype-json.html#JSON-CONTAINMENT
fn record_contains_record(lhs: &Value, rhs: &Value) -> bool {
    match (lhs, rhs) {
        (Value::Bool(b1), Value::Bool(b2)) => b1 == b2,
        (Value::Number(n1), Value::Number(n2)) => n1 == n2,
        (Value::Record(lhs), Value::Record(rhs)) => rhs.iter().all(|(r_key, r_val)| {
            lhs.iter()
                .any(|(l_key, l_val)| (l_key == r_key) && record_contains_record(l_val, r_val))
        }),
        (Value::Null(_), Value::Null(_)) => false, // these are true nulls, not json nulls. ehhhhh
        (_, _) => false,
    }
}

impl VarOp {
    fn eval(&self, args: &[Expr], env: &Env) -> Result<Value, EvalError> {
        match self {
            VarOp::And => {
                for expr in args {
                    match expr.eval(env)? {
                        Value::Bool(true) => (),
                        value @ Value::Bool(false) => return Ok(value),
                        value => return Err(EvalError::type_error(value, Type::Bool)),
                    }
                }

                return Ok(Value::Bool(true));
            }
            VarOp::Or => {
                for expr in args {
                    match expr.eval(env)? {
                        value @ Value::Bool(true) => return Ok(value),
                        Value::Bool(false) => (),
                        value => return Err(EvalError::type_error(value, Type::Bool)),
                    }
                }

                return Ok(Value::Bool(false));
            }
            VarOp::Coalesce(ty) => {
                for expr in args {
                    match expr.eval(env)? {
                        Value::Null(_) => (),
                        value => return Ok(value),
                    }
                }

                return Ok(Value::Null(*ty));
            }
        }
    }
}
