use core::num;

use crate::ast::Type;
use crate::eval::{Env, EvalError, Record, Value};

#[derive(Debug, Clone, Copy)]
pub enum Prim {
    Not,
    Negate,
    Plus,
    Minus,
    Times,
    Div,
    NumEq,
    BoolEq,
    RecordEq,
    RecordGet,
    RecordExtend,
    RecordContains,
}

impl Prim {
    fn run(
        &self,
        vstack: &mut Vec<Value>,
        numstack: &mut Vec<i64>,
        boolstack: &mut Vec<bool>,
        recordstack: &mut Vec<Record>,
    ) {
        match self {
            Prim::Not => {
                let b = boolstack.pop().unwrap();
                boolstack.push(!b);
            }
            Prim::Negate => {
                let n = numstack.pop().unwrap();
                numstack.push(-n);
            }
            Prim::Plus => {
                let lhs = numstack.pop().unwrap();
                let rhs = numstack.pop().unwrap();
                numstack.push(lhs + rhs);
            }
            Prim::Minus => {
                let lhs = numstack.pop().unwrap();
                let rhs = numstack.pop().unwrap();
                numstack.push(lhs - rhs);
            }
            Prim::Times => {
                let lhs = numstack.pop().unwrap();
                let rhs = numstack.pop().unwrap();
                numstack.push(lhs * rhs);
            }
            Prim::Div => {
                let lhs = numstack.pop().unwrap();
                let rhs = numstack.pop().unwrap();
                // TODO how do prims signal errors?
                numstack.push(lhs / rhs);
            }
            Prim::NumEq => {
                let lhs = numstack.pop().unwrap();
                let rhs = numstack.pop().unwrap();
                boolstack.push(lhs == rhs);
            }
            Prim::BoolEq => {
                let lhs = boolstack.pop().unwrap();
                let rhs = boolstack.pop().unwrap();
                boolstack.push(lhs == rhs);
            }
            Prim::RecordEq => {
                let lhs = recordstack.pop().unwrap();
                let rhs = recordstack.pop().unwrap();
                boolstack.push(lhs == rhs);
            }
            Prim::RecordGet => {
                let r = recordstack.pop().unwrap();
                let f = numstack.pop().unwrap();
                // TODO how do prims signal errors?
                vstack.push(r.get(&f).unwrap().clone()) // TODO sucks to clone!
            }
            Prim::RecordExtend => {
                let mut lhs = recordstack.pop().unwrap();
                let rhs = recordstack.pop().unwrap();
                lhs.extend(rhs);
                recordstack.push(lhs);
            }
            Prim::RecordContains => {
                let lhs = vstack.pop().unwrap();
                let rhs = vstack.pop().unwrap();
                boolstack.push(crate::eval::record_contains_record(&lhs, &rhs))
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Instruction {
    /// Pop a number of values off the value stack
    Pop(usize),
    /// Load a variable to the top of the value stack
    Load(usize),
    /// Push a constant on the numeric stack
    ConstNum(i64),
    /// Push a constant on the boolean stack
    ConstBool(bool),
    /// Push a null on the value stack
    ConstNull(Type),
    /// Move a value from the numeric stack to the value stack
    TagNum,
    /// Move a value from the boolean stack to the value stack
    TagBool,
    /// Move a value from the record stack to the value stack
    TagRecord,
    /// Call a primitive
    // TODO this could be specialized to call prims with different conventions
    CallPrim(Prim),
    /// Unconditional jump,
    Jump(usize),
    /// Branch if true
    Branch(usize),
    /// Branch if not numeric; if numeric, move the numeric part to the numeric stack
    CheckNum(usize),
    /// Branch if not bool; if bool, move the bool part to the bool stack
    CheckBool(usize),
    /// Branch if not record; if record, move the record part to the record stack
    CheckRecord(usize),
    /// Branch if not null
    CheckNull(usize),
    /// Signal a type error (giving the expected type)
    TypeError(Type),
}

impl Instruction {
    pub fn remap_jumps(&mut self, delta: usize) {
        match self {
            Instruction::Jump(tgt)
            | Instruction::Branch(tgt)
            | Instruction::CheckNum(tgt)
            | Instruction::CheckBool(tgt)
            | Instruction::CheckRecord(tgt) => *tgt += delta,
            _ => (),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Prog {
    instructions: Vec<Instruction>,
}

impl Prog {
    pub fn new() -> Self {
        Prog {
            instructions: Vec::with_capacity(1024),
        }
    }

    pub fn len(&self) -> usize {
        self.instructions.len()
    }

    pub fn push(&mut self, mut insn: Instruction) {
        insn.remap_jumps(self.instructions.len());
        self.instructions.push(insn);
    }

    // TODO this is kind of a weak-sauce way of handling jumps
    // it would be nicer to:
    //   - use labels and then fill things in
    //   - use an explicit CFG builder (saves us the trouble of reconstructing it)
    //   - use a builder pattern on `Prog` (basically, a mix of the prior two)
    pub fn extend(&mut self, other: Prog) {
        let delta = self.instructions.len();
        self.instructions
            .extend(other.instructions.into_iter().map(|mut i| {
                i.remap_jumps(delta);
                i
            }))
    }

    pub fn run(&self, env: &Env) -> Result<Value, EvalError> {
        let mut vstack: Vec<Value> = Vec::new();
        let mut numstack: Vec<i64> = Vec::new();
        let mut boolstack: Vec<bool> = Vec::new();
        let mut recordstack: Vec<Record> = Vec::new();
        let mut pc = 0;

        while pc != self.instructions.len() {
            match self.instructions[pc] {
                Instruction::Pop(n) => vstack.truncate(vstack.len() - n),
                Instruction::Load(v) => {
                    let value = env.get(v).ok_or_else(|| EvalError::UnboundVariable(v))?;
                    vstack.push(value.clone()); // sucks to clone!
                }
                Instruction::ConstNum(n) => numstack.push(n),
                Instruction::ConstBool(b) => boolstack.push(b),
                Instruction::ConstNull(ty) => vstack.push(Value::Null(ty)),
                Instruction::TagNum => vstack.push(Value::Number(
                    numstack.pop().ok_or_else(|| EvalError::StackUnderflow)?,
                )),
                Instruction::TagBool => vstack.push(Value::Bool(
                    boolstack.pop().ok_or_else(|| EvalError::StackUnderflow)?,
                )),
                Instruction::TagRecord => vstack.push(Value::Record(
                    recordstack.pop().ok_or_else(|| EvalError::StackUnderflow)?,
                )),
                Instruction::CallPrim(prim) => {
                    prim.run(&mut vstack, &mut numstack, &mut boolstack, &mut recordstack)
                }
                Instruction::Jump(tgt) => {
                    pc = tgt;
                    continue;
                }
                Instruction::Branch(tgt) => {
                    if boolstack.pop().ok_or_else(|| EvalError::StackUnderflow)? {
                        pc = tgt;
                        continue;
                    }
                }
                Instruction::CheckNum(tgt) => {
                    let v = vstack.pop().ok_or_else(|| EvalError::StackUnderflow)?;
                    match v {
                        Value::Number(n) => numstack.push(n),
                        _ => {
                            vstack.push(v);
                            pc = tgt;
                            continue;
                        }
                    }
                }
                Instruction::CheckBool(tgt) => {
                    let v = vstack.pop().ok_or_else(|| EvalError::StackUnderflow)?;
                    match v {
                        Value::Bool(b) => boolstack.push(b),
                        _ => {
                            vstack.push(v);
                            pc = tgt;
                            continue;
                        }
                    }
                }
                Instruction::CheckRecord(tgt) => {
                    let v = vstack.pop().ok_or_else(|| EvalError::StackUnderflow)?;
                    match v {
                        Value::Record(r) => recordstack.push(r),
                        _ => {
                            vstack.push(v);
                            pc = tgt;
                            continue;
                        }
                    }
                }
                Instruction::CheckNull(tgt) => {
                    let v = vstack.last().ok_or_else(|| EvalError::StackUnderflow)?;
                    match v {
                        Value::Null(_ty) => (),
                        _ => {
                            pc = tgt;
                            continue;
                        }
                    }
                }
                Instruction::TypeError(expected) => {
                    let value = vstack.pop().ok_or_else(|| EvalError::StackUnderflow)?;
                    let got = Type::from(&value);
                    return Err(EvalError::TypeError {
                        value,
                        expected,
                        got,
                    });
                }
            }

            // control flow instructions should `continue`
            pc += 1;
        }

        assert_eq!(vstack.len(), 1);
        assert!(numstack.is_empty());
        assert!(boolstack.is_empty());
        assert!(recordstack.is_empty());
        vstack.pop().ok_or_else(|| EvalError::StackUnderflow)
    }
}

impl From<Instruction> for Prog {
    fn from(i: Instruction) -> Self {
        Prog {
            instructions: vec![i],
        }
    }
}

impl From<Vec<Instruction>> for Prog {
    fn from(instructions: Vec<Instruction>) -> Self {
        Prog { instructions }
    }
}

impl std::fmt::Display for Prog {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let width = (usize::ilog10(self.instructions.len()) + 1) as usize;

        for (i, insn) in self.instructions.iter().enumerate() {
            writeln!(f, "{i:width$} {insn:?}")?;
        }
        Ok(())
    }
}
