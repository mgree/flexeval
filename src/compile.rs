use crate::ast::{BinOp, Const, Expr, Type, UnOp, VarOp};
use crate::op::{Instruction, Prim, Prog};

impl Expr {
    pub fn compile(&self) -> Prog {
        match self {
            Expr::Var(v) => Prog::from(Instruction::Load(*v)),
            Expr::Const(Const::Number(n)) => {
                Prog::from(vec![Instruction::ConstNum(*n), Instruction::TagNum])
            }
            Expr::Const(Const::Bool(b)) => {
                Prog::from(vec![Instruction::ConstBool(*b), Instruction::TagBool])
            }
            Expr::Const(Const::Null(ty)) => Prog::from(Instruction::ConstNull(*ty)),
            Expr::UnOp(op, expr) => {
                let mut prog = expr.compile();
                prog.extend(op.compile());
                prog
            }
            Expr::BinOp(op, lhs, rhs) => {
                let mut prog = lhs.compile();
                prog.extend(rhs.compile());
                prog.extend(op.compile());
                prog
            }
            Expr::VarOp(op, exprs) => op.compile(exprs),
            Expr::If(e_cond, e_then, e_else) => {
                let mut p_cond = e_cond.compile();
                let p_then = e_then.compile();
                let p_else = e_else.compile();

                // PLAN:
                //
                // p_cond
                // br (p_else.len())
                // p_else
                // jmp (p_then.len())
                // p_then

                let branch = Instruction::Branch(p_else.len());
                let jump = Instruction::Jump(p_then.len());

                p_cond.push(branch);
                p_cond.extend(p_else);
                p_cond.push(jump);
                p_cond.extend(p_then);

                p_cond
            }
        }
    }
}

impl UnOp {
    /// Given an input at the top of the value stack, implement the unary operation
    pub fn compile(&self) -> Prog {
        use Instruction::*;
        match self {
            UnOp::Not => Prog::from(vec![
                CheckBool(4),
                CallPrim(Prim::Not),
                TagBool,
                Jump(5),
                TypeError(Type::Bool),
            ]),
            UnOp::Negate => Prog::from(vec![
                CheckNum(4),
                CallPrim(Prim::Negate),
                TagNum,
                Jump(5),
                TypeError(Type::Bool),
            ]),
            UnOp::IsNull => Prog::from(vec![
                CheckNull(3),
                ConstBool(true),
                Jump(4),
                ConstBool(false),
            ]),
        }
    }
}

impl BinOp {
    /// Given inputs at the top of the value stack (rhs, then lhs), implement the unary operation
    pub fn compile(&self) -> Prog {
        use Instruction::*;
        match self {
            BinOp::Plus => Prog::from(vec![
                CheckNum(5),
                CheckNum(5),
                CallPrim(Prim::Plus),
                TagNum,
                Jump(6),
                TypeError(Type::Number),
            ]),
            BinOp::Minus => Prog::from(vec![
                CheckNum(5),
                CheckNum(5),
                CallPrim(Prim::Minus),
                TagNum,
                Jump(6),
                TypeError(Type::Number),
            ]),
            BinOp::Times => Prog::from(vec![
                CheckNum(5),
                CheckNum(5),
                CallPrim(Prim::Times),
                TagNum,
                Jump(6),
                TypeError(Type::Number),
            ]),
            BinOp::Div => Prog::from(vec![
                CheckNum(5), // TODO check that this is non-zero
                CheckNum(5),
                CallPrim(Prim::Div),
                TagNum,
                Jump(6),
                TypeError(Type::Number),
            ]),
            BinOp::Eq => todo!(),
            BinOp::RecordGet => todo!(),
            BinOp::RecordUnion => todo!(),
            BinOp::RecordContains => todo!(),
        }
    }
}

impl VarOp {
    pub fn compile(&self, _exprs: &[Expr]) -> Prog {
        match self {
            VarOp::And => todo!(),
            VarOp::Or => todo!(),
            VarOp::Coalesce(_) => todo!(),
        }
    }
}
