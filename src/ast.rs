#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Number,
    Bool,
    Record,
}
#[derive(Debug, Clone, Copy)]
pub enum Const {
    Number(i64),
    Bool(bool),
    Null(Type),
}

#[derive(Debug, Clone, Copy)]
pub enum UnOp {
    Not,
    Negate,
    IsNull,
}

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Plus,
    Minus,
    Times,
    Div,
    Eq,
    RecordGet,
    RecordUnion,
    RecordContains,
}

#[derive(Debug, Clone, Copy)]
pub enum VarOp {
    And,
    Or,
    Coalesce(Type),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Var(usize),
    Const(Const),
    UnOp(UnOp, Box<Expr>),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    VarOp(VarOp, Vec<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
}
