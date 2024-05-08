use flexeval::ast::{BinOp, Const, Expr, UnOp};
use flexeval::eval::Env;

fn main() {
    let e = Expr::UnOp(
        UnOp::Negate,
        Box::new(Expr::BinOp(
            BinOp::Minus,
            Box::new(Expr::Const(Const::Number(5))),
            Box::new(Expr::Const(Const::Number(7))),
        )),
    );

    let env: Env = vec![];
    println!("{e:?} => {:?}", e.eval(&env));
    let prog = e.compile();
    println!("{prog}");
    let res = prog.run(&env);
    println!("==> {res:?}");
}

#[cfg(test)]
mod test {
    use flexeval::eval::Value;

    use super::*;

    fn same_value(e: Expr, env: Env) {
        let prog = e.compile();
        assert_eq!(e.eval(&env).unwrap(), prog.run(&env).unwrap());
    }

    #[test]
    fn eg_negate_minus() {
        let e = Expr::UnOp(
            UnOp::Negate,
            Box::new(Expr::BinOp(
                BinOp::Minus,
                Box::new(Expr::Const(Const::Number(5))),
                Box::new(Expr::Const(Const::Number(7))),
            )),
        );

        same_value(e, Vec::new());
    }

    #[test]
    fn eg_negate_minus_vars() {
        let e = Expr::UnOp(
            UnOp::Negate,
            Box::new(Expr::BinOp(
                BinOp::Minus,
                Box::new(Expr::Var(0)),
                Box::new(Expr::Var(1)),
            )),
        );

        same_value(e, vec![Value::Number(100), Value::Number(53)]);
    }
}
