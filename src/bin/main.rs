use flexeval::ast::{BinOp, Const, Expr, UnOp};

fn main() {
    let e = Expr::UnOp(
        UnOp::Negate,
        Box::new(Expr::BinOp(
            BinOp::Minus,
            Box::new(Expr::Const(Const::Number(5))),
            Box::new(Expr::Const(Const::Number(7))),
        )),
    );

    let env = vec![];
    println!("{e:?} => {:?}", e.eval(&env));
    let prog = e.compile();
    println!("{prog}");
    let res = prog.run(&env);
    println!("==> {res:?}");
}
