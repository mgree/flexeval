use flexeval::ast::{BinOp, Const, Expr};

fn main() {
    let e = Expr::BinOp(
        BinOp::Plus,
        Box::new(Expr::Const(Const::Number(5))),
        Box::new(Expr::Const(Const::Number(7))),
    );

    let env = vec![];
    println!("{e:?} => {:?}", e.eval(&env));
}
