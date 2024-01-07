use crate::{
    parser::BinaryOperator,
    resolution::{self, Expr, Identifier},
    utils::expression::{app_fn, integer, let_expr, let_fn, op_mul, op_sub, var},
    vm::Instruction,
};

#[derive(Debug, Clone, PartialEq)]
struct Fun {
    ident: Identifier,
    params: Vec<Identifier>,
    body: Expr,
}

fn split_fun(expr: Expr) -> (Expr, Vec<Fun>) {
    match expr {
        Expr::Var(v) => (Expr::Var(v), Vec::new()),
        Expr::CstI(i) => (Expr::CstI(i), Vec::new()),
        Expr::CstF(f) => (Expr::CstF(f), Vec::new()),
        Expr::CstB(b) => (Expr::CstB(b), Vec::new()),
        Expr::Instant(i) => (Expr::Instant(i), Vec::new()),
        Expr::TimeSpan(t) => (Expr::TimeSpan(t), Vec::new()),
        Expr::Fn(f) => (Expr::Fn(f), Vec::new()),
        Expr::Let(l) => match l.value {
            Expr::Fn(f) => {
                let (fn_body, value_fns) = split_fun(f.body);
                let (main, scope_funs) = split_fun(l.scope);
                (
                    main,
                    vec![Fun {
                        ident: l.name,
                        params: f.params,
                        body: fn_body,
                    }]
                    .into_iter()
                    .chain(value_fns.into_iter())
                    .chain(scope_funs.into_iter())
                    .collect(),
                )
            }
            _ => (Expr::Let(l), Vec::new()),
        },
        Expr::App(p, b) => (Expr::App(p, b), Vec::new()),
        Expr::Le(e) => (Expr::Le(e), Vec::new()),
        Expr::If(e) => (Expr::If(e), Vec::new()),
        Expr::BinaryOperation(e) => (Expr::BinaryOperation(e), Vec::new()),
    }
}

#[test]
fn test_split_fun() {
    let expr = let_expr(
        "calc",
        let_fn(
            &["discount"],
            let_expr(
                "count",
                integer(3),
                let_expr(
                    "price",
                    integer(5),
                    op_sub(op_mul(var("price"), var("count")), var("discount")),
                ),
            ),
        ),
        app_fn("calc", &[integer(2)]),
    );
    let expr = resolution::compile(&expr);
    let (expr, funs) = split_fun(expr);
    println!("expr: {:#?}", expr);
    println!("funs: {:#?}", funs);
}

fn compile(expr: Expr) -> Vec<Fun> {
    // split_fun(expr)
    todo!()
}
