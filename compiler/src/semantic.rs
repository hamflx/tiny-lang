use std::sync::atomic::{AtomicUsize, Ordering};

use crate::{
    parser::Expression,
    resolution,
    utils::expression::{integer, let_expr, op_add, op_mul, op_sub, time_span, timestamp, var},
};

type Constraints = Vec<(Typ, Typ)>;

type Context = Vec<(resolution::Identifier, Typ)>;

type Substituation = Vec<(String, Typ)>;

#[derive(Debug, Clone)]
enum Typ {
    Int,
    Bool,
    Instant,
    Duration,
    Var(usize),
    Arrow(Box<ArrowType>),
}

#[derive(Debug, Clone)]
struct ArrowType {
    in_typ: Typ,
    out_typ: Typ,
}

static LAST_VAR_ID: AtomicUsize = AtomicUsize::new(0);
fn new_var() -> Typ {
    let id = LAST_VAR_ID.fetch_add(1, Ordering::Relaxed);
    Typ::Var(id)
}

fn check_expr(ctx: &Context, expr: &resolution::Expr) -> (Typ, Constraints) {
    match expr {
        resolution::Expr::Var(id) => (
            ctx.iter().find(|(name, _)| name == id).unwrap().1.clone(),
            Vec::new(),
        ),
        resolution::Expr::CstI(_) => (Typ::Int, Vec::new()),
        resolution::Expr::CstF(_) => todo!(),
        resolution::Expr::CstB(_) => todo!(),
        resolution::Expr::Instant(_) => (Typ::Instant, Vec::new()),
        resolution::Expr::TimeSpan(_) => (Typ::Duration, Vec::new()),
        resolution::Expr::Let(expr) => {
            check_expr(ctx, &expr.value);
            check_expr(ctx, &expr.scope);
            todo!();
        }
        resolution::Expr::BinaryOperation(expr) => {
            let (t1, c1) = check_expr(ctx, &expr.left);
            let (t2, c2) = check_expr(ctx, &expr.right);
            (
                Typ::Int,
                Vec::from([(t1, t2)])
                    .into_iter()
                    .chain(c1.into_iter())
                    .chain(c2.into_iter())
                    .collect(),
            )
        }
        resolution::Expr::Fn(_) => todo!(),
    }
}

#[test]
fn test_check_expr() {
    let expr = let_expr(
        "count",
        integer(3),
        let_expr(
            "price",
            integer(5),
            let_expr(
                "discount",
                integer(1),
                op_sub(op_mul(var("price"), var("count")), var("discount")),
            ),
        ),
    );
    let expr = resolution::compile(&expr);
    let (typ, constraints) = check_expr(&Vec::new(), &expr);
    println!("expr: {:#?}", expr);
    println!("typ: {:#?}", typ);
    println!("constraints: {:#?}", constraints);
}

fn solve(cs: Constraints) -> Substituation {
    todo!()
}

fn type_subst(typ: Typ, subst: Substituation) -> Typ {
    todo!()
}
