use std::sync::atomic::{AtomicUsize, Ordering};

use crate::{
    parser::Expression,
    utils::expression::{integer, op_add, time_span, timestamp},
};

type Constraints = Vec<(Typ, Typ)>;

type Context = Vec<(String, Typ)>;

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

fn check_expr(ctx: &Context, expr: &Expression) -> (Typ, Constraints) {
    match expr {
        Expression::Var(id) => (
            ctx.iter().find(|(name, _)| name == id).unwrap().1.clone(),
            Vec::new(),
        ),
        Expression::CstI(_) => (Typ::Int, Vec::new()),
        Expression::CstF(_) => todo!(),
        Expression::CstB(_) => todo!(),
        Expression::Instant(_) => (Typ::Instant, Vec::new()),
        Expression::TimeSpan(_) => (Typ::Duration, Vec::new()),
        Expression::BinaryOperation(expr) => {
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
        Expression::Let(_) => todo!(),
    }
}

#[test]
fn test_check_expr() {
    // let ctx: Context = vec![("time_add".to_string(), Typ::Arrow())];
    // let (typ, constraints) = check_expr(&Vec::new(), &op_add(integer(1), integer(2)));
    // let (typ, constraints) = check_expr(&Vec::new(), &op_add(timestamp(1), time_span(1)));
    // println!("typ: {:#?}", typ);
    // println!("constraints: {:#?}", constraints);
}

fn solve(cs: Constraints) -> Substituation {
    todo!()
}

fn type_subst(typ: Typ, subst: Substituation) -> Typ {
    todo!()
}
