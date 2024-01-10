use std::sync::atomic::{AtomicUsize, Ordering};

use crate::{
    parser::Expression,
    resolution,
    utils::expression::{
        if_expr, integer, let_expr, op_add, op_lt, op_mul, op_sub, time_span, timestamp, var,
    },
};

type Constraints = Vec<(Typ, Typ)>;

type Context = Vec<(resolution::Identifier, Typ)>;

type Substituation = Vec<(usize, Typ)>;

#[derive(Debug, Clone, PartialEq, Eq)]
enum Typ {
    Int,
    Bool,
    Instant,
    Duration,
    Var(usize),
    Arrow(Box<ArrowType>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ArrowType {
    in_typ: Vec<Typ>,
    out_typ: Typ,
}

static LAST_VAR_ID: AtomicUsize = AtomicUsize::new(0);
fn new_var() -> Typ {
    let id = LAST_VAR_ID.fetch_add(1, Ordering::Relaxed);
    Typ::Var(id)
}

fn check_expr(ctx: Context, expr: &resolution::Expr) -> (Typ, Constraints) {
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
            let (val_typ, val_cs) = check_expr(ctx.clone(), &expr.value);
            let (scope_typ, scope_cs) = check_expr(
                [(expr.name.clone(), val_typ)]
                    .into_iter()
                    .chain(ctx)
                    .collect(),
                &expr.scope,
            );
            (scope_typ, val_cs.into_iter().chain(scope_cs).collect())
        }
        resolution::Expr::BinaryOperation(expr) => {
            let (t1, c1) = check_expr(ctx.clone(), &expr.left);
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
        resolution::Expr::App(_, _) => todo!(),
        resolution::Expr::If(expr) => {
            let t = new_var();
            let (cond_typ, cond_cs) = check_expr(ctx.clone(), &expr.condition);
            let (then_typ, then_cs) = check_expr(ctx.clone(), &expr.then);
            let (other_typ, other_cs) = check_expr(ctx, &expr.other);
            (
                t.clone(),
                Vec::from([
                    (cond_typ, Typ::Bool),
                    (t.clone(), then_typ),
                    (t.clone(), other_typ),
                ])
                .into_iter()
                .chain(cond_cs)
                .chain(then_cs)
                .chain(other_cs)
                .collect(),
            )
        }
        resolution::Expr::LogicalExpression(expr) => {
            let (t1, cs1) = check_expr(ctx.clone(), &expr.left);
            let (t2, cs2) = check_expr(ctx, &expr.right);
            (
                Typ::Bool,
                Vec::from([(t1, Typ::Int), (t2, Typ::Int)])
                    .into_iter()
                    .chain(cs1.into_iter())
                    .chain(cs2.into_iter())
                    .collect(),
            )
        }
    }
}

#[test]
fn test_check_expr() {
    let expr = if_expr(
        op_lt(
            let_expr(
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
            ),
            integer(100),
        ),
        integer(1),
        integer(2),
    );
    let expr = resolution::compile(&expr);
    let (typ, constraints) = check_expr(Vec::new(), &expr);
    let subst = solve(constraints.clone());
    println!("typ: {:#?}", typ);
    println!("constraints: {:#?}", constraints);
    println!("subst: {:#?}", subst);
}

fn occurs(find: &Typ, typ: &Typ) -> bool {
    if find == typ {
        true
    } else {
        match typ {
            Typ::Arrow(typ) => {
                typ.in_typ.iter().any(|typ| occurs(find, typ)) || occurs(find, &typ.out_typ)
            }
            _ => false,
        }
    }
}

#[test]
fn test_occurs() {
    assert!(occurs(&Typ::Int, &Typ::Int));
    assert!(occurs(&Typ::Var(0), &Typ::Var(0)));
    assert!(occurs(
        &Typ::Var(0),
        &Typ::Arrow(
            ArrowType {
                in_typ: vec![Typ::Var(0)],
                out_typ: Typ::Int
            }
            .into()
        )
    ));
    assert!(occurs(
        &Typ::Var(0),
        &Typ::Arrow(
            ArrowType {
                in_typ: vec![Typ::Int],
                out_typ: Typ::Var(0)
            }
            .into()
        )
    ));

    assert!(!occurs(&Typ::Int, &Typ::Bool));
    assert!(!occurs(&Typ::Var(0), &Typ::Var(1)));
    assert!(!occurs(
        &Typ::Var(0),
        &Typ::Arrow(
            ArrowType {
                in_typ: vec![Typ::Int],
                out_typ: Typ::Int
            }
            .into()
        )
    ));
}

fn replace_type(find: &Typ, replace: &Typ, typ: &Typ) -> Typ {
    if typ == find {
        replace.clone()
    } else {
        match typ {
            Typ::Arrow(typ) => Typ::Arrow(
                ArrowType {
                    in_typ: typ
                        .in_typ
                        .iter()
                        .map(|typ| replace_type(find, replace, typ))
                        .collect(),
                    out_typ: replace_type(find, replace, &typ.out_typ),
                }
                .into(),
            ),
            t => t.clone(),
        }
    }
}

fn replace_constraints_type(find: Typ, replace: Typ, cs: Constraints) -> Constraints {
    cs.into_iter()
        .map(|(t1, t2)| {
            (
                replace_type(&find, &replace, &t1),
                replace_type(&find, &replace, &t2),
            )
        })
        .collect()
}

fn sovle_recurse(mut cs: Constraints, subst: Substituation) -> Substituation {
    if cs.is_empty() {
        return subst;
    }
    let constraint = cs.remove(0);
    match constraint {
        (Typ::Int, Typ::Int) | (Typ::Bool, Typ::Bool) => sovle_recurse(cs, subst),
        (Typ::Arrow(a1), Typ::Arrow(a2)) if a1.in_typ.len() == a2.in_typ.len() => sovle_recurse(
            a1.in_typ
                .into_iter()
                .zip(a2.in_typ)
                .chain([(a1.out_typ, a2.out_typ)])
                .chain(cs)
                .collect(),
            subst,
        ),
        (Typ::Var(x), t) | (t, Typ::Var(x)) => {
            println!("==> subst: {}={:?}", x, t);
            if occurs(&Typ::Var(x), &t) {
                panic!("occurs")
            }
            let cs = replace_constraints_type(Typ::Var(x), t.clone(), cs);
            sovle_recurse(cs, [(x, t)].into_iter().chain(subst).collect())
        }
        (t1, t2) => panic!("Type do not match {:?} != {:?}", t1, t2),
    }
}

fn solve(cs: Constraints) -> Substituation {
    sovle_recurse(cs, vec![])
}

fn type_subst(typ: Typ, subst: Substituation) -> Typ {
    todo!()
}
