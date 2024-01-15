use std::sync::atomic::{AtomicUsize, Ordering};

use crate::{
    resolution,
    utils::expression::{
        if_expr, integer, let_expr, op_add, op_lt, op_mul, op_sub, time_span, timestamp, var,
    },
};

type Constraints = Vec<(Typ, Typ)>;

type Context = Vec<(resolution::Identifier, Typ)>;

type Substituation = Vec<(usize, Typ)>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Typ {
    Unit,
    Int,
    Bool,
    Instant,
    Duration,
    String,
    Var(usize),
    Record(RecordType),
    Arrow(Box<ArrowType>),
}

impl Typ {
    pub fn arg_len(&self) -> Option<usize> {
        match self {
            Typ::Unit => None,
            Typ::Int => None,
            Typ::Bool => None,
            Typ::Instant => None,
            Typ::Duration => None,
            Typ::String => None,
            Typ::Var(_) => None,
            Typ::Record(_) => None,
            Typ::Arrow(arrow) => Some(arrow.in_typ.len()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct RecordType {
    pub(crate) fields: Vec<(String, Typ)>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ArrowType {
    in_typ: Vec<Typ>,
    out_typ: Typ,
}

pub(crate) fn t_arrow(ret: Typ, args: &[Typ]) -> Typ {
    Typ::Arrow(
        ArrowType {
            in_typ: args.into_iter().cloned().collect(),
            out_typ: ret,
        }
        .into(),
    )
}

static LAST_VAR_ID: AtomicUsize = AtomicUsize::new(0);
fn new_var() -> Typ {
    let id = LAST_VAR_ID.fetch_add(1, Ordering::Relaxed);
    Typ::Var(id)
}

pub(crate) fn solve(cs: Constraints) -> Substituation {
    sovle_recurse(cs, vec![])
}

pub(crate) fn apply_subst(typ: &Typ, subst: &Substituation) -> Typ {
    subst
        .iter()
        .fold(typ.clone(), |typ, (find, replace)| {
            replace_type(&Typ::Var(*find), replace, &typ)
        })
        .clone()
}

pub(crate) fn check_program(ctx: Context, prog: &resolution::AstProgram) -> Constraints {
    let (_, cs) = prog
        .items
        .iter()
        .fold((ctx, vec![]), |(mut ctx, constraints), decl| {
            let (ident, typ, cs) = match decl {
                resolution::AstDeclaration::Fn(resolution::AstFnDeclaration {
                    name,
                    params,
                    body,
                }) => {
                    let param_types: Vec<_> = params.iter().map(|_| new_var()).collect();
                    let ctx = params
                        .iter()
                        .cloned()
                        .zip(param_types.iter().cloned())
                        .chain(ctx.clone())
                        .collect();
                    let (ret_typ, cs) = check_expr(ctx, body);
                    (
                        name,
                        Typ::Arrow(
                            ArrowType {
                                in_typ: param_types,
                                out_typ: ret_typ,
                            }
                            .into(),
                        ),
                        cs,
                    )
                }
                resolution::AstDeclaration::Let(let_decl) => {
                    let (typ, cs) = check_expr(ctx.clone(), &let_decl.value);
                    (&let_decl.name, typ, cs)
                }
            };
            ctx.insert(0, (ident.clone(), typ));
            (ctx, cs.into_iter().chain(constraints).collect())
        });
    cs
}

pub(crate) fn check_expr(ctx: Context, expr: &resolution::Expr) -> (Typ, Constraints) {
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
            let ctx: Vec<_> = [(expr.name.clone(), val_typ)]
                .into_iter()
                .chain(ctx)
                .collect();
            let (scope_typ, scope_cs) = check_expr(ctx, &expr.scope);
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
        resolution::Expr::Fn(expr) => {
            let args: Vec<_> = expr.params.iter().map(|_| new_var()).collect();
            let ctx = expr
                .params
                .iter()
                .zip(&args)
                .map(|(id, typ)| (id.clone(), typ.clone()))
                .chain(ctx)
                .collect();
            let (ret_typ, cs) = check_expr(ctx, &expr.body);
            (
                Typ::Arrow(
                    ArrowType {
                        out_typ: ret_typ,
                        in_typ: args,
                    }
                    .into(),
                ),
                cs,
            )
        }
        resolution::Expr::App(ident, args) => {
            let t = new_var();
            let fn_type = ctx
                .iter()
                .find(|(name, _)| name == ident)
                .unwrap()
                .1
                .clone();
            let (typs, css): (Vec<Typ>, Vec<Constraints>) =
                args.iter().map(|a| check_expr(ctx.clone(), a)).unzip();
            (
                t.clone(),
                [(
                    fn_type,
                    Typ::Arrow(
                        ArrowType {
                            in_typ: typs,
                            out_typ: t,
                        }
                        .into(),
                    ),
                )]
                .into_iter()
                .chain(css.into_iter().flatten())
                .collect(),
            )
        }
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
        resolution::Expr::Comparison(expr) => {
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
        resolution::Expr::Logical(expr) => {
            let (t1, cs1) = check_expr(ctx.clone(), &expr.left);
            let (t2, cs2) = check_expr(ctx, &expr.right);
            (
                Typ::Bool,
                Vec::from([(t1, Typ::Bool), (t2, Typ::Bool)])
                    .into_iter()
                    .chain(cs1.into_iter())
                    .chain(cs2.into_iter())
                    .collect(),
            )
        }
        resolution::Expr::Not(expr) => {
            let (t1, cs1) = check_expr(ctx.clone(), &expr);
            (
                Typ::Bool,
                Vec::from([(t1, Typ::Bool)])
                    .into_iter()
                    .chain(cs1.into_iter())
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
            if occurs(&Typ::Var(x), &t) {
                panic!("occurs")
            }
            let cs = replace_constraints_type(Typ::Var(x), t.clone(), cs);
            sovle_recurse(cs, [(x, t)].into_iter().chain(subst).collect())
        }
        (t1, t2) => panic!("Type do not match {:?} != {:?}", t1, t2),
    }
}

fn type_subst(typ: Typ, subst: Substituation) -> Typ {
    todo!()
}
