use std::sync::atomic::{AtomicUsize, Ordering};

use crate::{
    parser::{BinaryOperator, Expression},
    utils::expression::{app_fn, integer, let_expr, let_fn, op_mul, op_sub, var},
};

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Identifier {
    pub(crate) name: String,
    pub(crate) stamp: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Expr {
    Var(Identifier),
    CstI(isize),
    CstF(f64),
    CstB(bool),
    Instant(usize),
    TimeSpan(usize),
    Fn(Box<FnExpression>),
    Let(Box<LetExpression>),
    App(Identifier, Vec<Expr>),
    Le(Box<LessEqualExpression>),
    If(Box<IfExpression>),
    BinaryOperation(Box<BinaryExpression>),
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct BinaryExpression {
    pub(crate) op: BinaryOperator,
    pub(crate) left: Expr,
    pub(crate) right: Expr,
}

impl BinaryExpression {
    fn new(op: BinaryOperator, left: Expr, right: Expr) -> Self {
        Self { op, left, right }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct LessEqualExpression {
    pub(crate) left: Expr,
    pub(crate) right: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct IfExpression {
    pub(crate) condition: Expr,
    pub(crate) then: Expr,
    pub(crate) other: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct FnExpression {
    pub(crate) params: Vec<Identifier>,
    pub(crate) body: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct LetExpression {
    pub(crate) name: Identifier,
    pub(crate) value: Expr,
    pub(crate) scope: Expr,
}

static LAST_IDENTIFIER_STAMP: AtomicUsize = AtomicUsize::new(0);

fn make_identifier(name: String) -> Identifier {
    let stamp = LAST_IDENTIFIER_STAMP.fetch_add(1, Ordering::Relaxed);
    Identifier { name, stamp }
}

fn compile_impl(expr: &Expression, env: Vec<Identifier>) -> Expr {
    match expr {
        Expression::Var(var_name) => {
            Expr::Var(env.iter().find(|i| i.name == *var_name).unwrap().clone())
        }
        Expression::CstI(i) => Expr::CstI(*i),
        Expression::CstF(f) => Expr::CstF(*f),
        Expression::CstB(b) => Expr::CstB(*b),
        Expression::Instant(i) => Expr::Instant(*i),
        Expression::TimeSpan(s) => Expr::TimeSpan(*s),
        Expression::Let(expr) => {
            let id = make_identifier(expr.name.clone());
            let value = compile_impl(&expr.value, env.clone());
            let env = vec![id.clone()].into_iter().chain(env).collect::<Vec<_>>();
            let scope = compile_impl(&expr.scope, env);
            Expr::Let(
                LetExpression {
                    name: id,
                    value,
                    scope,
                }
                .into(),
            )
        }
        Expression::BinaryOperation(expr) => {
            let left = compile_impl(&expr.left, env.clone());
            let right = compile_impl(&expr.right, env.clone());
            Expr::BinaryOperation(BinaryExpression::new(expr.op.clone(), left, right).into())
        }
        Expression::Fn(expr) => {
            let params: Vec<_> = expr
                .params
                .iter()
                .map(|p| make_identifier(p.clone()))
                .collect();
            let env = params.iter().cloned().chain(env.clone()).collect();
            let body = compile_impl(&expr.body, env);
            Expr::Fn(FnExpression { params, body }.into())
        }
        Expression::App(name, args) => {
            let id = env.iter().find(|i| i.name == *name).unwrap().clone();
            Expr::App(
                id,
                args.iter().map(|e| compile_impl(e, env.clone())).collect(),
            )
        }
        Expression::Le(expr) => Expr::Le(
            LessEqualExpression {
                left: compile_impl(&expr.left, env.clone()),
                right: compile_impl(&expr.right, env.clone()),
            }
            .into(),
        ),
        Expression::If(expr) => Expr::If(
            IfExpression {
                condition: compile_impl(&expr.condition, env.clone()),
                then: compile_impl(&expr.then, env.clone()),
                other: compile_impl(&expr.other, env.clone()),
            }
            .into(),
        ),
    }
}

pub(crate) fn compile(expr: &Expression) -> Expr {
    compile_impl(expr, Vec::new())
}

#[test]
fn test_resolve() {
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
    let expr = compile(&expr);
    println!("expr: {:#?}", expr);
}
