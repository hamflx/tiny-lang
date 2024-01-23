use std::sync::atomic::{AtomicUsize, Ordering};

use crate::{
    ast::{self, BinaryOperator, ComparisonOperator, Expression, LogicalOperator},
    semantic::Typ,
    utils::expression::{app_fn, integer, let_expr, let_fn, op_mul, op_sub, var},
};

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct Identifier {
    pub(crate) name: String,
    pub(crate) stamp: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct AstProgram {
    pub(crate) items: Vec<AstDeclaration>,
    pub(crate) constants: Vec<(Identifier, String)>,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum AstDeclaration {
    Fn(AstFnDeclaration),
    Let(AstLetDeclaration),
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct AstFnDeclaration {
    pub(crate) name: Identifier,
    pub(crate) params: Vec<(Identifier, Typ)>,
    pub(crate) typ: Typ,
    pub(crate) body: Vec<AstStatement>,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct AstLetDeclaration {
    pub(crate) name: Identifier,
    pub(crate) value: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum AstStatement {
    Let(AstLetDeclaration),
    Expr(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Expr {
    Var(Identifier),
    CstI(isize),
    CstF(f64),
    CstB(bool),
    StrLiteral(Identifier),
    Instant(usize),
    TimeSpan(usize),
    Fn(Box<FnExpression>),
    Let(Box<LetExpression>),
    App(Identifier, Vec<Expr>),
    If(Box<IfExpression>),
    BinaryOperation(Box<BinaryExpression>),
    Comparison(Box<ComparisonExpression>),
    Logical(Box<LogicalExpression>),
    Not(Box<Expr>),
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
pub(crate) struct ComparisonExpression {
    pub(crate) op: ComparisonOperator,
    pub(crate) left: Expr,
    pub(crate) right: Expr,
}

impl ComparisonExpression {
    pub(crate) fn new(op: ComparisonOperator, left: Expr, right: Expr) -> Self {
        Self { op, left, right }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct LogicalExpression {
    pub(crate) op: LogicalOperator,
    pub(crate) left: Expr,
    pub(crate) right: Expr,
}

impl LogicalExpression {
    pub(crate) fn new(op: LogicalOperator, left: Expr, right: Expr) -> Self {
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
pub(crate) struct CallExpression {
    pub(crate) callee: Identifier,
    pub(crate) args: Vec<Expr>,
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

pub(crate) fn make_identifier(name: String) -> Identifier {
    let stamp = LAST_IDENTIFIER_STAMP.fetch_add(1, Ordering::Relaxed);
    Identifier { name, stamp }
}

fn compile_impl(expr: &Expression, env: Vec<Identifier>) -> (Expr, Vec<(Identifier, String)>) {
    match expr {
        Expression::Var(var_name) => (
            Expr::Var(
                env.iter()
                    .find(|i| i.name == *var_name)
                    .ok_or_else(|| format!("No variable named {var_name}"))
                    .unwrap()
                    .clone(),
            ),
            Vec::new(),
        ),
        Expression::CstI(i) => (Expr::CstI(*i), Vec::new()),
        Expression::CstF(f) => (Expr::CstF(*f), Vec::new()),
        Expression::CstB(b) => (Expr::CstB(*b), Vec::new()),
        Expression::StrLiteral(str_lit) => {
            let str_lit_id = make_identifier(str_lit.clone());
            (
                Expr::StrLiteral(str_lit_id.clone()),
                vec![(str_lit_id.clone(), str_lit.clone())],
            )
        }
        Expression::Instant(i) => (Expr::Instant(*i), Vec::new()),
        Expression::TimeSpan(s) => (Expr::TimeSpan(*s), Vec::new()),
        Expression::Let(expr) => {
            let id = make_identifier(expr.name.clone());
            let (value, val_cs) = compile_impl(&expr.value, env.clone());
            let env = vec![id.clone()].into_iter().chain(env).collect::<Vec<_>>();
            let (scope, scope_cs) = compile_impl(&expr.scope, env);
            (
                Expr::Let(
                    LetExpression {
                        name: id,
                        value,
                        scope,
                    }
                    .into(),
                ),
                val_cs.into_iter().chain(scope_cs).collect(),
            )
        }
        Expression::BinaryOperation(expr) => {
            let (left, left_cs) = compile_impl(&expr.left, env.clone());
            let (right, right_cs) = compile_impl(&expr.right, env.clone());
            (
                Expr::BinaryOperation(BinaryExpression::new(expr.op.clone(), left, right).into()),
                left_cs.into_iter().chain(right_cs).collect(),
            )
        }
        Expression::Fn(expr) => {
            let params: Vec<_> = expr
                .params
                .iter()
                .map(|p| make_identifier(p.clone()))
                .collect();
            let env = params.iter().cloned().chain(env.clone()).collect();
            let (body, body_cs) = compile_impl(&expr.body, env);
            (Expr::Fn(FnExpression { params, body }.into()), body_cs)
        }
        Expression::App(name, args) => {
            let id = env
                .iter()
                .find(|i| i.name == *name)
                .ok_or_else(|| format!("Identifier {} not found", name))
                .unwrap()
                .clone();
            let (args, cs): (Vec<_>, Vec<_>) =
                args.iter().map(|e| compile_impl(e, env.clone())).unzip();
            (Expr::App(id, args), cs.into_iter().flatten().collect())
        }
        Expression::If(expr) => {
            let (condition, condition_cs) = compile_impl(&expr.condition, env.clone());
            let (then, then_cs) = compile_impl(&expr.then, env.clone());
            let (other, other_cs) = compile_impl(&expr.other, env.clone());
            (
                Expr::If(
                    IfExpression {
                        condition,
                        then,
                        other,
                    }
                    .into(),
                ),
                condition_cs
                    .into_iter()
                    .chain(then_cs)
                    .chain(other_cs)
                    .collect(),
            )
        }
        Expression::Comparison(expr) => {
            let (left, left_cs) = compile_impl(&expr.left, env.clone());
            let (right, right_cs) = compile_impl(&expr.right, env.clone());
            (
                Expr::Comparison(
                    ComparisonExpression {
                        op: expr.op.clone(),
                        left,
                        right,
                    }
                    .into(),
                ),
                left_cs.into_iter().chain(right_cs).collect(),
            )
        }
        Expression::Logical(expr) => {
            let (left, left_cs) = compile_impl(&expr.left, env.clone());
            let (right, right_cs) = compile_impl(&expr.right, env.clone());
            (
                Expr::Logical(
                    LogicalExpression {
                        op: expr.op.clone(),
                        left,
                        right,
                    }
                    .into(),
                ),
                left_cs.into_iter().chain(right_cs).collect(),
            )
        }
        Expression::Not(expr) => {
            let (expr, cs) = compile_impl(expr, env.clone());
            (Expr::Not(expr.into()), cs)
        }
    }
}

pub(crate) fn compile(expr: &Expression) -> (Expr, Vec<(Identifier, String)>) {
    compile_impl(expr, Vec::new())
}

pub(crate) fn compile_with_env(
    expr: &Expression,
    env: Vec<Identifier>,
) -> (Expr, Vec<(Identifier, String)>) {
    compile_impl(expr, env)
}

pub(crate) fn compile_program(prog: &ast::AstProgram, env: Vec<Identifier>) -> AstProgram {
    let (items, constants, _) = prog.items.iter().fold(
        (Vec::<AstDeclaration>::new(), Vec::new(), env),
        |(mut decls, mut constants, mut env), decl| {
            let (item, cs) = compile_declaration(decl, env.clone());
            let decl_name = match &item {
                AstDeclaration::Fn(f) => &f.name,
                AstDeclaration::Let(l) => &l.name,
            };
            env.insert(0, decl_name.clone());
            decls.push(item);
            constants.extend(cs);
            (decls, constants, env)
        },
    );
    AstProgram { items, constants }
}

pub(crate) fn compile_statement_list(
    stmts: &[ast::AstStatement],
    env: Vec<Identifier>,
) -> (Vec<AstStatement>, Vec<(Identifier, String)>) {
    let (_, list, constants) = stmts.iter().fold(
        (env, Vec::new(), Vec::new()),
        |(mut env, mut list, mut constants), stmt| {
            match stmt {
                ast::AstStatement::Let(let_decl) => {
                    let ident = make_identifier(let_decl.name.to_string());
                    let (value, cs) = compile_impl(&let_decl.value, env.clone());
                    list.push(AstStatement::Let(AstLetDeclaration {
                        name: ident.clone(),
                        value,
                    }));
                    constants.extend(cs);
                    env.push(ident);
                }
                ast::AstStatement::Expr(expr) => {
                    let (expr, cs) = compile_impl(&expr, env.clone());
                    constants.extend(cs);
                    list.push(AstStatement::Expr(expr));
                }
            }
            (env, list, constants)
        },
    );
    (list, constants)
}

pub(crate) fn compile_declaration(
    decl: &ast::AstDeclaration,
    env: Vec<Identifier>,
) -> (AstDeclaration, Vec<(Identifier, String)>) {
    match decl {
        ast::AstDeclaration::Fn(fn_decl) => {
            let fn_ident = make_identifier(fn_decl.name.to_string());
            let params: Vec<_> = fn_decl
                .params
                .iter()
                .map(|(p, typ)| (make_identifier(p.to_string()), typ.clone()))
                .collect();
            let env: Vec<_> = [fn_ident.clone()]
                .into_iter()
                .chain(params.iter().map(|(ident, _)| ident.clone()))
                .chain(env)
                .collect();
            let (stmts, cs) = compile_statement_list(&fn_decl.body, env.clone());
            (
                AstDeclaration::Fn(AstFnDeclaration {
                    name: fn_ident.clone(),
                    params,
                    typ: fn_decl.typ.clone(),
                    body: stmts,
                }),
                cs,
            )
        }
        ast::AstDeclaration::Let(let_decl) => {
            let ident = make_identifier(let_decl.name.to_string());
            let (value, cs) = compile_impl(&let_decl.value, env);
            (
                AstDeclaration::Let(AstLetDeclaration { name: ident, value }),
                cs,
            )
        }
    }
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
