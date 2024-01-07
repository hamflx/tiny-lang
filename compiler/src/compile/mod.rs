use crate::{
    parser::BinaryOperator,
    resolution::{self, Identifier},
    utils::expression::{app_fn, integer, let_expr, let_fn, op_add, op_mul, op_sub, var},
};

#[derive(Debug, Clone, PartialEq)]
struct Fun {
    ident: Identifier,
    params: Vec<Identifier>,
    body: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Expr {
    Var(Identifier),
    CstI(isize),
    CstF(f64),
    CstB(bool),
    Instant(usize),
    TimeSpan(usize),
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

fn split_fun(expr: resolution::Expr) -> (Expr, Vec<Fun>) {
    match expr {
        resolution::Expr::Var(v) => (Expr::Var(v), Vec::new()),
        resolution::Expr::CstI(i) => (Expr::CstI(i), Vec::new()),
        resolution::Expr::CstF(f) => (Expr::CstF(f), Vec::new()),
        resolution::Expr::CstB(b) => (Expr::CstB(b), Vec::new()),
        resolution::Expr::Instant(i) => (Expr::Instant(i), Vec::new()),
        resolution::Expr::TimeSpan(t) => (Expr::TimeSpan(t), Vec::new()),
        resolution::Expr::Fn(_) => unimplemented!(),
        resolution::Expr::Let(l) => match l.value {
            resolution::Expr::Fn(f) => {
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
            _ => {
                let (value_body, value_fns) = split_fun(l.value);
                let (scope_body, scope_funs) = split_fun(l.scope);
                (
                    Expr::Let(
                        LetExpression {
                            name: l.name,
                            value: value_body,
                            scope: scope_body,
                        }
                        .into(),
                    ),
                    value_fns
                        .into_iter()
                        .chain(scope_funs.into_iter())
                        .collect(),
                )
            }
        },
        resolution::Expr::App(p, args) => {
            let (args, funs) = args.into_iter().fold(
                (Vec::new(), Vec::new()),
                |(mut expr_list, mut fun_list), p| {
                    let (expr, funs) = split_fun(p);
                    expr_list.push(expr);
                    fun_list.extend(funs);
                    (expr_list, fun_list)
                },
            );
            (Expr::App(p, args), funs)
        }
        resolution::Expr::Le(e) => {
            let (left, funs_left) = split_fun(e.left);
            let (right, funs_right) = split_fun(e.right);
            (
                Expr::Le(LessEqualExpression { left, right }.into()),
                funs_left.into_iter().chain(funs_right).collect(),
            )
        }
        resolution::Expr::If(e) => {
            let (condition, funs_cond) = split_fun(e.condition);
            let (then, funs_then) = split_fun(e.then);
            let (other, funs_other) = split_fun(e.other);
            (
                Expr::If(
                    IfExpression {
                        condition,
                        then,
                        other,
                    }
                    .into(),
                ),
                funs_cond
                    .into_iter()
                    .chain(funs_then)
                    .chain(funs_other)
                    .collect(),
            )
        }
        resolution::Expr::BinaryOperation(e) => {
            let (left, funs_left) = split_fun(e.left);
            let (right, funs_right) = split_fun(e.right);
            (
                Expr::BinaryOperation(
                    BinaryExpression {
                        op: e.op,
                        left,
                        right,
                    }
                    .into(),
                ),
                funs_left.into_iter().chain(funs_right).collect(),
            )
        }
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

#[test]
fn test_split_embed_fun() {
    let expr = let_expr(
        "add",
        let_fn(
            &["a", "b"],
            let_expr(
                "log",
                let_fn(&["n"], var("n")),
                app_fn("log", &[op_add(var("a"), var("b"))]),
            ),
        ),
        app_fn("add", &[integer(1), integer(2)]),
    );
    let expr = resolution::compile(&expr);
    let (expr, funs) = split_fun(expr);
    println!("expr: {:#?}", expr);
    println!("funs: {:#?}", funs);
}

enum StackValue {
    Slocal(Identifier),
    Stmp,
}

#[derive(Debug)]
pub(crate) enum Instruction {
    Label(Identifier),
    Const(isize),
    Add,
    Sub,
    Mul,
    Le,
    Var(usize),
    Pop,
    Swap,
    Call(usize, usize),
    Ret(usize),
    Goto(usize),
    IfZero(usize),
    Exit,
}

fn compile_expr(expr: Expr, stack: Vec<StackValue>) -> Vec<Instruction> {
    match expr {
        Expr::Var(id) => {
            let pos = stack
                .iter()
                .position(|v| match v {
                    StackValue::Slocal(l) => id == *l,
                    StackValue::Stmp => false,
                })
                .unwrap();
            vec![Instruction::Var(pos)]
        }
        Expr::CstI(i) => vec![Instruction::Const(i)],
        Expr::CstF(_) => todo!(),
        Expr::CstB(_) => todo!(),
        Expr::Instant(_) => todo!(),
        Expr::TimeSpan(_) => todo!(),
        Expr::Let(_) => todo!(),
        Expr::App(_, _) => todo!(),
        Expr::Le(_) => todo!(),
        Expr::If(_) => todo!(),
        Expr::BinaryOperation(_) => todo!(),
    }
}

fn compile_fun(fun: Fun) -> Vec<Instruction> {
    let params_len = fun.params.len();
    let stack = fun.params.into_iter().map(|p| StackValue::Slocal(p)).rev();
    let instrs = compile_expr(
        fun.body,
        vec![StackValue::Stmp].into_iter().chain(stack).collect(),
    );
    vec![Instruction::Label(fun.ident)]
        .into_iter()
        .chain(instrs)
        .chain(vec![Instruction::Ret(params_len)])
        .collect()
}
