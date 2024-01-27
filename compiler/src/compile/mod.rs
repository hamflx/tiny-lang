pub(crate) mod assemble;
pub(crate) mod bytecode;
pub(crate) mod llvm;
pub(crate) mod native;

use crate::{
    ast::{BinaryOperator, ComparisonOperator, LogicalOperator},
    resolution::{
        self, make_identifier, make_raw_identifier, AstStatement, BinaryExpression,
        ComparisonExpression, Expr, Identifier, IfExpression, LetExpression, LogicalExpression,
    },
    utils::expression::{app_fn, integer, let_expr, let_fn, op_add, op_mul, op_sub, var},
};

#[derive(Debug, Clone, PartialEq)]
struct Fun {
    ident: Identifier,
    params: Vec<Identifier>,
    body: Vec<resolution::AstStatement>,
}

fn extract_fun(expr: Expr) -> (Expr, Vec<Fun>) {
    match expr {
        Expr::Var(v) => (Expr::Var(v), Vec::new()),
        Expr::CstI(i) => (Expr::CstI(i), Vec::new()),
        Expr::CstF(f) => (Expr::CstF(f), Vec::new()),
        Expr::CstB(b) => (Expr::CstB(b), Vec::new()),
        Expr::StrLiteral(str_lit) => (Expr::StrLiteral(str_lit), Vec::new()),
        Expr::Instant(i) => (Expr::Instant(i), Vec::new()),
        Expr::TimeSpan(t) => (Expr::TimeSpan(t), Vec::new()),
        Expr::Fn(_) => unimplemented!(),
        Expr::Let(l) => match l.value {
            Expr::Fn(f) => {
                let (fn_body, value_fns) = extract_fun(f.body);
                let (main, scope_funs) = extract_fun(l.scope);
                (
                    main,
                    vec![Fun {
                        ident: l.name,
                        params: f.params,
                        body: vec![AstStatement::Expr(fn_body)],
                    }]
                    .into_iter()
                    .chain(value_fns.into_iter())
                    .chain(scope_funs.into_iter())
                    .collect(),
                )
            }
            _ => {
                let (value_body, value_fns) = extract_fun(l.value);
                let (scope_body, scope_funs) = extract_fun(l.scope);
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
        Expr::App(p, args) => {
            let (args, funs) = args.into_iter().fold(
                (Vec::new(), Vec::new()),
                |(mut expr_list, mut fun_list), p| {
                    let (expr, funs) = extract_fun(p);
                    expr_list.push(expr);
                    fun_list.extend(funs);
                    (expr_list, fun_list)
                },
            );
            (Expr::App(p, args), funs)
        }
        Expr::If(e) => {
            let (condition, funs_cond) = extract_fun(e.condition);
            let (then, funs_then) = extract_fun(e.then);
            let (other, funs_other) = extract_fun(e.other);
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
        Expr::BinaryOperation(e) => {
            let (left, funs_left) = extract_fun(e.left);
            let (right, funs_right) = extract_fun(e.right);
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
        Expr::Comparison(e) => {
            let (left, funs_left) = extract_fun(e.left);
            let (right, funs_right) = extract_fun(e.right);
            (
                Expr::Comparison(
                    ComparisonExpression {
                        op: e.op,
                        left,
                        right,
                    }
                    .into(),
                ),
                funs_left.into_iter().chain(funs_right).collect(),
            )
        }
        Expr::Logical(expr) => {
            let (left, funs_left) = extract_fun(expr.left);
            let (right, funs_right) = extract_fun(expr.right);
            (
                Expr::Logical(
                    LogicalExpression {
                        op: expr.op,
                        left,
                        right,
                    }
                    .into(),
                ),
                funs_left.into_iter().chain(funs_right).collect(),
            )
        }
        Expr::Not(expr) => {
            let (expr, funs) = extract_fun(*expr);
            (Expr::Not(expr.into()), funs)
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
    let (expr, _) = resolution::compile(&expr);
    let (expr, funs) = extract_fun(expr);
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
    let (expr, _) = resolution::compile(&expr);
    let (expr, funs) = extract_fun(expr);
    println!("expr: {:#?}", expr);
    println!("funs: {:#?}", funs);
}

#[derive(Debug, Clone)]
enum StackValue {
    Slocal(Identifier),
    Stmp,
}

#[derive(Debug, Clone)]
pub(crate) enum Instruction {
    Label(Identifier),
    Const(isize),
    Add,
    Sub,
    Mul,
    Gt,
    Lt,
    Ge,
    Le,
    And,
    Or,
    Not,
    Var(usize),
    Pop,
    Swap,
    Ldstr(Identifier),
    Bytes(Vec<u8>),
    Call(Identifier, usize),
    SysCall(usize, usize),
    Ret(usize),
    Goto(Identifier),
    IfZero(Identifier),
    Exit,
}

impl Instruction {
    fn size(&self) -> usize {
        match self {
            Instruction::Label(_) => 0,
            Instruction::Const(_)
            | Instruction::Var(_)
            | Instruction::Ret(_)
            | Instruction::IfZero(_)
            | Instruction::Goto(_)
            | Instruction::Ldstr(_) => 2,
            Instruction::Le
            | Instruction::Lt
            | Instruction::Ge
            | Instruction::Gt
            | Instruction::And
            | Instruction::Or
            | Instruction::Not
            | Instruction::Add
            | Instruction::Sub
            | Instruction::Mul
            | Instruction::Pop
            | Instruction::Swap
            | Instruction::Exit => 1,
            Instruction::Call(_, _) | Instruction::SysCall(_, _) => 3,
            Instruction::Bytes(b) => (b.len() + 1 + 3) / 4,
        }
    }
}

fn compile_fn_stmt_list(stmt_list: Vec<AstStatement>, stack: Vec<StackValue>) -> Vec<Instruction> {
    let len = stmt_list.len();
    let (head, _) =
        stmt_list
            .into_iter()
            .fold((Vec::new(), stack), |(mut head, mut stack), stmt| {
                match stmt {
                    AstStatement::Let(let_decl) => {
                        let let_instr = compile_expr(let_decl.value, stack.clone());
                        stack.insert(0, StackValue::Slocal(let_decl.name));
                        head.extend(let_instr);
                    }
                    AstStatement::Expr(expr) => {
                        let instr_list = compile_expr(expr, stack.clone());
                        stack.insert(0, StackValue::Stmp);
                        head.extend(instr_list);
                    }
                }
                (head, stack)
            });
    let pops = (0..len - 1).fold(Vec::new(), |mut pops, _| {
        pops.push(Instruction::Swap);
        pops.push(Instruction::Pop);
        pops
    });
    head.into_iter().chain(pops).collect()
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
        Expr::StrLiteral(i) => vec![Instruction::Ldstr(i)],
        Expr::Instant(_) => todo!(),
        Expr::TimeSpan(_) => todo!(),
        Expr::Let(expr) => {
            let value_instrs = compile_expr(expr.value, stack.clone());
            let scope_instrs = compile_expr(
                expr.scope,
                vec![StackValue::Slocal(expr.name)]
                    .into_iter()
                    .chain(stack)
                    .collect(),
            );
            value_instrs
                .into_iter()
                .chain(scope_instrs)
                .chain([Instruction::Swap, Instruction::Pop])
                .collect()
        }
        Expr::App(ident, expr) => {
            let param_count = expr.len();
            let (instrs, _) = expr
                .into_iter()
                .fold((Vec::new(), stack), |(instrs, stack), arg| {
                    let arg_instrs = compile_expr(arg, stack.clone());
                    (
                        instrs.into_iter().chain(arg_instrs).collect(),
                        [StackValue::Stmp].into_iter().chain(stack).collect(),
                    )
                });
            instrs
                .into_iter()
                .chain([Instruction::Call(ident, param_count)])
                .collect()
        }
        Expr::If(expr) => {
            let cond_instrs = compile_expr(expr.condition, stack.clone());
            let then_instrs = compile_expr(expr.then, stack.clone());
            let other_instrs = compile_expr(expr.other, stack);
            let other_ident = resolution::make_identifier("if_other".to_string());
            let end_ident = resolution::make_identifier("if_end".to_string());
            cond_instrs
                .into_iter()
                .chain([Instruction::IfZero(other_ident.clone())])
                .chain(then_instrs)
                .chain([
                    Instruction::Goto(end_ident.clone()),
                    Instruction::Label(other_ident),
                ])
                .chain(other_instrs)
                .chain([Instruction::Label(end_ident)])
                .collect()
        }
        Expr::BinaryOperation(expr) => {
            let left_instrs = compile_expr(expr.left, stack.clone());
            let right_instrs = compile_expr(
                expr.right,
                [StackValue::Stmp].into_iter().chain(stack).collect(),
            );
            let op_instr = match expr.op {
                BinaryOperator::Add => Instruction::Add,
                BinaryOperator::Sub => Instruction::Sub,
                BinaryOperator::Mul => Instruction::Mul,
                BinaryOperator::Div => todo!(),
            };
            left_instrs
                .into_iter()
                .chain(right_instrs)
                .chain([op_instr])
                .collect()
        }
        Expr::Comparison(expr) => {
            let left_instrs = compile_expr(expr.left, stack.clone());
            let right_instrs = compile_expr(
                expr.right,
                [StackValue::Stmp].into_iter().chain(stack).collect(),
            );
            let op_instr = match expr.op {
                ComparisonOperator::Lt => Instruction::Lt,
                ComparisonOperator::Gt => Instruction::Gt,
                ComparisonOperator::Ge => Instruction::Ge,
                ComparisonOperator::Le => Instruction::Le,
            };
            left_instrs
                .into_iter()
                .chain(right_instrs)
                .chain([op_instr])
                .collect()
        }
        Expr::Logical(expr) => {
            let left_instrs = compile_expr(expr.left, stack.clone());
            let right_instrs = compile_expr(
                expr.right,
                [StackValue::Stmp].into_iter().chain(stack).collect(),
            );
            let op_instr = match expr.op {
                LogicalOperator::And => Instruction::And,
                LogicalOperator::Or => Instruction::Or,
            };
            left_instrs
                .into_iter()
                .chain(right_instrs)
                .chain([op_instr])
                .collect()
        }
        Expr::Not(expr) => {
            let instrs = compile_expr(*expr, stack);
            instrs.into_iter().chain([Instruction::Not]).collect()
        }
        Expr::Fn(_) => unreachable!(),
    }
}

fn compile_fun(fun: Fun) -> Vec<Instruction> {
    let params_len = fun.params.len();
    let stack = fun.params.into_iter().map(|p| StackValue::Slocal(p)).rev();
    let instrs = compile_fn_stmt_list(
        fun.body,
        vec![StackValue::Stmp].into_iter().chain(stack).collect(),
    );
    vec![Instruction::Label(fun.ident)]
        .into_iter()
        .chain(instrs)
        .chain(vec![Instruction::Ret(params_len)])
        .collect()
}

pub(crate) fn build_syscall_stub(name: Identifier, no: usize, len: usize) -> Vec<Instruction> {
    [Instruction::Label(name.clone())]
        .into_iter()
        .chain((0..len).into_iter().map(|_| Instruction::Var(len)))
        .chain([Instruction::SysCall(no, len), Instruction::Ret(len)])
        .collect()
}

pub(crate) fn compile(expr: Expr) -> Vec<Instruction> {
    let (main_expr, fun_list) = extract_fun(expr);
    let main_ident = make_identifier("main".to_string());
    let main_fun = Fun {
        ident: main_ident.clone(),
        body: vec![AstStatement::Expr(main_expr)],
        params: Vec::new(),
    };
    let fun_instrs = [main_fun]
        .into_iter()
        .chain(fun_list)
        .map(|f| compile_fun(f))
        .flatten()
        .collect::<Vec<_>>();
    [Instruction::Call(main_ident, 0), Instruction::Exit]
        .into_iter()
        .chain(fun_instrs)
        .collect()
}

fn extract_prog_fun(items: Vec<resolution::AstDeclaration>) -> Vec<Fun> {
    items
        .into_iter()
        .map(|decl| match decl {
            resolution::AstDeclaration::Fn(fn_decl) => Fun {
                ident: fn_decl.name,
                params: fn_decl.params.into_iter().map(|(ident, _)| ident).collect(),
                body: fn_decl.body,
            },
            resolution::AstDeclaration::Let(_) => todo!(),
        })
        .collect()
}

pub(crate) fn compile_program(prog: resolution::AstProgram) -> Vec<Instruction> {
    let constants: Vec<_> = prog
        .constants
        .into_iter()
        .map(|(ident, cst)| {
            [
                Instruction::Label(ident),
                Instruction::Bytes(cst.into_bytes()),
            ]
        })
        .flatten()
        .collect();
    let fun_list = extract_prog_fun(prog.items);
    let main = fun_list
        .iter()
        .find(|f| f.ident.name == "main")
        .ok_or("No `main`")
        .unwrap();
    let main_ident = main.ident.clone();
    let fun_instrs = fun_list
        .into_iter()
        .map(|f| compile_fun(f))
        .flatten()
        .collect::<Vec<_>>();
    [
        Instruction::Label(make_raw_identifier("_start".to_string())),
        Instruction::Call(main_ident, 0),
        Instruction::Exit,
    ]
    .into_iter()
    .chain(fun_instrs)
    .chain(constants)
    .collect()
}

#[test]
fn test_compile() {
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
    let (expr, _) = resolution::compile(&expr);
    let instrs = compile(expr);
    println!("instrs: {:#?}", instrs);
}
