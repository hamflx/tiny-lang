mod ast;
mod compile;
mod lexer;
mod parser;
mod resolution;
mod semantic;
mod utils;
mod vm;

use std::{
    collections::HashMap,
    time::{SystemTime, UNIX_EPOCH},
};

use ast::{BinaryOperator, Expression};
use compile::build_syscall_stub;
use parser::parse_code;
use resolution::{make_identifier, Identifier};
use semantic::{check_expr, solve};
use vm::{CallContext, SysCall, Vm};

use crate::{
    compile::native::run_code_native,
    semantic::{apply_subst, t_arrow, Typ},
};

fn evaluate(expr: &Expression, env: &HashMap<String, isize>) -> isize {
    match expr {
        Expression::Var(ident) => env[ident],
        Expression::CstI(int) => *int,
        Expression::CstF(_) => todo!(),
        Expression::CstB(_) => todo!(),
        Expression::BinaryOperation(expr) => {
            let left = evaluate(&expr.left, env);
            let right = evaluate(&expr.right, env);
            match expr.op {
                BinaryOperator::Add => left + right,
                BinaryOperator::Sub => left - right,
                BinaryOperator::Mul => left * right,
                BinaryOperator::Div => left / right,
            }
        }
        Expression::Let(_) => todo!(),
        Expression::Instant(_) => todo!(),
        Expression::TimeSpan(_) => todo!(),
        Expression::Fn(_) => todo!(),
        Expression::App(_, _) => todo!(),
        Expression::If(_) => todo!(),
        Expression::Comparison(_) => todo!(),
        Expression::Logical(_) => todo!(),
        Expression::Not(_) => todo!(),
    }
}

fn evaluate_code(code: &str) -> isize {
    let expr = parse_code(&code);
    evaluate(&expr, &HashMap::new())
}

#[test]
fn test_evalute() {
    macro_rules! test_evaluate_code {
        ($($t:tt)*) => {
            assert_eq!(evaluate_code(stringify!($($t)*)), $($t)*);
        };
    }
    test_evaluate_code!(1 + 2 * 3);
    test_evaluate_code!(1 + 2 * -3);
    test_evaluate_code!(1 + 2 - -3);
    test_evaluate_code!(1 + 2 * 3 - (5 - 2));
    test_evaluate_code!(1 + 2 * 3 - -(5 - 2));
    test_evaluate_code!(1 + 2 * 3 / -(5 - 2));
    test_evaluate_code!(2 / 3);
    test_evaluate_code!(8 / 3);
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct SysVariableTable {
    rows: Vec<SysVariableTableRecord>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct SysVariableTableRecord {
    id: Identifier,
    no: usize,
    typ: usize,
}

struct SysCallTable {
    rows: Vec<SysCallTableRecord>,
}

struct SysCallTableRecord {
    ptr: *const (),
    id: Identifier,
    typ: Typ,
    ctx: Option<Box<dyn CallContext>>,
}

fn replace_var_with_call(
    expr: resolution::Expr,
    get_ident: Identifier,
    table: &SysVariableTable,
) -> resolution::Expr {
    match expr {
        resolution::Expr::Var(ident) => {
            let record = table.rows.iter().find(|r| r.id == ident).unwrap();
            let no = resolution::Expr::CstI(record.no as _);
            let typ = resolution::Expr::CstI(record.typ as _);
            let args = vec![no, typ];
            resolution::Expr::App(get_ident.clone(), args)
        }
        resolution::Expr::Fn(expr) => resolution::Expr::Fn(
            resolution::FnExpression {
                params: expr.params,
                body: replace_var_with_call(expr.body, get_ident, table),
            }
            .into(),
        ),
        resolution::Expr::Let(expr) => resolution::Expr::Let(
            resolution::LetExpression {
                name: expr.name,
                value: replace_var_with_call(expr.value, get_ident.clone(), table),
                scope: replace_var_with_call(expr.scope, get_ident.clone(), table),
            }
            .into(),
        ),
        resolution::Expr::App(ident, args) => resolution::Expr::App(
            ident,
            args.into_iter()
                .map(|p| replace_var_with_call(p, get_ident.clone(), table))
                .collect(),
        ),
        resolution::Expr::If(expr) => resolution::Expr::If(
            resolution::IfExpression {
                condition: replace_var_with_call(expr.condition, get_ident.clone(), table),
                then: replace_var_with_call(expr.then, get_ident.clone(), table),
                other: replace_var_with_call(expr.other, get_ident.clone(), table),
            }
            .into(),
        ),
        resolution::Expr::BinaryOperation(expr) => resolution::Expr::BinaryOperation(
            resolution::BinaryExpression {
                op: expr.op,
                left: replace_var_with_call(expr.left, get_ident.clone(), table),
                right: replace_var_with_call(expr.right, get_ident.clone(), table),
            }
            .into(),
        ),
        resolution::Expr::Comparison(expr) => resolution::Expr::Comparison(
            resolution::ComparisonExpression {
                op: expr.op,
                left: replace_var_with_call(expr.left, get_ident.clone(), table),
                right: replace_var_with_call(expr.right, get_ident.clone(), table),
            }
            .into(),
        ),
        resolution::Expr::Logical(expr) => resolution::Expr::Logical(
            resolution::LogicalExpression {
                op: expr.op,
                left: replace_var_with_call(expr.left, get_ident.clone(), table),
                right: replace_var_with_call(expr.right, get_ident.clone(), table),
            }
            .into(),
        ),
        resolution::Expr::Not(expr) => {
            resolution::Expr::Not(replace_var_with_call(*expr, get_ident.clone(), table).into())
        }
        expr => expr,
    }
}

fn compile_to_byte_code(
    code: &str,
    sys_calls: &SysCallTable,
    sys_vars: &SysVariableTable,
    get_index: usize,
) -> Vec<u8> {
    let expr = parse_code(code);
    let get_var_record = &sys_calls.rows[get_index];
    let expr = resolution::compile_with_env(
        &expr,
        sys_calls
            .rows
            .iter()
            .map(|r| r.id.clone())
            .chain(sys_vars.rows.iter().map(|r| r.id.clone()))
            .collect(),
    );
    let expr = replace_var_with_call(expr, get_var_record.id.clone(), &sys_vars);
    let (typ, cs) = check_expr(
        sys_calls
            .rows
            .iter()
            .map(|r| (r.id.clone(), r.typ.clone()))
            .collect(),
        &expr,
    );
    let subst = solve(cs);
    let typ = apply_subst(&typ, &subst);
    println!("{}: {:?}", code, typ);
    let stub: Vec<_> = sys_calls
        .rows
        .iter()
        .enumerate()
        .map(|(i, r)| build_syscall_stub(r.id.clone(), i, r.typ.arg_len().unwrap()))
        .flatten()
        .collect();
    let instrs = compile::compile(expr);
    let instrs = instrs.into_iter().chain(stub).collect();
    compile::bytecode::compile(instrs)
}

struct SysCallGetContext {
    get: Box<dyn Fn(u32, u32) -> u32>,
}

impl CallContext for SysCallGetContext {}

impl std::fmt::Debug for SysCallGetContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SysCallGetContext")
            .field("get", &"Fn(u32, u32) -> u32")
            .finish()
    }
}

fn build_default_sys_calls(get_var: impl Fn(u32, u32) -> u32 + 'static) -> SysCallTable {
    fn now(_: &SysCall) -> u32 {
        return SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs() as _;
    }

    fn get_sys_var(sys_call: &SysCall, no: u32, typ: u32) -> u32 {
        println!("get sys var: no={no}, typ={typ}");
        let ctx = sys_call.ctx.as_ref().unwrap().as_ref();
        let ctx: &dyn std::any::Any = ctx.as_any();
        let ctx = ctx.downcast_ref::<SysCallGetContext>().unwrap();
        return (ctx.get)(no, typ);
    }

    SysCallTable {
        rows: vec![
            SysCallTableRecord {
                ptr: get_sys_var as *const (),
                id: make_identifier("get".to_string()),
                typ: t_arrow(Typ::Int, &[Typ::Int, Typ::Int]),
                ctx: Some(Box::new(SysCallGetContext {
                    get: Box::new(get_var),
                })),
            },
            SysCallTableRecord {
                ptr: now as *const (),
                id: make_identifier("now".to_string()),
                typ: t_arrow(Typ::Int, &[]),
                ctx: None,
            },
        ],
    }
}

fn compile_and_run(code: &str) -> isize {
    compile_and_run_with_vars(code, &[])
}

fn compile_and_run_with_vars(code: &str, vars: &[(&str, u32)]) -> isize {
    let get_var = {
        let vars: Vec<_> = vars.iter().map(|(s, v)| (s.to_string(), *v)).collect();
        move |no: u32, typ: u32| vars[no as usize].1
    };
    let sys_calls = build_default_sys_calls(get_var);
    let rows = vars
        .iter()
        .enumerate()
        .map(|(i, (s, v))| SysVariableTableRecord {
            id: make_identifier(s.to_string()),
            no: i,
            typ: 1,
        })
        .collect();
    let bytecode = compile_to_byte_code(code, &sys_calls, &SysVariableTable { rows }, 0);
    let mut vm = Vm::create(bytecode);
    for SysCallTableRecord { ptr, typ, ctx, .. } in sys_calls.rows {
        vm.add_sys_call(SysCall::new(ptr, typ.arg_len().unwrap(), ctx));
    }
    vm.start() as isize
}

#[test]
fn test_compile_and_run_now() {
    assert!(
        (compile_and_run("now() + 1")
            - SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .as_secs() as isize)
            .abs()
            < 2
    );
}

#[test]
fn test_compile_and_run_vars() {
    assert_eq!(
        compile_and_run_with_vars("age + 1", &[("age", 10010)]),
        10011
    );
}

#[test]
fn test_compile_and_run_fn() {
    assert_eq!(compile_and_run("fn hello() { 1 + 1 } hello()"), 2);
}

#[test]
fn test_compile_and_run() {
    macro_rules! run {
        ($($t:tt)*) => {
            assert_eq!(compile_and_run(stringify!($($t)*)), $($t)*);
        };
    }
    run!(1 + 2 * 3);
    run!(if 5 > 2 { 1 } else { 0 });
    run!(if 2 > 2 { 1 } else { 0 });
    run!(if 2 >= 2 { 1 } else { 0 });
    run!(if 2 >= 4 { 1 } else { 0 });

    run!(if 5 < 2 { 1 } else { 0 });
    run!(if 2 < 2 { 1 } else { 0 });
    run!(if 2 <= 2 { 1 } else { 0 });
    run!(if 2 <= 4 { 1 } else { 0 });
    run!(if 2 < 4 { 1 } else { 0 });

    run!(if 2 < 4 && 3 > 2 { 1 } else { 0 });
    run!(if 2 < 4 && 3 > 5 { 1 } else { 0 });

    run!(if 2 < 4 && 3 > 5 || 5 > 1 { 1 } else { 0 });

    run!(if 2 < 4 && 3 > 5 || !(5 > 1) { 1 } else { 0 });

    // todo 虚拟机的负数支持。
    // run!(1 + 2 * -3);
    // run!(1 + 2 - -3);
    run!(1 + 2 * 3 - (5 - 2));
    // run!(1 + 2 * 3 - -(5 - 2));
    // run!(1 + 2 * 3 / -(5 - 2));
}

#[test]
fn test_run_native() {
    macro_rules! run {
        ($($t:tt)*) => {
            assert_eq!(run_code_native(stringify!($($t)*)), $($t)*);
        };
    }
    run!(1 + 2 * 3);
    run!(if 5 > 2 { 1 } else { 0 });
    run!(if 2 > 2 { 1 } else { 0 });
    run!(if 2 >= 2 { 1 } else { 0 });
    run!(if 2 >= 4 { 1 } else { 0 });

    run!(if 5 < 2 { 1 } else { 0 });
    run!(if 2 < 2 { 1 } else { 0 });
    run!(if 2 <= 2 { 1 } else { 0 });
    run!(if 2 <= 4 { 1 } else { 0 });
    run!(if 2 < 4 { 1 } else { 0 });

    run!(if 2 < 4 && 3 > 2 { 1 } else { 0 });
    run!(if 2 < 4 && 3 > 5 { 1 } else { 0 });

    run!(if 2 < 4 && 3 > 5 || 5 > 1 { 1 } else { 0 });

    run!(if 2 < 4 && 3 > 5 || !(5 > 1) { 1 } else { 0 });

    run!(1 + 2 * 3 - (5 - 2));
}

fn main() {
    for line in std::io::stdin().lines() {
        let line = line.unwrap();
        let result = compile_and_run(&line);
        println!("Run on VM = {result}");
        let result = run_code_native(&line);
        println!("Run native = {result}");
    }
}
