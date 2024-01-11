mod compile;
mod lexer;
mod parser;
mod parser_gen;
mod resolution;
mod semantic;
mod utils;
mod vm;

use std::collections::HashMap;

use compile::build_syscall_stub;
use parser::{parse_code, BinaryOperator, Expression};
use resolution::{make_identifier, Identifier};
use semantic::{check_expr, solve};
use vm::{SysCall, Vm};

use crate::semantic::{apply_subst, t_arrow, Typ};

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

fn now() -> u32 {
    return 10086;
}

fn get_sys_var(no: u32, typ: u32) -> u32 {
    println!("get sys var: no={no}, typ={typ}");
    return 10010;
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

fn compile_to_byte_code(code: &str) -> Vec<u8> {
    let expr = parse_code(code);
    let now_ident = make_identifier("now".to_string());
    let get_var_ident = make_identifier("get".to_string());
    let age_ident = make_identifier("age".to_string());
    let sys_vars = SysVariableTable {
        rows: vec![SysVariableTableRecord {
            id: age_ident,
            no: 0,
            typ: 1,
        }],
    };
    let expr = resolution::compile_with_env(
        &expr,
        vec![now_ident.clone(), get_var_ident.clone()]
            .into_iter()
            .chain(sys_vars.rows.iter().map(|r| r.id.clone()))
            .collect(),
    );
    let expr = replace_var_with_call(expr, get_var_ident.clone(), &sys_vars);
    let (typ, cs) = check_expr(
        vec![
            (now_ident.clone(), t_arrow(Typ::Int, &[])),
            (
                get_var_ident.clone(),
                t_arrow(Typ::Int, &[Typ::Int, Typ::Int]),
            ),
        ],
        &expr,
    );
    let subst = solve(cs);
    let typ = apply_subst(&typ, &subst);
    println!("{}: {:?}", code, typ);
    let stub: Vec<_> = build_syscall_stub(now_ident, 0, 0)
        .into_iter()
        .chain(build_syscall_stub(get_var_ident, 1, 2))
        .collect();
    let instrs = compile::compile(expr);
    let instrs = instrs.into_iter().chain(stub).collect();
    compile::bytecode::compile(instrs)
}

fn compile_and_run(code: &str) -> isize {
    let bytecode = compile_to_byte_code(code);
    let mut vm = Vm::create(bytecode);
    vm.add_sys_call(SysCall::new(now as *const (), 0));
    vm.add_sys_call(SysCall::new(get_sys_var as *const (), 2));
    vm.start() as isize
}

#[test]
fn test_compile_and_run_now() {
    assert_eq!(compile_and_run("now() + 1"), 10087);
}

#[test]
fn test_compile_and_run_vars() {
    assert_eq!(compile_and_run("age + 1"), 10011);
}

#[test]
fn test_compile_and_run() {
    macro_rules! test_evaluate_code {
        ($($t:tt)*) => {
            assert_eq!(compile_and_run(stringify!($($t)*)), $($t)*);
        };
    }
    test_evaluate_code!(1 + 2 * 3);
    test_evaluate_code!(if 5 > 2 { 1 } else { 0 });
    test_evaluate_code!(if 2 > 2 { 1 } else { 0 });
    test_evaluate_code!(if 2 >= 2 { 1 } else { 0 });
    test_evaluate_code!(if 2 >= 4 { 1 } else { 0 });

    test_evaluate_code!(if 5 < 2 { 1 } else { 0 });
    test_evaluate_code!(if 2 < 2 { 1 } else { 0 });
    test_evaluate_code!(if 2 <= 2 { 1 } else { 0 });
    test_evaluate_code!(if 2 <= 4 { 1 } else { 0 });
    test_evaluate_code!(if 2 < 4 { 1 } else { 0 });

    test_evaluate_code!(if 2 < 4 && 3 > 2 { 1 } else { 0 });
    test_evaluate_code!(if 2 < 4 && 3 > 5 { 1 } else { 0 });

    test_evaluate_code!(if 2 < 4 && 3 > 5 || 5 > 1 { 1 } else { 0 });

    test_evaluate_code!(if 2 < 4 && 3 > 5 || !(5 > 1) { 1 } else { 0 });

    // todo 虚拟机的负数支持。
    // test_evaluate_code!(1 + 2 * -3);
    // test_evaluate_code!(1 + 2 - -3);
    test_evaluate_code!(1 + 2 * 3 - (5 - 2));
    // test_evaluate_code!(1 + 2 * 3 - -(5 - 2));
    // test_evaluate_code!(1 + 2 * 3 / -(5 - 2));
}

fn main() {
    for line in std::io::stdin().lines() {
        let line = line.unwrap();
        let result = compile_and_run(&line);
        println!("={result}");
    }
}
