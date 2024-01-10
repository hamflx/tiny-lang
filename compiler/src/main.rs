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
use resolution::make_identifier;
use vm::{SysCall, Vm};

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

fn compile_to_byte_code(code: &str) -> Vec<u8> {
    let expr = parse_code(code);
    let now_ident = make_identifier("now".to_string());
    let expr = resolution::compile_with_env(&expr, vec![now_ident.clone()]);
    let stub = build_syscall_stub(now_ident, 0, 0);
    let instrs = compile::compile(expr);
    let instrs = instrs.into_iter().chain(stub).collect();
    compile::bytecode::compile(instrs)
}

fn compile_and_run(code: &str) -> isize {
    let bytecode = compile_to_byte_code(code);
    let mut vm = Vm::create(bytecode);
    vm.add_sys_call(SysCall::new(now as *const (), 0));
    vm.start() as isize
}

#[test]
fn test_compile_and_run_now() {
    assert_eq!(compile_and_run("now() + 1"), 10087);
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
