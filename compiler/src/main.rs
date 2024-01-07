mod compile;
mod lexer;
mod parser;
mod resolution;
mod semantic;
mod utils;
mod vm;

use std::collections::HashMap;

use parser::{parse_code, BinaryOperator, Expression};
use vm::Vm;

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
        Expression::Le(_) => todo!(),
        Expression::If(_) => todo!(),
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

fn compile_to_byte_code(code: &str) -> Vec<u8> {
    let expr = parse_code(code);
    let expr = resolution::compile(&expr);
    let instrs = compile::compile(expr);
    compile::bytecode::compile(instrs)
}

#[test]
fn test_compile_and_run() {
    let bytecode = compile_to_byte_code("1 + 3 + 5");
    let mut vm = Vm::create(bytecode);
    let res = vm.start();
    assert_eq!(res, 9);
}

fn main() {
    for line in std::io::stdin().lines() {
        let line = line.unwrap();
        let result = evaluate_code(&line);
        println!("={result}");
    }
}
