mod lexer;
mod parser;
mod semantic;

use std::collections::HashMap;

use parser::{parse_code, BinaryOperator, Expression, NumberValue};

fn evaluate(expr: &Expression, env: &HashMap<String, isize>) -> isize {
    match expr {
        Expression::Id(ident) => env[ident],
        Expression::Number(NumberValue::Integer(int)) => *int,
        Expression::Number(NumberValue::Float(_)) => todo!(),
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
        Expression::Instant(_) => todo!(),
        Expression::TimeSpan(_) => todo!(),
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

fn main() {
    for line in std::io::stdin().lines() {
        let line = line.unwrap();
        let result = evaluate_code(&line);
        println!("={result}");
    }
}
