use std::{collections::HashMap, iter::Peekable, str::Chars};

#[derive(Debug, Clone, PartialEq)]
enum Token {
    Id(String),
    Num(String),
    LParen,
    RParen,
    Plus,
    Minus,
    Mul,
    Div,
    Eof,
}

struct Tokenizer<'c> {
    code: Peekable<Chars<'c>>,
    token: Token,
}

impl<'c> Tokenizer<'c> {
    fn new(code: &'c str) -> Self {
        Self {
            code: code.chars().peekable(),
            token: Token::Eof,
        }
    }

    fn token(&self) -> &Token {
        &self.token
    }

    fn advance(&mut self) {
        while let Some(ch) = self.code.next() {
            match ch {
                '+' => self.token = Token::Plus,
                '-' => self.token = Token::Minus,
                '*' => self.token = Token::Mul,
                '/' => self.token = Token::Div,
                '(' => self.token = Token::LParen,
                ')' => self.token = Token::RParen,
                ch if ch.is_ascii_alphabetic() => {
                    let mut ident = String::from(ch);
                    while let Some(ch) = self
                        .code
                        .next_if(|ch| ch.is_ascii_alphabetic() || ch.is_ascii_alphanumeric())
                    {
                        ident.push(ch);
                    }
                    self.token = Token::Id(ident);
                }
                ch if ch.is_ascii_alphanumeric() => {
                    let mut num_str = String::from(ch);
                    let mut has_dot = false;
                    while let Some(ch) = self.code.next_if(|ch| {
                        if !has_dot && *ch == '.' {
                            has_dot = true;
                            true
                        } else {
                            ch.is_ascii_alphanumeric()
                        }
                    }) {
                        num_str.push(ch);
                    }
                    self.token = Token::Num(num_str);
                }
                ch if ch.is_ascii_whitespace() => continue,
                ch => panic!("invalid token: {ch}"),
            }
            return;
        }
        self.token = Token::Eof;
    }

    fn eat(&mut self, token: Token) {
        if self.token == token {
            self.advance();
        } else {
            panic!("invalid token: {:#?}", self.token);
        }
    }
}

fn to_token_list(code: &str) -> Vec<Token> {
    let mut token_list = Vec::new();
    let mut tokenizer = Tokenizer::new(code);
    tokenizer.advance();
    while *tokenizer.token() != Token::Eof {
        token_list.push(tokenizer.token().clone());
        tokenizer.advance();
    }
    token_list
}

#[test]
fn test_tokenizer() {
    assert_eq!(
        to_token_list("1 + 2 * (3 - 4)"),
        vec![
            Token::Num("1".to_string()),
            Token::Plus,
            Token::Num("2".to_string()),
            Token::Mul,
            Token::LParen,
            Token::Num("3".to_string()),
            Token::Minus,
            Token::Num("4".to_string()),
            Token::RParen,
        ]
    );
}

#[derive(Debug, Clone, PartialEq)]
enum NumberValue {
    Integer(isize),
    Float(f64),
}

#[derive(Debug, Clone, PartialEq)]
enum Expression {
    Id(String),
    Number(NumberValue),
    BinaryOperation(Box<BinaryExpression>),
}

#[derive(Debug, Clone, PartialEq)]
enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone, PartialEq)]
struct BinaryExpression {
    op: BinaryOperator,
    left: Expression,
    right: Expression,
}

impl BinaryExpression {
    fn new(op: BinaryOperator, left: Expression, right: Expression) -> Self {
        Self { op, left, right }
    }
}

fn parse_factor(tokenizer: &mut Tokenizer) -> Expression {
    match tokenizer.token() {
        Token::Id(ident) => {
            let expr = Expression::Id(ident.clone());
            tokenizer.advance();
            expr
        }
        Token::Num(num) => {
            let expr = Expression::Number(if num.contains('.') {
                NumberValue::Float(num.parse().unwrap())
            } else {
                NumberValue::Integer(num.parse().unwrap())
            });
            tokenizer.advance();
            expr
        }
        Token::LParen => {
            tokenizer.eat(Token::LParen);
            let expr = parse_expression(tokenizer);
            tokenizer.eat(Token::RParen);
            expr
        }
        Token::Minus => {
            tokenizer.advance();
            let expr = match tokenizer.token() {
                Token::Id(ident) => {
                    let expr = Expression::Id(ident.clone());
                    tokenizer.advance();
                    expr
                }
                Token::Num(num) => {
                    let expr = Expression::Number(if num.contains('.') {
                        NumberValue::Float(num.parse().unwrap())
                    } else {
                        NumberValue::Integer(num.parse().unwrap())
                    });
                    tokenizer.advance();
                    expr
                }
                Token::LParen => {
                    tokenizer.eat(Token::LParen);
                    let expr = parse_expression(tokenizer);
                    tokenizer.eat(Token::RParen);
                    expr
                }
                token => panic!("invalid token: {:#?}", token),
            };
            Expression::BinaryOperation(
                BinaryExpression::new(
                    BinaryOperator::Sub,
                    Expression::Number(NumberValue::Integer(0)),
                    expr,
                )
                .into(),
            )
        }
        token => panic!("invalid token: {:#?}", token),
    }
}

fn parse_term_(tokenizer: &mut Tokenizer, left: Expression) -> Expression {
    match tokenizer.token() {
        Token::Mul => {
            tokenizer.advance();
            let factor = parse_factor(tokenizer);
            let expr = BinaryExpression::new(BinaryOperator::Mul, left, factor);
            parse_term_(tokenizer, Expression::BinaryOperation(expr.into()))
        }
        Token::Div => {
            tokenizer.advance();
            let factor = parse_factor(tokenizer);
            let expr = BinaryExpression::new(BinaryOperator::Div, left, factor);
            parse_term_(tokenizer, Expression::BinaryOperation(expr.into()))
        }
        Token::RParen | Token::Plus | Token::Minus | Token::Eof => left,
        token => panic!("invalid token: {:#?}", token),
    }
}

fn parse_term(tokenizer: &mut Tokenizer) -> Expression {
    match tokenizer.token() {
        Token::Id(_) | Token::Num(_) | Token::Minus | Token::LParen => {
            let factor = parse_factor(tokenizer);
            parse_term_(tokenizer, factor)
        }
        token => panic!("invalid token: {:#?}", token),
    }
}

fn parse_expression_(tokenizer: &mut Tokenizer, left: Expression) -> Expression {
    match tokenizer.token() {
        Token::Plus => {
            tokenizer.advance();
            let term = parse_term(tokenizer);
            let expr = BinaryExpression::new(BinaryOperator::Add, left, term);
            parse_expression_(tokenizer, Expression::BinaryOperation(expr.into()))
        }
        Token::Minus => {
            tokenizer.advance();
            let term = parse_term(tokenizer);
            let expr = BinaryExpression::new(BinaryOperator::Sub, left, term);
            parse_expression_(tokenizer, Expression::BinaryOperation(expr.into()))
        }
        Token::RParen | Token::Eof => left,
        token => panic!("invalid token: {:#?}", token),
    }
}

fn parse_expression(tokenizer: &mut Tokenizer) -> Expression {
    match tokenizer.token() {
        Token::Id(_) | Token::Num(_) | Token::Minus | Token::LParen => {
            let term = parse_term(tokenizer);
            parse_expression_(tokenizer, term)
        }
        token => panic!("invalid token: {:#?}", token),
    }
}

fn parse_program(tokenizer: &mut Tokenizer) -> Expression {
    match tokenizer.token() {
        Token::Id(_) | Token::Num(_) | Token::Minus | Token::LParen => {
            let expr = parse_expression(tokenizer);
            tokenizer.eat(Token::Eof);
            expr
        }
        token => panic!("invalid token: {:#?}", token),
    }
}

fn parse_code(code: &str) -> Expression {
    let mut tokenizer = Tokenizer::new(code);
    tokenizer.advance();
    parse_program(&mut tokenizer)
}

#[test]
fn test_parser() {
    let mut tokenizer = Tokenizer::new("1 + 2 * (3 - 4)");
    tokenizer.advance();
    assert_eq!(
        parse_program(&mut tokenizer),
        Expression::BinaryOperation(
            BinaryExpression::new(
                BinaryOperator::Add,
                Expression::Number(NumberValue::Integer(1)),
                Expression::BinaryOperation(
                    BinaryExpression::new(
                        BinaryOperator::Mul,
                        Expression::Number(NumberValue::Integer(2)),
                        Expression::BinaryOperation(
                            BinaryExpression::new(
                                BinaryOperator::Sub,
                                Expression::Number(NumberValue::Integer(3)),
                                Expression::Number(NumberValue::Integer(4))
                            )
                            .into()
                        )
                    )
                    .into()
                )
            )
            .into()
        )
    );
}

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
