use crate::lexer::{Token, Tokenizer};

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum NumberValue {
    Integer(isize),
    Float(f64),
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Expression {
    Id(String),
    Number(NumberValue),
    BinaryOperation(Box<BinaryExpression>),
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct BinaryExpression {
    pub(crate) op: BinaryOperator,
    pub(crate) left: Expression,
    pub(crate) right: Expression,
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

pub(crate) fn parse_code(code: &str) -> Expression {
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
