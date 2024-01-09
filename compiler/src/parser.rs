use crate::{
    lexer::{TimeUnit, Token, Tokenizer},
    parser_gen::parseP,
    utils::expression::{integer, op_add, op_gt, op_mul, op_sub},
};

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct LetExpression {
    pub(crate) name: String,
    pub(crate) value: Expression,
    pub(crate) scope: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct FnExpression {
    pub(crate) params: Vec<String>,
    pub(crate) body: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Expression {
    Var(String),
    CstI(isize),
    CstF(f64),
    CstB(bool),
    Instant(usize),
    TimeSpan(usize),
    Fn(Box<FnExpression>),
    Let(Box<LetExpression>),
    App(String, Vec<Expression>),
    Le(Box<LessEqualExpression>),
    If(Box<IfExpression>),
    BinaryOperation(Box<BinaryExpression>),
    Logical(Box<LogicalExpression>),
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
    pub(crate) fn new(op: BinaryOperator, left: Expression, right: Expression) -> Self {
        Self { op, left, right }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum LogicalOperator {
    Lt,
    Gt,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct LogicalExpression {
    pub(crate) op: LogicalOperator,
    pub(crate) left: Expression,
    pub(crate) right: Expression,
}

impl LogicalExpression {
    pub(crate) fn new(op: LogicalOperator, left: Expression, right: Expression) -> Self {
        Self { op, left, right }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct LessEqualExpression {
    pub(crate) left: Expression,
    pub(crate) right: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct IfExpression {
    pub(crate) condition: Expression,
    pub(crate) then: Expression,
    pub(crate) other: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct CallExpression {
    pub(crate) callee: String,
    pub(crate) args: Vec<Expression>,
}

fn parse_factor(tokenizer: &mut Tokenizer) -> Expression {
    match tokenizer.token() {
        Token::Id(ident) => {
            let expr = Expression::Var(ident.clone());
            tokenizer.advance();
            expr
        }
        Token::Num(num) => {
            let expr = if num.contains('.') {
                Expression::CstF(num.parse().unwrap())
            } else {
                Expression::CstI(num.parse().unwrap())
            };
            tokenizer.advance();
            expr
        }
        Token::TimeLiteral(num, unit) => {
            let expr = match unit {
                TimeUnit::Timestamp => Expression::Instant(num.parse::<usize>().unwrap()),
                TimeUnit::Day => {
                    Expression::TimeSpan(num.parse::<usize>().unwrap() * 24 * 3600 * 1000)
                }
                TimeUnit::Hour => Expression::TimeSpan(num.parse::<usize>().unwrap() * 3600 * 1000),
                TimeUnit::Second => Expression::TimeSpan(num.parse::<usize>().unwrap() * 1000),
            };
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
                    let expr = Expression::Var(ident.clone());
                    tokenizer.advance();
                    expr
                }
                Token::Num(num) => {
                    let expr = if num.contains('.') {
                        Expression::CstF(num.parse().unwrap())
                    } else {
                        Expression::CstI(num.parse().unwrap())
                    };
                    tokenizer.advance();
                    expr
                }
                Token::TimeLiteral(num, unit) => {
                    let expr = match unit {
                        TimeUnit::Timestamp => Expression::Instant(num.parse::<usize>().unwrap()),
                        TimeUnit::Day => {
                            Expression::TimeSpan(num.parse::<usize>().unwrap() * 24 * 3600 * 1000)
                        }
                        TimeUnit::Hour => {
                            Expression::TimeSpan(num.parse::<usize>().unwrap() * 3600 * 1000)
                        }
                        TimeUnit::Second => {
                            Expression::TimeSpan(num.parse::<usize>().unwrap() * 1000)
                        }
                    };
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
                BinaryExpression::new(BinaryOperator::Sub, Expression::CstI(0), expr).into(),
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
        Token::RParen
        | Token::Plus
        | Token::Minus
        | Token::GreaterThan
        | Token::LessThan
        | Token::Eof => left,
        token => panic!("invalid token: {:#?}", token),
    }
}

fn parse_term(tokenizer: &mut Tokenizer) -> Expression {
    match tokenizer.token() {
        Token::Id(_) | Token::Num(_) | Token::TimeLiteral(_, _) | Token::Minus | Token::LParen => {
            let factor = parse_factor(tokenizer);
            parse_term_(tokenizer, factor)
        }
        token => panic!("invalid token: {:#?}", token),
    }
}

fn parse_logical_(tokenizer: &mut Tokenizer, left: Expression) -> Expression {
    match tokenizer.token() {
        Token::Plus => {
            tokenizer.advance();
            let term = parse_term(tokenizer);
            let expr = BinaryExpression::new(BinaryOperator::Add, left, term);
            parse_logical_(tokenizer, Expression::BinaryOperation(expr.into()))
        }
        Token::Minus => {
            tokenizer.advance();
            let term = parse_term(tokenizer);
            let expr = BinaryExpression::new(BinaryOperator::Sub, left, term);
            parse_logical_(tokenizer, Expression::BinaryOperation(expr.into()))
        }
        Token::RParen | Token::GreaterThan | Token::LessThan | Token::Eof => left,
        token => panic!("invalid token: {:#?}", token),
    }
}

fn parse_logical(tokenizer: &mut Tokenizer) -> Expression {
    match tokenizer.token() {
        Token::Id(_) | Token::Num(_) | Token::TimeLiteral(_, _) | Token::Minus | Token::LParen => {
            let term = parse_term(tokenizer);
            parse_logical_(tokenizer, term)
        }
        token => panic!("invalid token: {:#?}", token),
    }
}

fn parse_expression_(tokenizer: &mut Tokenizer, left: Expression) -> Expression {
    match tokenizer.token() {
        Token::LessThan => {
            tokenizer.advance();
            let term = parse_logical(tokenizer);
            let expr = LogicalExpression::new(LogicalOperator::Lt, left, term);
            parse_expression_(tokenizer, Expression::Logical(expr.into()))
        }
        Token::GreaterThan => {
            tokenizer.advance();
            let term = parse_logical(tokenizer);
            let expr = LogicalExpression::new(LogicalOperator::Gt, left, term);
            parse_expression_(tokenizer, Expression::Logical(expr.into()))
        }
        Token::RParen | Token::Eof => left,
        token => panic!("invalid token: {:#?}", token),
    }
}

fn parse_expression(tokenizer: &mut Tokenizer) -> Expression {
    match tokenizer.token() {
        Token::Id(_) | Token::Num(_) | Token::TimeLiteral(_, _) | Token::Minus | Token::LParen => {
            let term = parse_logical(tokenizer);
            parse_expression_(tokenizer, term)
        }
        token => panic!("invalid token: {:#?}", token),
    }
}

fn parse_program(tokenizer: &mut Tokenizer) -> Expression {
    match tokenizer.token() {
        Token::Id(_) | Token::Num(_) | Token::TimeLiteral(_, _) | Token::Minus | Token::LParen => {
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
    parseP(&mut tokenizer)
}

#[test]
fn test_parser() {
    parse_code("1 + 2 * (3 - 4) > 3");
    assert_eq!(
        parse_code("1 + 2 * (3 - 4) > 3"),
        op_gt(
            op_add(
                integer(1),
                op_mul(integer(2), op_sub(integer(3), integer(4)))
            ),
            integer(3)
        )
    );
    assert_eq!(
        parse_code("1t + 2d"),
        Expression::BinaryOperation(
            BinaryExpression::new(
                BinaryOperator::Add,
                Expression::Instant(1),
                Expression::TimeSpan(2 * 24 * 3600 * 1000)
            )
            .into()
        )
    );
}
