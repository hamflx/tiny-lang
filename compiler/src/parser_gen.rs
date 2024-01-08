use crate::{
    lexer::{Token, Tokenizer},
    parser::Expression,
    utils::expression::{if_expr, integer, op_add, op_div, op_gt, op_lt, op_mul, op_sub, var},
};

#[test]
fn test_parser() {
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
        parse_code("if 3 > 2 { 1 } else {0}"),
        if_expr(op_gt(integer(3), integer(2)), integer(1), integer(0))
    );
}

pub(crate) fn parse_code(code: &str) -> Expression {
    let mut tokenizer = Tokenizer::new(code);
    tokenizer.advance();
    parseP(&mut tokenizer)
}

fn parseP(tokenizer: &mut Tokenizer) -> Expression {
    match tokenizer.token() {
        Token::LParen | Token::Minus | Token::Id(_) | Token::If | Token::Num(_) => {
            let expr = parseE(tokenizer);
            tokenizer.eat(Token::Eof);
            expr
        }
        tok => panic!("invalid token: {:#?}", tok),
    }
}
fn parseE(tokenizer: &mut Tokenizer) -> Expression {
    match tokenizer.token() {
        Token::LParen | Token::Minus | Token::Id(_) | Token::If | Token::Num(_) => {
            let logical = parseB(tokenizer);
            parseE_(tokenizer, logical)
        }
        tok => panic!("invalid token: {:#?}", tok),
    }
}
fn parseE_(tokenizer: &mut Tokenizer, left: Expression) -> Expression {
    match tokenizer.token() {
        Token::Eof | Token::RParen | Token::LBrace | Token::RBrace => left,
        Token::LessThan => {
            tokenizer.eat(Token::LessThan);
            let right = parseB(tokenizer);
            parseE_(tokenizer, op_lt(left, right))
        }
        Token::GreaterThan => {
            tokenizer.eat(Token::GreaterThan);
            let right = parseB(tokenizer);
            parseE_(tokenizer, op_gt(left, right))
        }
        tok => panic!("invalid token: {:#?}", tok),
    }
}
fn parseB(tokenizer: &mut Tokenizer) -> Expression {
    match tokenizer.token() {
        Token::LParen | Token::Minus | Token::Id(_) | Token::If | Token::Num(_) => {
            let left = parseT(tokenizer);
            parseB_(tokenizer, left)
        }
        tok => panic!("invalid token: {:#?}", tok),
    }
}
fn parseB_(tokenizer: &mut Tokenizer, left: Expression) -> Expression {
    match tokenizer.token() {
        Token::Minus => {
            tokenizer.eat(Token::Minus);
            let right = parseT(tokenizer);
            parseB_(tokenizer, op_sub(left, right))
        }
        Token::Eof
        | Token::RParen
        | Token::LBrace
        | Token::RBrace
        | Token::LessThan
        | Token::GreaterThan => left,
        Token::Plus => {
            tokenizer.eat(Token::Plus);
            let right = parseT(tokenizer);
            parseB_(tokenizer, op_add(left, right))
        }
        tok => panic!("invalid token: {:#?}", tok),
    }
}
fn parseT(tokenizer: &mut Tokenizer) -> Expression {
    match tokenizer.token() {
        Token::LParen | Token::Minus | Token::Id(_) | Token::If | Token::Num(_) => {
            let left = parseF(tokenizer);
            parseT_(tokenizer, left)
        }
        tok => panic!("invalid token: {:#?}", tok),
    }
}
fn parseT_(tokenizer: &mut Tokenizer, left: Expression) -> Expression {
    match tokenizer.token() {
        Token::Minus
        | Token::Eof
        | Token::RParen
        | Token::LBrace
        | Token::RBrace
        | Token::LessThan
        | Token::GreaterThan
        | Token::Plus => left,
        Token::Mul => {
            tokenizer.eat(Token::Mul);
            let right = parseF(tokenizer);
            parseT_(tokenizer, op_mul(left, right))
        }
        Token::Div => {
            tokenizer.eat(Token::Div);
            let right = parseF(tokenizer);
            parseT_(tokenizer, op_div(left, right))
        }
        tok => panic!("invalid token: {:#?}", tok),
    }
}
fn parseF(tokenizer: &mut Tokenizer) -> Expression {
    match tokenizer.token() {
        Token::LParen | Token::Id(_) | Token::If | Token::Num(_) => parseN(tokenizer),
        Token::Minus => {
            tokenizer.eat(Token::Minus);
            parseN(tokenizer)
        }
        tok => panic!("invalid token: {:#?}", tok),
    }
}
fn parseN(tokenizer: &mut Tokenizer) -> Expression {
    match tokenizer.token() {
        Token::LParen => {
            tokenizer.eat(Token::LParen);
            let expr = parseE(tokenizer);
            tokenizer.eat(Token::RParen);
            expr
        }
        Token::Id(id) => {
            let expr = var(id);
            tokenizer.advance();
            expr
        }
        Token::If => {
            tokenizer.eat(Token::If);
            let cond = parseE(tokenizer);
            tokenizer.eat(Token::LBrace);
            let then = parseE(tokenizer);
            tokenizer.eat(Token::RBrace);
            tokenizer.eat(Token::Else);
            tokenizer.eat(Token::LBrace);
            let other = parseE(tokenizer);
            tokenizer.eat(Token::RBrace);
            if_expr(cond, then, other)
        }
        Token::Num(num) => {
            let expr = integer(num.parse().unwrap());
            tokenizer.advance();
            expr
        }
        tok => panic!("invalid token: {:#?}", tok),
    }
}
