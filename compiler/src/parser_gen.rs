use crate::{
    lexer::{Token, Tokenizer},
    parser::Expression,
    utils::expression::{
        call_expr, if_expr, integer, op_add, op_and, op_div, op_ge, op_gt, op_le, op_lt, op_mul,
        op_not, op_or, op_sub, var,
    },
};

#[test]
fn test_parser() {
    assert_eq!(
        parse_code("1 + 2 * -3"),
        op_add(
            integer(1),
            op_mul(integer(2), op_sub(integer(0), integer(3)))
        )
    );
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
    assert_eq!(
        parse_code("5 + if 3 > 2 { 1 } else {0}"),
        op_add(
            integer(5),
            if_expr(op_gt(integer(3), integer(2)), integer(1), integer(0))
        )
    );
    assert_eq!(
        parse_code("log(now() + 5)"),
        call_expr(
            "log".into(),
            vec![op_add(call_expr("now".into(), vec![]), integer(5))]
        )
    );
}

pub(crate) fn parse_code(code: &str) -> Expression {
    let mut tokenizer = Tokenizer::new(code);
    tokenizer.advance();
    parseP(&mut tokenizer)
}

pub(crate) fn parseP(tokenizer: &mut Tokenizer) -> Expression {
    match tokenizer.token() {
        Token::Not | Token::LParen | Token::Minus | Token::Id(_) | Token::If | Token::Num(_) => {
            let expr = parseE(tokenizer);
            tokenizer.eat(Token::Eof);
            expr
        }
        tok => panic!("expected `Token::Not`, `Token::LParen`, `Token::Minus`, `Token::Id(_)`, `Token::If`, `Token::Num(_)`, but got {:#?}", tok),
    }
}
fn parseE(tokenizer: &mut Tokenizer) -> Expression {
    match tokenizer.token() {
        Token::Not | Token::LParen | Token::Minus | Token::Id(_) | Token::If | Token::Num(_) => {
            let left = parseO(tokenizer);
            parseE_(tokenizer, left)
        }
        tok => panic!("expected `Token::Not`, `Token::LParen`, `Token::Minus`, `Token::Id(_)`, `Token::If`, `Token::Num(_)`, but got {:#?}", tok),
    }
}
fn parseE_(tokenizer: &mut Tokenizer, left: Expression) -> Expression {
    match tokenizer.token() {
        Token::Eof | Token::RParen | Token::Comma | Token::LBrace | Token::RBrace => left,
        Token::Or => {
            tokenizer.eat(Token::Or);
            let right = parseO(tokenizer);
            parseE_(tokenizer, op_or(left, right))
        }
        tok => panic!("expected `Token::Eof`, `Token::RParen`, `Token::Comma`, `Token::LBrace`, `Token::RBrace`, `Token::Or`, but got {:#?}", tok),
    }
}
fn parseO(tokenizer: &mut Tokenizer) -> Expression {
    match tokenizer.token() {
        Token::Not | Token::LParen | Token::Minus | Token::Id(_) | Token::If | Token::Num(_) => {
            let left = parseA(tokenizer);
            parseO_(tokenizer, left)
        }
        tok => panic!("expected `Token::Not`, `Token::LParen`, `Token::Minus`, `Token::Id(_)`, `Token::If`, `Token::Num(_)`, but got {:#?}", tok),
    }
}
fn parseO_(tokenizer: &mut Tokenizer, left: Expression) -> Expression {
    match tokenizer.token() {
        Token::Eof | Token::RParen | Token::Comma | Token::LBrace | Token::RBrace | Token::Or => {
            left
        }
        Token::And => {
            tokenizer.eat(Token::And);
            let right = parseA(tokenizer);
            parseO_(tokenizer, op_and(left, right))
        }
        tok => panic!("expected `Token::Eof`, `Token::RParen`, `Token::Comma`, `Token::LBrace`, `Token::RBrace`, `Token::Or`, `Token::And`, but got {:#?}", tok),
    }
}
fn parseA(tokenizer: &mut Tokenizer) -> Expression {
    match tokenizer.token() {
        Token::Not | Token::LParen | Token::Minus | Token::Id(_) | Token::If | Token::Num(_) => {
            let left = parseB(tokenizer);
            parseA_(tokenizer, left)
        }
        tok => panic!("expected `Token::Not`, `Token::LParen`, `Token::Minus`, `Token::Id(_)`, `Token::If`, `Token::Num(_)`, but got {:#?}", tok),
    }
}
fn parseA_(tokenizer: &mut Tokenizer, left: Expression) -> Expression {
    match tokenizer.token() {
        Token::Eof
        | Token::RParen
        | Token::Comma
        | Token::LBrace
        | Token::RBrace
        | Token::Or
        | Token::And => left,
        Token::LessThan => {
            tokenizer.eat(Token::LessThan);
            let right = parseB(tokenizer);
            parseA_(tokenizer, op_lt(left, right))
        }
        Token::LessEqual => {
            tokenizer.eat(Token::LessEqual);
            let right = parseB(tokenizer);
            parseA_(tokenizer, op_le(left, right))
        }
        Token::GreaterThan => {
            tokenizer.eat(Token::GreaterThan);
            let right = parseB(tokenizer);
            parseA_(tokenizer, op_gt(left, right))
        }
        Token::GreaterEqual => {
            tokenizer.eat(Token::GreaterEqual);
            let right = parseB(tokenizer);
            parseA_(tokenizer, op_ge(left, right))
        }
        tok => panic!("expected `Token::Eof`, `Token::RParen`, `Token::Comma`, `Token::LBrace`, `Token::RBrace`, `Token::Or`, `Token::And`, `Token::LessThan`, `Token::LessEqual`, `Token::GreaterThan`, `Token::GreaterEqual`, but got {:#?}", tok),
    }
}
fn parseB(tokenizer: &mut Tokenizer) -> Expression {
    match tokenizer.token() {
        Token::Not | Token::LParen | Token::Minus | Token::Id(_) | Token::If | Token::Num(_) => {
            let left = parseT(tokenizer);
            parseB_(tokenizer, left)
        }
        tok => panic!("expected `Token::Not`, `Token::LParen`, `Token::Minus`, `Token::Id(_)`, `Token::If`, `Token::Num(_)`, but got {:#?}", tok),
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
        | Token::Comma
        | Token::LBrace
        | Token::RBrace
        | Token::Or
        | Token::And
        | Token::LessThan
        | Token::LessEqual
        | Token::GreaterThan
        | Token::GreaterEqual => left,
        Token::Plus => {
            tokenizer.eat(Token::Plus);
            let right = parseT(tokenizer);
            parseB_(tokenizer, op_add(left, right))
        }
        tok => panic!("expected `Token::Minus`, `Token::Eof`, `Token::RParen`, `Token::Comma`, `Token::LBrace`, `Token::RBrace`, `Token::Or`, `Token::And`, `Token::LessThan`, `Token::LessEqual`, `Token::GreaterThan`, `Token::GreaterEqual`, `Token::Plus`, but got {:#?}", tok),
    }
}
fn parseT(tokenizer: &mut Tokenizer) -> Expression {
    match tokenizer.token() {
        Token::Not | Token::LParen | Token::Minus | Token::Id(_) | Token::If | Token::Num(_) => {
            let left = parseF(tokenizer);
            parseT_(tokenizer, left)
        }
        tok => panic!("expected `Token::Not`, `Token::LParen`, `Token::Minus`, `Token::Id(_)`, `Token::If`, `Token::Num(_)`, but got {:#?}", tok),
    }
}
fn parseT_(tokenizer: &mut Tokenizer, left: Expression) -> Expression {
    match tokenizer.token() {
        Token::Minus
        | Token::Eof
        | Token::RParen
        | Token::Comma
        | Token::LBrace
        | Token::RBrace
        | Token::Or
        | Token::And
        | Token::LessThan
        | Token::LessEqual
        | Token::GreaterThan
        | Token::GreaterEqual
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
        tok => panic!("expected `Token::Minus`, `Token::Eof`, `Token::RParen`, `Token::Comma`, `Token::LBrace`, `Token::RBrace`, `Token::Or`, `Token::And`, `Token::LessThan`, `Token::LessEqual`, `Token::GreaterThan`, `Token::GreaterEqual`, `Token::Plus`, `Token::Mul`, `Token::Div`, but got {:#?}", tok),
    }
}
fn parseF(tokenizer: &mut Tokenizer) -> Expression {
    match tokenizer.token() {
        Token::Not => {
            tokenizer.eat(Token::Not);
            op_not(parseN(tokenizer))
        }
        Token::LParen | Token::Id(_) | Token::If | Token::Num(_) => parseN(tokenizer),
        Token::Minus => {
            tokenizer.eat(Token::Minus);
            op_sub(integer(0), parseN(tokenizer))
        }
        tok => panic!("expected `Token::Not`, `Token::LParen`, `Token::Id(_)`, `Token::If`, `Token::Num(_)`, `Token::Minus`, but got {:#?}", tok),
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
            let id = id.to_string();
            tokenizer.advance();
            parseI(tokenizer, id)
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
        Token::Num(n) => {
            let expr = integer(n.parse().unwrap());
            tokenizer.advance();
            expr
        }
        tok => panic!(
            "expected `Token::LParen`, `Token::Id(_)`, `Token::If`, `Token::Num(_)`, but got {:#?}",
            tok
        ),
    }
}
fn parseI(tokenizer: &mut Tokenizer, callee: String) -> Expression {
    match tokenizer.token() {
        Token::LParen => {
            tokenizer.eat(Token::LParen);
            let args = parseL(tokenizer);
            tokenizer.eat(Token::RParen);
            call_expr(callee, args)
        }
        Token::Minus
        | Token::Eof
        | Token::RParen
        | Token::Comma
        | Token::LBrace
        | Token::RBrace
        | Token::Or
        | Token::And
        | Token::LessThan
        | Token::LessEqual
        | Token::GreaterThan
        | Token::GreaterEqual
        | Token::Plus
        | Token::Mul
        | Token::Div => Expression::Var(callee),
        tok => panic!("expected `Token::LParen`, `Token::Minus`, `Token::Eof`, `Token::RParen`, `Token::Comma`, `Token::LBrace`, `Token::RBrace`, `Token::Or`, `Token::And`, `Token::LessThan`, `Token::LessEqual`, `Token::GreaterThan`, `Token::GreaterEqual`, `Token::Plus`, `Token::Mul`, `Token::Div`, but got {:#?}", tok),
    }
}
fn parseL(tokenizer: &mut Tokenizer) -> Vec<Expression> {
    match tokenizer.token() {
        Token::Not | Token::LParen | Token::Minus | Token::Id(_) | Token::If | Token::Num(_) => {
            parseM(tokenizer)
        }
        Token::RParen => Vec::new(),
        tok => panic!("expected `Token::Not`, `Token::LParen`, `Token::Minus`, `Token::Id(_)`, `Token::If`, `Token::Num(_)`, `Token::RParen`, but got {:#?}", tok),
    }
}
fn parseM(tokenizer: &mut Tokenizer) -> Vec<Expression> {
    match tokenizer.token() {
        Token::Not | Token::LParen | Token::Minus | Token::Id(_) | Token::If | Token::Num(_) => {
            let head = parseE(tokenizer);
            parseM_(tokenizer, vec![head])
        }
        tok => panic!("expected `Token::Not`, `Token::LParen`, `Token::Minus`, `Token::Id(_)`, `Token::If`, `Token::Num(_)`, but got {:#?}", tok),
    }
}
fn parseM_(tokenizer: &mut Tokenizer, args: Vec<Expression>) -> Vec<Expression> {
    match tokenizer.token() {
        Token::RParen => args,
        Token::Comma => {
            tokenizer.eat(Token::Comma);
            let item = parseE(tokenizer);
            parseM_(tokenizer, args.into_iter().chain([item]).collect())
        }
        tok => panic!(
            "expected `Token::RParen`, `Token::Comma`, but got {:#?}",
            tok
        ),
    }
}
