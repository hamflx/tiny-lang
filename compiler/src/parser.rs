use crate::{
    ast::Expression,
    lexer::{Token, Tokenizer},
    utils::expression::{
        call_expr, fn_expr, if_expr, integer, let_expr, op_add, op_and, op_div, op_ge, op_gt,
        op_le, op_lt, op_mul, op_not, op_or, op_sub,
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

#[test]
fn test_parser_fn() {
    assert_eq!(
        parse_code("fn hello(){1} hello()"),
        let_expr(
            "hello",
            fn_expr(&[], integer(1)),
            call_expr("hello".to_string(), vec![]),
        )
    );
}

#[test]
fn test_parser_fn_main() {
    assert_eq!(
        parse_code("fn hello() {1} fn main(){hello()}"),
        let_expr(
            "hello",
            fn_expr(&[], integer(1)),
            let_expr(
                "main",
                fn_expr(&[], call_expr("hello".to_string(), vec![])),
                call_expr("main".to_string(), vec![]),
            ),
        )
    );
}

pub(crate) fn parse_code(code: &str) -> Expression {
    let mut tokenizer = Tokenizer::new(code);
    tokenizer.advance();
    parseP(&mut tokenizer)
}

fn err(expected: &[&str], got: &Token) -> ! {
    panic!("Expected: {:?}, but got {:?}", expected, got)
}

pub(crate) fn parseP(tokenizer: &mut Tokenizer) -> Expression {
    match tokenizer.token() {
        Token::Eof | Token::Fn | Token::Let => {
            let e0 = parseItems(tokenizer, vec![]);
            tokenizer.eat(Token::Eof);
            e0.into_iter().rev().fold(
                call_expr("main".to_string(), vec![]),
                |scope, (id, body)| let_expr(&id, body, scope),
            )
        }
        tok => err(&["$", "fn", "let"], &tok),
    }
}
fn parseItems(
    tokenizer: &mut Tokenizer,
    prefix: Vec<(String, Expression)>,
) -> Vec<(String, Expression)> {
    match tokenizer.token() {
        Token::Eof => prefix,
        Token::Fn | Token::Let => {
            let e0 = parseItem(tokenizer);
            let e1 = parseItems(tokenizer, prefix.into_iter().chain([e0]).collect());
            e1
        }
        tok => err(&["$", "fn", "let"], &tok),
    }
}
fn parseItem(tokenizer: &mut Tokenizer) -> (String, Expression) {
    match tokenizer.token() {
        Token::Fn => parseFn(tokenizer),
        Token::Let => parseLet(tokenizer),
        tok => err(&["fn", "let"], &tok),
    }
}
fn parseLet(tokenizer: &mut Tokenizer) -> (String, Expression) {
    match tokenizer.token() {
        Token::Let => {
            tokenizer.eat(Token::Let);
            let id = match tokenizer.token() {
                Token::Id(id) => id.to_string(),
                tok => err(&["id"], &tok),
            };
            tokenizer.advance();
            tokenizer.eat(Token::Assign);
            let e3 = parseE(tokenizer);
            tokenizer.eat(Token::Semi);
            (id, e3)
        }
        tok => err(&["let"], &tok),
    }
}
fn parseFn(tokenizer: &mut Tokenizer) -> (String, Expression) {
    match tokenizer.token() {
        Token::Fn => {
            tokenizer.eat(Token::Fn);
            let id = match tokenizer.token() {
                Token::Id(id) => id.to_string(),
                tok => err(&["id"], &tok),
            };
            tokenizer.advance();
            tokenizer.eat(Token::LParen);
            let e3 = parseFnArgsOpt(tokenizer);
            tokenizer.eat(Token::RParen);
            tokenizer.eat(Token::LBrace);
            let e6 = parseE(tokenizer);
            tokenizer.eat(Token::RBrace);
            (id, fn_expr(&e3, e6))
        }
        tok => err(&["fn"], &tok),
    }
}
fn parseFnArgsOpt(tokenizer: &mut Tokenizer) -> Vec<String> {
    match tokenizer.token() {
        Token::Id(_) => parseFnArgs(tokenizer),
        Token::RParen => Vec::new(),
        tok => err(&["id", ")"], &tok),
    }
}
fn parseFnArgs(tokenizer: &mut Tokenizer) -> Vec<String> {
    match tokenizer.token() {
        Token::Id(_) => {
            let id = match tokenizer.token() {
                Token::Id(id) => id.to_string(),
                tok => err(&["id"], &tok),
            };
            tokenizer.advance();
            parseFnArgs_(tokenizer, vec![id])
        }
        tok => err(&["id"], &tok),
    }
}
fn parseFnArgs_(tokenizer: &mut Tokenizer, prefix: Vec<String>) -> Vec<String> {
    match tokenizer.token() {
        Token::RParen => prefix,
        Token::Comma => {
            tokenizer.eat(Token::Comma);
            let id = match tokenizer.token() {
                Token::Id(id) => id.to_string(),
                tok => err(&["id"], &tok),
            };
            tokenizer.advance();
            parseFnArgs_(tokenizer, prefix.into_iter().chain([id]).collect())
        }
        tok => err(&[")", ","], &tok),
    }
}
fn parseE(tokenizer: &mut Tokenizer) -> Expression {
    match tokenizer.token() {
        Token::Not | Token::LParen | Token::Minus | Token::Id(_) | Token::If | Token::Num(_) => {
            let e0 = parseO(tokenizer);
            parseE_(tokenizer, e0)
        }
        tok => err(&["!", "(", "-", "id", "if", "num"], &tok),
    }
}
fn parseE_(tokenizer: &mut Tokenizer, prefix: Expression) -> Expression {
    match tokenizer.token() {
        Token::RParen | Token::Comma | Token::Semi | Token::LBrace | Token::RBrace => prefix,
        Token::Or => {
            tokenizer.eat(Token::Or);
            let e1 = parseO(tokenizer);
            parseE_(tokenizer, op_or(prefix, e1))
        }
        tok => err(&[")", ",", ";", "{", "}", "||"], &tok),
    }
}
fn parseO(tokenizer: &mut Tokenizer) -> Expression {
    match tokenizer.token() {
        Token::Not | Token::LParen | Token::Minus | Token::Id(_) | Token::If | Token::Num(_) => {
            let e0 = parseA(tokenizer);
            parseO_(tokenizer, e0)
        }
        tok => err(&["!", "(", "-", "id", "if", "num"], &tok),
    }
}
fn parseO_(tokenizer: &mut Tokenizer, prefix: Expression) -> Expression {
    match tokenizer.token() {
        Token::RParen | Token::Comma | Token::Semi | Token::LBrace | Token::RBrace | Token::Or => {
            prefix
        }
        Token::And => {
            tokenizer.eat(Token::And);
            let e1 = parseA(tokenizer);
            parseO_(tokenizer, op_and(prefix, e1))
        }
        tok => err(&[")", ",", ";", "{", "}", "||", "&&"], &tok),
    }
}
fn parseA(tokenizer: &mut Tokenizer) -> Expression {
    match tokenizer.token() {
        Token::Not | Token::LParen | Token::Minus | Token::Id(_) | Token::If | Token::Num(_) => {
            let e0 = parseB(tokenizer);
            parseA_(tokenizer, e0)
        }
        tok => err(&["!", "(", "-", "id", "if", "num"], &tok),
    }
}
fn parseA_(tokenizer: &mut Tokenizer, prefix: Expression) -> Expression {
    match tokenizer.token() {
        Token::RParen
        | Token::Comma
        | Token::Semi
        | Token::LBrace
        | Token::RBrace
        | Token::Or
        | Token::And => prefix,
        Token::LessThan => {
            tokenizer.eat(Token::LessThan);
            let e1 = parseB(tokenizer);
            parseA_(tokenizer, op_lt(prefix, e1))
        }
        Token::LessEqual => {
            tokenizer.eat(Token::LessEqual);
            let e1 = parseB(tokenizer);
            parseA_(tokenizer, op_le(prefix, e1))
        }
        Token::GreaterThan => {
            tokenizer.eat(Token::GreaterThan);
            let e1 = parseB(tokenizer);
            parseA_(tokenizer, op_gt(prefix, e1))
        }
        Token::GreaterEqual => {
            tokenizer.eat(Token::GreaterEqual);
            let e1 = parseB(tokenizer);
            parseA_(tokenizer, op_ge(prefix, e1))
        }
        tok => err(
            &[")", ",", ";", "{", "}", "||", "&&", "<", "<=", ">", ">="],
            &tok,
        ),
    }
}
fn parseB(tokenizer: &mut Tokenizer) -> Expression {
    match tokenizer.token() {
        Token::Not | Token::LParen | Token::Minus | Token::Id(_) | Token::If | Token::Num(_) => {
            let e0 = parseT(tokenizer);
            parseB_(tokenizer, e0)
        }
        tok => err(&["!", "(", "-", "id", "if", "num"], &tok),
    }
}
fn parseB_(tokenizer: &mut Tokenizer, prefix: Expression) -> Expression {
    match tokenizer.token() {
        Token::Minus => {
            tokenizer.eat(Token::Minus);
            let e1 = parseT(tokenizer);
            parseB_(tokenizer, op_sub(prefix, e1))
        }
        Token::RParen
        | Token::Comma
        | Token::Semi
        | Token::LBrace
        | Token::RBrace
        | Token::Or
        | Token::And
        | Token::LessThan
        | Token::LessEqual
        | Token::GreaterThan
        | Token::GreaterEqual => prefix,
        Token::Plus => {
            tokenizer.eat(Token::Plus);
            let e1 = parseT(tokenizer);
            parseB_(tokenizer, op_add(prefix, e1))
        }
        tok => err(
            &[
                ")", ",", ";", "{", "}", "||", "&&", "<", "<=", ">", ">=", "-", "+",
            ],
            &tok,
        ),
    }
}
fn parseT(tokenizer: &mut Tokenizer) -> Expression {
    match tokenizer.token() {
        Token::Not | Token::LParen | Token::Minus | Token::Id(_) | Token::If | Token::Num(_) => {
            let e0 = parseF(tokenizer);
            parseT_(tokenizer, e0)
        }
        tok => err(&["!", "(", "-", "id", "if", "num"], &tok),
    }
}
fn parseT_(tokenizer: &mut Tokenizer, prefix: Expression) -> Expression {
    match tokenizer.token() {
        Token::RParen
        | Token::Comma
        | Token::Minus
        | Token::Semi
        | Token::LBrace
        | Token::RBrace
        | Token::Or
        | Token::And
        | Token::LessThan
        | Token::LessEqual
        | Token::GreaterThan
        | Token::GreaterEqual
        | Token::Plus => prefix,
        Token::Mul => {
            tokenizer.eat(Token::Mul);
            let e1 = parseF(tokenizer);
            parseT_(tokenizer, op_mul(prefix, e1))
        }
        Token::Div => {
            tokenizer.eat(Token::Div);
            let e1 = parseF(tokenizer);
            parseT_(tokenizer, op_div(prefix, e1))
        }
        tok => err(
            &[
                ")", ",", "-", ";", "{", "}", "||", "&&", "<", "<=", ">", ">=", "+", "*", "/",
            ],
            &tok,
        ),
    }
}
fn parseF(tokenizer: &mut Tokenizer) -> Expression {
    match tokenizer.token() {
        Token::Not => {
            tokenizer.eat(Token::Not);
            let e1 = parseN(tokenizer);
            op_not(e1)
        }
        Token::LParen | Token::Id(_) | Token::If | Token::Num(_) => {
            let e0 = parseN(tokenizer);
            e0
        }
        Token::Minus => {
            tokenizer.eat(Token::Minus);
            let e1 = parseN(tokenizer);
            op_sub(integer(0), e1)
        }
        tok => err(&["!", "(", "id", "if", "num", "-"], &tok),
    }
}
fn parseN(tokenizer: &mut Tokenizer) -> Expression {
    match tokenizer.token() {
        Token::LParen => {
            tokenizer.eat(Token::LParen);
            let e1 = parseE(tokenizer);
            tokenizer.eat(Token::RParen);
            e1
        }
        Token::Id(id) => {
            let id = id.to_string();
            tokenizer.advance();
            let e1 = parseI(tokenizer, id);
            e1
        }
        Token::If => {
            tokenizer.eat(Token::If);
            let e1 = parseE(tokenizer);
            tokenizer.eat(Token::LBrace);
            let e3 = parseE(tokenizer);
            tokenizer.eat(Token::RBrace);
            tokenizer.eat(Token::Else);
            tokenizer.eat(Token::LBrace);
            let e7 = parseE(tokenizer);
            tokenizer.eat(Token::RBrace);
            if_expr(e1, e3, e7)
        }
        Token::Num(n) => {
            let expr = integer(n.parse().unwrap());
            tokenizer.advance();
            expr
        }
        tok => err(&["(", "id", "if", "num"], &tok),
    }
}
fn parseI(tokenizer: &mut Tokenizer, callee: String) -> Expression {
    match tokenizer.token() {
        Token::LParen => {
            tokenizer.eat(Token::LParen);
            let e1 = parseL(tokenizer);
            tokenizer.eat(Token::RParen);
            call_expr(callee, e1)
        }
        Token::RParen
        | Token::Comma
        | Token::Minus
        | Token::Semi
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
        tok => err(
            &[
                ")", ",", "-", ";", "{", "}", "||", "&&", "<", "<=", ">", ">=", "+", "*", "/", "(",
            ],
            &tok,
        ),
    }
}
fn parseL(tokenizer: &mut Tokenizer) -> Vec<Expression> {
    match tokenizer.token() {
        Token::Not | Token::LParen | Token::Minus | Token::Id(_) | Token::If | Token::Num(_) => {
            parseM(tokenizer)
        }
        Token::RParen => Vec::new(),
        tok => err(&["!", "(", "-", "id", "if", "num", ")"], &tok),
    }
}
fn parseM(tokenizer: &mut Tokenizer) -> Vec<Expression> {
    match tokenizer.token() {
        Token::Not | Token::LParen | Token::Minus | Token::Id(_) | Token::If | Token::Num(_) => {
            let e0 = parseE(tokenizer);
            parseM_(tokenizer, vec![e0])
        }
        tok => err(&["!", "(", "-", "id", "if", "num"], &tok),
    }
}
fn parseM_(tokenizer: &mut Tokenizer, prefix: Vec<Expression>) -> Vec<Expression> {
    match tokenizer.token() {
        Token::RParen => prefix,
        Token::Comma => {
            tokenizer.eat(Token::Comma);
            let e1 = parseE(tokenizer);
            parseM_(tokenizer, prefix.into_iter().chain([e1]).collect())
        }
        tok => err(&[")", ","], &tok),
    }
}
