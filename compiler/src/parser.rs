use crate::{
    ast::{
        AstDeclaration, AstFnDeclaration, AstLetDeclaration, AstProgram, AstStatement, Expression,
    },
    lexer::{Token, Tokenizer},
    semantic::{RecordType, Typ},
    utils::expression::{
        ast_fn, ast_fn_stmt, ast_prog, call_expr, fn_expr, if_expr, integer, let_expr, lit_string,
        op_add, op_and, op_div, op_ge, op_gt, op_le, op_lt, op_mul, op_not, op_or, op_sub,
        stmt_expr, stmt_let, var,
    },
};

#[test]
fn test_parser() {
    assert_eq!(
        parse_expr_code("1 + 2 * -3"),
        op_add(
            integer(1),
            op_mul(integer(2), op_sub(integer(0), integer(3)))
        )
    );
    assert_eq!(
        parse_expr_code("1 + 2 * (3 - 4) > 3"),
        op_gt(
            op_add(
                integer(1),
                op_mul(integer(2), op_sub(integer(3), integer(4)))
            ),
            integer(3)
        )
    );
    assert_eq!(
        parse_expr_code("if 3 > 2 { 1 } else {0}"),
        if_expr(op_gt(integer(3), integer(2)), integer(1), integer(0))
    );
    assert_eq!(
        parse_expr_code("5 + if 3 > 2 { 1 } else {0}"),
        op_add(
            integer(5),
            if_expr(op_gt(integer(3), integer(2)), integer(1), integer(0))
        )
    );
    assert_eq!(
        parse_expr_code("log(now() + 5)"),
        call_expr(
            "log".into(),
            vec![op_add(call_expr("now".into(), vec![]), integer(5))]
        )
    );
}

#[test]
fn test_parser_fn_main() {
    assert_eq!(
        parse_code("fn hello() -> usize {1} fn main()-> usize{hello()}"),
        ast_prog(&[
            ast_fn("hello", &[], Typ::Int, integer(1)),
            ast_fn(
                "main",
                &[],
                Typ::Int,
                call_expr("hello".to_string(), vec![])
            ),
        ])
    );
    assert_eq!(
        parse_code("fn main () -> () { let hello = 1; hello }"),
        ast_prog(&[ast_fn_stmt(
            "main",
            &[],
            Typ::Unit,
            &[stmt_let("hello", integer(1)), stmt_expr(var("hello"))]
        )])
    );
    assert_eq!(
        parse_code("fn main () -> () { print(\"hello\") }"),
        ast_prog(&[ast_fn_stmt(
            "main",
            &[],
            Typ::Unit,
            &[stmt_expr(call_expr(
                "print".to_string(),
                [lit_string("hello")].to_vec()
            ))]
        )])
    );
}

pub(crate) fn parse_code(code: &str) -> AstProgram {
    let mut tokenizer = Tokenizer::new(code);
    tokenizer.advance();
    parseP(&mut tokenizer)
}

pub(crate) fn parse_expr_code(code: &str) -> Expression {
    let wrapper = format!("fn main() -> () {{{code}}}");
    let mut tokenizer = Tokenizer::new(&wrapper);
    tokenizer.advance();
    let main = parseP(&mut tokenizer)
        .items
        .into_iter()
        .find_map(|item| match item {
            AstDeclaration::Fn(f) if f.name == "main" => Some(f),
            _ => None,
        })
        .unwrap();
    match main.body.into_iter().next() {
        Some(AstStatement::Expr(expr)) => expr,
        _ => todo!(),
    }
}

fn err(expected: &[&str], got: &Token) -> ! {
    panic!("Expected: {:?}, but got {:?}", expected, got)
}

pub(crate) fn parse_expr(tokenizer: &mut Tokenizer) -> Expression {
    let expr = parseE(tokenizer);
    tokenizer.eat(Token::Eof);
    expr
}

fn parse_block_statement(tokenizer: &mut Tokenizer) -> Option<AstStatement> {
    match tokenizer.token() {
        Token::RBrace => None,
        Token::Semi => {
            tokenizer.advance();
            parse_block_statement(tokenizer)
        }
        Token::Let => Some(AstStatement::Let(parseLet(tokenizer))),
        Token::Id(_) | Token::If | Token::LParen | Token::Minus | Token::Not | Token::Num(_) => {
            let stmt = Some(AstStatement::Expr(parseE(tokenizer)));
            match tokenizer.token() {
                Token::Semi | Token::RBrace => {}
                tok => err(&[";", "}"], &tok),
            }
            stmt
        }
        tok => err(&["(", "id", "!", "-", "if", "num", ";", "}"], &tok),
    }
}

fn parse_block_statement_list(tokenizer: &mut Tokenizer) -> Vec<AstStatement> {
    let mut stmts = Vec::new();
    while let Some(stmt) = parse_block_statement(tokenizer) {
        stmts.push(stmt);
    }
    stmts
}

pub(crate) fn parseP(tokenizer: &mut Tokenizer) -> AstProgram {
    match tokenizer.token() {
        Token::Eof | Token::Fn | Token::Let => {
            let e0 = parseItems(tokenizer, vec![]);
            tokenizer.eat(Token::Eof);
            AstProgram { items: e0 }
        }
        tok => err(&["$", "fn", "let"], &tok),
    }
}
fn parseType(tokenizer: &mut Tokenizer) -> Typ {
    match tokenizer.token() {
        Token::Bool => {
            tokenizer.eat(Token::Bool);
            Typ::Bool
        }
        Token::ISize => {
            tokenizer.eat(Token::ISize);
            Typ::Int
        }
        Token::LBrace => parseRecord(tokenizer),
        Token::LParen => {
            tokenizer.eat(Token::LParen);
            tokenizer.eat(Token::RParen);
            Typ::Unit
        }
        Token::String => {
            tokenizer.eat(Token::String);
            Typ::String
        }
        Token::USize => {
            tokenizer.eat(Token::USize);
            Typ::Int
        }
        tok => err(&["(", "bool", "isize", "string", "usize", "{"], &tok),
    }
}
fn parseRecord(tokenizer: &mut Tokenizer) -> Typ {
    match tokenizer.token() {
        Token::LBrace => {
            tokenizer.eat(Token::LBrace);
            let e1 = parseRecordFields(tokenizer, vec![]);
            tokenizer.eat(Token::RBrace);
            Typ::Record(RecordType { fields: e1 })
        }
        tok => err(&["{"], &tok),
    }
}
fn parseRecordFields(tokenizer: &mut Tokenizer, prefix: Vec<(String, Typ)>) -> Vec<(String, Typ)> {
    match tokenizer.token() {
        Token::Id(_) => {
            let e0 = parseRecordFieldItem(tokenizer);
            tokenizer.eat(Token::Comma);
            let e2 = parseRecordFields(tokenizer, prefix.into_iter().chain([e0]).collect());
            e2
        }
        Token::RBrace => prefix,
        tok => err(&["id", "}"], &tok),
    }
}
fn parseRecordFieldItem(tokenizer: &mut Tokenizer) -> (String, Typ) {
    match tokenizer.token() {
        Token::Id(_) => {
            let id = match tokenizer.token() {
                Token::Id(id) => id.to_string(),
                tok => err(&["id"], &tok),
            };
            tokenizer.eat(Token::Colon);
            let e2 = parseType(tokenizer);
            (id, e2)
        }
        tok => err(&["id"], &tok),
    }
}
fn parseItems(tokenizer: &mut Tokenizer, prefix: Vec<AstDeclaration>) -> Vec<AstDeclaration> {
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
fn parseItem(tokenizer: &mut Tokenizer) -> AstDeclaration {
    match tokenizer.token() {
        Token::Fn => parseFn(tokenizer),
        Token::Let => AstDeclaration::Let(parseLet(tokenizer)),
        tok => err(&["fn", "let"], &tok),
    }
}
fn parseLet(tokenizer: &mut Tokenizer) -> AstLetDeclaration {
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
            AstLetDeclaration {
                name: id,
                value: e3,
            }
        }
        tok => err(&["let"], &tok),
    }
}
fn parseFn(tokenizer: &mut Tokenizer) -> AstDeclaration {
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
            tokenizer.eat(Token::Arrow);
            let e6 = parseType(tokenizer);
            tokenizer.eat(Token::LBrace);
            let body = parse_block_statement_list(tokenizer);
            tokenizer.eat(Token::RBrace);
            AstDeclaration::Fn(AstFnDeclaration {
                name: id,
                params: e3,
                typ: e6,
                body,
            })
        }
        tok => err(&["fn"], &tok),
    }
}
fn parseFnArgsOpt(tokenizer: &mut Tokenizer) -> Vec<(String, Typ)> {
    match tokenizer.token() {
        Token::Id(_) => {
            let e0 = parseFnArgs(tokenizer);
            e0
        }
        Token::RParen => Vec::new(),
        tok => err(&[")", "id"], &tok),
    }
}
fn parseFnArgs(tokenizer: &mut Tokenizer) -> Vec<(String, Typ)> {
    match tokenizer.token() {
        Token::Id(_) => {
            let e0 = parseFnArg(tokenizer);
            parseFnArgs_(tokenizer, vec![e0])
        }
        tok => err(&["id"], &tok),
    }
}
fn parseFnArgs_(tokenizer: &mut Tokenizer, prefix: Vec<(String, Typ)>) -> Vec<(String, Typ)> {
    match tokenizer.token() {
        Token::Comma => {
            tokenizer.eat(Token::Comma);
            let e1 = parseFnArg(tokenizer);
            parseFnArgs_(tokenizer, prefix.into_iter().chain([e1]).collect())
        }
        Token::RParen => prefix,
        tok => err(&[")", ","], &tok),
    }
}
fn parseFnArg(tokenizer: &mut Tokenizer) -> (String, Typ) {
    match tokenizer.token() {
        Token::Id(_) => {
            let id = match tokenizer.token() {
                Token::Id(id) => id.to_string(),
                tok => err(&["id"], &tok),
            };
            tokenizer.advance();
            tokenizer.eat(Token::Colon);
            let e2 = parseType(tokenizer);
            (id, e2)
        }
        tok => err(&["id"], &tok),
    }
}
fn parseE(tokenizer: &mut Tokenizer) -> Expression {
    match tokenizer.token() {
        Token::Id(_)
        | Token::If
        | Token::LParen
        | Token::Minus
        | Token::Not
        | Token::Num(_)
        | Token::StrLiteral(_) => {
            let e0 = parseO(tokenizer);
            parseE_(tokenizer, e0)
        }
        tok => err(&["(", "string", "id", "!", "-", "if", "num"], &tok),
    }
}
fn parseE_(tokenizer: &mut Tokenizer, prefix: Expression) -> Expression {
    match tokenizer.token() {
        Token::Comma | Token::LBrace | Token::RBrace | Token::RParen | Token::Semi => prefix,
        Token::Or => {
            tokenizer.eat(Token::Or);
            let e1 = parseO(tokenizer);
            parseE_(tokenizer, op_or(prefix, e1))
        }
        tok => err(&["{", ")", ",", "}", ";", "||"], &tok),
    }
}
fn parseO(tokenizer: &mut Tokenizer) -> Expression {
    match tokenizer.token() {
        Token::Id(_)
        | Token::If
        | Token::LParen
        | Token::Minus
        | Token::Not
        | Token::Num(_)
        | Token::StrLiteral(_) => {
            let e0 = parseA(tokenizer);
            parseO_(tokenizer, e0)
        }
        tok => err(&["(", "string", "id", "!", "-", "if", "num"], &tok),
    }
}
fn parseO_(tokenizer: &mut Tokenizer, prefix: Expression) -> Expression {
    match tokenizer.token() {
        Token::And => {
            tokenizer.eat(Token::And);
            let e1 = parseA(tokenizer);
            parseO_(tokenizer, op_and(prefix, e1))
        }
        Token::Comma | Token::LBrace | Token::Or | Token::RBrace | Token::RParen | Token::Semi => {
            prefix
        }
        tok => err(&["{", ")", ",", "}", ";", "||", "&&"], &tok),
    }
}
fn parseA(tokenizer: &mut Tokenizer) -> Expression {
    match tokenizer.token() {
        Token::Id(_)
        | Token::If
        | Token::LParen
        | Token::Minus
        | Token::Not
        | Token::Num(_)
        | Token::StrLiteral(_) => {
            let e0 = parseB(tokenizer);
            parseA_(tokenizer, e0)
        }
        tok => err(&["(", "string", "id", "!", "-", "if", "num"], &tok),
    }
}
fn parseA_(tokenizer: &mut Tokenizer, prefix: Expression) -> Expression {
    match tokenizer.token() {
        Token::And
        | Token::Comma
        | Token::LBrace
        | Token::Or
        | Token::RBrace
        | Token::RParen
        | Token::Semi => prefix,
        Token::GreaterEqual => {
            tokenizer.eat(Token::GreaterEqual);
            let e1 = parseB(tokenizer);
            parseA_(tokenizer, op_ge(prefix, e1))
        }
        Token::GreaterThan => {
            tokenizer.eat(Token::GreaterThan);
            let e1 = parseB(tokenizer);
            parseA_(tokenizer, op_gt(prefix, e1))
        }
        Token::LessEqual => {
            tokenizer.eat(Token::LessEqual);
            let e1 = parseB(tokenizer);
            parseA_(tokenizer, op_le(prefix, e1))
        }
        Token::LessThan => {
            tokenizer.eat(Token::LessThan);
            let e1 = parseB(tokenizer);
            parseA_(tokenizer, op_lt(prefix, e1))
        }
        tok => err(
            &["{", ")", ",", "}", ";", "||", "&&", "<", "<=", ">", ">="],
            &tok,
        ),
    }
}
fn parseB(tokenizer: &mut Tokenizer) -> Expression {
    match tokenizer.token() {
        Token::Id(_)
        | Token::If
        | Token::LParen
        | Token::Minus
        | Token::Not
        | Token::Num(_)
        | Token::StrLiteral(_) => {
            let e0 = parseT(tokenizer);
            parseB_(tokenizer, e0)
        }
        tok => err(&["(", "string", "id", "!", "-", "if", "num"], &tok),
    }
}
fn parseB_(tokenizer: &mut Tokenizer, prefix: Expression) -> Expression {
    match tokenizer.token() {
        Token::And
        | Token::Comma
        | Token::GreaterEqual
        | Token::GreaterThan
        | Token::LBrace
        | Token::LessEqual
        | Token::LessThan
        | Token::Or
        | Token::RBrace
        | Token::RParen
        | Token::Semi => prefix,
        Token::Minus => {
            tokenizer.eat(Token::Minus);
            let e1 = parseT(tokenizer);
            parseB_(tokenizer, op_sub(prefix, e1))
        }
        Token::Plus => {
            tokenizer.eat(Token::Plus);
            let e1 = parseT(tokenizer);
            parseB_(tokenizer, op_add(prefix, e1))
        }
        tok => err(
            &[
                "{", ")", ",", "}", ";", "||", "&&", "<", "<=", ">", ">=", "-", "+",
            ],
            &tok,
        ),
    }
}
fn parseT(tokenizer: &mut Tokenizer) -> Expression {
    match tokenizer.token() {
        Token::Id(_)
        | Token::If
        | Token::LParen
        | Token::Minus
        | Token::Not
        | Token::Num(_)
        | Token::StrLiteral(_) => {
            let e0 = parseF(tokenizer);
            parseT_(tokenizer, e0)
        }
        tok => err(&["(", "string", "id", "!", "-", "if", "num"], &tok),
    }
}
fn parseT_(tokenizer: &mut Tokenizer, prefix: Expression) -> Expression {
    match tokenizer.token() {
        Token::And
        | Token::Comma
        | Token::GreaterEqual
        | Token::GreaterThan
        | Token::LBrace
        | Token::LessEqual
        | Token::LessThan
        | Token::Minus
        | Token::Or
        | Token::Plus
        | Token::RBrace
        | Token::RParen
        | Token::Semi => prefix,
        Token::Div => {
            tokenizer.eat(Token::Div);
            let e1 = parseF(tokenizer);
            parseT_(tokenizer, op_div(prefix, e1))
        }
        Token::Mul => {
            tokenizer.eat(Token::Mul);
            let e1 = parseF(tokenizer);
            parseT_(tokenizer, op_mul(prefix, e1))
        }
        tok => err(
            &[
                "{", ")", ",", "}", "-", ";", "||", "&&", "<", "<=", ">", ">=", "+", "*", "/",
            ],
            &tok,
        ),
    }
}
fn parseF(tokenizer: &mut Tokenizer) -> Expression {
    match tokenizer.token() {
        Token::Id(_) | Token::If | Token::LParen | Token::Num(_) | Token::StrLiteral(_) => {
            let e0 = parseN(tokenizer);
            e0
        }
        Token::Minus => {
            tokenizer.eat(Token::Minus);
            let e1 = parseN(tokenizer);
            op_sub(integer(0), e1)
        }
        Token::Not => {
            tokenizer.eat(Token::Not);
            let e1 = parseN(tokenizer);
            op_not(e1)
        }
        tok => err(&["(", "string", "id", "if", "num", "!", "-"], &tok),
    }
}
fn parseN(tokenizer: &mut Tokenizer) -> Expression {
    match tokenizer.token() {
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
        Token::LParen => {
            tokenizer.eat(Token::LParen);
            let e1 = parseE(tokenizer);
            tokenizer.eat(Token::RParen);
            e1
        }
        Token::Num(n) => {
            let expr = integer(n.parse().unwrap());
            tokenizer.advance();
            expr
        }
        Token::StrLiteral(str_lit) => {
            let str_lit = str_lit.to_string();
            tokenizer.advance();
            Expression::StrLiteral(str_lit)
        }
        tok => err(&["(", "string", "id", "if", "num"], &tok),
    }
}
fn parseI(tokenizer: &mut Tokenizer, callee: String) -> Expression {
    match tokenizer.token() {
        Token::And
        | Token::Comma
        | Token::Div
        | Token::GreaterEqual
        | Token::GreaterThan
        | Token::LBrace
        | Token::LessEqual
        | Token::LessThan
        | Token::Minus
        | Token::Mul
        | Token::Or
        | Token::Plus
        | Token::RBrace
        | Token::RParen
        | Token::Semi => Expression::Var(callee),
        Token::LParen => {
            tokenizer.eat(Token::LParen);
            let e1 = parseL(tokenizer);
            tokenizer.eat(Token::RParen);
            call_expr(callee, e1)
        }
        tok => err(
            &[
                "(", "{", ")", ",", "}", "-", ";", "||", "&&", "<", "<=", ">", ">=", "+", "*", "/",
            ],
            &tok,
        ),
    }
}
fn parseL(tokenizer: &mut Tokenizer) -> Vec<Expression> {
    match tokenizer.token() {
        Token::Id(_)
        | Token::If
        | Token::LParen
        | Token::Minus
        | Token::Not
        | Token::Num(_)
        | Token::StrLiteral(_) => {
            let e0 = parseM(tokenizer);
            e0
        }
        Token::RParen => Vec::new(),
        tok => err(&["(", "string", "id", "!", "-", "if", "num", ")"], &tok),
    }
}
fn parseM(tokenizer: &mut Tokenizer) -> Vec<Expression> {
    match tokenizer.token() {
        Token::Id(_)
        | Token::If
        | Token::LParen
        | Token::Minus
        | Token::Not
        | Token::Num(_)
        | Token::StrLiteral(_) => {
            let e0 = parseE(tokenizer);
            parseM_(tokenizer, vec![e0])
        }
        tok => err(&["(", "string", "id", "!", "-", "if", "num"], &tok),
    }
}
fn parseM_(tokenizer: &mut Tokenizer, prefix: Vec<Expression>) -> Vec<Expression> {
    match tokenizer.token() {
        Token::Comma => {
            tokenizer.eat(Token::Comma);
            let e1 = parseE(tokenizer);
            parseM_(tokenizer, prefix.into_iter().chain([e1]).collect())
        }
        Token::RParen => prefix,
        tok => err(&[")", ","], &tok),
    }
}
