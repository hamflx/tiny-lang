use std::{iter::Peekable, str::Chars};

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Token {
    Id(String),
    Num(String),
    StrLiteral(String),
    TimeLiteral(String, TimeUnit),
    Arrow,
    Comma,
    Colon,
    Semi,
    Assign,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LessThan,
    GreaterThan,
    LessEqual,
    GreaterEqual,
    Not,
    Or,
    And,
    Plus,
    Minus,
    Mul,
    Div,
    If,
    Else,
    Fn,
    Let,
    Bool,
    ISize,
    USize,
    String,
    Eof,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum TimeUnit {
    Timestamp,
    Day,
    Hour,
    Second,
}

impl TryFrom<char> for TimeUnit {
    type Error = &'static str;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        Ok(match value {
            't' => TimeUnit::Timestamp,
            'd' => TimeUnit::Day,
            'h' => TimeUnit::Hour,
            's' => TimeUnit::Second,
            _ => return Err("unknown unit"),
        })
    }
}

fn scan_string(code: &mut Peekable<Chars<'_>>) -> String {
    let mut text = String::new();
    while let Some(ch) = code.next() {
        match ch {
            '\\' => match code.next() {
                Some('\\') => text.push('\\'),
                Some('"') => text.push('"'),
                Some(ch) => panic!("invalid token: {}", ch),
                None => break,
            },
            '"' => break,
            ch => text.push(ch),
        }
    }
    text
}

pub(crate) struct Tokenizer<'c> {
    code: Peekable<Chars<'c>>,
    token: Token,
}

impl<'c> Tokenizer<'c> {
    pub(crate) fn new(code: &'c str) -> Self {
        Self {
            code: code.chars().peekable(),
            token: Token::Eof,
        }
    }

    pub(crate) fn token(&self) -> &Token {
        &self.token
    }

    pub(crate) fn advance(&mut self) {
        while let Some(ch) = self.code.next() {
            match ch {
                '"' => self.token = Token::StrLiteral(scan_string(&mut self.code)),
                '|' => match self.code.next_if(|ch| *ch == '|') {
                    Some(_) => self.token = Token::Or,
                    None => panic!("invalid token: {ch}"),
                },
                '&' => match self.code.next_if(|ch| *ch == '&') {
                    Some(_) => self.token = Token::And,
                    None => panic!("invalid token: {ch}"),
                },
                '!' => self.token = Token::Not,
                ',' => self.token = Token::Comma,
                ':' => self.token = Token::Colon,
                ';' => self.token = Token::Semi,
                '=' => self.token = Token::Assign,
                '+' => self.token = Token::Plus,
                '-' => match self.code.next_if(|ch| *ch == '>') {
                    Some(_) => self.token = Token::Arrow,
                    None => self.token = Token::Minus,
                },
                '*' => self.token = Token::Mul,
                '/' => self.token = Token::Div,
                '<' => match self.code.next_if(|ch| *ch == '=') {
                    Some(_) => self.token = Token::LessEqual,
                    None => self.token = Token::LessThan,
                },
                '>' => match self.code.next_if(|ch| *ch == '=') {
                    Some(_) => self.token = Token::GreaterEqual,
                    None => self.token = Token::GreaterThan,
                },
                '{' => self.token = Token::LBrace,
                '}' => self.token = Token::RBrace,
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
                    self.token = match ident.as_str() {
                        "if" => Token::If,
                        "else" => Token::Else,
                        "fn" => Token::Fn,
                        "let" => Token::Let,
                        "bool" => Token::Bool,
                        "isize" => Token::ISize,
                        "usize" => Token::USize,
                        "string" => Token::String,
                        _ => Token::Id(ident),
                    };
                }
                ch if ch.is_ascii_alphanumeric() => {
                    let mut num_str = String::from(ch);
                    let mut has_dot = false;
                    while let Some(ch) = self.code.next_if(|ch| {
                        if !has_dot && *ch == '.' {
                            has_dot = true;
                            true
                        } else {
                            ch.is_ascii_digit()
                        }
                    }) {
                        num_str.push(ch);
                    }
                    if let Some(unit) = self.code.next_if(|ch| TimeUnit::try_from(*ch).is_ok()) {
                        self.token = Token::TimeLiteral(num_str, TimeUnit::try_from(unit).unwrap());
                    } else {
                        self.token = Token::Num(num_str);
                    }
                }
                ch if ch.is_ascii_whitespace() => continue,
                ch => panic!("invalid token: {ch}"),
            }
            return;
        }
        self.token = Token::Eof;
    }

    pub(crate) fn eat(&mut self, token: Token) {
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
    assert_eq!(
        to_token_list("1t + 2d"),
        vec![
            Token::TimeLiteral("1".to_string(), TimeUnit::Timestamp),
            Token::Plus,
            Token::TimeLiteral("2".to_string(), TimeUnit::Day),
        ]
    );
    assert_eq!(
        to_token_list("\"hello\""),
        vec![Token::StrLiteral("hello".to_string())]
    );
}
