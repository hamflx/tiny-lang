use std::{iter::Peekable, str::Chars};

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Token {
    Id(String),
    Num(String),
    TimeLiteral(String, char),
    LParen,
    RParen,
    Plus,
    Minus,
    Mul,
    Div,
    Eof,
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
                            ch.is_ascii_digit()
                        }
                    }) {
                        num_str.push(ch);
                    }
                    if let Some(unit) = self.code.next_if(|ch| matches!(ch, 't' | 'd' | 'h' | 's'))
                    {
                        self.token = Token::TimeLiteral(num_str, unit);
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
            Token::TimeLiteral("1".to_string(), 't'),
            Token::Plus,
            Token::TimeLiteral("2".to_string(), 'd'),
        ]
    );
}
