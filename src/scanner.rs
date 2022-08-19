// Copyright 2022 Joshua Wong.
// SPDX-License-Identifier: Apache-2.0 OR MIT

use std::{
    iter::{self, Peekable},
    str::Chars,
};

pub struct Scanner<'a> {
    chars: Peekable<Chars<'a>>,
    line_num: u64,
}

impl<'a> Scanner<'a> {
    pub fn new(input: &str) -> Scanner<'_> {
        Scanner {
            chars: input.chars().peekable(),
            line_num: 0,
        }
    }

    fn scan_token(&mut self) -> Option<Token> {
        let ch = self.chars.next()?;
        match ch {
            '(' => Some(self.create_token(TokenTy::LParen)),
            ')' => Some(self.create_token(TokenTy::RParen)),
            '{' => Some(self.create_token(TokenTy::LBrace)),
            '}' => Some(self.create_token(TokenTy::RBrace)),
            ',' => Some(self.create_token(TokenTy::Comma)),
            '.' => Some(self.create_token(TokenTy::Dot)),
            '-' => Some(self.create_token(TokenTy::Minus)),
            '+' => Some(self.create_token(TokenTy::Plus)),
            ';' => Some(self.create_token(TokenTy::Semi)),
            '*' => Some(self.create_token(TokenTy::Star)),

            '!' => Some(self.create_one_or_two_char_token("!=", TokenTy::BangEq, TokenTy::Bang)),
            '=' => Some(self.create_one_or_two_char_token("==", TokenTy::EqEq, TokenTy::Eq)),
            '<' => Some(self.create_one_or_two_char_token("<=", TokenTy::Le, TokenTy::Lt)),
            '>' => Some(self.create_one_or_two_char_token(">=", TokenTy::Ge, TokenTy::Gt)),
            '/' => {
                if self.chars.next_if(|&ch| ch == '/').is_some() {
                    if self.chars.any(|ch| ch == '\n') {
                        self.line_num += 1;
                    }
                    None
                } else {
                    Some(self.create_token(TokenTy::Slash))
                }
            }
            ' ' | '\r' | '\t' => None,
            '\n' => {
                self.line_num += 1;
                None
            }
            '\'' | '"' => {
                let mut num_lines = 0;
                let delimiter = ch;
                let mut delimiter_at_end = false;
                let raw_str: String = self
                    .chars
                    .by_ref()
                    .inspect(|&ch| {
                        if ch == '\n' {
                            num_lines += 1;
                        }
                        delimiter_at_end |= ch == delimiter;
                    })
                    .take_while(|&ch| ch != delimiter)
                    .collect();
                if !delimiter_at_end {
                    panic!("no closing quote at line {}", self.line_num);
                }
                Some(self.create_token(TokenTy::Str(raw_str)))
            }
            '0'..='9' => {
                let ref_iter = self.chars.by_ref();
                let numeric: String = iter::once(ch)
                    .chain(iter::from_fn(|| {
                        ref_iter.next_if(|&ch| ch.is_numeric() || ch == '.')
                    }))
                    .collect();
                Some(
                    self.create_token(TokenTy::Num(
                        numeric
                            .parse::<f64>()
                            .map_err(|_| {
                                let line = self.line_num;
                                format!("error: could not parse number at line: {line}")
                            })
                            .unwrap(),
                    )),
                )
            }
            'A'..='Z' | 'a'..='z' | '_' => {
                let ref_iter = self.chars.by_ref();
                let ident: String = iter::once(ch)
                    .chain(iter::from_fn(|| {
                        ref_iter.next_if(|&ch| ch.is_alphanumeric() || ch == '_')
                    }))
                    .collect();
                let keyword = TokenTy::parse_keyword(&ident).unwrap_or(TokenTy::Ident(ident));
                Some(self.create_token(keyword))
            }

            _ => panic!("could not parse tokens at line {}", self.line_num),
        }
    }

    fn create_one_or_two_char_token(
        &mut self,
        chars: &'static str,
        matches: TokenTy,
        default: TokenTy,
    ) -> Token {
        let next_char = chars.chars().nth(1).unwrap();
        if self.chars.next_if(|&ch| ch == next_char).is_some() {
            self.create_token(matches)
        } else {
            self.create_token(default)
        }
    }

    fn create_token(&self, ty: TokenTy) -> Token {
        Token::new(self.line_num, ty)
    }
}

impl<'a> IntoIterator for Scanner<'a> {
    type Item = Token;
    type IntoIter = ScannerIter<'a>;
    fn into_iter(self) -> Self::IntoIter {
        ScannerIter(self)
    }
}

pub struct ScannerIter<'a>(Scanner<'a>);

impl Iterator for ScannerIter<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        while self.0.chars.peek().is_some() {
            if let Some(token) = self.0.scan_token() {
                return Some(token);
            }
        }
        None
    }
}

#[derive(Debug, PartialEq)]
pub struct Token {
    pub line: u64,
    pub ty: TokenTy,
}

impl Token {
    pub fn new(line: u64, ty: TokenTy) -> Self {
        Token { line, ty }
    }
}

#[derive(Debug, PartialEq)]
pub enum TokenTy {
    // single character tokens
    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semi,
    Slash,
    Star,

    // one or two character tokens
    Bang,
    BangEq,
    Eq,
    EqEq,
    Gt,
    Ge,
    Lt,
    Le,

    // literals
    Ident(String),
    Str(String),
    Num(f64),

    // keywords
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    // not parsed at the moment
    LineComment(String),

    Eof,
}

impl TokenTy {
    fn parse_keyword(ident: &str) -> Option<Self> {
        match ident {
            "and" => Some(Self::And),
            "Class" => Some(Self::Class),
            "else" => Some(Self::Else),
            "false" => Some(Self::False),
            "fun" => Some(Self::Fun),
            "for" => Some(Self::For),
            "if" => Some(Self::If),
            "nil" => Some(Self::Nil),
            "or" => Some(Self::Or),
            "print" => Some(Self::Print),
            "return" => Some(Self::Return),
            "super" => Some(Self::Super),
            "this" => Some(Self::This),
            "var" => Some(Self::Var),
            "while" => Some(Self::While),
            _ => None,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    macro_rules! assert_tokens {
        ($input:literal, $( $ty:expr ),* $(,)?) => {
            {
                let mut scanner_iter = Scanner::new($input).into_iter();
                $(
                    assert!(scanner_iter.next().is_some_and(|tok| tok.ty == $ty));
                )*
                assert!(scanner_iter.next().is_none());
            }
        };
    }

    #[test]
    fn test_operators() {
        assert_tokens! {
            "1 + / 3 -",
            TokenTy::Num(1.0),
            TokenTy::Plus,
            TokenTy::Slash,
            TokenTy::Num(3.0),
            TokenTy::Minus,
        }
    }

    #[test]
    fn test_string_tokens() {
        assert_tokens! {
            r#"1 +"bruh moment""#,
            TokenTy::Num(1.0),
            TokenTy::Plus,
            TokenTy::Str("bruh moment".to_string()),
        }
    }

    #[test]
    fn test_idents() {
        assert_tokens! {
            "i _what _some - \n bruh \"some\"",
            TokenTy::Ident("i".to_string()),
            TokenTy::Ident("_what".to_string()),
            TokenTy::Ident("_some".to_string()),
            TokenTy::Minus,
            TokenTy::Ident("bruh".to_string()),
            TokenTy::Str("some".to_string()),
        }
    }

    #[test]
    fn test_packed_tokens() {
        assert_tokens! {
            "packed+some 1-/32 \"what\"=bruh",
            TokenTy::Ident("packed".to_string()),
            TokenTy::Plus,
            TokenTy::Ident("some".to_string()),
            TokenTy::Num(1.0),
            TokenTy::Minus,
            TokenTy::Slash,
            TokenTy::Num(32.0),
            TokenTy::Str("what".to_string()),
            TokenTy::Eq,
            TokenTy::Ident("bruh".to_string()),
        }
    }

    #[test]
    #[should_panic(expected = "error: could not parse number at line: 0")]
    fn test_invalid_num() {
        Scanner::new("1.2.3").into_iter().for_each(|_t| ());
    }

    #[test]
    fn test_only_whitespace() {
        assert_tokens! {
            " \n  \r\n \t",
        }
    }
}
