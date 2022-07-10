// Copyright 2022 Joshua Wong.
// SPDX-License-Identifier: Apache-2.0 OR MIT

use std::iter::Peekable;

use crate::{
    ast::{Binary, Expr, Unary},
    scanner::{Token, TokenTy},
};

pub struct Parser<I: Iterator<Item = Token>> {
    tokens: Peekable<I>,
    #[allow(dead_code)]
    current: usize,
}

type ExprResult = Result<Expr, &'static str>;

impl<I: Iterator<Item = Token>> Parser<I> {
    pub fn new<T>(tokens: T) -> Self
    where
        T: IntoIterator<Item = Token, IntoIter = I>,
    {
        Parser {
            tokens: tokens.into_iter().peekable(),
            current: 0,
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Expr>, &'static str> {
        let mut statements = Vec::new();

        while !self.is_at_end() {
            statements.push(self.expression()?);
        }

        Ok(statements)
    }

    fn equality(&mut self) -> ExprResult {
        let mut expr = self.comparison()?;

        while let Some(Token { ty: op, .. }) = self
            .tokens
            .next_if(|tok| matches!(tok.ty, TokenTy::EqEq | TokenTy::BangEq))
        {
            let right = self.comparison()?;
            expr = Expr::Binary(Binary {
                left: Box::new(expr),
                op: op.try_into()?,
                right: Box::new(right),
            });
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> ExprResult {
        let mut expr = self.term()?;
        while let Some(Token { ty: op, .. }) = self.tokens.next_if(|tok| {
            matches!(
                tok.ty,
                TokenTy::Ge | TokenTy::Gt | TokenTy::Lt | TokenTy::Le
            )
        }) {
            let right = self.term()?;
            expr = Expr::Binary(Binary {
                left: Box::new(expr),
                op: op.try_into()?,
                right: Box::new(right),
            });
        }
        Ok(expr)
    }

    fn is_at_end(&mut self) -> bool {
        self.tokens.peek().is_none()
    }

    fn expression(&mut self) -> ExprResult {
        self.equality()
    }

    fn term(&mut self) -> ExprResult {
        let mut expr = self.factor()?;
        while let Some(Token { ty: op, .. }) = self
            .tokens
            .next_if(|tok| matches!(tok.ty, TokenTy::Minus | TokenTy::Plus))
        {
            let right = self.factor()?;
            expr = Expr::Binary(Binary {
                left: Box::new(expr),
                op: op.try_into()?,
                right: Box::new(right),
            });
        }
        Ok(expr)
    }

    fn factor(&mut self) -> ExprResult {
        let mut expr = self.unary()?;
        while let Some(Token { ty: op, .. }) = self
            .tokens
            .next_if(|tok| matches!(tok.ty, TokenTy::Star | TokenTy::Slash))
        {
            let right = self.unary()?;
            expr = Expr::Binary(Binary {
                left: Box::new(expr),
                op: op.try_into()?,
                right: Box::new(right),
            });
        }
        Ok(expr)
    }

    fn unary(&mut self) -> ExprResult {
        if let Some(Token { ty: op, .. }) = self
            .tokens
            .next_if(|tok| matches!(tok.ty, TokenTy::Bang | TokenTy::Minus))
        {
            let right = self.unary()?;
            return Ok(Expr::Unary(Unary {
                op: op.try_into()?,
                right: Box::new(right),
            }));
        }
        self.primary()
    }

    fn primary(&mut self) -> ExprResult {
        self.tokens
            .next_if(|tok| {
                matches!(
                    tok.ty,
                    TokenTy::Num(_)
                        | TokenTy::True
                        | TokenTy::False
                        | TokenTy::Nil
                        | TokenTy::Str(_)
                )
            })
            .and_then(|tok| Some(Expr::Literal(tok.ty.try_into().ok()?)))
            .ok_or("Expected primary expression")
    }
}

#[cfg(test)]
mod test {
    use std::vec;

    use crate::ast::{BinaryOp, Literal};

    use super::*;

    #[test]
    fn test_parser() {
        let tokens = vec![
            Token::new(0, TokenTy::Num(1.0)),
            Token::new(0, TokenTy::Plus),
            Token::new(0, TokenTy::Num(2.0)),
        ];
        let mut parser = Parser::new(tokens);
        let statements = parser.parse().unwrap();
        assert_eq!(statements.len(), 1);
        assert_eq!(
            statements[0],
            Expr::Binary(Binary {
                left: Box::new(Expr::Literal(Literal::Number(1.0))),
                op: BinaryOp::Plus,
                right: Box::new(Expr::Literal(Literal::Number(2.0))),
            })
        );
    }
}
