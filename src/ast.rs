// Copyright 2022 Joshua Wong.
// SPDX-License-Identifier: Apache-2.0 OR MIT

use std::fmt::{self, Display};

use crate::scanner::TokenTy;

#[derive(Debug, Clone, PartialEq)]
pub struct Binary {
    pub left: Box<Expr>,
    pub op: BinaryOp,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Unary {
    pub op: UnaryOp,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Grouping(Box<Expr>),
    Unary(Unary),
    Binary(Binary),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    String(String),
    Number(f64),
    Boolean(bool),
    Nil,
}

impl Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::String(s) => write!(f, "{s}"),
            Literal::Number(n) => write!(f, "{n}"),
            Literal::Boolean(b) => write!(f, "{b}"),
            Literal::Nil => write!(f, "nil"),
        }
    }
}

impl TryFrom<TokenTy> for Literal {
    type Error = &'static str;

    fn try_from(ty: TokenTy) -> Result<Self, Self::Error> {
        match ty {
            TokenTy::Str(s) => Ok(Literal::String(s)),
            TokenTy::Num(num) => Ok(Literal::Number(num)),
            TokenTy::True => Ok(Literal::Boolean(true)),
            TokenTy::False => Ok(Literal::Boolean(false)),
            TokenTy::Nil => Ok(Literal::Nil),
            _ => Err("invalid literal"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Bang,
    Minus,
}

impl UnaryOp {
    pub fn as_str(&self) -> &'static str {
        match self {
            UnaryOp::Bang => "!",
            UnaryOp::Minus => "-",
        }
    }
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOp::Bang => write!(f, "!"),
            UnaryOp::Minus => write!(f, "-"),
        }
    }
}

impl TryFrom<TokenTy> for UnaryOp {
    type Error = &'static str;

    fn try_from(t: TokenTy) -> Result<Self, Self::Error> {
        match t {
            TokenTy::Bang => Ok(UnaryOp::Bang),
            TokenTy::Minus => Ok(UnaryOp::Minus),
            _ => Err("not a unary operator"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Plus,
    Minus,
    Star,
    Slash,
    BangEq,
    EqEq,
    Le,
    Lt,
    Ge,
    Gt,
}

impl TryFrom<TokenTy> for BinaryOp {
    type Error = &'static str;
    fn try_from(value: TokenTy) -> Result<Self, Self::Error> {
        match value {
            TokenTy::Plus => Ok(BinaryOp::Plus),
            TokenTy::Minus => Ok(BinaryOp::Minus),
            TokenTy::Star => Ok(BinaryOp::Star),
            TokenTy::Slash => Ok(BinaryOp::Slash),
            TokenTy::BangEq => Ok(BinaryOp::BangEq),
            TokenTy::EqEq => Ok(BinaryOp::EqEq),
            TokenTy::Le => Ok(BinaryOp::Le),
            TokenTy::Lt => Ok(BinaryOp::Lt),
            TokenTy::Ge => Ok(BinaryOp::Ge),
            TokenTy::Gt => Ok(BinaryOp::Gt),
            _ => Err("not a binary operator"),
        }
    }
}

impl BinaryOp {
    pub fn as_str(&self) -> &'static str {
        match self {
            BinaryOp::Plus => "+",
            BinaryOp::Minus => "-",
            BinaryOp::Star => "*",
            BinaryOp::Slash => "/",
            BinaryOp::BangEq => "!=",
            BinaryOp::EqEq => "==",
            BinaryOp::Le => "<=",
            BinaryOp::Lt => "<",
            BinaryOp::Ge => ">=",
            BinaryOp::Gt => ">",
        }
    }
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOp::Plus => write!(f, "+"),
            BinaryOp::Minus => write!(f, "-"),
            BinaryOp::Star => write!(f, "*"),
            BinaryOp::Slash => write!(f, "/"),
            BinaryOp::BangEq => write!(f, "!="),
            BinaryOp::EqEq => write!(f, "=="),
            BinaryOp::Le => write!(f, "<="),
            BinaryOp::Lt => write!(f, "<"),
            BinaryOp::Ge => write!(f, ">="),
            BinaryOp::Gt => write!(f, ">"),
        }
    }
}

trait AstVisitor {
    fn visit_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Literal(literal) => self.visit_literal_expr(literal),
            Expr::Grouping(grouping) => self.visit_grouping_expr(grouping),
            Expr::Unary(unary) => self.visit_unary_expr(unary),
            Expr::Binary(binary) => self.visit_binary_expr(binary),
        }
    }
    fn visit_literal_expr(&mut self, expr: &Literal);
    fn visit_binary_expr(&mut self, expr: &Binary);
    fn visit_unary_expr(&mut self, expr: &Unary);
    #[inline]
    fn visit_grouping_expr(&mut self, expr: &Expr) {
        self.visit_expr(expr);
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parenthized_visitor() {
        struct ParentheizedVisitor {
            output: String,
        }
        impl AstVisitor for ParentheizedVisitor {
            fn visit_literal_expr(&mut self, expr: &Literal) {
                self.output.push_str(&expr.to_string());
            }
            fn visit_binary_expr(&mut self, expr: &Binary) {
                self.output.push('(');
                self.output.push_str(expr.op.as_str());
                self.output.push(' ');
                self.visit_expr(&expr.left);
                self.output.push(' ');
                self.visit_expr(&expr.right);
                self.output.push(')');
            }
            fn visit_unary_expr(&mut self, expr: &Unary) {
                self.output.push('(');
                self.output.push_str(expr.op.as_str());
                self.output.push(' ');
                self.visit_expr(&expr.right);
                self.output.push(')');
            }
            fn visit_grouping_expr(&mut self, expr: &Expr) {
                self.output.push('(');
                self.visit_expr(expr);
                self.output.push(')');
            }
        }

        let mut visitor = ParentheizedVisitor {
            output: String::new(),
        };
        let expr = Expr::Binary(Binary {
            left: Box::new(Expr::Literal(Literal::Number(1.0))),
            op: BinaryOp::Plus,
            right: Box::new(Expr::Literal(Literal::Number(2.0))),
        });

        visitor.visit_expr(&expr);
        assert_eq!(visitor.output, "(+ 1 2)");
        visitor.output.clear();

        let expr = Expr::Unary(Unary {
            op: UnaryOp::Bang,
            right: Box::new(Expr::Literal(Literal::Boolean(true))),
        });
        visitor.visit_expr(&expr);
        assert_eq!(visitor.output, "(! true)");
    }
}
