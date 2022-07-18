// Copyright 2022 Joshua Wong.
// SPDX-License-Identifier: Apache-2.0 OR MIT

use std::ops::{Add, Div, Mul, Neg, Not, Sub};

use crate::ast::{AstFolder, Binary, BinaryOp, Literal, Unary, UnaryOp};

struct Interpreter;

#[derive(Debug, Clone, PartialEq, Default)]
pub enum LoxTerm {
    Number(f64),
    String(String),
    Boolean(bool),
    #[default]
    Nil,
}

impl Add for LoxTerm {
    type Output = LoxTerm;

    fn add(self, other: LoxTerm) -> LoxTerm {
        match (self, other) {
            (LoxTerm::Number(n), LoxTerm::Number(m)) => LoxTerm::Number(n + m),
            _ => LoxTerm::Nil,
        }
    }
}

impl Sub for LoxTerm {
    type Output = LoxTerm;

    fn sub(self, other: LoxTerm) -> LoxTerm {
        match (self, other) {
            (LoxTerm::Number(n), LoxTerm::Number(m)) => LoxTerm::Number(n - m),
            _ => LoxTerm::Nil,
        }
    }
}

impl Mul for LoxTerm {
    type Output = LoxTerm;

    fn mul(self, other: LoxTerm) -> LoxTerm {
        match (self, other) {
            (LoxTerm::Number(n), LoxTerm::Number(m)) => LoxTerm::Number(n * m),
            _ => LoxTerm::Nil,
        }
    }
}

impl Div for LoxTerm {
    type Output = LoxTerm;

    fn div(self, other: LoxTerm) -> LoxTerm {
        match (self, other) {
            (LoxTerm::Number(n), LoxTerm::Number(m)) => LoxTerm::Number(n / m),
            _ => LoxTerm::Nil,
        }
    }
}

impl Not for LoxTerm {
    type Output = LoxTerm;

    fn not(self) -> LoxTerm {
        match self {
            LoxTerm::Boolean(b) => LoxTerm::Boolean(!b),
            _ => LoxTerm::Nil,
        }
    }
}

impl Neg for LoxTerm {
    type Output = LoxTerm;

    fn neg(self) -> LoxTerm {
        match self {
            LoxTerm::Number(n) => LoxTerm::Number(-n),
            _ => LoxTerm::Nil,
        }
    }
}

impl AstFolder for Interpreter {
    type Output = LoxTerm;

    fn fold_binary_expr(&mut self, expr: &Binary) -> Self::Output {
        let left = self.fold_expr(&expr.left);
        let right = self.fold_expr(&expr.right);
        match expr.op {
            BinaryOp::Add => left + right,
            BinaryOp::Sub => left - right,
            BinaryOp::Mul => left * right,
            BinaryOp::Div => left / right,
            _ => unimplemented!(),
        }
    }

    fn fold_literal_expr(&mut self, expr: &Literal) -> Self::Output {
        match expr {
            Literal::Number(n) => LoxTerm::Number(*n),
            Literal::String(s) => LoxTerm::String(s.clone()),
            Literal::Boolean(b) => LoxTerm::Boolean(*b),
            Literal::Nil => LoxTerm::Nil,
        }
    }

    fn fold_unary_expr(&mut self, expr: &Unary) -> Self::Output {
        match expr.op {
            UnaryOp::Neg => -self.fold_expr(&expr.right),
            UnaryOp::Not => !self.fold_expr(&expr.right),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::ast::Expr;

    use super::*;
    use pretty_assertions::{assert_eq, assert_ne};

    #[test]
    fn test_add() {
        assert_eq!(
            Interpreter.fold_binary_expr(&Binary {
                left: Box::new(Expr::Literal(Literal::Number(1.0))),
                op: BinaryOp::Add,
                right: Box::new(Expr::Literal(Literal::Number(2.0))),
            }),
            LoxTerm::Number(3.0)
        );

        assert_ne!(
            Interpreter.fold_binary_expr(&Binary {
                left: Box::new(Expr::Literal(Literal::Number(1.0))),
                op: BinaryOp::Add,
                right: Box::new(Expr::Literal(Literal::String("3".to_string()))),
            }),
            LoxTerm::Number(4.0)
        );
    }
}
