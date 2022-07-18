// Copyright 2022 Joshua Wong.
// SPDX-License-Identifier: Apache-2.0 OR MIT

use std::ops::{Add, Div, Mul, Neg, Not, Sub};

use crate::ast::{AstFolder, Binary, BinaryOp, Literal, Unary, UnaryOp};

struct Interpreter;

type LoxOperation = Result<LoxTerm, &'static str>;

#[derive(Debug, Clone, PartialEq, Default)]
pub enum LoxTerm {
    Number(f64),
    String(String),
    Boolean(bool),
    #[default]
    Nil,
}

impl Add for LoxTerm {
    type Output = LoxOperation;

    fn add(self, other: LoxTerm) -> Self::Output {
        match (self, other) {
            (LoxTerm::Number(n), LoxTerm::Number(m)) => Ok(LoxTerm::Number(n + m)),
            _ => Err("invalid addition"),
        }
    }
}

impl Sub for LoxTerm {
    type Output = LoxOperation;

    fn sub(self, other: LoxTerm) -> Self::Output {
        match (self, other) {
            (LoxTerm::Number(n), LoxTerm::Number(m)) => Ok(LoxTerm::Number(n - m)),
            _ => Err("invalid subtraction"),
        }
    }
}

impl Mul for LoxTerm {
    type Output = LoxOperation;

    fn mul(self, other: LoxTerm) -> Self::Output {
        match (self, other) {
            (LoxTerm::Number(n), LoxTerm::Number(m)) => Ok(LoxTerm::Number(n * m)),
            _ => Err("invalid multiplication"),
        }
    }
}

impl Div for LoxTerm {
    type Output = LoxOperation;

    fn div(self, other: LoxTerm) -> Self::Output {
        match (self, other) {
            (LoxTerm::Number(n), LoxTerm::Number(m)) => Ok(LoxTerm::Number(n / m)),
            _ => Err("invalid division"),
        }
    }
}

impl Not for LoxTerm {
    type Output = LoxOperation;

    fn not(self) -> Self::Output {
        match self {
            LoxTerm::Boolean(b) => Ok(LoxTerm::Boolean(!b)),
            _ => Err("not only works on booleans"),
        }
    }
}

impl Neg for LoxTerm {
    type Output = LoxOperation;

    fn neg(self) -> Self::Output {
        match self {
            LoxTerm::Number(n) => Ok(LoxTerm::Number(-n)),
            _ => Err("negation only works on numbers"),
        }
    }
}

impl AstFolder for Interpreter {
    type Output = LoxOperation;

    fn fold_binary_expr(&mut self, expr: &Binary) -> Self::Output {
        let left = self.fold_expr(&expr.left)?;
        let right = self.fold_expr(&expr.right)?;
        match expr.op {
            BinaryOp::Add => left + right,
            BinaryOp::Sub => left - right,
            BinaryOp::Mul => left * right,
            BinaryOp::Div => left / right,
            _ => unimplemented!(),
        }
    }

    fn fold_literal_expr(&mut self, expr: &Literal) -> Self::Output {
        Ok(match expr {
            Literal::Number(n) => LoxTerm::Number(*n),
            Literal::String(s) => LoxTerm::String(s.clone()),
            Literal::Boolean(b) => LoxTerm::Boolean(*b),
            Literal::Nil => LoxTerm::Nil,
        })
    }

    fn fold_unary_expr(&mut self, expr: &Unary) -> Self::Output {
        self.fold_expr(&expr.right)
            .and_then(|operand| match expr.op {
                UnaryOp::Neg => -operand,
                UnaryOp::Not => !operand,
            })
    }
}

#[cfg(test)]
mod test {
    use crate::ast::Expr;

    use super::*;
    use pretty_assertions::{assert_eq, assert_ne};

    #[test]
    fn test_add_numeric() {
        assert_eq!(
            Interpreter.fold_binary_expr(&Binary {
                left: Box::new(Expr::Literal(Literal::Number(1.0))),
                op: BinaryOp::Add,
                right: Box::new(Expr::Literal(Literal::Number(2.0))),
            }),
            Ok(LoxTerm::Number(3.0))
        );

        assert_ne!(
            Interpreter.fold_binary_expr(&Binary {
                left: Box::new(Expr::Literal(Literal::Number(1.0))),
                op: BinaryOp::Add,
                right: Box::new(Expr::Literal(Literal::String("3".to_string()))),
            }),
            Ok(LoxTerm::Number(4.0))
        );

        assert_eq!(
            Interpreter.fold_unary_expr(&Unary {
                op: UnaryOp::Neg,
                right: Box::new(Expr::Literal(Literal::Number(1.0))),
            }),
            Ok(LoxTerm::Number(-1.0))
        );

        assert_eq!(
            Interpreter.fold_unary_expr(&Unary {
                op: UnaryOp::Not,
                right: Box::new(Expr::Literal(Literal::Number(1.0))),
            }),
            Err("not only works on booleans")
        );
    }
}
