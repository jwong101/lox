// Copyright 2022 Joshua Wong.
// SPDX-License-Identifier: Apache-2.0 OR MIT

use std::{
    cmp::Ordering,
    ops::{Add, Div, Mul, Neg, Not, Sub},
};

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

impl From<bool> for LoxTerm {
    fn from(b: bool) -> Self {
        LoxTerm::Boolean(b)
    }
}

impl PartialOrd for LoxTerm {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (LoxTerm::Number(n), LoxTerm::Number(m)) => n.partial_cmp(m),
            (LoxTerm::String(s), LoxTerm::String(m)) => s.partial_cmp(m),
            (LoxTerm::Boolean(b), LoxTerm::Boolean(m)) => b.partial_cmp(m),
            (LoxTerm::Nil, LoxTerm::Nil) => Some(Ordering::Equal),
            _ => None,
        }
    }
}

impl LoxTerm {
    fn coerce_to_bool(&self) -> bool {
        match self {
            Self::Boolean(b) => *b,
            Self::Number(n) => n.partial_cmp(&0.0).is_some_and(|o| o.is_ne()),
            Self::String(s) => !s.is_empty(),
            Self::Nil => false,
        }
    }

    fn try_cmp_with(
        &self,
        other: &Self,
        ordering_pred: impl FnOnce(Ordering) -> bool,
    ) -> LoxOperation {
        self.partial_cmp(other)
            .map(|ord| Self::Boolean(ordering_pred(ord)))
            .ok_or("could not compare types")
    }
}

impl Add for LoxTerm {
    type Output = LoxOperation;

    fn add(self, other: LoxTerm) -> Self::Output {
        match (self, &other) {
            (Self::Number(lhs), Self::Number(rhs)) => Ok(Self::Number(lhs + rhs)),
            (Self::Number(lhs), Self::String(rhs)) => Ok(Self::String(lhs.to_string() + rhs)),
            (Self::String(lhs), Self::String(rhs)) => Ok(Self::String(lhs + rhs)),
            (Self::String(lhs), Self::Number(rhs)) => Ok(Self::String(lhs + &rhs.to_string())),
            _ => Err("invalid addition"),
        }
    }
}

impl Sub for LoxTerm {
    type Output = LoxOperation;

    fn sub(self, other: LoxTerm) -> Self::Output {
        match (self, other) {
            (Self::Number(lhs), Self::Number(rhs)) => Ok(Self::Number(lhs - rhs)),
            _ => Err("invalid subtraction"),
        }
    }
}

impl Mul for LoxTerm {
    type Output = LoxOperation;

    fn mul(self, other: LoxTerm) -> Self::Output {
        match (self, other) {
            (Self::Number(lhs), Self::Number(rhs)) => Ok(Self::Number(lhs * rhs)),
            _ => Err("invalid multiplication"),
        }
    }
}

impl Div for LoxTerm {
    type Output = LoxOperation;

    fn div(self, other: LoxTerm) -> Self::Output {
        match (self, other) {
            (Self::Number(lhs), Self::Number(rhs)) => Ok(Self::Number(lhs / rhs)),
            _ => Err("invalid division"),
        }
    }
}

impl Not for LoxTerm {
    type Output = LoxOperation;

    fn not(self) -> Self::Output {
        Ok(Self::Boolean(!self.coerce_to_bool()))
    }
}

impl Neg for LoxTerm {
    type Output = LoxOperation;

    fn neg(self) -> Self::Output {
        match self {
            Self::Number(n) => Ok(LoxTerm::Number(-n)),
            Self::Nil | Self::String(_) | Self::Boolean(_) => Err("negation only works on numbers"),
        }
    }
}

impl AstFolder for Interpreter {
    type Output = LoxOperation;

    fn fold_literal_expr(&mut self, expr: &Literal) -> Self::Output {
        Ok(match expr {
            Literal::Number(n) => LoxTerm::Number(*n),
            Literal::String(s) => LoxTerm::String(s.clone()),
            Literal::Boolean(b) => LoxTerm::Boolean(*b),
            Literal::Nil => LoxTerm::Nil,
        })
    }

    fn fold_binary_expr(&mut self, expr: &Binary) -> Self::Output {
        let left = self.fold_expr(&expr.left)?;
        let right = self.fold_expr(&expr.right)?;
        match expr.op {
            BinaryOp::Add => left + right,
            BinaryOp::Sub => left - right,
            BinaryOp::Mul => left * right,
            BinaryOp::Div => left / right,
            BinaryOp::Eq => left.try_cmp_with(&right, Ordering::is_eq),
            BinaryOp::NotEq => left.try_cmp_with(&right, Ordering::is_ne),
            BinaryOp::Gt => left.try_cmp_with(&right, Ordering::is_gt),
            BinaryOp::Ge => left.try_cmp_with(&right, Ordering::is_ge),
            BinaryOp::Lt => left.try_cmp_with(&right, Ordering::is_lt),
            BinaryOp::Le => left.try_cmp_with(&right, Ordering::is_le),
        }
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
            Ok(LoxTerm::Boolean(false))
        );
    }

    #[test]
    fn test_cmp() {
        assert_eq!(
            Interpreter.fold_binary_expr(&Binary {
                left: Box::new(Expr::Literal(Literal::Number(1.0))),
                op: BinaryOp::Eq,
                right: Box::new(Expr::Literal(Literal::Number(1.0))),
            }),
            Ok(LoxTerm::Boolean(true))
        );

        assert_eq!(
            Interpreter.fold_binary_expr(&Binary {
                left: Box::new(Expr::Literal(Literal::Number(1.0))),
                op: BinaryOp::NotEq,
                right: Box::new(Expr::Literal(Literal::Number(1.0))),
            }),
            Ok(LoxTerm::Boolean(false))
        );

        assert_eq!(
            Interpreter.fold_binary_expr(&Binary {
                left: Box::new(Expr::Literal(Literal::Number(1.0))),
                op: BinaryOp::Lt,
                right: Box::new(Expr::Literal(Literal::Number(2.0))),
            }),
            Ok(LoxTerm::Boolean(true))
        );

        assert_eq!(
            Interpreter.fold_binary_expr(&Binary {
                left: Box::new(Expr::Literal(Literal::Number(1.0))),
                op: BinaryOp::Le,
                right: Box::new(Expr::Literal(Literal::Number(2.0))),
            }),
            Ok(LoxTerm::Boolean(true))
        );
        assert_eq!(
            Interpreter.fold_binary_expr(&Binary {
                left: Box::new(Expr::Literal(Literal::Number(1.0))),
                op: BinaryOp::Ge,
                right: Box::new(Expr::Literal(Literal::Number(2.0))),
            }),
            Ok(LoxTerm::Boolean(false))
        );

        assert_eq!(
            Interpreter.fold_binary_expr(&Binary {
                left: Box::new(Expr::Literal(Literal::Number(1.0))),
                op: BinaryOp::Gt,
                right: Box::new(Expr::Literal(Literal::Number(2.0))),
            }),
            Ok(LoxTerm::Boolean(false))
        );
    }
}
