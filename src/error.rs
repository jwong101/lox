// Copyright 2022 Joshua Wong.
// SPDX-License-Identifier: Apache-2.0
use std::error::Error;
use std::fmt;

#[derive(Debug)]
pub enum CompileError {
    LexerError(String),
    ParserError(String),
    InterpreterError(String),
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CompileError::LexerError(s) => write!(f, "LexerError: {}", s),
            CompileError::ParserError(s) => write!(f, "ParserError: {}", s),
            CompileError::InterpreterError(s) => write!(f, "InterpreterError: {}", s),
        }
    }
}

impl Error for CompileError {}
