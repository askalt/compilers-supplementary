use std::num::ParseIntError;

use thiserror::Error;

/// Describers an error that can be returned from lexer.
/// TODO: specify lineno and position in error.
#[derive(Error, Debug)]
pub enum Error {
    #[error("internal error: {0}")]
    Internal(String),
    #[error("unexpected tokens starting from: {}", &.0[0..(.0.len().min(40))])]
    UnexepectedTokens(String),
    #[error("{0}")]
    ParseIntError(ParseIntError),
    #[error("parser error")]
    ParseError,
}

impl Error {
    pub fn internal<E: std::error::Error>(e: E) -> Self {
        Self::Internal(e.to_string())
    }
}

/// Result type.
pub type Result<T> = std::result::Result<T, Error>;
