use std::num::ParseFloatError;
use std::num::ParseIntError;

use crate::reader::Position;

#[derive(Debug, PartialEq, Clone)]
pub enum NamelistError {
    UnexpectedToken {
        found: String,
        expected: String,
        position: Position,
    },
    ParseInt {
        error: ParseIntError,
        position: Position,
    },
    ParseFloat {
        error: ParseFloatError,
        position: Position,
    },
    ParseIndex {
        error: ParseIntError,
        position: Position,
    },
    UnexpectedEOF(Position),
    SyntaxError(Position),
    AlreadyAssigned {
        key: String,
        position: Position,
    },
    AlreadyAssignedIndexed {
        idx: usize,
        position: Position,
    },
    UnsupportedSerialization,
    Custom(String),
}

impl NamelistError {
    pub(crate) fn unexpected_byte(found: u8, expected: &str, position: Position) -> NamelistError {
        NamelistError::UnexpectedToken {
            found: String::from_utf8_lossy(&[found]).into_owned(),
            expected: expected.to_owned(),
            position,
        }
    }

    pub(crate) fn unexpected_token(
        found: &str,
        expected: &str,
        position: Position,
    ) -> NamelistError {
        NamelistError::UnexpectedToken {
            found: found.to_owned(),
            expected: expected.to_owned(),
            position,
        }
    }

    pub(crate) fn parse_int(error: ParseIntError, position: Position) -> NamelistError {
        NamelistError::ParseInt { error, position }
    }

    pub(crate) fn parse_float(error: ParseFloatError, position: Position) -> NamelistError {
        NamelistError::ParseFloat { error, position }
    }

    pub(crate) fn parse_index(error: ParseIntError, position: Position) -> NamelistError {
        NamelistError::ParseIndex { error, position }
    }

    pub(crate) fn custom(string: String) -> Self {
        NamelistError::Custom(string)
    }
}

impl std::fmt::Display for NamelistError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NamelistError::UnexpectedToken {
                found,
                expected,
                position,
            } => write!(
                f,
                "Expected {expected} at position {position}. Found {found} instead"
            )?,
            NamelistError::UnexpectedEOF(position) => {
                write!(f, "Unexpected end of file at position {position}")?
            }
            NamelistError::ParseInt { error, position } => {
                write!(f, "Failed to parse integer at position {position}: {error}")?
            }
            NamelistError::ParseFloat { error, position } => {
                write!(f, "Failed to parse float at position {position}: {error}")?
            }
            NamelistError::ParseIndex { error, position } => {
                write!(f, "Failed to parse index at position {position}: {error}")?
            }
            NamelistError::SyntaxError(position) => {
                write!(f, "Syntax error at position {position}")?
            }
            NamelistError::AlreadyAssigned { key, position } => {
                write!(f, "Repeated assignment to key {key} at position {position}")?
            }
            NamelistError::AlreadyAssignedIndexed { idx, position } => write!(
                f,
                "Repeated assignment to index {idx} at position {position}"
            )?,
            NamelistError::Custom(string) => write!(f, "{string}")?,
            NamelistError::UnsupportedSerialization => {
                write!(f, "serialization is not supported (yet)")?
            }
        }

        Ok(())
    }
}

impl std::error::Error for NamelistError {}
