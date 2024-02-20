use std::num::ParseFloatError;
use std::num::ParseIntError;
use std::string::FromUtf8Error;

use crate::reader::Position;

#[derive(Debug)]
enum ErrorImpl {
    ParseError(String, Position),
    UnexpectedEOF(Position),
    ItemAlreadyAssigned(String, Position),
    IndexAlreadyAssigned(usize, Position),
    UnsupportedSerialization,
    NonContainerOnTopLevel,
    Serde(String),
}

#[derive(Debug)]
pub struct Error(Box<ErrorImpl>);

impl Error {
    pub(crate) fn unexpected_token(found: &str, expected: &str, position: Position) -> Error {
        Error(Box::new(ErrorImpl::ParseError(
            format!("Expected {}, found '{}'", expected, found),
            position,
        )))
    }

    pub(crate) fn unexpected_byte(byte: u8, expected: &str, position: Position) -> Error {
        Error(Box::new(ErrorImpl::ParseError(
            format!("Unexpected byte {} expected {}", expected, byte),
            position,
        )))
    }

    pub(crate) fn parse_int(err: ParseIntError, position: Position) -> Error {
        Error(Box::new(ErrorImpl::ParseError(
            format!("{}", err),
            position,
        )))
    }

    pub(crate) fn parse_float(err: ParseFloatError, position: Position) -> Error {
        Error(Box::new(ErrorImpl::ParseError(
            format!("{}", err),
            position,
        )))
    }

    pub(crate) fn parse_index(err: ParseIntError, position: Position) -> Error {
        Error(Box::new(ErrorImpl::ParseError(
            format!("Could not parse index: {}", err),
            position,
        )))
    }

    pub(crate) fn decode_utf8_str(err: FromUtf8Error, position: Position) -> Error {
        Error(Box::new(ErrorImpl::ParseError(
            format!("Could not decode string: {}", err),
            position,
        )))
    }

    pub(crate) fn unexpted_eof(position: Position) -> Error {
        Error(Box::new(ErrorImpl::UnexpectedEOF(position)))
    }

    pub(crate) fn item_already_assigned(key: &str, position: Position) -> Error {
        Error(Box::new(ErrorImpl::ItemAlreadyAssigned(
            key.to_owned(),
            position,
        )))
    }

    pub(crate) fn index_already_assigned(idx: usize, position: Position) -> Error {
        Error(Box::new(ErrorImpl::IndexAlreadyAssigned(idx, position)))
    }

    pub(crate) fn unsupported_serialization() -> Error {
        Error(Box::new(ErrorImpl::UnsupportedSerialization))
    }

    pub(crate) fn non_container_on_top_level() -> Error {
        Error(Box::new(ErrorImpl::NonContainerOnTopLevel))
    }

    pub(crate) fn serde(string: String) -> Error {
        Error(Box::new(ErrorImpl::Serde(string)))
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0.as_ref() {
            ErrorImpl::ParseError(message, pos) => {
                write!(f, "{} at position {}", message, pos)
            }
            ErrorImpl::UnexpectedEOF(pos) => {
                write!(f, "Unexpected end of input at position {}", pos)
            }
            ErrorImpl::ItemAlreadyAssigned(item_name, pos) => {
                write!(
                    f,
                    "Item '{item_name}' assigned at position {pos} was already assigned previously"
                )
            }
            ErrorImpl::IndexAlreadyAssigned(idx, pos) => {
                write!(
                    f,
                    "Array index {idx} assigned at position {pos} was already assigned previously"
                )
            }
            ErrorImpl::NonContainerOnTopLevel => {
                write!(f, "Can only serialize struct and map into Namelist group")
            }
            ErrorImpl::UnsupportedSerialization => {
                write!(f, "The serialization of this type is not supported")
            }
            ErrorImpl::Serde(string) => {
                write!(f, "Serde error: {string}")
            }
        }
    }
}

impl std::error::Error for Error {}
