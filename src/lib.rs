mod constant;
mod constant_list;
mod error;
mod formatter;
mod namelist;
mod parser;
mod reader;
mod value;

pub use namelist::Namelist;

pub use error::NamelistError;

type Result<T> = std::result::Result<T, NamelistError>;
