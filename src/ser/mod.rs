use crate::Error;

mod derived;
mod group;
mod item;

pub use group::GroupSerializer;

impl serde::ser::Error for Error {
    fn custom<T>(msg: T) -> Self
    where
        T: std::fmt::Display,
    {
        Error::serde(msg.to_string())
    }
}
