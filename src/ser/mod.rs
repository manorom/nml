use crate::NamelistError;

mod derived;
mod group;
mod item;

pub use group::GroupSerializer;

impl serde::ser::Error for NamelistError {
    fn custom<T>(msg: T) -> Self
    where
        T: std::fmt::Display,
    {
        NamelistError::custom(msg.to_string())
    }
}
