use crate::{error::Error, namelist::NamelistGroup};

mod array;
mod derived;
mod group;
mod item;

pub use group::{GroupDeserializer, GroupRefDeserializer};
