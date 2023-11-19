use crate::error::NamelistError;
use crate::formatter::NamelistFormatter;
use crate::parser::Parser;
use std::str::FromStr;

mod array;
mod item;
mod literal_constant;

pub type Map<K, V> = std::collections::BTreeMap<K, V>;
pub type MapIntoIter<K, V> = std::collections::btree_map::IntoIter<K, V>;

pub(crate) use array::{Array, ArrayIter};
pub(crate) use item::Item;
pub(crate) use literal_constant::LiteralConstant;

#[derive(Debug)]
pub struct NamelistGroup {
    pub(crate) group_name: String,
    pub(crate) items: Map<String, Item>,
}

impl NamelistGroup {
    pub(crate) fn new(group_name: impl Into<String>, items: Map<String, Item>) -> NamelistGroup {
        NamelistGroup {
            group_name: group_name.into(),
            items,
        }
    }
}

impl std::ops::Deref for NamelistGroup {
    type Target = Map<String, Item>;

    fn deref(&self) -> &Self::Target {
        &self.items
    }
}

impl FromStr for NamelistGroup {
    type Err = NamelistError;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        Parser::new(s).parse()
    }
}

impl std::fmt::Display for NamelistGroup {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        NamelistFormatter::new().fmt_namelist(self, f)
    }
}
