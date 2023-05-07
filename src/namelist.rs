use crate::error::NamelistError;
use crate::formatter::NamelistFormatter;
use crate::parser::Parser;
use crate::value::{Map, Value};
use std::str::FromStr;

#[derive(Debug)]
pub struct Namelist {
    pub group_name: String,
    pub(crate) items: Map<String, Value>,
}

impl Namelist {
    pub(crate) fn new(group_name: impl Into<String>, items: Map<String, Value>) -> Namelist {
        Namelist {
            group_name: group_name.into(),
            items,
        }
    }
}

impl<'a> std::ops::Deref for Namelist {
    type Target = Map<String, Value>;

    fn deref(&self) -> &Self::Target {
        &self.items
    }
}

impl FromStr for Namelist {
    type Err = NamelistError;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        Parser::new(s).parse()
    }
}

impl std::fmt::Display for Namelist {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        NamelistFormatter::new().fmt_namelist(self, f)
    }
}
