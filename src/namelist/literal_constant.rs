use super::Item;

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralConstant {
    Null,
    String(String),
    Int(i64),
    Float(f64),
    Bool(bool),
}

impl LiteralConstant {
    pub(crate) fn is_type_compatible(&self, other: &LiteralConstant) -> Option<bool> {
        use LiteralConstant::*;
        match (self, other) {
            (Null, _) | (_, Null) => None,
            (String { .. }, String { .. }) => Some(true),
            (Int(_), Int(_)) => Some(true),
            (Float(_), Float(_)) => Some(true),
            (Bool(_), Bool(_)) => Some(true),
            _ => Some(false),
        }
    }

    pub fn into_item(self) -> Item {
        Item::Literal(self)
    }
}

impl std::fmt::Display for LiteralConstant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LiteralConstant::Null => write!(f, " "),
            LiteralConstant::Bool(true) => write!(f, ".TRUE."),
            LiteralConstant::Bool(false) => write!(f, ".FALSE."),
            LiteralConstant::Int(i) => write!(f, "{i}"),
            LiteralConstant::Float(n) => write!(f, "{n}"),
            LiteralConstant::String(s) => write!(f, "'{s}'"),
        }
    }
}
