use super::array::Array;
use super::literal_constant::LiteralConstant;
use super::Map;

#[derive(Debug, Clone)]
pub enum Item {
    Literal(LiteralConstant),
    Derived(Map<String, Item>),
    Array(Array),
}

impl Item {
    pub fn null() -> Item {
        Item::Literal(LiteralConstant::Null)
    }

    pub fn is_null(&self) -> bool {
        if let Item::Literal(LiteralConstant::Null) = self {
            true
        } else {
            false
        }
    }

    pub fn is_literal(&self) -> bool {
        match self {
            Item::Literal(..) => true,
            _ => false,
        }
    }

    pub fn is_derived(&self) -> bool {
        match self {
            Item::Derived(..) => true,
            _ => false,
        }
    }

    pub fn is_array(&self) -> bool {
        match self {
            Item::Array(..) => true,
            _ => false,
        }
    }
}
