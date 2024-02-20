use super::Error;
use crate::namelist::Item;
use crate::namelist::LiteralConstant;

use super::array::{ArraySeqAccess, BorrowedArraySeqAccess};
use super::derived::{BorrowedItemMapAccess, ItemMapAccess};

pub fn deserialize_literal_constant<'de, V>(
    lit: &LiteralConstant,
    visitor: V,
) -> Result<V::Value, Error>
where
    V: serde::de::Visitor<'de>,
{
    match lit {
        LiteralConstant::Null => visitor.visit_none(),
        LiteralConstant::Bool(b) => visitor.visit_bool(*b),
        LiteralConstant::Float(f) => visitor.visit_f64(*f),
        LiteralConstant::String(s) => visitor.visit_str(s),
        LiteralConstant::Int(i) => visitor.visit_i64(*i),
    }
}

pub struct BorrowedLiteralDeserializer<'a>(pub &'a LiteralConstant);

impl<'de, 'a> serde::Deserializer<'de> for BorrowedLiteralDeserializer<'a> {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        deserialize_literal_constant(self.0, visitor)
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char seq str string
        bytes byte_buf unit unit_struct newtype_struct tuple option
        tuple_struct map struct enum identifier ignored_any
    }
}

pub struct BorrowedItemDeserializer<'a> {
    pub item: &'a Item,
}

impl<'a> BorrowedItemDeserializer<'a> {
    pub fn new(item: &'a Item) -> Self {
        BorrowedItemDeserializer { item }
    }
}

impl<'de, 'a> serde::Deserializer<'de> for BorrowedItemDeserializer<'a> {
    type Error = Error;
    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        if self.item.is_null() {
            visitor.visit_none()
        } else {
            visitor.visit_some(self)
        }
    }

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.item {
            Item::Derived(m) => visitor.visit_map(BorrowedItemMapAccess::new(m)),
            Item::Array(array) => visitor.visit_seq(BorrowedArraySeqAccess::new(array.into_iter())),
            Item::Literal(lit) => deserialize_literal_constant(lit, visitor),
        }
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char seq str string
        bytes byte_buf unit unit_struct newtype_struct tuple
        tuple_struct map struct enum identifier ignored_any
    }
}

pub struct ItemDeserializer {
    item: Item,
}

impl ItemDeserializer {
    pub fn new(item: Item) -> ItemDeserializer {
        ItemDeserializer { item }
    }
}

impl<'de> serde::Deserializer<'de> for ItemDeserializer {
    type Error = Error;

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        if self.item.is_null() {
            visitor.visit_none()
        } else {
            visitor.visit_some(self)
        }
    }

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.item {
            Item::Derived(m) => visitor.visit_map(ItemMapAccess::new(m)),
            Item::Array(array) => visitor.visit_seq(ArraySeqAccess::new(array)),
            Item::Literal(lit) => match lit {
                LiteralConstant::Null => visitor.visit_none(),
                LiteralConstant::Bool(b) => visitor.visit_bool(b),
                LiteralConstant::Float(f) => visitor.visit_f64(f),
                LiteralConstant::String(s) => visitor.visit_string(s),
                LiteralConstant::Int(i) => visitor.visit_i64(i),
            },
        }
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char seq str string
        bytes byte_buf unit unit_struct newtype_struct tuple
        tuple_struct map struct enum identifier ignored_any
    }
}
