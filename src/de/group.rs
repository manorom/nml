use super::{derived, NamelistError, NamelistGroup};

impl serde::de::Error for NamelistError {
    fn custom<T>(msg: T) -> Self
    where
        T: std::fmt::Display,
    {
        NamelistError::custom(msg.to_string())
    }
}

/// A structure that deserializes a Namelist Group into Rust values. This
/// structure owns the parsed Namelist Group.
#[derive(Debug)]
pub struct GroupDeserializer {
    parsed_group: NamelistGroup,
}

impl GroupDeserializer {
    pub(crate) fn new(namelist: NamelistGroup) -> GroupDeserializer {
        GroupDeserializer {
            parsed_group: namelist,
        }
    }

    /// Returns the group name of the parsed Namelist
    pub fn name(&self) -> &str {
        &self.parsed_group.group_name
    }
}

impl<'de> serde::de::Deserializer<'de> for GroupDeserializer {
    type Error = NamelistError;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        self.deserialize_map(visitor)
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_map(derived::ItemMapAccess::new(self.parsed_group.items))
    }

    fn deserialize_struct<V>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        self.deserialize_map(visitor)
    }

    fn deserialize_newtype_struct<V>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_newtype_struct(self)
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct seq tuple
        tuple_struct enum identifier ignored_any
    }
}

/// A structure that deserializes a Namelist group into Rust values. This
/// structure only holds a reference of the parse namelist group.
#[derive(Debug, Clone, Copy)]
pub struct GroupRefDeserializer<'a>(&'a NamelistGroup);

impl<'a> GroupRefDeserializer<'a> {
    pub(crate) fn new(namelist_group: &'a NamelistGroup) -> GroupRefDeserializer<'a> {
        GroupRefDeserializer(namelist_group)
    }

    /// Returns the group name of the parsed Namelist
    pub fn name(&self) -> &str {
        &self.0.group_name
    }
}

impl<'de, 'a> serde::de::Deserializer<'de> for GroupRefDeserializer<'a> {
    type Error = NamelistError;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        self.deserialize_map(visitor)
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_map(derived::BorrowedItemMapAccess::new(&self.0.items))
    }

    fn deserialize_struct<V>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        self.deserialize_map(visitor)
    }

    fn deserialize_newtype_struct<V>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_newtype_struct(self)
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct seq tuple
        tuple_struct enum identifier ignored_any
    }
}
