use super::{derived, NamelistError, NamelistGroup};

impl serde::de::Error for NamelistError {
    fn custom<T>(msg: T) -> Self
    where
        T: std::fmt::Display,
    {
        NamelistError::custom(msg.to_string())
    }
}

pub struct GroupDeserializer {
    parsed_group: NamelistGroup,
}

impl GroupDeserializer {
    pub fn new(namelist: NamelistGroup) -> GroupDeserializer {
        GroupDeserializer {
            parsed_group: namelist,
        }
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
