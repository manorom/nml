use crate::{error::NmlError, Namelist};
use crate::parser::VarMap;

use serde::de;

struct Deserializer {
    parsed: Namelist
}



impl<'de> de::Deserializer<'de> for Deserializer {
    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
        where
            V: de::Visitor<'de> {
        
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct newtype_struct seq tuple
        tuple_struct map struct enum identifier ignored_any
    }
}