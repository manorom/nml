use super::item::ItemDeserializer;
use super::NamelistError;
use crate::namelist::Item;
use crate::namelist::Map;
use crate::namelist::MapIntoIter;

struct KeyDeserializer(String);

impl<'de> serde::de::Deserializer<'de> for KeyDeserializer {
    type Error = NamelistError;

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_string(self.0)
    }

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        self.deserialize_string(visitor)
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str
        bytes byte_buf option unit unit_struct newtype_struct seq tuple
        tuple_struct map struct enum identifier ignored_any
    }
}

pub struct ItemMapAccess {
    iter: MapIntoIter<String, Item>,
    value: Option<Item>,
}

impl ItemMapAccess {
    pub fn new(map: Map<String, Item>) -> ItemMapAccess {
        ItemMapAccess {
            iter: map.into_iter(),
            value: None,
        }
    }
}

impl<'de> serde::de::MapAccess<'de> for ItemMapAccess {
    type Error = NamelistError;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: serde::de::DeserializeSeed<'de>,
    {
        match self.iter.next() {
            Some((k, v)) => {
                self.value = Some(v);
                seed.deserialize(KeyDeserializer(k)).map(Some)
            }
            None => Ok(None),
        }
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::DeserializeSeed<'de>,
    {
        match self.value.take() {
            Some(value) => seed.deserialize(ItemDeserializer::new(value)),
            None => {
                panic!("no more values in next_value_seed, internal error in ItemDeserializer")
            }
        }
    }
}
