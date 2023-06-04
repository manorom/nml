use super::item::SerializeItem;
use crate::namelist::{Item, Map};
use crate::NamelistError;

pub struct DerivedSerializer {
    map: Map<String, Item>,
    next_key: Option<String>,
}

impl DerivedSerializer {
    pub fn new() -> Self {
        DerivedSerializer {
            map: Map::new(),
            next_key: None,
        }
    }
}

impl serde::ser::SerializeMap for DerivedSerializer {
    type Ok = Item;

    type Error = NamelistError;

    fn serialize_key<T: ?Sized>(&mut self, key: &T) -> Result<(), Self::Error>
    where
        T: serde::Serialize,
    {
        self.next_key = Some(key.serialize(SerializeMapKey)?);

        Ok(())
    }

    fn serialize_value<T: ?Sized>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: serde::Serialize,
    {
        let map_key = self
            .next_key
            .take()
            .expect(".serialize_key() not called before .serialize_value()");
        let map_value = value.serialize(SerializeItem)?;
        self.map.insert(map_key, map_value);
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(Item::Derived(self.map))
    }
}

impl serde::ser::SerializeStruct for DerivedSerializer {
    type Ok = Item;

    type Error = NamelistError;

    fn serialize_field<T: ?Sized>(
        &mut self,
        key: &'static str,
        value: &T,
    ) -> Result<(), Self::Error>
    where
        T: serde::Serialize,
    {
        let map_value = value.serialize(SerializeItem)?;
        self.map.insert(key.to_string(), map_value);
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(Item::Derived(self.map))
    }
}

pub struct SerializeMapKey;

impl serde::ser::Serializer for SerializeMapKey {
    type Ok = String;

    type Error = NamelistError;

    type SerializeSeq = serde::ser::Impossible<String, NamelistError>;

    type SerializeTuple = serde::ser::Impossible<String, NamelistError>;

    type SerializeTupleStruct = serde::ser::Impossible<String, NamelistError>;

    type SerializeTupleVariant = serde::ser::Impossible<String, NamelistError>;

    type SerializeMap = serde::ser::Impossible<String, NamelistError>;

    type SerializeStruct = serde::ser::Impossible<String, NamelistError>;

    type SerializeStructVariant = serde::ser::Impossible<String, NamelistError>;

    fn serialize_bool(self, _v: bool) -> Result<Self::Ok, Self::Error> {
        Err(NamelistError::UnsupportedSerialization)
    }

    fn serialize_i8(self, _v: i8) -> Result<Self::Ok, Self::Error> {
        Err(NamelistError::UnsupportedSerialization)
    }

    fn serialize_i16(self, _v: i16) -> Result<Self::Ok, Self::Error> {
        Err(NamelistError::UnsupportedSerialization)
    }

    fn serialize_i32(self, _v: i32) -> Result<Self::Ok, Self::Error> {
        Err(NamelistError::UnsupportedSerialization)
    }

    fn serialize_i64(self, _v: i64) -> Result<Self::Ok, Self::Error> {
        Err(NamelistError::UnsupportedSerialization)
    }

    fn serialize_u8(self, _v: u8) -> Result<Self::Ok, Self::Error> {
        Err(NamelistError::UnsupportedSerialization)
    }

    fn serialize_u16(self, _v: u16) -> Result<Self::Ok, Self::Error> {
        Err(NamelistError::UnsupportedSerialization)
    }

    fn serialize_u32(self, _v: u32) -> Result<Self::Ok, Self::Error> {
        Err(NamelistError::UnsupportedSerialization)
    }

    fn serialize_u64(self, _v: u64) -> Result<Self::Ok, Self::Error> {
        Err(NamelistError::UnsupportedSerialization)
    }

    fn serialize_f32(self, _v: f32) -> Result<Self::Ok, Self::Error> {
        Err(NamelistError::UnsupportedSerialization)
    }

    fn serialize_f64(self, _v: f64) -> Result<Self::Ok, Self::Error> {
        Err(NamelistError::UnsupportedSerialization)
    }

    fn serialize_char(self, v: char) -> Result<Self::Ok, Self::Error> {
        Ok(v.to_string())
    }

    fn serialize_str(self, v: &str) -> Result<Self::Ok, Self::Error> {
        Ok(v.to_string())
    }

    fn serialize_bytes(self, _v: &[u8]) -> Result<Self::Ok, Self::Error> {
        Err(NamelistError::UnsupportedSerialization)
    }

    fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
        Err(NamelistError::UnsupportedSerialization)
    }

    fn serialize_some<T: ?Sized>(self, _value: &T) -> Result<Self::Ok, Self::Error>
    where
        T: serde::Serialize,
    {
        Err(NamelistError::UnsupportedSerialization)
    }

    fn serialize_unit(self) -> Result<Self::Ok, Self::Error> {
        Err(NamelistError::UnsupportedSerialization)
    }

    fn serialize_unit_struct(self, _name: &'static str) -> Result<Self::Ok, Self::Error> {
        Err(NamelistError::UnsupportedSerialization)
    }

    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
    ) -> Result<Self::Ok, Self::Error> {
        Err(NamelistError::UnsupportedSerialization)
    }

    fn serialize_newtype_struct<T: ?Sized>(
        self,
        _name: &'static str,
        _value: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: serde::Serialize,
    {
        Err(NamelistError::UnsupportedSerialization)
    }

    fn serialize_newtype_variant<T: ?Sized>(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _value: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: serde::Serialize,
    {
        Err(NamelistError::UnsupportedSerialization)
    }

    fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq, Self::Error> {
        Err(NamelistError::UnsupportedSerialization)
    }

    fn serialize_tuple(self, _len: usize) -> Result<Self::SerializeTuple, Self::Error> {
        Err(NamelistError::UnsupportedSerialization)
    }

    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleStruct, Self::Error> {
        Err(NamelistError::UnsupportedSerialization)
    }

    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleVariant, Self::Error> {
        Err(NamelistError::UnsupportedSerialization)
    }

    fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap, Self::Error> {
        Err(NamelistError::UnsupportedSerialization)
    }

    fn serialize_struct(
        self,
        _name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStruct, Self::Error> {
        Err(NamelistError::UnsupportedSerialization)
    }

    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStructVariant, Self::Error> {
        Err(NamelistError::UnsupportedSerialization)
    }
}
