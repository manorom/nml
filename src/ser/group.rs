use super::{derived, item, NamelistError};
use crate::namelist::{Item, Map, NamelistGroup};

pub struct GroupSerializer {
    group_name: Option<String>,
    items: Map<String, Item>,
    next_key: Option<String>,
}

impl GroupSerializer {
    pub fn new(group_name: impl Into<String>) -> Self {
        GroupSerializer {
            group_name: Some(group_name.into()),
            items: Map::new(),
            next_key: None,
        }
    }
}

impl serde::ser::Serializer for GroupSerializer {
    type Ok = NamelistGroup;

    type Error = NamelistError;

    type SerializeSeq = serde::ser::Impossible<NamelistGroup, NamelistError>;

    type SerializeTuple = serde::ser::Impossible<NamelistGroup, NamelistError>;

    type SerializeTupleStruct = serde::ser::Impossible<NamelistGroup, NamelistError>;

    type SerializeTupleVariant = serde::ser::Impossible<NamelistGroup, NamelistError>;

    type SerializeMap = Self;

    type SerializeStruct = Self;

    type SerializeStructVariant = serde::ser::Impossible<NamelistGroup, NamelistError>;

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

    fn serialize_char(self, _v: char) -> Result<Self::Ok, Self::Error> {
        Err(NamelistError::UnsupportedSerialization)
    }

    fn serialize_str(self, _v: &str) -> Result<Self::Ok, Self::Error> {
        Err(NamelistError::UnsupportedSerialization)
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
        Ok(self)
    }

    fn serialize_struct(
        self,
        name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStruct, Self::Error> {
        let mut s = self;
        s.group_name = Some(name.to_string());
        Ok(s)
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

impl serde::ser::SerializeMap for GroupSerializer {
    type Ok = NamelistGroup;

    type Error = NamelistError;

    fn serialize_key<T: ?Sized>(&mut self, key: &T) -> Result<(), Self::Error>
    where
        T: serde::Serialize,
    {
        use derived::SerializeMapKey;
        let map_key = key.serialize(SerializeMapKey)?;
        self.next_key = Some(map_key);
        Ok(())
    }

    fn serialize_value<T: ?Sized>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: serde::Serialize,
    {
        let key = self
            .next_key
            .take()
            .expect("Called .serialize_value() before .serialize_key()");
        let value = value.serialize(item::SerializeItem)?;
        self.items.insert(key, value);
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        let group_name = self
            .group_name
            .ok_or(NamelistError::UnsupportedSerialization)?;
        Ok(NamelistGroup::new(group_name, self.items))
    }
}

impl serde::ser::SerializeStruct for GroupSerializer {
    type Ok = NamelistGroup;

    type Error = NamelistError;

    fn serialize_field<T: ?Sized>(
        &mut self,
        key: &'static str,
        value: &T,
    ) -> Result<(), Self::Error>
    where
        T: serde::Serialize,
    {
        let key = key.to_string();
        let value = value.serialize(item::SerializeItem)?;
        self.items.insert(key, value);
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        let group_name = self
            .group_name
            .ok_or(NamelistError::UnsupportedSerialization)?;
        Ok(NamelistGroup::new(group_name, self.items))
    }
}
