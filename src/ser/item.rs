use serde::Serialize;

use super::NamelistError;
use crate::namelist::{Array, Item, LiteralConstant};

use super::derived::DerivedSerializer;

pub struct SerializeItem;

impl serde::ser::Serializer for SerializeItem {
    type Ok = Item;
    type Error = NamelistError;

    type SerializeSeq = SerializeList;

    type SerializeTuple = SerializeList;

    type SerializeTupleStruct = SerializeList;

    type SerializeTupleVariant = serde::ser::Impossible<Item, NamelistError>;

    type SerializeMap = DerivedSerializer;

    type SerializeStruct = DerivedSerializer;

    type SerializeStructVariant = serde::ser::Impossible<Item, NamelistError>;

    fn serialize_bool(self, v: bool) -> Result<Self::Ok, Self::Error> {
        Ok(Item::Literal(LiteralConstant::Bool(v)))
    }

    fn serialize_i8(self, v: i8) -> Result<Self::Ok, Self::Error> {
        Ok(Item::Literal(LiteralConstant::Int(v.into())))
    }

    fn serialize_i16(self, v: i16) -> Result<Self::Ok, Self::Error> {
        Ok(Item::Literal(LiteralConstant::Int(v.into())))
    }

    fn serialize_i32(self, v: i32) -> Result<Self::Ok, Self::Error> {
        Ok(Item::Literal(LiteralConstant::Int(v.into())))
    }

    fn serialize_i64(self, v: i64) -> Result<Self::Ok, Self::Error> {
        Ok(Item::Literal(LiteralConstant::Int(v)))
    }

    fn serialize_u8(self, v: u8) -> Result<Self::Ok, Self::Error> {
        Ok(Item::Literal(LiteralConstant::Int(v.into())))
    }

    fn serialize_u16(self, v: u16) -> Result<Self::Ok, Self::Error> {
        Ok(Item::Literal(LiteralConstant::Int(v.into())))
    }

    fn serialize_u32(self, v: u32) -> Result<Self::Ok, Self::Error> {
        Ok(Item::Literal(LiteralConstant::Int(v.into())))
    }

    fn serialize_u64(self, v: u64) -> Result<Self::Ok, Self::Error> {
        self.serialize_u32(
            v.try_into()
                .map_err(|_| NamelistError::UnsupportedSerialization)?,
        )
    }

    fn serialize_f32(self, v: f32) -> Result<Self::Ok, Self::Error> {
        Ok(Item::Literal(LiteralConstant::Float(v.into())))
    }

    fn serialize_f64(self, v: f64) -> Result<Self::Ok, Self::Error> {
        Ok(Item::Literal(LiteralConstant::Float(v)))
    }

    fn serialize_char(self, v: char) -> Result<Self::Ok, Self::Error> {
        Ok(LiteralConstant::String(v.into()).into_item())
    }

    fn serialize_str(self, v: &str) -> Result<Self::Ok, Self::Error> {
        Ok(LiteralConstant::String(v.to_owned()).into_item())
    }

    fn serialize_bytes(self, _v: &[u8]) -> Result<Self::Ok, Self::Error> {
        Err(NamelistError::UnsupportedSerialization)
    }

    fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
        Ok(Item::Literal(LiteralConstant::Null))
    }

    fn serialize_some<T: ?Sized>(self, v: &T) -> Result<Self::Ok, Self::Error>
    where
        T: serde::Serialize,
    {
        v.serialize(self)
    }

    fn serialize_unit(self) -> Result<Self::Ok, Self::Error> {
        self.serialize_none()
    }

    fn serialize_unit_struct(self, _name: &'static str) -> Result<Self::Ok, Self::Error> {
        self.serialize_unit()
    }

    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
    ) -> Result<Self::Ok, Self::Error> {
        self.serialize_str(variant)
    }

    fn serialize_newtype_struct<T: ?Sized>(
        self,
        _name: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: serde::Serialize,
    {
        value.serialize(self)
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
        Ok(SerializeList { list: Vec::new() })
    }

    fn serialize_tuple(self, len: usize) -> Result<Self::SerializeTuple, Self::Error> {
        self.serialize_seq(Some(len))
    }

    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleStruct, Self::Error> {
        self.serialize_seq(Some(len))
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
        Ok(DerivedSerializer::new())
    }

    fn serialize_struct(
        self,
        _name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeStruct, Self::Error> {
        self.serialize_map(Some(len))
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

pub struct SerializeList {
    list: Vec<(usize, LiteralConstant)>,
}

impl SerializeList {
    fn is_type_compatible(&self, lit: &LiteralConstant) -> bool {
        for (_, other_lit) in self.list.iter() {
            if let Some(b) = other_lit.is_type_compatible(lit) {
                return b;
            }
        }
        true
    }
}

impl serde::ser::SerializeSeq for SerializeList {
    type Ok = Item;

    type Error = NamelistError;

    fn serialize_element<T: ?Sized>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: serde::Serialize,
    {
        let lit = if let Item::Literal(lit) = value.serialize(SerializeItem)? {
            if self.is_type_compatible(&lit) {
                Ok(lit)
            } else {
                Err(NamelistError::UnsupportedSerialization)
            }
        } else {
            Err(NamelistError::UnsupportedSerialization)
        }?;

        self.list.push((1, lit));

        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(Item::Array(Array::List(self.list)))
    }
}

impl serde::ser::SerializeTuple for SerializeList {
    type Ok = Item;
    type Error = NamelistError;

    fn serialize_element<T: ?Sized>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: serde::Serialize,
    {
        let lit = if let Item::Literal(lit) = value.serialize(SerializeItem)? {
            if self.is_type_compatible(&lit) {
                Ok(lit)
            } else {
                Err(NamelistError::UnsupportedSerialization)
            }
        } else {
            Err(NamelistError::UnsupportedSerialization)
        }?;

        self.list.push((1, lit));

        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(Item::Array(Array::List(self.list)))
    }
}

impl serde::ser::SerializeTupleStruct for SerializeList {
    type Ok = Item;

    type Error = NamelistError;

    fn serialize_field<T: ?Sized>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: Serialize,
    {
        let lit = if let Item::Literal(lit) = value.serialize(SerializeItem)? {
            if self.is_type_compatible(&lit) {
                Err(NamelistError::UnsupportedSerialization)
            } else {
                Ok(lit)
            }
        } else {
            Err(NamelistError::UnsupportedSerialization)
        }?;

        self.list.push((1, lit));

        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(Item::Array(Array::List(self.list)))
    }
}
