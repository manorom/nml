use serde::de::SeqAccess;

use super::item::{BorrowedItemDeserializer, BorrowedLiteralDeserializer, ItemDeserializer};
use crate::namelist::LiteralConstant;
use crate::namelist::{Array, ArrayIter, ArrayRefIter, ItemRef};
use crate::NamelistError;

pub struct ArraySeqAccess {
    iter: ArrayIter,
}

impl ArraySeqAccess {
    pub fn new(array: Array) -> Self {
        ArraySeqAccess {
            iter: array.into_iter(),
        }
    }
}

impl<'de> SeqAccess<'de> for ArraySeqAccess {
    type Error = NamelistError;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: serde::de::DeserializeSeed<'de>,
    {
        if let Some(item) = self.iter.next() {
            seed.deserialize(ItemDeserializer::new(item)).map(Some)
        } else {
            Ok(None)
        }
    }
}

pub struct BorrowedArraySeqAccess<'a>(ArrayRefIter<'a>);

impl<'a> BorrowedArraySeqAccess<'a> {
    pub fn new(iter: ArrayRefIter<'a>) -> Self {
        BorrowedArraySeqAccess(iter)
    }
}

impl<'de, 'a> SeqAccess<'de> for BorrowedArraySeqAccess<'a> {
    type Error = NamelistError;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: serde::de::DeserializeSeed<'de>,
    {
        match self.0.next() {
            Some(item_ref) => match item_ref {
                ItemRef::Item(item) => seed
                    .deserialize(BorrowedItemDeserializer::new(item))
                    .map(Some),
                ItemRef::Literal(literal) => seed
                    .deserialize(BorrowedLiteralDeserializer(literal))
                    .map(Some),
                ItemRef::Null => seed
                    .deserialize(BorrowedLiteralDeserializer(&LiteralConstant::Null))
                    .map(Some),
            },
            None => Ok(None),
        }
    }
}
