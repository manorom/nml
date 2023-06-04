use serde::de::SeqAccess;

use super::item::ItemDeserializer;
use crate::namelist::{Array, ArrayIter};
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
