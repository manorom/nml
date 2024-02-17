use super::literal_constant::LiteralConstant;
use super::{Item, Map};

#[derive(Debug, Clone)]
pub enum Array {
    RepeatedConstant(usize, LiteralConstant),
    List(Vec<(usize, LiteralConstant)>),
    Indexed(Map<usize, Item>),
}

impl Array {
    pub fn into_item(self) -> Item {
        Item::Array(self)
    }
    pub fn new_indexed() -> Item {
        Item::Array(Array::Indexed(Map::new()))
    }
}

impl IntoIterator for Array {
    type Item = Item;

    type IntoIter = ArrayIter;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            Array::RepeatedConstant(num, lit) => ArrayIter::RepeatedConstant(num, lit),
            Array::List(vec) => ArrayIter::List {
                iter: vec.into_iter(),
                repeats: None,
            },
            Array::Indexed(map) => ArrayIter::Indexed {
                last: 0,
                max: *map.keys().max().unwrap_or(&0),
                map,
            },
        }
    }
}

pub enum ArrayIter {
    RepeatedConstant(usize, LiteralConstant),
    List {
        iter: std::vec::IntoIter<(usize, LiteralConstant)>,
        repeats: Option<(usize, LiteralConstant)>,
    },
    Indexed {
        last: usize,
        max: usize,
        map: Map<usize, Item>,
    },
}

impl Iterator for ArrayIter {
    type Item = Item;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            ArrayIter::RepeatedConstant(num, lit) => {
                if *num > 0 {
                    *num -= 1;
                    Some(lit.clone().into_item())
                } else {
                    None
                }
            }
            ArrayIter::List { iter, repeats } => {
                if let Some((num, lit)) = repeats {
                    if *num > 1 {
                        *num -= 1;
                        Some(lit.clone().into_item())
                    } else {
                        let lit = lit.clone();
                        *repeats = None;
                        Some(lit.into_item())
                    }
                } else {
                    if let Some((num, lit)) = iter.next() {
                        if num > 1 {
                            *repeats = Some((num - 1, lit.clone()));
                        }
                        Some(lit.into_item())
                    } else {
                        None
                    }
                }
            }
            ArrayIter::Indexed { last, max, map } => {
                if last == max {
                    None
                } else {
                    let next = *last + 1;
                    *last = next;
                    if let Some(item) = map.remove(&next) {
                        Some(item)
                    } else {
                        Some(Item::null())
                    }
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum ItemRef<'a> {
    Item(&'a Item),
    Literal(&'a LiteralConstant),
    Null,
}

pub struct ArrayRefIter<'a>(Box<dyn Iterator<Item = ItemRef<'a>> + 'a>);

impl<'a> ArrayRefIter<'a> {
    fn new(array: &'a Array) -> ArrayRefIter<'a> {
        match array {
            Array::RepeatedConstant(repeats, literal) => ArrayRefIter(Box::new(
                std::iter::repeat(literal)
                    .take(*repeats)
                    .map(|lit| ItemRef::Literal(lit)),
            )),
            Array::List(list) => ArrayRefIter(Box::new(
                list.iter()
                    .map(|(repeats, literal)| {
                        std::iter::repeat(literal)
                            .take(*repeats)
                            .map(|lit| ItemRef::Literal(lit))
                    })
                    .flatten(),
            )),
            Array::Indexed(map) => {
                let min = map.keys().cloned().min().unwrap_or(1);
                let max = map.keys().cloned().max().unwrap_or(1);
                ArrayRefIter(Box::new((min..=max).map(|idx| match map.get(&idx) {
                    Some(item) => ItemRef::Item(item),
                    None => ItemRef::Null,
                })))
            }
        }
    }
}

impl<'a> Iterator for ArrayRefIter<'a> {
    type Item = ItemRef<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

impl<'a> IntoIterator for &'a Array {
    type Item = ItemRef<'a>;

    type IntoIter = ArrayRefIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        ArrayRefIter::new(self)
    }
}
