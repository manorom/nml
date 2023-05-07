use crate::constant_list::ConstantList;

pub type Map<K, V> = std::collections::BTreeMap<K, V>;

#[derive(Debug)]
pub enum Value {
    ConstantList(ConstantList),
    Derived(Map<String, Value>),
}

impl Value {
    pub fn as_constant_list(&self) -> Option<&ConstantList> {
        match self {
            Value::ConstantList(cl) => Some(cl),
            _ => None,
        }
    }

    pub fn as_constant_list_mut(&mut self) -> Option<&mut ConstantList> {
        match self {
            Value::ConstantList(cl) => Some(cl),
            _ => None,
        }
    }

    pub fn as_derived(&self) -> Option<&Map<String, Value>> {
        match self {
            Value::Derived(m) => Some(m),
            _ => None,
        }
    }
}
