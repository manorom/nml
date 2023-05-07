use crate::constant::{Constant, LiteralConstant};

#[derive(Debug)]
pub enum ConstantList {
    Single(Constant),
    Multiple(Vec<Constant>),
}

impl ConstantList {
    pub(crate) fn is_type_compatible(&self, constant: &Constant) -> bool {
        match self {
            ConstantList::Single(c) => c.is_type_compatible(constant).unwrap_or(true),
            ConstantList::Multiple(l) => {
                for c in l.iter() {
                    if let Some(compatible) = c.is_type_compatible(constant) {
                        return compatible;
                    }
                }
                true
            }
        }
    }
    pub(crate) fn push(&mut self, constant: Constant) {
        match self {
            ConstantList::Single(c) => *self = ConstantList::Multiple(vec![c.clone(), constant]),
            ConstantList::Multiple(v) => v.push(constant),
        }
    }

    pub fn as_literal_constant(&self) -> Option<&LiteralConstant> {
        match self {
            ConstantList::Single(Constant::Literal(l)) => Some(l),
            _ => None,
        }
    }

    pub fn as_literal_constant_mut(&mut self) -> Option<&mut LiteralConstant> {
        match self {
            ConstantList::Single(Constant::Literal(l)) => Some(l),
            _ => None,
        }
    }
}

impl std::fmt::Display for ConstantList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConstantList::Single(c) => c.fmt(f),
            ConstantList::Multiple(v) => {
                let mut entries = v.iter();
                if let Some(entry) = entries.next() {
                    entry.fmt(f)?;
                }

                for entry in entries {
                    write!(f, ", {}", entry)?;
                }

                Ok(())
            }
        }
    }
}
