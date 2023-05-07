#[derive(Debug, Clone, PartialEq)]
pub enum LiteralConstant {
    Null,
    String(String),
    Int(i64),
    Float(f64),
    Bool(bool),
}

impl LiteralConstant {
    pub(crate) fn is_type_compatible(&self, other: &LiteralConstant) -> Option<bool> {
        use LiteralConstant::*;
        match (self, other) {
            (Null, _) | (_, Null) => None,
            (String { .. }, String { .. }) => Some(true),
            (Int(_), Int(_)) => Some(true),
            (Float(_), Float(_)) => Some(true),
            (Bool(_), Bool(_)) => Some(true),
            _ => Some(false),
        }
    }
}

impl std::fmt::Display for LiteralConstant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LiteralConstant::Null => write!(f, " "),
            LiteralConstant::Bool(true) => write!(f, ".TRUE."),
            LiteralConstant::Bool(false) => write!(f, ".FALSE."),
            LiteralConstant::Int(i) => write!(f, "{}", i),
            LiteralConstant::Float(n) => write!(f, "{}", n),
            LiteralConstant::String(s) => {
                write!(f, "'")?;
                for byte in s.as_bytes() {
                    if *byte == b'\'' {
                        write!(f, "''")?;
                    } else {
                        write!(f, "{}", byte)?;
                    }
                }
                Ok(())
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Constant {
    Literal(LiteralConstant),
    Repeated(usize, LiteralConstant),
}

impl Constant {
    fn literal_constant<'b>(&'b self) -> &'b LiteralConstant {
        match self {
            Constant::Literal(l) => l,
            Constant::Repeated(_, l) => l,
        }
    }
    pub(crate) fn is_type_compatible(&self, other: &Constant) -> Option<bool> {
        self.literal_constant()
            .is_type_compatible(other.literal_constant())
    }

    pub fn len(&self) -> usize {
        match self {
            Constant::Literal(_) => 1,
            Constant::Repeated(n, _) => *n,
        }
    }
}

impl std::fmt::Display for Constant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Constant::Literal(lit) => lit.fmt(f),
            Constant::Repeated(n, lit) => {
                write!(f, "{}*", n)?;
                lit.fmt(f)
            }
        }
    }
}
