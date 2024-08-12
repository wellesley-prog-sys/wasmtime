use std::cmp::Ordering;

use cranelift_isle::ast::{Ident, ModelType};

/// Width of a bit vector.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Width {
    Unknown,
    Bits(usize),
}

impl PartialOrd for Width {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Width::Unknown, Width::Unknown) => Some(Ordering::Equal),
            (Width::Unknown, Width::Bits(_)) => Some(Ordering::Less),
            (Width::Bits(_), Width::Unknown) => Some(Ordering::Greater),
            (Width::Bits(l), Width::Bits(r)) if l == r => Some(Ordering::Equal),
            (Width::Bits(_), Width::Bits(_)) => None,
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Type {
    Unknown,
    BitVector(Width),
    Int,
    Bool,
}

impl Type {
    pub fn is_concrete(&self) -> bool {
        match self {
            Self::Unknown | Self::BitVector(Width::Unknown) => false,
            Self::BitVector(Width::Bits(_)) | Self::Int | Self::Bool => true,
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Unknown => write!(f, "unk"),
            Self::BitVector(Width::Bits(w)) => write!(f, "bv {w}"),
            Self::BitVector(Width::Unknown) => write!(f, "bv _"),
            Self::Int => write!(f, "int"),
            Self::Bool => write!(f, "bool"),
        }
    }
}

impl PartialOrd for Type {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Type::Unknown, Type::Unknown) => Some(Ordering::Equal),
            (Type::Unknown, _) => Some(Ordering::Less),
            (_, Type::Unknown) => Some(Ordering::Greater),
            (Type::BitVector(l), Type::BitVector(r)) => l.partial_cmp(r),
            (Type::Int, Type::Int) => Some(Ordering::Equal),
            (Type::Bool, Type::Bool) => Some(Ordering::Equal),
            (_, _) => None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Compound {
    Primitive(Type),
    Struct(Vec<Field>),
    // TODO(mbm): intern name identifier
    Named(Ident),
}

#[derive(Debug, Clone)]
pub struct Field {
    // TODO(mbm): intern name identifier
    pub name: Ident,
    pub ty: Compound,
}

impl Compound {
    pub fn from_ast(model: &ModelType) -> Self {
        match model {
            ModelType::Int => Self::Primitive(Type::Int),
            ModelType::Bool => Self::Primitive(Type::Bool),
            ModelType::BitVec(None) => Self::Primitive(Type::BitVector(Width::Unknown)),
            ModelType::BitVec(Some(bits)) => Self::Primitive(Type::BitVector(Width::Bits(*bits))),
            ModelType::Struct(fields) => Self::Struct(
                fields
                    .iter()
                    .map(|m| Field {
                        name: m.name.clone(),
                        ty: Self::from_ast(&m.ty),
                    })
                    .collect(),
            ),
            ModelType::Named(name) => Self::Named(name.clone()),
        }
    }

    pub fn as_primitive(&self) -> Option<&Type> {
        match self {
            Compound::Primitive(ty) => Some(ty),
            _ => None,
        }
    }
}

impl std::fmt::Display for Compound {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Compound::Primitive(ty) => ty.fmt(f),
            Compound::Struct(fields) => write!(
                f,
                "{{{fields}}}",
                fields = fields
                    .iter()
                    .map(|f| format!("{}: {}", f.name.0, f.ty))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Compound::Named(name) => write!(f, "{}", name.0),
        }
    }
}

// QUESTION(mbm): can this be deduped with the corresponding spec constant type?
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Const {
    Bool(bool),
    Int(i128),
    BitVector(usize, u128),
}

impl Const {
    pub fn ty(&self) -> Type {
        match self {
            Self::Bool(_) => Type::Bool,
            Self::Int(_) => Type::Int,
            Self::BitVector(w, _) => Type::BitVector(Width::Bits(*w)),
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Const::Bool(b) => Some(*b),
            _ => None,
        }
    }

    pub fn as_int(&self) -> Option<i128> {
        match self {
            Const::Int(v) => Some(*v),
            _ => None,
        }
    }
}

impl std::fmt::Display for Const {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Bool(b) => write!(f, "{b}"),
            Self::Int(v) => write!(f, "{v}"),
            Self::BitVector(bits, v) => {
                if bits % 4 == 0 {
                    write!(f, "#x{v:0>nibbles$x}", nibbles = bits / 4)
                } else {
                    write!(f, "#b{v:0>bits$b}")
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testing::assert_partial_order_properties;

    #[test]
    fn test_width_partial_order_less_than() {
        assert!(Width::Unknown < Width::Bits(64));
    }

    #[test]
    fn test_width_partial_order_properties() {
        assert_partial_order_properties(&[Width::Unknown, Width::Bits(32), Width::Bits(64)]);
    }

    #[test]
    fn test_type_partial_order_less_than() {
        assert!(Type::Unknown < Type::BitVector(Width::Unknown));
        assert!(Type::BitVector(Width::Unknown) < Type::BitVector(Width::Bits(64)));
        assert!(Type::Unknown < Type::Int);
        assert!(Type::Unknown < Type::Bool);
    }

    #[test]
    fn test_type_partial_order_properties() {
        assert_partial_order_properties(&[
            Type::Unknown,
            Type::BitVector(Width::Unknown),
            Type::BitVector(Width::Bits(32)),
            Type::BitVector(Width::Bits(64)),
            Type::Int,
            Type::Bool,
        ]);
    }
}
