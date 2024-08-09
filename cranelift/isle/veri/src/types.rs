use std::cmp::Ordering;

use cranelift_isle::ast;

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
    pub fn from_ast(model: &ast::ModelType) -> Self {
        match model {
            ast::ModelType::Int => Self::Int,
            ast::ModelType::Bool => Self::Bool,
            ast::ModelType::BitVec(None) => Self::BitVector(Width::Unknown),
            ast::ModelType::BitVec(Some(bits)) => Self::BitVector(Width::Bits(*bits)),
        }
    }

    pub fn is_concrete(&self) -> bool {
        match self {
            Self::BitVector(Width::Bits(_)) | Self::Int | Self::Bool => true,
            Self::Unknown | Self::BitVector(Width::Unknown) => false,
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

// QUESTION(mbm): can this be deduped with the corresponding spec constant type?
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Const {
    Bool(bool),
    Int(i128),
    BitVector(usize, i128),
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
            Self::BitVector(w, v) => {
                if w % 4 == 0 {
                    write!(f, "#x{v:0>nibbles$x}", nibbles = w / 4)
                } else {
                    write!(f, "#b{v:0>bits$b}", bits = w)
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
