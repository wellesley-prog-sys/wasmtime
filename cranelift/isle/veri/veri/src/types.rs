use std::cmp::Ordering;

use anyhow::Result;
use cranelift_isle::{
    ast::{Ident, ModelType},
    lexer::Pos,
    sema::{self, TypeEnv, VariantId},
};

/// Width of a bit vector.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Width {
    Unknown,
    Bits(usize),
}

impl Width {
    pub fn as_bits(&self) -> Option<usize> {
        match self {
            Width::Unknown => None,
            Width::Bits(bits) => Some(*bits),
        }
    }
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
    Unspecified,
    Unknown,
    BitVector(Width),
    Int,
    Bool,
    Unit,
}

impl Type {
    pub fn is_concrete(&self) -> bool {
        match self {
            Self::Unspecified => true,
            Self::Unknown | Self::BitVector(Width::Unknown) => false,
            Self::BitVector(Width::Bits(_)) | Self::Int | Self::Bool | Self::Unit => true,
        }
    }

    pub fn as_bit_vector_width(&self) -> Option<&Width> {
        match self {
            Type::BitVector(w) => Some(w),
            _ => None,
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Unspecified => write!(f, "\u{2a33}"),
            Self::Unknown => write!(f, "unk"),
            Self::BitVector(Width::Bits(w)) => write!(f, "bv {w}"),
            Self::BitVector(Width::Unknown) => write!(f, "bv _"),
            Self::Int => write!(f, "int"),
            Self::Bool => write!(f, "bool"),
            Self::Unit => write!(f, "unit"),
        }
    }
}

impl PartialOrd for Type {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            // Unspecified is equal to itself, but otherwise incomparible.
            (Type::Unspecified, Type::Unspecified) => Some(Ordering::Equal),
            (Type::Unspecified, _) | (_, Type::Unspecified) => None,

            (Type::Unknown, Type::Unknown) => Some(Ordering::Equal),
            (Type::Unknown, _) => Some(Ordering::Less),
            (_, Type::Unknown) => Some(Ordering::Greater),
            (Type::BitVector(l), Type::BitVector(r)) => l.partial_cmp(r),
            (Type::Int, Type::Int) => Some(Ordering::Equal),
            (Type::Bool, Type::Bool) => Some(Ordering::Equal),
            (Type::Unit, Type::Unit) => Some(Ordering::Equal),
            (_, _) => None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Compound {
    Primitive(Type),
    Struct(Vec<Field>),
    Enum(Vec<Variant>),
    // TODO(mbm): intern name identifier
    Named(Ident),
}

#[derive(Debug, Clone)]
pub struct Field {
    // TODO(mbm): intern name identifier
    pub name: Ident,
    pub ty: Compound,
}

impl Field {
    fn from_isle(field: &sema::Field, tyenv: &TypeEnv) -> Self {
        let ty = &tyenv.types[field.ty.index()];
        Self {
            name: Ident(tyenv.syms[field.name.index()].clone(), Pos::default()),
            ty: Compound::named_from_isle(ty, tyenv),
        }
    }

    /// Resolve any named types.
    pub fn resolve<F>(&self, lookup: &mut F) -> Result<Self>
    where
        F: FnMut(&Ident) -> Result<Compound>,
    {
        Ok(Field {
            name: self.name.clone(),
            ty: self.ty.resolve(lookup)?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct Variant {
    pub name: Ident,
    pub id: VariantId,
    pub fields: Vec<Field>,
}

impl Variant {
    fn from_isle(variant: &sema::Variant, tyenv: &TypeEnv) -> Self {
        Self {
            name: Ident(tyenv.syms[variant.name.index()].clone(), variant.pos),
            id: variant.id,
            fields: variant
                .fields
                .iter()
                .map(|f| Field::from_isle(f, tyenv))
                .collect(),
        }
    }

    pub fn is_unit(&self) -> bool {
        self.fields.is_empty()
    }

    pub fn ty(&self) -> Compound {
        Compound::Struct(self.fields.clone())
    }

    /// Resolve any named types.
    pub fn resolve<F>(&self, lookup: &mut F) -> Result<Self>
    where
        F: FnMut(&Ident) -> Result<Compound>,
    {
        Ok(Variant {
            name: self.name.clone(),
            id: self.id,
            fields: self
                .fields
                .iter()
                .map(|f| f.resolve(lookup))
                .collect::<Result<_>>()?,
        })
    }
}

impl std::fmt::Display for Variant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_unit() {
            write!(f, "{name}", name = self.name.0)
        } else {
            write!(f, "{name} {ty}", name = self.name.0, ty = self.ty())
        }
    }
}

impl Compound {
    pub fn from_ast(model: &ModelType) -> Self {
        match model {
            ModelType::Unspecified => Self::Primitive(Type::Unspecified),
            ModelType::Auto => Self::Primitive(Type::Unknown),
            ModelType::Int => Self::Primitive(Type::Int),
            ModelType::Bool => Self::Primitive(Type::Bool),
            ModelType::Unit => Self::Primitive(Type::Unit),
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

    /// Derive a type corresponding to the given ISLE type, if possible. For
    /// ISLE internal enumerations, this will build the corresponding VeriISLE
    /// enum representation.
    pub fn from_isle(ty: &sema::Type, tyenv: &TypeEnv) -> Option<Self> {
        match ty {
            sema::Type::Enum { variants, .. } if !variants.is_empty() => Some(Self::Enum(
                variants
                    .iter()
                    .map(|v| Variant::from_isle(v, tyenv))
                    .collect(),
            )),
            _ => None,
        }
    }

    /// Build a named reference to the given ISLE type.
    pub fn named_from_isle(ty: &sema::Type, tyenv: &TypeEnv) -> Self {
        Self::Named(Ident(ty.name(tyenv).to_string(), ty.pos()))
    }

    pub fn as_primitive(&self) -> Option<&Type> {
        match self {
            Compound::Primitive(ty) => Some(ty),
            _ => None,
        }
    }

    pub fn as_enum(&self) -> Option<&Vec<Variant>> {
        match self {
            Compound::Enum(variants) => Some(variants),
            _ => None,
        }
    }

    /// Resolve any named types.
    pub fn resolve<F>(&self, lookup: &mut F) -> Result<Self>
    where
        F: FnMut(&Ident) -> Result<Compound>,
    {
        match self {
            Compound::Primitive(_) => Ok(self.clone()),
            Compound::Struct(fields) => Ok(Compound::Struct(
                fields
                    .iter()
                    .map(|f| f.resolve(lookup))
                    .collect::<Result<_>>()?,
            )),
            Compound::Enum(variants) => Ok(Compound::Enum(
                variants
                    .iter()
                    .map(|v| v.resolve(lookup))
                    .collect::<Result<_>>()?,
            )),
            Compound::Named(name) => {
                // TODO(mbm): named type model cycle detection
                let ty = lookup(name)?;
                ty.resolve(lookup)
            }
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
            Compound::Enum(variants) => {
                write!(
                    f,
                    "enum{{{variants}}}",
                    variants = variants
                        .iter()
                        .map(|v| v.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
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
    Unspecified,
}

impl Const {
    pub fn ty(&self) -> Type {
        match self {
            Self::Bool(_) => Type::Bool,
            Self::Int(_) => Type::Int,
            Self::BitVector(w, _) => Type::BitVector(Width::Bits(*w)),
            Self::Unspecified => Type::Unspecified,
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
            Self::Unspecified => write!(f, "\u{2a33}"),
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
            Type::Unspecified,
            Type::Unknown,
            Type::BitVector(Width::Unknown),
            Type::BitVector(Width::Bits(32)),
            Type::BitVector(Width::Bits(64)),
            Type::Int,
            Type::Bool,
            Type::Unit,
        ]);
    }
}
