use cranelift_isle::ast::{Ident, SpecExpr, SpecOp};
use cranelift_isle::lexer::Pos;

pub fn spec_const_int(val: i128) -> SpecExpr {
    SpecExpr::ConstInt {
        val,
        pos: Pos::default(),
    }
}

pub fn spec_const_bool(val: bool) -> SpecExpr {
    SpecExpr::ConstBool {
        val,
        pos: Pos::default(),
    }
}

pub fn spec_true() -> SpecExpr {
    spec_const_bool(true)
}

pub fn spec_const_bit_vector(val: u128, width: usize) -> SpecExpr {
    assert!(width > 0);
    SpecExpr::ConstBitVec {
        val,
        width,
        pos: Pos::default(),
    }
}

pub fn spec_unary(op: SpecOp, x: SpecExpr) -> SpecExpr {
    spec_op(op, vec![x])
}

pub fn spec_binary(op: SpecOp, x: SpecExpr, y: SpecExpr) -> SpecExpr {
    spec_op(op, vec![x, y])
}

pub fn spec_ternary(op: SpecOp, x: SpecExpr, y: SpecExpr, z: SpecExpr) -> SpecExpr {
    spec_op(op, vec![x, y, z])
}

pub fn spec_if(c: SpecExpr, t: SpecExpr, e: SpecExpr) -> SpecExpr {
    spec_ternary(SpecOp::If, c, t, e)
}

pub fn spec_eq(x: SpecExpr, y: SpecExpr) -> SpecExpr {
    spec_binary(SpecOp::Eq, x, y)
}

pub fn spec_or(args: Vec<SpecExpr>) -> SpecExpr {
    spec_op(SpecOp::Or, args)
}

pub fn spec_and(args: Vec<SpecExpr>) -> SpecExpr {
    spec_op(SpecOp::And, args)
}

pub fn spec_all(xs: Vec<SpecExpr>) -> SpecExpr {
    match xs.len() {
        0 => spec_true(),
        1 => xs[0].clone(),
        _ => spec_and(xs),
    }
}

pub fn spec_op(op: SpecOp, args: Vec<SpecExpr>) -> SpecExpr {
    SpecExpr::Op {
        op,
        args,
        pos: Pos::default(),
    }
}

pub fn spec_enum(name: String, variant: String) -> SpecExpr {
    SpecExpr::Enum {
        name: spec_ident(format!("{}.{}", name, variant)),
        pos: Pos::default(),
    }
}

pub fn spec_field(field: String, x: SpecExpr) -> SpecExpr {
    SpecExpr::Field {
        field: spec_ident(field),
        x: Box::new(x),
        pos: Pos::default(),
    }
}

pub fn spec_var(id: String) -> SpecExpr {
    SpecExpr::Var {
        var: spec_ident(id),
        pos: Pos::default(),
    }
}

pub fn spec_with(decls: Vec<Ident>, body: SpecExpr) -> SpecExpr {
    SpecExpr::With {
        decls,
        body: Box::new(body),
        pos: Pos::default(),
    }
}

pub fn spec_idents(ids: &[String]) -> Vec<Ident> {
    ids.iter().cloned().map(spec_ident).collect()
}

pub fn spec_ident(id: String) -> Ident {
    Ident(id, Pos::default())
}

#[derive(Clone)]
pub struct Conditions {
    pub requires: Vec<SpecExpr>,
    pub provides: Vec<SpecExpr>,
}

impl Conditions {
    pub fn new() -> Self {
        Self {
            requires: Vec::new(),
            provides: Vec::new(),
        }
    }

    pub fn merge(cs: Vec<Self>) -> Self {
        match cs.len() {
            0 => Self::new(),
            1 => cs[0].clone(),
            _ => Self {
                requires: vec![spec_or(
                    cs.iter().map(|c| spec_all(c.requires.clone())).collect(),
                )],
                provides: cs
                    .iter()
                    .map(|c| {
                        spec_binary(
                            SpecOp::Imp,
                            spec_all(c.requires.clone()),
                            spec_all(c.provides.clone()),
                        )
                    })
                    .collect(),
            },
        }
    }
}
