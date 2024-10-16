//! Printer for ISLE language.

use crate::ast::*;
use crate::error::Errors;
use pretty::{Doc, Pretty, RcAllocator, RcDoc};
use std::io::Write;

/// Printable is a trait satisfied by AST nodes that can be printed.
pub trait Printable {
    /// Map the node to a pretty document.
    fn to_doc(&self) -> RcDoc<()>;
}

/// Print the given AST node with specified line width.
pub fn print<N, W>(node: &N, width: usize, out: &mut W) -> Result<(), Errors>
where
    N: Printable,
    W: ?Sized + Write,
{
    node.to_doc()
        .render(width, out)
        .map_err(|e| Errors::from_io(e, "failed to print isle"))
}

/// Dump AST node to standard output.
pub fn dump<N: Printable>(node: &N) -> Result<(), Errors> {
    let mut stdout = std::io::stdout();
    print(node, 78, &mut stdout)
}

impl<P: Printable> Printable for Vec<P> {
    fn to_doc(&self) -> RcDoc<()> {
        let sep = RcDoc::hardline().append(Doc::hardline());
        RcDoc::intersperse(self.iter().map(|d| d.to_doc()), sep).append(Doc::hardline())
    }
}

impl Printable for Def {
    fn to_doc(&self) -> RcDoc<()> {
        match self {
            Def::Pragma(_) => unimplemented!("pragmas not supported"),
            Def::Type(ref t) => {
                let mut parts = vec![RcDoc::text("type")];
                parts.push(t.name.to_doc());
                if t.is_extern {
                    parts.push(RcDoc::text("extern"));
                }
                if t.is_nodebug {
                    parts.push(RcDoc::text("nodebug"));
                }
                parts.push(t.ty.to_doc());
                sexp(parts)
            }
            Def::Rule(ref r) => {
                let mut parts = Vec::new();
                parts.push(RcDoc::text("rule"));
                if let Some(name) = &r.name {
                    parts.push(name.to_doc());
                }
                if let Some(prio) = &r.prio {
                    parts.push(RcDoc::as_string(prio));
                }
                parts.push(r.pattern.to_doc());
                parts.extend(r.iflets.iter().map(|il| il.to_doc()));
                parts.push(r.expr.to_doc());
                sexp(parts)
            }
            Def::Extractor(ref e) => sexp(vec![
                RcDoc::text("extractor"),
                sexp(
                    Vec::from([e.term.to_doc()])
                        .into_iter()
                        .chain(e.args.iter().map(|v| v.to_doc())),
                ),
                e.template.to_doc(),
            ]),
            Def::Decl(ref d) => {
                let mut parts = Vec::new();
                parts.push(RcDoc::text("decl"));
                if d.pure {
                    parts.push(RcDoc::text("pure"));
                }
                if d.multi {
                    parts.push(RcDoc::text("multi"));
                }
                if d.partial {
                    parts.push(RcDoc::text("partial"));
                }
                parts.push(d.term.to_doc());
                parts.push(sexp(d.arg_tys.iter().map(|ty| ty.to_doc())));
                parts.push(d.ret_ty.to_doc());
                sexp(parts)
            }
            Def::Attr(ref a) => a.to_doc(),
            Def::Spec(ref s) => s.to_doc(),
            Def::SpecMacro(ref m) => m.to_doc(),
            Def::State(ref s) => s.to_doc(),
            Def::Model(ref m) => sexp(vec![RcDoc::text("model"), m.name.to_doc(), m.val.to_doc()]),
            Def::Form(ref f) => {
                let mut parts = vec![RcDoc::text("form")];
                parts.push(f.name.to_doc());
                parts.extend(f.signatures.iter().map(|s| s.to_doc()));
                sexp(parts)
            }
            Def::Instantiation(ref i) => {
                let mut parts = vec![RcDoc::text("instantiate"), i.term.to_doc()];
                if let Some(form) = &i.form {
                    parts.push(form.to_doc());
                } else {
                    parts.extend(i.signatures.iter().map(|s| s.to_doc()));
                }
                sexp(parts)
            }
            Def::Extern(ref e) => e.to_doc(),
            Def::Converter(ref c) => sexp(vec![
                RcDoc::text("convert"),
                c.inner_ty.to_doc(),
                c.outer_ty.to_doc(),
                c.term.to_doc(),
            ]),
        }
    }
}

impl Printable for Ident {
    fn to_doc(&self) -> RcDoc<()> {
        RcDoc::text(self.0.clone())
    }
}

impl Printable for TypeValue {
    fn to_doc(&self) -> RcDoc<()> {
        match self {
            TypeValue::Primitive(ref name, _) => {
                sexp(vec![RcDoc::text("primitive"), name.to_doc()])
            }
            TypeValue::Enum(ref variants, _) => sexp(
                // TODO(mbm): convenience for sexp with a fixed first element
                Vec::from([RcDoc::text("enum")])
                    .into_iter()
                    .chain(variants.iter().map(|v| v.to_doc())),
            ),
        }
    }
}

impl Printable for Variant {
    fn to_doc(&self) -> RcDoc<()> {
        sexp(
            // TODO(mbm): convenience for sexp with a fixed first element
            Vec::from([self.name.to_doc()])
                .into_iter()
                .chain(self.fields.iter().map(|f| f.to_doc())),
        )
    }
}

impl Printable for Field {
    fn to_doc(&self) -> RcDoc<()> {
        sexp(vec![self.name.to_doc(), self.ty.to_doc()])
    }
}

impl Printable for Attr {
    fn to_doc(&self) -> RcDoc<()> {
        sexp(
            vec![RcDoc::text("attr"), self.name.to_doc()]
                .into_iter()
                .chain(self.kinds.iter().map(|k| k.to_doc())),
        )
    }
}

impl Printable for AttrKind {
    fn to_doc(&self) -> RcDoc<()> {
        match self {
            AttrKind::Chain => sexp(vec![RcDoc::text("veri"), RcDoc::text("chain")]),
            AttrKind::Priority => sexp(vec![RcDoc::text("veri"), RcDoc::text("priority")]),
            AttrKind::Tag(tag) => sexp(vec![RcDoc::text("tag"), tag.to_doc()]),
        }
    }
}

impl Printable for ModelValue {
    fn to_doc(&self) -> RcDoc<()> {
        match self {
            ModelValue::TypeValue(ref mt) => sexp(vec![RcDoc::text("type"), mt.to_doc()]),
            ModelValue::ConstValue(ref c) => sexp(vec![RcDoc::text("const"), c.to_doc()]),
        }
    }
}

impl Printable for ModelType {
    fn to_doc(&self) -> RcDoc<()> {
        match self {
            ModelType::Unspecified => RcDoc::text("!"),
            ModelType::Auto => RcDoc::text("_"),
            ModelType::Unit => RcDoc::text("Unit"),
            ModelType::Int => RcDoc::text("Int"),
            ModelType::Bool => RcDoc::text("Bool"),
            ModelType::BitVec(Some(size)) => sexp(vec![RcDoc::text("bv"), RcDoc::as_string(size)]),
            ModelType::BitVec(None) => sexp(vec![RcDoc::text("bv")]),
            ModelType::Struct(fields) => sexp(
                vec![RcDoc::text("struct")]
                    .into_iter()
                    .chain(fields.iter().map(|f| f.to_doc())),
            ),
            ModelType::Named(name) => sexp(vec![RcDoc::text("named"), name.to_doc()]),
        }
    }
}

impl Printable for ModelField {
    fn to_doc(&self) -> RcDoc<()> {
        sexp(vec![self.name.to_doc(), self.ty.to_doc()])
    }
}

impl Printable for Signature {
    fn to_doc(&self) -> RcDoc<()> {
        sexp(vec![
            sexp(
                Vec::from([RcDoc::text("args")])
                    .into_iter()
                    .chain(self.args.iter().map(|a| a.to_doc())),
            ),
            sexp(vec![RcDoc::text("ret"), self.ret.to_doc()]),
        ])
    }
}

impl Printable for SpecExpr {
    fn to_doc(&self) -> RcDoc<()> {
        match self {
            SpecExpr::ConstInt { val, .. } => RcDoc::as_string(val),
            SpecExpr::ConstBitVec { val, width, .. } => RcDoc::text(if width % 4 == 0 {
                format!("#x{val:0width$x}", width = *width / 4)
            } else {
                format!("#b{val:0width$b}", width = *width)
            }),
            SpecExpr::ConstBool { val, .. } => RcDoc::text(if *val { "true" } else { "false" }),
            SpecExpr::Var { var, .. } => var.to_doc(),
            SpecExpr::Op { op, args, .. } => sexp(
                Vec::from([op.to_doc()])
                    .into_iter()
                    .chain(args.iter().map(|a| a.to_doc())),
            ),
            SpecExpr::Pair { l, r, .. } => sexp(vec![l.to_doc(), r.to_doc()]),
            SpecExpr::Enum {
                name,
                variant,
                args,
                pos: _,
            } => sexp(
                Vec::from([RcDoc::text(format!("{}.{}", name.0, variant.0))])
                    .into_iter()
                    .chain(args.iter().map(|a| a.to_doc())),
            ),
            SpecExpr::Field { field, x, pos: _ } => {
                sexp(vec![RcDoc::text(format!(":{}", field.0)), x.to_doc()])
            }
            SpecExpr::Discriminator { variant, x, pos: _ } => {
                sexp(vec![RcDoc::text(format!("{}?", variant.0)), x.to_doc()])
            }
            SpecExpr::Match { x, arms, pos: _ } => sexp(
                Vec::from([RcDoc::text("match"), x.to_doc()])
                    .into_iter()
                    .chain(arms.iter().map(|arm| arm.to_doc())),
            ),
            SpecExpr::Let { defs, body, pos: _ } => sexp(vec![
                RcDoc::text("let"),
                sexp(defs.iter().map(|(n, e)| sexp(vec![n.to_doc(), e.to_doc()]))),
                body.to_doc(),
            ]),
            SpecExpr::With {
                decls,
                body,
                pos: _,
            } => sexp(vec![
                RcDoc::text("with"),
                sexp(decls.iter().map(Printable::to_doc)),
                body.to_doc(),
            ]),
            SpecExpr::Macro { name, args, pos: _ } => sexp(
                Vec::from([RcDoc::text(format!("{}!", name.0))])
                    .into_iter()
                    .chain(args.iter().map(Printable::to_doc)),
            ),
        }
    }
}

impl Printable for SpecOp {
    fn to_doc(&self) -> RcDoc<()> {
        RcDoc::text(match self {
            SpecOp::Eq => "=",
            SpecOp::And => "and",
            SpecOp::Not => "not",
            SpecOp::Imp => "=>",
            SpecOp::Or => "or",
            SpecOp::Lte => "<=",
            SpecOp::Lt => "<",
            SpecOp::Gte => ">=",
            SpecOp::Gt => ">",
            SpecOp::BVNot => "bvnot",
            SpecOp::BVAnd => "bvand",
            SpecOp::BVOr => "bvor",
            SpecOp::BVXor => "bvxor",
            SpecOp::BVNeg => "bvneg",
            SpecOp::BVAdd => "bvadd",
            SpecOp::BVSub => "bvsub",
            SpecOp::BVMul => "bvmul",
            SpecOp::BVUdiv => "bvudiv",
            SpecOp::BVUrem => "bvurem",
            SpecOp::BVSdiv => "bvsdiv",
            SpecOp::BVSrem => "bvsrem",
            SpecOp::BVShl => "bvshl",
            SpecOp::BVLshr => "bvlshr",
            SpecOp::BVAshr => "bvashr",
            SpecOp::BVSaddo => "bvsaddo",
            SpecOp::BVUle => "bvule",
            SpecOp::BVUlt => "bvult",
            SpecOp::BVUgt => "bvugt",
            SpecOp::BVUge => "bvuge",
            SpecOp::BVSlt => "bvslt",
            SpecOp::BVSle => "bvsle",
            SpecOp::BVSgt => "bvsgt",
            SpecOp::BVSge => "bvsge",
            SpecOp::Rotr => "rotr",
            SpecOp::Rotl => "rotl",
            SpecOp::Extract => "extract",
            SpecOp::ZeroExt => "zero_ext",
            SpecOp::SignExt => "sign_ext",
            SpecOp::Concat => "concat",
            SpecOp::ConvTo => "conv_to",
            SpecOp::Int2BV => "int2bv",
            SpecOp::BV2Int => "bv2int",
            SpecOp::WidthOf => "widthof",
            SpecOp::If => "if",
            SpecOp::Switch => "switch",
            SpecOp::Subs => "subs",
            SpecOp::Popcnt => "popcnt",
            SpecOp::Rev => "rev",
            SpecOp::Cls => "cls",
            SpecOp::Clz => "clz",
        })
    }
}

impl Printable for Arm {
    fn to_doc(&self) -> RcDoc<()> {
        sexp(vec![
            sexp(
                Vec::from([self.variant.to_doc()])
                    .into_iter()
                    .chain(self.args.iter().map(|a| a.to_doc())),
            ),
            self.body.to_doc(),
        ])
    }
}

impl Printable for SpecMacro {
    fn to_doc(&self) -> RcDoc<()> {
        let mut parts = vec![RcDoc::text("macro")];
        parts.push(sexp(
            Vec::from([self.name.to_doc()])
                .into_iter()
                .chain(self.params.iter().map(|a| a.to_doc())),
        ));
        parts.push(self.body.to_doc());
        sexp(parts)
    }
}

impl Printable for Spec {
    fn to_doc(&self) -> RcDoc<()> {
        let mut parts = vec![RcDoc::text("spec")];
        parts.push(sexp(
            Vec::from([self.term.to_doc()])
                .into_iter()
                .chain(self.args.iter().map(|a| a.to_doc())),
        ));
        if !self.modifies.is_empty() {
            parts.push(sexp(
                Vec::from([RcDoc::text("modifies")])
                    .into_iter()
                    .chain(self.modifies.iter().map(|e| e.to_doc())),
            ));
        }
        if !self.provides.is_empty() {
            parts.push(sexp(
                Vec::from([RcDoc::text("provide")])
                    .into_iter()
                    .chain(self.provides.iter().map(|e| e.to_doc())),
            ));
        }
        if !self.requires.is_empty() {
            parts.push(sexp(
                Vec::from([RcDoc::text("require")])
                    .into_iter()
                    .chain(self.requires.iter().map(|e| e.to_doc())),
            ));
        }
        if !self.matches.is_empty() {
            parts.push(sexp(
                Vec::from([RcDoc::text("match")])
                    .into_iter()
                    .chain(self.matches.iter().map(|e| e.to_doc())),
            ));
        }
        sexp(parts)
    }
}

impl Printable for State {
    fn to_doc(&self) -> RcDoc<()> {
        sexp(vec![
            RcDoc::text("state"),
            self.name.to_doc(),
            sexp(vec![RcDoc::text("type"), self.ty.to_doc()]),
            sexp(vec![RcDoc::text("default"), self.default.to_doc()]),
        ])
    }
}

impl Printable for Pattern {
    fn to_doc(&self) -> RcDoc<()> {
        match self {
            Pattern::Var { var, .. } => var.to_doc(),
            Pattern::BindPattern { var, subpat, .. } => RcDoc::intersperse(
                vec![var.to_doc(), RcDoc::text("@"), subpat.to_doc()],
                Doc::space(),
            ),
            Pattern::ConstInt { val, .. } => RcDoc::as_string(val),
            Pattern::ConstPrim { val, .. } => RcDoc::text("$").append(val.to_doc()),
            Pattern::Wildcard { .. } => RcDoc::text("_"),
            Pattern::Term { sym, args, .. } => sexp(
                // TODO(mbm): convenience for sexp with a fixed first element
                Vec::from([sym.to_doc()])
                    .into_iter()
                    .chain(args.iter().map(|f| f.to_doc())),
            ),
            Pattern::And { subpats, .. } => sexp(
                Vec::from([RcDoc::text("and")])
                    .into_iter()
                    .chain(subpats.iter().map(|p| p.to_doc())),
            ),
            Pattern::MacroArg { .. } => unimplemented!("macro arguments are for internal use only"),
        }
    }
}

impl Printable for IfLet {
    fn to_doc(&self) -> RcDoc<()> {
        // TODO(mbm): `if` shorthand when pattern is wildcard
        sexp(vec![
            RcDoc::text("if-let"),
            self.pattern.to_doc(),
            self.expr.to_doc(),
        ])
    }
}

impl Printable for Expr {
    fn to_doc(&self) -> RcDoc<()> {
        match self {
            Expr::Term { sym, args, .. } => sexp(
                // TODO(mbm): convenience for sexp with a fixed first element
                Vec::from([sym.to_doc()])
                    .into_iter()
                    .chain(args.iter().map(|f| f.to_doc())),
            ),
            Expr::Var { name, .. } => name.to_doc(),
            Expr::ConstInt { val, .. } => RcDoc::as_string(val),
            Expr::ConstPrim { val, .. } => RcDoc::text("$").append(val.to_doc()),
            Expr::Let { defs, body, .. } => {
                let mut parts = Vec::new();
                parts.push(RcDoc::text("let"));
                parts.push(sexp(defs.iter().map(|d| d.to_doc())));
                parts.push(body.to_doc());
                sexp(parts)
            }
        }
    }
}

impl Printable for LetDef {
    fn to_doc(&self) -> RcDoc<()> {
        sexp(vec![self.var.to_doc(), self.ty.to_doc(), self.val.to_doc()])
    }
}

impl Printable for Extern {
    fn to_doc(&self) -> RcDoc<()> {
        match self {
            Extern::Extractor {
                term,
                func,
                pos: _,
                infallible,
            } => {
                let mut parts = vec![RcDoc::text("extern"), RcDoc::text("extractor")];
                if *infallible {
                    parts.push(RcDoc::text("infallible"));
                }
                parts.push(term.to_doc());
                parts.push(func.to_doc());
                sexp(parts)
            }
            Extern::Constructor { term, func, .. } => sexp(vec![
                RcDoc::text("extern"),
                RcDoc::text("constructor"),
                term.to_doc(),
                func.to_doc(),
            ]),
            Extern::Const { name, ty, .. } => sexp(vec![
                RcDoc::text("extern"),
                RcDoc::text("const"),
                RcDoc::text("$").append(name.to_doc()),
                ty.to_doc(),
            ]),
        }
    }
}

fn sexp<'a, I, A>(docs: I) -> RcDoc<'a, A>
where
    I: IntoIterator,
    I::Item: Pretty<'a, RcAllocator, A>,
    A: Clone,
{
    RcDoc::text("(")
        .append(RcDoc::intersperse(docs, Doc::line()).nest(4).group())
        .append(RcDoc::text(")"))
}
