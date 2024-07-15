//! Printer for ISLE language.

use crate::ast::*;
use crate::error::Errors;
use std::io::Write;

/// Printable is a trait satisfied by AST nodes that can be printed.
pub trait Printable {
    /// Map the node to a string.
    fn to_doc(&self) -> String;
}

/// Print the given AST node with specified line width.
pub fn print<N, W>(node: &N, width: usize, out: &mut W) -> Result<(), Errors>
where
    N: Printable,
    W: ?Sized + Write,
{
    todo!("Meg TODO")
}

impl Printable for Defs {
    fn to_doc(&self) -> String {
        todo!("Meg TODO") 
    }
}

// Is this the proper approach?
impl Printable for Def {
    fn to_doc(&self) -> String {
        match self {
            Def::Pragma(_) => unimplemented!("pragmas not supported"),
            Def::Type(ref t) => {
                let mut parts = String::from("type");
                parts.push_str(&t.name.to_doc());
                if t.is_extern {
                    parts.push_str("extern");
                }
                if t.is_nodebug {
                    parts.push_str("nodebug");
                }
                parts.push_str(&t.ty.to_doc());
                // Should a return statement be used here?
                return parts
            }
            Def::Rule(ref r) => {
                let mut parts = String::from("rule");
                if let Some(prio) = &r.prio {
                    // Is this the best way to convert from &i64 to string?
                    parts.push_str(&format!("{}", prio));

                }
                parts.push_str(&r.pattern.to_doc());
                // This function converts to a map of type il? but how is that printed as a string?
                parts.extend(&r.iflets.iter().map(|il| il.to_doc()));
                parts.push_str(&r.expr.to_doc());
                return parts
            }
            Def::Extractor(ref e) => {
                // sexp(vec![
                //     RcDoc::text("extractor"),
                //     sexp(
                //         Vec::from([e.term.to_doc()])
                //             .into_iter()
                //             .chain(e.args.iter().map(|v| v.to_doc())),
                //     ),
                //     e.template.to_doc(),
                // ])

                let mut parts = String::from("extractor");
                parts.push_str(&e.term.to_doc());
                // Add something else here in the middle
                parts.push_str(&e.template.to_doc());
                return parts
            }
            Def::Decl(ref d) => {
                let mut parts = String::from("decl");
                if d.pure {
                    parts.push_str("pure");
                }
                if d.multi {
                    parts.push_str("multi");
                }
                if d.partial {
                    parts.push_str("partial");
                }
                parts.push_str(&d.term.to_doc());
                // Same question how do you convert from a Map to a String?
                parts.push_str(sexp(d.arg_tys.iter().map(|ty| ty.to_doc())));
                parts.push_str(&d.ret_ty.to_doc());
                return parts
            }
            Def::Extern(ref e) => e.to_doc(),
            Def::Converter(ref c) => {
                let mut parts = String::from("convert");
                parts.push_str(&c.inner_ty.to_doc());
                parts.push_str(&c.outer_ty.to_doc());
                parts.push_str(&c.term.to_doc());
                return parts
            }
        }
    }
}

impl Printable for Ident {
    fn to_doc(&self) -> String {
        self.0.clone() 
    }
}

impl Printable for TypeValue {
    fn to_doc(&self) -> String {
        match self {
            TypeValue::Primitive(ref name, _) => {
                format!("primative {}", &name.to_doc())
            }
            TypeValue::Enum(ref variants, _) => {
                // What does this function do?
                // sexp(
                //     Vec::from([RcDoc::text("enum")])
                //         .into_iter()
                //         .chain(variants.iter().map(|v| v.to_doc()))
            }
        }
    }
}


impl Printable for Variant {
    fn to_doc(&self) -> String {
        todo!("Meg TODO") 
    }
}

impl Printable for Field {
    fn to_doc(&self) -> String {
        format!("{} {}", &self.name.to_doc(), &self.ty.to_doc())
    }
}

impl Printable for Pattern {
    fn to_doc(&self) -> String {
        match self {
            Pattern::Var { var, .. } => var.to_doc(),
            Pattern::BindPattern { var, subpat, .. } => format!("{}@{}", &var.to_doc(), &subpat.to_doc()),
            Pattern::ConstInt { val, .. } => format!("{}", val),
            Pattern::ConstPrim { val, .. } => format!("${}", &val.to_doc()),
            Pattern::Wildcard { .. } => String::from("_"),
            Pattern::Term { sym, args, .. } => {
                // sexp(
                //     Vec::from([sym.to_doc()])
                //         .into_iter()
                //         .chain(args.iter().map(|f| f.to_doc()))
            }
            Pattern::And { subpats, .. } => {
                // sexp(
                //     Vec::from([RcDoc::text("and")])
                //         .into_iter()
                //         .chain(subpats.iter().map(|p| p.to_doc()))
            }
            Pattern::MacroArg { .. } => unimplemented!("macro arguments are for internal use only"),
        }
    }
}

impl Printable for IfLet {
    fn to_doc(&self) -> String {
        format!("if-let{}{}", &self.pattern.to_doc(), &self.expr.to_doc())
    }
}

impl Printable for Expr {
    fn to_doc(&self) -> String {
        match self {
            Expr::Term { sym, args, .. } => {
                // sexp(
                // Vec::from([sym.to_doc()])
                //     .into_iter()
                //     .chain(args.iter().map(|f| f.to_doc())),
            }
            Expr::Var { name, .. } => name.to_doc(),
            Expr::ConstInt { val, .. } => format!("{}", val),
            Expr::ConstPrim { val, .. } => format!("${}", val.to_doc()),
            Expr::Let { defs, body, .. } => {
                let mut parts = String::from("let");
                // Convert from a map to a string
                parts.push_str(sexp(defs.iter().map(|d| d.to_doc())));
                parts.push_str(&body.to_doc());
                return parts
            }
        }
    }   
}

impl Printable for LetDef {
    fn to_doc(&self) -> String {
        format!("{}{}{}", &self.var.to_doc(), &self.ty.to_doc(), &self.val.to_doc())
    }
}

impl Printable for Extern {
    fn to_doc(&self) -> String {
        match self {
            Extern::Extractor {
                term,
                func,
                pos: _,
                infallible,
            } => {
                let mut parts = String::from("extern-extractor");
                if *infallible {
                    parts.push_str("infallible");
                }
                parts.push_str(&term.to_doc());
                parts.push_str(&func.to_doc());
                return parts
            }
            Extern::Constructor { term, func, .. } => format!("extern-constructor{}{}", &term.to_doc(), &func.to_doc()),
            Extern::Const { name, ty, .. } => format!("extern-const${}{}", &name.to_doc(), &ty.to_doc()),
        }
    }
}
