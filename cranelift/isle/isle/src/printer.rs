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
    // Confused what this function is achieving...
    // node.to_doc()
    //     .render(width, out)
    //     .map_err(|e| Errors::from_io(e, "failed to print isle"))
}

impl Printable for Defs {
    fn to_doc(&self) -> String {
        // Confused what this function is achieving...
        // let sep = RcDoc::hardline().append(Doc::hardline());
        // RcDoc::intersperse(self.defs.iter().map(|d| d.to_doc()), sep).append(Doc::hardline()) 
    }
}

impl Printable for Def {
    fn to_doc(&self) -> String {
        // Missing match arm error?
        match self {
            Def::Pragma(_) => unimplemented!("pragmas not supported"),
            Def::Type(ref t) => {
                let mut parts = format!("(type {} ", &t.name.to_doc());
                if t.is_extern {
                    parts.push_str("extern ");
                }
                if t.is_nodebug {
                    parts.push_str("nodebug ");
                }
                parts.push_str(&format!("{})", &t.ty.to_doc()));
                return parts
            }
            Def::Rule(ref r) => {
                let mut parts = String::from("(rule ");
                if let Some(prio) = &r.prio {
                    parts.push_str(&format!("{} ", &prio.to_string()));
                }
                parts.push_str(&format!(
                    "{} {} {})", 
                    &r.pattern.to_doc(),
                    // join is not defined for Maps
                    &r.iflets.iter().map(|il| il.to_doc()).join(" "),
                    &r.expr.to_doc()
                ));
                return parts
            }
            Def::Extractor(ref e) => format!(
                "(extractor {} {} {})",
                &e.term.to_doc(),
                // Same error
                &e.args.iter().map(|v| v.to_doc()).join(" "),
                &e.template.to_doc()
            ),
            Def::Decl(ref d) => {
                let mut parts = String::from("(decl ");
                if d.pure {
                    parts.push_str("pure");
                }
                if d.multi {
                    parts.push_str("multi");
                }
                if d.partial {
                    parts.push_str("partial");
                }
                parts.push_str(&format!(
                    " {} ({}) {})",
                    &d.term.to_doc(),
                    // Same error
                    &(d.arg_tys.iter().map(|ty| ty.to_doc())).join(" "),
                    &d.ret_ty.to_doc()
                ));
                return parts
            }
            Def::Extern(ref e) => e.to_doc(),
            Def::Converter(ref c) => format!(
                "(convert {} {} {})",
                &c.inner_ty.to_doc(),
                &c.outer_ty.to_doc(),
                &c.term.to_doc()
            ),
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
            TypeValue::Primitive(ref name, _) => format!(
                "(primative {})", 
                &name.to_doc()
            ),
            TypeValue::Enum(ref variants, _) => format!(
                "(enum {})", 
                // Same error
                &variants.iter().map(|v| v.to_doc()).join(" ")
            ),
        }
    }
}

impl Printable for Variant {
    fn to_doc(&self) -> String {
        format!(
            "({} {})",
            self.name.to_doc(),
            // Same error
            self.fields.iter().map(|f| f.to_doc()).join(" ")
        )
    }
}

impl Printable for Field {
    fn to_doc(&self) -> String {
        format!(
            "({} {})", 
            &self.name.to_doc(), 
            &self.ty.to_doc()
        )
    }
}

impl Printable for Pattern {
    fn to_doc(&self) -> String {
        match self {
            Pattern::Var { var, .. } => var.to_doc(),
            // Unsure of what RcDoc::intersperse() function accpmplishes
            Pattern::BindPattern { var, subpat, .. } => format!(
                "{} @ {}", 
                &var.to_doc(), 
                &subpat.to_doc()
            ),
            Pattern::ConstInt { val, .. } => val.to_string(),
            Pattern::ConstPrim { val, .. } => format!("${}", &val.to_doc()),
            Pattern::Wildcard { .. } => String::from("_"),
            Pattern::Term { sym, args, .. } => format!(
                "({} {})",
                &sym.to_doc(),
                // Same error
                &args.iter().map(|f| f.to_doc()).join(" ")
            ),
            Pattern::And { subpats, .. } => format!(
                "(and {})",
                // Same error
                &subpats.iter().map(|p| p.to_doc()).join(" ")
            ),
            Pattern::MacroArg { .. } => unimplemented!("macro arguments are for internal use only"),
        }
    }
}

impl Printable for IfLet {
    fn to_doc(&self) -> String {
        format!(
            "(if-let {} {})", 
            &self.pattern.to_doc(), 
            &self.expr.to_doc()
        )
    }
}

impl Printable for Expr {
    fn to_doc(&self) -> String {
        match self {
            Expr::Term { sym, args, .. } => format!(
                "({} {})",
                &sym.to_doc(),
                // Same error
                &args.iter().map(|f| f.to_doc()).join(" ")
            ),
            Expr::Var { name, .. } => name.to_doc(),
            Expr::ConstInt { val, .. } => val.to_string(),
            Expr::ConstPrim { val, .. } => format!("(${})", val.to_doc()),
            Expr::Let { defs, body, .. } => format!(
                "(let {} {})",
                // Same error
                &defs.iter().map(|d| d.to_doc()).join(" "),
                &body.to_doc()
            ),
        }
    }   
}

impl Printable for LetDef {
    fn to_doc(&self) -> String {
        format!(
            "({} {} {})", 
            &self.var.to_doc(), 
            &self.ty.to_doc(), 
            &self.val.to_doc()
        )
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
                let mut parts = String::from("(extern extractor ");
                if *infallible {
                    parts.push_str("infallible ");
                }
                parts.push_str(&format!(
                    "{} {})",
                    &term.to_doc(),
                    &func.to_doc()
                ));
                return parts
            }
            Extern::Constructor { term, func, .. } => format!(
                "extern constructor {} {})", 
                &term.to_doc(), 
                &func.to_doc()
            ),
            Extern::Const { name, ty, .. } => format!(
                "(extern const ${} {})",
                &name.to_doc(),
                &ty.to_doc()
            ),
        }
    }
}