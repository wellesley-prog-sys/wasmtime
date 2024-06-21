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

impl Printable for Def {
    fn to_doc(&self) -> String {
        todo!("Meg TODO") 
    }
}

impl Printable for Ident {
    fn to_doc(&self) -> String {
        todo!("Meg TODO") 
    }
}

impl Printable for TypeValue {
    fn to_doc(&self) -> String {
        todo!("Meg TODO") 
    }
}

impl Printable for Variant {
    fn to_doc(&self) -> String {
        todo!("Meg TODO") 
    }
}

impl Printable for Field {
    fn to_doc(&self) -> String {
        todo!("Meg TODO") 
    }
}

impl Printable for Pattern {
    fn to_doc(&self) -> String {
        todo!("Meg TODO") 
    }
}

impl Printable for IfLet {
    fn to_doc(&self) -> String {
        todo!("Meg TODO") 
    }
}

impl Printable for Expr {
    fn to_doc(&self) -> String {
        todo!("Meg TODO") 
    }   
}

impl Printable for LetDef {
    fn to_doc(&self) -> String {
        todo!("Meg TODO") 
    }
}

impl Printable for Extern {
    fn to_doc(&self) -> String {
        todo!("Meg TODO") 
    }
}
