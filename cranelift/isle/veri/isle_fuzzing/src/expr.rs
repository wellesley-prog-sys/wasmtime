use cranelift_isle as isle;
use isle::ast::{Decl, Defs};
use isle::sema::{Pattern, TermEnv, TypeEnv, VarId};
use std::path::Path;
use isle::lexer::Pos;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expr {
  Var(String),
  Int(i128),
  Inst(Inst),
  NotAnInst(Vec<Expr>)
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Ident(pub String, pub Pos);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Inst {
    pub name: Ident,
    pub args: Vec<Expr>,
}


fn convert_rules(filename: impl AsRef<Path>) -> Vec<Expr>{
    let lexer = isle::lexer::Lexer::from_files([&filename]).unwrap();
    let defs = isle::parser::parse(lexer).expect("should parse");
    dbg!(&defs);
    todo!()
    //return defs;
    //Defs = parse(filename) => defs.iter().filter() matches (Rule!) (just keep the definitions that are rules
}

fn main() {
    let toPrint: Vec<Expr> = convert_rules("construct_and_extract.isle");
    println!("{:?}",toPrint);
}
