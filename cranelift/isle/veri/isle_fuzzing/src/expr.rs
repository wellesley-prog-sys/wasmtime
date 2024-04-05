use cranelift_isle as isle;
use isle::ast::*;
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


fn convert_rules(filename: impl AsRef<Path>) -> Vec<cranelift_isle::ast::Rule>{
    let lexer = isle::lexer::Lexer::from_files([&filename]).unwrap();
    let defs = isle::parser::parse(lexer).expect("should parse");

    let rules: Vec<_> = defs.defs.iter().filter_map(|def| {
        if let Def::Rule(rule) = def {
            Some(rule)
        } else {
            None
        }
    }).cloned().collect(); 
    let mut list_Expr = Vec::new(); //Expr
    for r in &rules{
        list_Expr.push(convert_pattern(&r.pattern));
    }
    //dbg!(&defs);
    //println!(defs);
    println!("{:?}", list_Expr);
    return rules;
}

fn convert_pattern<Pattern>(pattern: Pattern) -> Pattern{ //Return Expr
    return pattern; //Return Expr
}

fn main() {
    let to_print: Vec<cranelift_isle::ast::Rule> = convert_rules("construct_and_extract.isle");
    //println!("{:?}",to_print);

    for rule in to_print {
        let pattern = &rule.pattern;
        // Now you can use `pattern` as needed
        println!("{:?}", pattern);
    }
    // dbg!(&pattern)
}
