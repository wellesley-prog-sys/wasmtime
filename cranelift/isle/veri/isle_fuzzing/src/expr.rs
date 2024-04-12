use cranelift_isle as isle;
use isle::ast::*;
use isle::sema::{Pattern, TermEnv, TypeEnv, VarId};
use std::path::Path;
// use crate::isle::sema;
use crate::isle::ast;
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

fn print_type_of<T>(_: &T) {
    println!("{}", std::any::type_name::<T>())
}

fn convert_pattern(pattern: &ast::Pattern) -> Expr {
    match pattern {
        ast::Pattern::Var { .. } => Expr::Var("".to_string()),
        ast::Pattern::ConstInt { val, .. } => Expr::Int(*val),
        ast::Pattern::Term { sym, args, .. } => {
            let args_expr: Vec<Expr> = args.iter().map(|arg| convert_pattern(arg)).collect();
            Expr::Inst(Inst {
                name: Ident(sym.0.clone(), Pos::default()), //Pos????
                args: args_expr,
            })
        }
        ast::Pattern::Wildcard { .. } => todo!(),
        ast::Pattern::BindPattern{..} => todo!(),
        _ => todo!(),
    }
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
        // println!("Test");
        list_Expr.push(convert_pattern(&r.pattern));
    }
    //dbg!(&defs);
    //println!(defs);
    //println!("{:?}", list_Expr);
    //print_type_of(&list_Expr);
    // println!("{:?}",  list_Expr[0]);
    // print_type_of(&list_Expr[0]);
    return rules;
}

fn main() {
    let to_print: Vec<cranelift_isle::ast::Rule> = convert_rules("construct_and_extract.isle");

    println!("{:?}",to_print);
    for rule in to_print {
        let pattern = &rule.pattern;
        // Now you can use `pattern` as needed
        println!(); 
        println!("{:?}", pattern);
        println!(); 
        print_type_of(&pattern);
    }
    // dbg!(&pattern)
}