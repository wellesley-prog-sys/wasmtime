use cranelift_isle as isle;
use isle::ast::*;
use isle::sema::{Pattern, TermEnv, TypeEnv, VarId};
use std::path::Path;
// use crate::isle::sema;
use crate::isle::ast;
use crate::isle::StableMap;
use std::collections::HashMap;
use isle::lexer::Pos;
use isle::compile::create_envs;

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


fn convert_pattern(pattern: &ast::Pattern, typeenv: &TypeEnv) -> Expr {
    match pattern {
        ast::Pattern::Var {var,pos} => {
            let name = var.0.clone();
            Expr::Var(name)
        }, 
        ast::Pattern::ConstInt { val, .. } => Expr::Int(*val),
        ast::Pattern::Term { sym, args, .. } => {
            let converted_args = args.iter().map(|a| convert_pattern(a,typeenv)).collect();
            Expr::Inst(
                Inst {
                    name: Ident(sym.0.clone(), Pos::default()), //Pos????
                    //check against list of instructions vs. name
                    args: converted_args,
            })
        }
        ast::Pattern::Wildcard { .. } => todo!(), 
        ast::Pattern::BindPattern{..} => todo!(),
        _ => todo!(), 
    }
}


fn convert_rules(filename: impl AsRef<Path>) -> Vec<Expr>{
    let lexer = isle::lexer::Lexer::from_files([&filename]).unwrap();
    let defs = isle::parser::parse(lexer).expect("should parse");

    // Produces environments including terms, rules, and maps from symbols and
    // names to types
    let (typeenv, termenv) = create_envs(&defs).unwrap();

    let rules: Vec<_> = defs.defs.iter().filter_map(|def| {
        if let Def::Rule(rule) = def {
            Some(rule)
        } else {
            None
        }
    }).cloned().collect(); 
    let mut list_Expr = Vec::new(); //Expr
    // let mut if_Expr = Vec::new(); //Expr
    for r in &rules{
        // println!("{:?}", r);
        list_Expr.push(convert_pattern(&r.pattern,&typeenv));
        // for iflet in &r.iflets {
        //     if_Expr.push(convert_pattern(&iflet.pattern,&typeenv));
        // }
    }
    //dbg!(&defs);
    //println!(defs);
    // println!("{:?}", list_Expr);
    return list_Expr;
}


fn main() {

    // let typeenv = TypeEnv {
    //     filenames: Vec::new(),
    //     file_texts: Vec::new(),
    //     syms: Vec::new(),
    //     sym_map: StableMap::default(), // Initialize with StableMap
    //     types: Vec::new(),
    //     type_map: StableMap::default(), // Initialize with StableMap
    //     const_types: StableMap::default(), // Initialize with StableMap
    //     errors: Vec::new(),
    // };

    let to_print: Vec<Expr> = convert_rules("amod_unextended.isle");
    println!("{:?}",to_print);

    for rule in to_print {
        // let pattern = &rule.pattern;
        // Now you can use `pattern` as needed
        // println!("{:?}", pattern);
        // println!("{:?}", rule);
    }
    // dbg!(&pattern)
}

// TODO 4/16
// 1. check term against list of instructions --> only want isntructions, rest be notinst.
    // not print pos by first converting it into a string
// 2. convert a expr into valid input (webasm) cranelift IR (do cranelift) code -> operating on exprs (tree traversal)
    // most deeply nested thing gets used first
