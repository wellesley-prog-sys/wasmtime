use cranelift_isle as isle;
use isle::ast::*;
use isle::sema::{Pattern, TermEnv, TypeEnv, VarId};
use std::path::Path;
// use crate::isle::sema;
use crate::isle::ast;
use isle::lexer::Pos;
use isle::compile::create_envs;
use std::fs::File;
use std::io::{self, BufRead, BufReader};

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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NotAnInst {
    pub name: Ident,
    pub args: Vec<Expr>,
}

pub fn ident_string(varname: Ident) -> String {
    format!("{} {:?}", varname.0, varname.1)
}

fn convert_pattern(pattern: &ast::Pattern, typeenv: &TypeEnv) -> Expr {
    match pattern {
        ast::Pattern::Var { var, .. } => {
            let name = var.0.clone();
            Expr::Var(name)
        }, 
        ast::Pattern::ConstInt { val, .. } => Expr::Int(*val),
        ast::Pattern::Term { sym, args, .. } => {
            let converted_args = args.iter().map(|a| convert_pattern(a, typeenv)).collect();
            let instr = lines_from_file("instructions.txt");
            let name = sym.0.clone();
            let is_inst_in_list = instr.contains(&name);
            if is_inst_in_list {
                Expr::Inst(
                    Inst {
                        name: Ident(sym.0.clone(), Pos::default()),
                        args: converted_args,
                    }
                )
            } else {
                let not_an_inst_expr = NotAnInst {
                    name: Ident(sym.0.clone(), Pos::default()),
                    args: converted_args,
                };
                Expr::NotAnInst(not_an_inst_expr.args)
            }
        }
        ast::Pattern::Wildcard { .. } => todo!(), 
        ast::Pattern::BindPattern {..} => todo!(),
        _ => todo!(), 
    }
}

fn lines_from_file(filename: impl AsRef<Path>) -> Vec<String> {
    let file = File::open(filename).expect("no such file");
    let buf = BufReader::new(file);
    buf.lines()
        .map(|l| l.expect("Could not parse line"))
        .collect()
}

fn convert_rules(filename: impl AsRef<Path>) -> Vec<Expr>{
    let lexer = isle::lexer::Lexer::from_files([&filename]).unwrap();
    let defs = isle::parser::parse(lexer).expect("should parse");
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

fn to_clif_list(
    e: Expr,
    index: &mut i32,
    variables: &mut Vec<String>,
) -> Vec<(String, String)> {
    match e {
        Expr::Inst(i) => {
            if ident_string(i.name.clone()) == "iconst" {
                let fresh = format!("v{}", *index);
                *index += 1;
                return vec![(fresh, "iconst ?".to_string())];
            }
            let mut insts: Vec<(String, String)> = Vec::new();
            let mut this_inst = ident_string(i.name);
            for a in i.args {
                let result = to_clif_list(a, index, variables);
                insts.extend(result.clone()); 
                let val_returned = result.last().unwrap().0.clone();
                this_inst += &format!(" {}", val_returned);
            }
            let fresh = format!("v{}", *index);
            *index += 1;
            insts.push((fresh.clone(), this_inst));
            insts
        }
        Expr::NotAnInst(i) => { todo!();
        //     let mut this_inst = ident_string(i.name);
        //     for a in i.args { // question
        //         let result = to_clif_list(a, index, variables);
        //         insts.extend(result.clone()); // Extend instead of append
        //         let val_returned = result.last().unwrap().0.clone(); // Get the last element
        //         this_inst += &format!(" {}", val_returned); // Modify this_inst
        //     }, // Implement this branch as per your requirement
         }
        Expr::Var(s) => {
            variables.push(s.clone());
            let fresh = format!("v{}", *index);
            *index += 1;
            vec![(fresh, s)]
        }
        Expr::Int(i) => {
            let fresh = format!("v{}", *index);
            *index += 1;
            vec![(fresh, i.to_string())]
        }
    }
}

// (ineg (iadd (isub x y) (iconst k)))
/**
 * [ ("v0", "x"),
 *   ("v1", "y"),
 *   ("v2", "isub v0 v1"),
 *   ("v3", "iconst ?"),
 *   ("v4", "iadd v2 v3"),
 *   ("v5", "ineg v4"),
 * ]
 * 
 * 
 *   block(x: i32, y: i32):
 *  v0 = x
 *  v1 = y
 *  v2 = isub v0 v1
 * 
 * 
 * 
 *   v5 = ineg v4
 *   return v5
 */


fn main() {

    //let to_print: Vec<Expr> = convert_rules("amod_unextended.isle");
    let list_Expr: Vec<Expr> = convert_rules("small.isle");
    let mut result: Vec<(String, String)> = Vec::new();
    let variables: &mut Vec<String> = &mut Vec::new();
    let mut count: i32 = 0;

    for element in list_Expr{
        result.extend(to_clif_list(element, &mut count, variables));
    }
    println!("{:?}", result);
    //println!("{:?}",to_print);
    }

    // for rule in to_print {
        // let pattern = &rule.pattern;
        // Now you can use `pattern` as needed
        // println!("{:?}", pattern);
        // println!("{:?}", rule);
    // }
    // dbg!(&pattern)

// TODO 4/16
// 1. check term against list of instructions --> only want isntructions, rest be notinst.
    // not print pos by first converting it into a string
// 2. convert a expr into valid input (webasm) cranelift IR (do cranelift) code -> operating on exprs (tree traversal)
    // most deeply nested thing gets used first


    // cargo run --bin expr



/*pub fn find_instr(exprs: Vec<Expr>) -> Vec<String> {
    let instr = lines_from_file("instructions.txt");
    let mut filtered_instr = Vec::new();
    for expr in exprs {
        if let Expr::Inst(inst) = expr {
            if let Ident(ident, _) = inst.name {
                filtered_instr.push(ident);
            }
        }
    }
    filtered_instr
}
*/