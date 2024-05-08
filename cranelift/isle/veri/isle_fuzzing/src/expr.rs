use cranelift_isle as isle;
use isle::ast::*;
use isle::sema::{Pattern, TermEnv, TypeEnv, VarId};
use std::path::Path;
use crate::isle::ast;
use isle::lexer::Pos;
use isle::compile::create_envs;
use std::fs::File;
use std::io::{self, BufRead, BufReader};
use rand::Rng;

/*
Creating the Enums and Structs
Expr has type Var, Int, Inst and NotAnInst
*/

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
/*
Function ident_string
Arguments, ident
Returns String
Notes: Helper function to turn Idents to a String type
*/
pub fn ident_string(varname: Ident) -> String {
    format!("{}", varname.0)
}
/*
Function convert_pattern
Arguments, pattern, typeEnv
Returns Expr
Notes: Compares each pattern against the instruction list, labels it to Inst, NotAnInst, Var or Int and creates an Expression (as defined in the Struct Above)
*/
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

/*
Function lines_from_file
Arguments: filename
Returns Vector of Strings
Notes: Creates a vector of strings given the text file. Used to generate the list of instructions. 
Credit: Taken from //https://stackoverflow.com/questions/30801031/read-a-file-and-get-an-array-of-strings
*/

fn lines_from_file(filename: impl AsRef<Path>) -> Vec<String> {
    let file = File::open(filename).expect("no such file");
    let buf = BufReader::new(file);
    buf.lines()
        .map(|l| l.expect("Could not parse line"))
        .collect()
}

/*
Function convert_rules
Arguments: filename
Returns Vector of Expr
Notes: Parses isle file into the rule, Uses convert_pattern to create a vector of expressions
*/
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
    let mut list_Expr = Vec::new();
    for r in &rules{
        list_Expr.push(convert_pattern(&r.pattern,&typeenv));
    }
    return list_Expr;
}

/*
Function to_clif_list
Arguments: Expr, Index, Variables, random_uextend
Returns Vector of Expr
Notes: Takes in an Expr, runs through a match statement of Inst, NotAnInst, Var and Int and returns a Tuple in (variable, instruction) format
*/
fn to_clif_list(
    e: Expr,
    index: &mut i32,
    variables: &mut Vec<String>,
    random_uextend: String,
) -> Vec<(String, String)> {
    match e {
        Expr::Inst(i) => {
            let mut name = ident_string(i.name.clone());
            let littledots = vec![".i8",".i16", ".i32", ".i64"];
            if name.clone() == "iconst" {
                let mut rng = rand::thread_rng();
                let newname = name + littledots[rng.gen_range(0..4)];
                let new = newname.clone() + &" ?".to_string();
                let fresh = format!("v{}", *index);
                *index += 1;
                return vec![(fresh, new.to_string())];
            }
            if name.clone() == "uextend" {
                name = name + &random_uextend;
            }
            let mut insts: Vec<(String, String)> = Vec::new();
            let mut this_inst = name;
            let mut v_values = Vec::new();
            for a in i.args {
            
                let result = to_clif_list(a, index, variables, random_uextend.to_string());
                insts.extend(result.clone()); 
                let val_returned = result.last().unwrap().0.clone();
                v_values.push(val_returned.clone()); 
            }
                    if v_values.len() >= 2 {
                        this_inst += " ";
                        this_inst += &v_values.join(", ");
                    } else if let Some(single_v) = v_values.first() {
                        let new = format!(" {}", single_v);
                        this_inst += &new;
                    }

            let fresh = format!("v{}", *index);
            *index += 1;
            insts.push((fresh.clone(), this_inst));
            insts
        }
        Expr::NotAnInst(args) => {
            let mut insts: Vec<(String, String)> = Vec::new();
            for a in args {
                let result = to_clif_list(a, index, variables, random_uextend.to_string());
                insts.extend(result.clone()); 
            }
            insts
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

/*
Function format_output
Arguments: tuple, variable, random_uextend
Returns String
Notes: Takes in the tuple and formats it to a string containing the clif file. 
*/
 fn format_output(tup: Vec<(String, String)>, vars: Vec<String>, random_uextend: String) -> String {
    let mut output = format!("function %f0(");
    let uextenddotless = random_uextend.replace(".", "");
    for (i, _) in vars.iter().enumerate() {
        let temp_str = &("i32) ->".to_string() + &uextenddotless.to_string() + " {\n");

        output += if i == vars.len() - 1 { temp_str} else { "i32, " };
    }
    output += &format!("block0(");
    for (i, _) in vars.iter().enumerate() {
        let block = if i == vars.len() - 1 {
            format!("{}:i32):\n", tup[i].0)
        } else {
            format!("{}: i32, ", tup[i].0)
        };
        output += &block;
    }
    for (var, expr) in &tup {
        let add_to_output = false;
        let expr_with_rand = if expr.contains("?") {
            let mut rng = rand::thread_rng();
            let random_int: i32 = rng.gen_range(0..10);
            expr.replace("?", &random_int.to_string())
        } else {
            expr.clone()
        };
        let add_to_output = !vars.iter().any(|r| expr_with_rand.contains(r));

        if add_to_output {
            output += &format!("  {} = {}\n", var, expr_with_rand);
        }
    }
    let last_var = &tup[tup.len() - 1].0;
    output += &format!("return {};\n}}", last_var);
    println!("{}", output);
    output
}

fn main() {
    let littledots = vec![".i8",".i16", ".i32", ".i64"];
    let list_Expr: Vec<Expr> = convert_rules("amod_unextended.isle");
    let mut result: Vec<(String, String)> = Vec::new();
    let mut variables: Vec<String> = Vec::new();
    let mut rng = rand::thread_rng();
    let mut count: i32 = 0;
    let random_uextend = littledots[rng.gen_range(0..4)];
    for element in list_Expr{
        result.extend(to_clif_list(element, &mut count, &mut variables, random_uextend.to_string()));
    }
    let mut program = String::new();
    program = format_output(result, variables, random_uextend.to_string());
    }