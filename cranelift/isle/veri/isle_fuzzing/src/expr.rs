use cranelift_isle::trie_again::RuleSet;
use cranelift_isle as isle;
use isle::ast::*;
use isle::sema::{Pattern, TermEnv, TypeEnv, VarId, TermKind, ConstructorKind, ExtractorKind, Sym,IfLet};
use isle::ast::Expr as semaExpr;
use std::path::Path;
use crate::isle::ast;
use isle::lexer::Pos;
use isle::compile::create_envs;
use std::fs::File;
use std::io::{self, BufRead, BufReader};
use rand::Rng;
use isle::codegen::*;
// use isle::serialize::{RuleSet, Rule, Binding};



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


fn process_non_inst_term(
    // will take in the name from convert pattern
    term_name: &ast::Ident,
    termenv: &TermEnv,
    typeenv: &TypeEnv,
    constructors: &mut Vec<String>,   // ExternalConstructor list
    extractors: &mut Vec<String>      // ExternalExtractor list
) {
    println!("{:?}", term_name.0);
    // Find the equivalent sema term
    if let Some(term_id) = termenv.get_term_by_name(typeenv, term_name) {
        // Use term_id to access the whole term from termenv
        if let Some(term) = termenv.terms.get(term_id.index()) {
            match &term.kind { //term as two kinds
                TermKind::Decl { .. } => {
                    if term.has_external_constructor() {
                        constructors.push(term_name.0.clone());
                        if let Some(sig) = term.constructor_sig(typeenv) {
                            println!("Constructor: {}\n", sig.full_name);
                        }
                    }
                    if term.has_external_extractor() {
                        extractors.push(term_name.0.clone());
                        if let Some(sig) = term.extractor_sig(typeenv) {
                            println!("Extractor: {}\n", sig.full_name);
                        }
                    }
                }
                TermKind::EnumVariant { .. } => {
                    println!("Term is an enum variant: {:?}\n", term_name.0);
                }
            }
        } else {
            println!("No term found for term_id: {:?}\n", term_id);
        }
    } else {
        println!("No term found for name: {:?}\n", term_name.0);
    }
}

fn match_inst_or_non_inst(
    sym: &ast::Ident,
    args: Vec<Expr>,
    termenv: &TermEnv,
    typeenv: &TypeEnv,     
    constructors: &mut Vec<String>,
    extractors: &mut Vec<String>
) -> Option<Expr>{
    let instr = lines_from_file("instructions.txt");
    let name = sym.0.clone();

    if instr.contains(&name) {
        Some(Expr::Inst(
            Inst {
                name: Ident(sym.0.clone(), Pos::default()),
                args,
            }
        ))
    } else {
        process_non_inst_term(sym, termenv, typeenv, constructors, extractors);
        let not_an_inst_expr = NotAnInst {
            name: Ident(sym.0.clone(), Pos::default()),
            args,
        };
        Some(Expr::NotAnInst(not_an_inst_expr.args))
        
    }
}


/*
Function convert_pattern
Arguments, pattern, typeEnv
Returns Expr
Notes: Compares each pattern against the instruction list, labels it to Inst, NotAnInst, Var or Int and creates an Expression (as defined in the Struct Above)
*/
fn convert_pattern(
    pattern: &ast::Pattern, 
    typeenv: &TypeEnv,     
    termenv: &TermEnv,
    constructors: &mut Vec<String>,
    extractors: &mut Vec<String>
) -> Option<Expr> {
    match pattern {
        ast::Pattern::Var { var, .. } => {
            let name = var.0.clone();
            println!("I see Var: {}", name);
            Some(Expr::Var(name))
        }, 
        ast::Pattern::ConstInt { val, .. } => {
            println!("I see ConstInt: {}", val);
            Some(Expr::Int(*val))
        },
        ast::Pattern::Term { sym, args, .. } => {
            println!("I see Term: {}", sym.0);
            let converted_args: Vec<Option<Expr>> = args.iter().map(|a| convert_pattern(a, typeenv, termenv, constructors, extractors)).collect();
            let converted_args: Vec<Expr> = converted_args.into_iter().flatten().collect();
            match_inst_or_non_inst(sym, converted_args,termenv,typeenv,constructors,extractors)
        }
        // wildcard was not very specified in sema to begin with
        ast::Pattern::Wildcard { .. } => {
            println!("I see Wildcard");
            None
        }, 
        // 11.20: recurr on box<pattern> for bindpattern; read box in rust box, deref the pat of box patten
        // ast::Pattern::BindPattern {var,subpat,..} => {            
            // 11.20: match inst and non_inst, call helpfer function, see above => expecting non_inst that is external extractor or const
            // let converted_arg = convert_pattern(&*subpat, typeenv, termenv, constructors, extractors)?;
            // match_inst_or_non_inst(var, vec![converted_arg], termenv, typeenv, constructors, extractors)
        ast::Pattern::BindPattern { var, subpat, .. } => {
            println!("Recursing on BindPattern with var: {:?} and subpat: {:?}", var.0, subpat);
            let converted_arg = convert_pattern(&*subpat, typeenv, termenv, constructors, extractors)?;
            match_inst_or_non_inst(var, vec![converted_arg], termenv, typeenv, constructors, extractors)
        

            // 11.20: future, we wanna call the extractor as rust function; before that we need to figureout what args it need
            // 11.20: (More reading/open ended): start finding how to generate Rust, given a name. Likely within cranelift/isle/isle/src/codegen.rs
        },
        _ => todo!(), 
    }
}


// 12.4: convert_expr should look very similar to convert pattern, convert sema.rs expr into our own expr
// 12.4: 2 cases, one for convert pattern (pattern -> our expr), one for convert expr (sema expr -> our expr); put the rest
//              into helpfer function and call after convert_expr/convert_pattern
fn convert_expr(
    expr: &semaExpr,
    typeenv: &TypeEnv,
    termenv: &TermEnv,
    constructors: &mut Vec<String>,
    extractors: &mut Vec<String>,
) -> Option<Expr> {
    match expr {
        semaExpr::Term{sym, args,..} => {
            // convert each arg recursively.
            let converted_args: Vec<Expr> = args.iter()
                .filter_map(|a| convert_expr(a, typeenv, termenv, constructors, extractors))
                .collect();
                match_inst_or_non_inst(sym, converted_args, termenv, typeenv, constructors, extractors)

        }

        semaExpr::Var{name,..} => {

            Some(Expr::Var(name.0.clone()))
        }

        semaExpr::ConstInt{val,..} => {
            println!("convert_expr: I see ConstInt: {}", val);
            Some(Expr::Int(*val))
        }


        semaExpr::ConstPrim{val, ..} => {
            Some(Expr::Var(val.0.clone()))
        }

        semaExpr::Let { defs, body,..} => {
            println!("convert_expr: I see Let");
            let mut exprs = Vec::new();
            // convert body
            if let Some(converted_body) = convert_expr(body, typeenv, termenv, constructors, extractors) {
                exprs.push(converted_body);
            }

            Some(Expr::NotAnInst(exprs))
        }
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
fn convert_rules(
    filename: impl AsRef<Path>,
    constructors: &mut Vec<String>,
    extractors: &mut Vec<String>
) -> (Vec<Expr>,TypeEnv,TermEnv){
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
        if let Some(expr) = convert_pattern(&r.pattern,&typeenv, &termenv, constructors, extractors) {
            list_Expr.push(expr);
        }
        
        // for loop over the iflets
        // find iflet for each r and call convert pattern recursive
        // for each iflet in r.iflet, convert the LHS and RHS of it (property of iflet)
        for iflet in &r.iflets {
            println!("I see iflet: {:?}", iflet.pattern);

            // Recurse on the `iflet` LHS pattern
            if let Some(expr) = convert_pattern(&iflet.pattern, &typeenv, &termenv, constructors, extractors) {
                println!("Processed iflet pattern to expr LHS: {:?}", expr);
                list_Expr.push(expr);
            }

            // RHS
            // 12.4: convert_expr should look very similar to convert pattern, convert sema.rs expr into our own expr
            // 12.4: 2 cases, one for convert pattern (pattern -> our expr), one for convert expr (sema expr -> our expr); put the rest
            //              into helpfer function and call after convert_expr/convert_pattern
            if let Some(expr) = convert_expr(&iflet.expr, &typeenv, &termenv, constructors, extractors) {   // 12.4 TODO: convert expr to get RHS iflet
                println!("Processed iflet pattern to expr RHS: {:?}", expr);
                list_Expr.push(expr);
            }
            // 12.4 TODO: once this works, see how to call rust function w/ the same name & only compile if match rule constraint
                            // might want to do this in a separate rust file and call that file in here to generate ->reference codegen.rs
        }
    }
    return (list_Expr,typeenv,termenv);
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
            let random_int: i32 = rng.gen_range(0..99999999);
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

// Filters and prints only the External Extractors
fn filter_external_extractors(extractors: &Vec<String>) {
    println!("\nExternal Extractors:");
    for extractor in extractors {
        println!("{}", extractor);
    }
    println!()
}

// 12.11: check if these are actually filtering *external* const and exst; also wrong spelling
// Filters and prints only the External Constructors
// already filtered to external because used 'has_external_constructor' when making the constructors list
fn filter_external_constructors(constructors: &Vec<String>) {
    println!("\nExternal Constructors:");
    for constructor in constructors {
        println!("{}", constructor);
    }
    println!()
}

// fn generate_rust(&self, options: &CodegenOptions) -> String {
//     let mut code = String::new();

//     self.generate_header(&mut code, options);
//     self.generate_ctx_trait(&mut code);
//     self.generate_internal_types(&mut code);
//     self.generate_internal_term_constructors(&mut code).unwrap();

//     code
// }
// fn generate_rust_attempt(typeenv: &TypeEnv, termenv: &TermEnv, terms,options)->String{
//     return isle::codegen::codegen(typeenv, termenv, terms, options);
// }


fn main() {
    let littledots = vec![".i8",".i16", ".i32", ".i64"];

    let mut constructors: Vec<String> = Vec::new();  // List to store ExternalConstructor terms
    let mut extractors: Vec<String> = Vec::new();    // List to store ExternalExtractor terms
    
    let convert_rules_result = convert_rules("amod_unextended.isle", &mut constructors, &mut extractors);

    let list_Expr: Vec<Expr> = convert_rules_result.0;
    let typeenv: TypeEnv = convert_rules_result.1;
    let termenv : TermEnv = convert_rules_result.2;

    let mut result: Vec<(String, String)> = Vec::new();
    let mut variables: Vec<String> = Vec::new();
    let mut rng = rand::thread_rng();
    let mut count: i32 = 0;
    let random_uextend = littledots[rng.gen_range(0..4)];
    for element in list_Expr{
        result.extend(to_clif_list(element, &mut count, &mut variables, random_uextend.to_string()));
    }
    
    filter_external_extractors(&extractors);
    // 12.11 TODO: filter external constractors as well, print them, and try calling them as rust functions
    // 12.11 TODO: look at codegen.rs for call rust func inspiration; or add to extractor list also the # of args that needs to be passed in
    filter_external_constructors(&constructors);

    let mut program = String::new();
    program = format_output(result, variables, random_uextend.to_string());


    // 12.15 code gen attempts

    // let amode_add_sym = ast::Ident("amode_add".to_string(), Pos::default()); 
    // // loopup amode_add in termenv to try hard coding term ID
    // let amode_add_term_id = termenv
    //     .get_term_by_name(&typeenv, &amode_add_sym);
    // println!("{:?}",amode_add_term_id); // term id of amode_add is 9

    // !!!HOW DO I GET RULESET

    // let amode_add_ruleset = build_ruleset_for_term(amode_add_term_id, &defs, &typeenv, &termenv);

    // let terms: Vec<(TermId, RuleSet)> = vec![
    //     (amode_add_term_id, amode_add_ruleset)
    // ];

    // let options = CodegenOptions { exclude_global_allow_pragmas: false };
    // let mut generated = generate_rust_attempt(&typeenv, &termenv, &terms, &options);
}



// final goal for generation:   (if (u32_lteq (u8_as_u32 shift) 3)) -> can generate variable that matches this lteq 3 constraint 
                        //          this is iflet RHS, which 12.4 us is not handling rn