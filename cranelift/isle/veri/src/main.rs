use std::collections::HashMap;

use clap::Parser;
use cranelift_codegen_meta::{generate_isle, isle::get_isle_compilations};
use cranelift_isle::error::Errors;
use cranelift_isle::sema::{
    self, ConstructorKind, ExtractorKind, Pattern, Rule, RuleId, Term, TermEnv, TermId, TermKind,
    TypeEnv,
};
use cranelift_isle::{lexer, parser};

#[derive(Parser)]
struct Opts {
    /// Name of the ISLE compilation.
    #[arg(long, required = true)]
    name: String,

    /// Path to codegen crate directory.
    #[arg(long, required = true)]
    codegen_crate_dir: std::path::PathBuf,

    /// Working directory.
    #[arg(long, required = true)]
    work_dir: std::path::PathBuf,
}

impl Opts {
    fn isle_input_files(&self) -> anyhow::Result<Vec<std::path::PathBuf>> {
        // Generate ISLE files.
        let gen_dir = &self.work_dir;
        generate_isle(gen_dir)?;

        // Lookup ISLE compilations.
        let compilations = get_isle_compilations(&self.codegen_crate_dir, gen_dir);

        // Return inputs from the matching compilation, if any.
        Ok(compilations
            .lookup(&self.name)
            .ok_or(anyhow::format_err!(
                "unknown ISLE compilation: {}",
                self.name
            ))?
            .inputs())
    }
}

fn main() -> anyhow::Result<()> {
    let opts = Opts::parse();

    // Read ISLE inputs.
    let inputs = opts.isle_input_files()?;
    let prog = Program::from_files(&inputs)?;

    // Stats.
    let rules = prog.rules_by_term();
    let mut total_num_terms = 0;
    let mut total_num_rules = 0;
    let mut term_class_counts: HashMap<String, usize> = HashMap::new();
    for term in &prog.termenv.terms {
        let rule_ids = rules.get(&term.id).cloned().unwrap_or_default();
        let class = classify_term(&prog, term, &rule_ids);
        *term_class_counts.entry(class.clone()).or_default() += 1;

        total_num_terms += 1;
        total_num_rules += rule_ids.len();

        println!("term = {}", prog.term_name(term.id));
        println!("\tclass = {class}");
        println!("\tnum_rules = {}", rule_ids.len());
    }

    println!("total_num_terms = {total_num_terms}",);
    println!("total_num_rules = {total_num_rules}");
    for (class, count) in term_class_counts {
        println!("total_class: {class} = {count}");
    }

    Ok(())
}

fn classify_term(prog: &Program, term: &Term, rule_ids: &Vec<RuleId>) -> String {
    if term.is_enum_variant() {
        return "enum_variant".to_string();
    }

    if term.has_external_constructor() || term.has_external_extractor() {
        return "external".to_string();
    }

    if term.has_extractor() {
        return "extractor".to_string();
    }

    assert!(term.has_constructor());

    if rule_ids.len() == 1 && is_macro_rule(prog.rule(rule_ids[0])) {
        return "macro".to_string();
    }

    return "other".to_string();
}

pub struct Program {
    tyenv: TypeEnv,
    termenv: TermEnv,
}

impl Program {
    pub fn from_files(paths: &Vec<std::path::PathBuf>) -> Result<Self, Errors> {
        let lexer = lexer::Lexer::from_files(paths)?;
        let defs = parser::parse(lexer)?;
        let mut tyenv = sema::TypeEnv::from_ast(&defs)?;
        let termenv = sema::TermEnv::from_ast(&mut tyenv, &defs)?;

        Ok(Self { tyenv, termenv })
    }

    pub fn term(&self, term_id: TermId) -> &Term {
        self.termenv
            .terms
            .get(term_id.index())
            .expect("invalid term id")
    }

    pub fn term_name(&self, term_id: TermId) -> String {
        let term = self.term(term_id);
        self.tyenv.syms[term.name.index()].clone()
    }

    pub fn rule(&self, rule_id: RuleId) -> &Rule {
        self.termenv
            .rules
            .get(rule_id.index())
            .expect("invalid rule id")
    }

    pub fn rules_by_term(&self) -> HashMap<TermId, Vec<RuleId>> {
        let mut rules: HashMap<TermId, Vec<RuleId>> = HashMap::new();
        for rule in &self.termenv.rules {
            rules.entry(rule.root_term).or_default().push(rule.id);
        }
        rules
    }
}

fn is_macro_rule(rule: &Rule) -> bool {
    if !rule.iflets.is_empty() {
        return false;
    }

    for arg in &rule.args {
        if !is_any_pattern(arg) {
            return false;
        }
    }

    return true;
}

fn is_any_pattern(pattern: &Pattern) -> bool {
    match pattern {
        Pattern::BindPattern(_, _, subpat) => is_any_pattern(&subpat),
        Pattern::Wildcard(_) => true,
        _ => false,
    }
}
