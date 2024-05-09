use cranelift_isle::error::Errors;
use cranelift_isle::sema::{self, Rule, RuleId, Term, TermEnv, TermId, TypeEnv};
use cranelift_isle::{lexer, parser};
use std::collections::HashMap;

pub struct Program {
    pub tyenv: TypeEnv,
    pub termenv: TermEnv,
}

impl Program {
    pub fn from_files(paths: &Vec<std::path::PathBuf>) -> Result<Self, Errors> {
        let lexer = lexer::Lexer::from_files(paths)?;
        let defs = parser::parse(lexer)?;
        let mut tyenv = sema::TypeEnv::from_ast(&defs)?;
        let expand_internal_extractors = false;
        let termenv = sema::TermEnv::from_ast(&mut tyenv, &defs, expand_internal_extractors)?;

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
