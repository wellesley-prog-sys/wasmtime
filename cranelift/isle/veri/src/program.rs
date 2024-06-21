use cranelift_isle::ast::Ident;
use cranelift_isle::error::{Errors, ErrorsBuilder};
use cranelift_isle::lexer::Pos;
use cranelift_isle::sema::{self, Rule, RuleId, Term, TermEnv, TermId, TypeEnv};
use cranelift_isle::trie_again::{self, RuleSet};
use cranelift_isle::{lexer, parser};
use std::collections::HashMap;

use crate::spec::{self, SpecEnv};

pub struct Program {
    pub tyenv: TypeEnv,
    pub termenv: TermEnv,
    pub specenv: SpecEnv,
}

impl Program {
    pub fn from_files(
        paths: &Vec<std::path::PathBuf>,
        expand_internal_extractors: bool,
    ) -> Result<Self, Errors> {
        let lexer = lexer::Lexer::from_files(paths)?;
        let defs = parser::parse(lexer)?;
        let mut tyenv = sema::TypeEnv::from_ast(&defs)?;
        let termenv = sema::TermEnv::from_ast(&mut tyenv, &defs, expand_internal_extractors)?;
        let specenv = spec::SpecEnv::from_ast(&defs, &termenv, &tyenv);

        Ok(Self {
            tyenv,
            termenv,
            specenv,
        })
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

    pub fn get_rule_by_identifier(&self, id: &str) -> Option<&Rule> {
        self.termenv
            .rules
            .iter()
            .find(|r| r.identifier(&self.tyenv) == id)
    }

    pub fn get_term_by_name(&self, name: &str) -> Option<TermId> {
        let sym = Ident(name.to_string(), Pos::default());
        self.termenv.get_term_by_name(&self.tyenv, &sym)
    }

    pub fn build_trie(&self) -> Result<Vec<(TermId, RuleSet)>, Errors> {
        let (terms, errors) = trie_again::build(&self.termenv);
        if errors.is_empty() {
            Ok(terms)
        } else {
            Err(ErrorsBuilder::new()
                .errors(errors)
                .file_info_from_tyenv(&self.tyenv)
                .build())
        }
    }
}
