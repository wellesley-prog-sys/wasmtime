use crate::spec::{self, SpecEnv};
use anyhow::{bail, Result};
use cranelift_isle::ast::Ident;
use cranelift_isle::error::{Errors, ErrorsBuilder};
use cranelift_isle::files::Files;
use cranelift_isle::lexer::Pos;
use cranelift_isle::sema::{
    self, Rule, RuleId, Term, TermEnv, TermId, Type, TypeEnv, TypeId, VariantId,
};
use cranelift_isle::trie_again::{self, RuleSet};
use cranelift_isle::{lexer, parser};
use std::collections::HashMap;
use std::sync::Arc;

pub struct Program {
    pub files: Arc<Files>,
    pub tyenv: TypeEnv,
    pub termenv: TermEnv,
    pub specenv: SpecEnv,
}

impl Program {
    pub fn from_files(
        paths: &Vec<std::path::PathBuf>,
        expand_internal_extractors: bool,
    ) -> Result<Self> {
        let files = match Files::from_paths(paths) {
            Ok(files) => files,
            Err((path, err)) => {
                bail!(Errors::from_io(
                    err,
                    format!("cannot read file {}", path.display()),
                ))
            }
        };

        let files = Arc::new(files);

        let mut defs = Vec::new();
        for (file, src) in files.file_texts.iter().enumerate() {
            let lexer = match lexer::Lexer::new(file, src) {
                Ok(lexer) => lexer,
                Err(err) => bail!(Errors::new(vec![err], files)),
            };

            match parser::parse(lexer) {
                Ok(mut ds) => defs.append(&mut ds),
                Err(err) => bail!(Errors::new(vec![err], files)),
            }
        }

        let mut tyenv = match sema::TypeEnv::from_ast(&defs) {
            Ok(type_env) => type_env,
            Err(errs) => bail!(Errors::new(errs, files)),
        };

        let termenv = match sema::TermEnv::from_ast(&mut tyenv, &defs, expand_internal_extractors) {
            Ok(term_env) => term_env,
            Err(errs) => bail!(Errors::new(errs, files)),
        };

        let specenv = spec::SpecEnv::from_ast(&defs, &termenv, &tyenv)?;

        Ok(Self {
            files,
            tyenv,
            termenv,
            specenv,
        })
    }

    pub fn ty(&self, type_id: TypeId) -> &Type {
        self.tyenv
            .types
            .get(type_id.index())
            .expect("invalid type id")
    }

    pub fn type_name(&self, type_id: TypeId) -> &str {
        self.ty(type_id).name(&self.tyenv)
    }

    pub fn term(&self, term_id: TermId) -> &Term {
        self.termenv
            .terms
            .get(term_id.index())
            .expect("invalid term id")
    }

    pub fn term_name(&self, term_id: TermId) -> &str {
        let term = self.term(term_id);
        &self.tyenv.syms[term.name.index()]
    }

    pub fn get_variant_term(&self, ty: TypeId, variant: VariantId) -> TermId {
        self.termenv.get_variant_term(&self.tyenv, ty, variant)
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
            .find(|r| r.identifier(&self.tyenv, &self.files) == id)
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
                .files(self.files.clone())
                .build())
        }
    }
}
