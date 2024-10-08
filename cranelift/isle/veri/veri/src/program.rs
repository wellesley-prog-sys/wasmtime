use crate::expand::Expansion;
use crate::spec::{self, SpecEnv};
use crate::veri::{Call, UnsatResult};
use crate::veri::{Model, Value};
use cranelift_isle::ast::Ident;
use cranelift_isle::error::{Errors, ErrorsBuilder};
use cranelift_isle::files::Files;
use cranelift_isle::lexer::Pos;
use cranelift_isle::sema::{
    self, Pattern, Rule, RuleId, Term, TermEnv, TermId, Type, TypeEnv, TypeId, VariantId,
};
use cranelift_isle::trie_again::{self, RuleSet};
use cranelift_isle::{lexer, parser};
use easy_smt::{Context, SExpr};
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
    ) -> Result<Self, Errors> {
        let files = match Files::from_paths(paths) {
            Ok(files) => files,
            Err((path, err)) => {
                return Err(Errors::from_io(
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
                Err(err) => return Err(Errors::new(vec![err], files)),
            };

            match parser::parse(lexer) {
                Ok(mut ds) => defs.append(&mut ds),
                Err(err) => return Err(Errors::new(vec![err], files)),
            }
        }

        let mut tyenv = match sema::TypeEnv::from_ast(&defs) {
            Ok(type_env) => type_env,
            Err(errs) => return Err(Errors::new(errs, files)),
        };

        let termenv = match sema::TermEnv::from_ast(&mut tyenv, &defs, expand_internal_extractors) {
            Ok(term_env) => term_env,
            Err(errs) => return Err(Errors::new(errs, files)),
        };

        let specenv = spec::SpecEnv::from_ast(&defs, &termenv, &tyenv);

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

    pub fn display_rule(
        &self,
        expansion: &Expansion,
        model: &Model,
        calls: &Vec<UnsatResult>,
        smt: &Context,
    ) -> Vec<SExpr> {
        // SExpr
        let mut rules_sexpr: Vec<SExpr> = Vec::new();

        // // All TermIds from Calls./script/veri.sh > output/veri-bad.out
        // let term_ids: Vec<TermId> = calls.iter().map(|call| call.term.clone()).collect();

        // // Keep only Rules that involve Calls
        // let term_to_rules: HashMap<TermId, Vec<RuleId>> = self
        //     .rules_by_term()
        //     .iter()
        //     .filter(|(term_id, rule_ids)| term_ids.contains(*term_id) && !rule_ids.is_empty())
        //     .map(|(term_id, rule_ids)| (term_id.clone(), rule_ids.clone()))
        //     .collect();

        for rule_id in expansion.rules.clone() {
            let rule = self.rule(rule_id);
            let sexpr = self.to_sexpr(
                calls,
                model,
                rule,
                smt,
                &Pattern::Term(
                    cranelift_isle::sema::TypeId(0),
                    rule.root_term,
                    rule.args.clone(),
                ),
            );
            rules_sexpr.push(sexpr);
        }

        return rules_sexpr;
    }

    pub fn get_result_from_term_id(&self, calls: &Vec<UnsatResult>, term_id: &TermId) -> String {
        for call in calls {
            if &call.term == term_id {
                match &call.ret {
                    Some(value) => return value.to_string(),
                    None => return "None".to_string(),
                };
            }
        }
        return "None".to_string();
    }

    pub fn to_sexpr(
        &self,
        calls: &Vec<UnsatResult>,
        model: &Model,
        rule: &Rule,
        smt: &Context,
        pat: &Pattern,
    ) -> SExpr {
        match pat {
            sema::Pattern::Term(_, term_id, args) => {
                let sym = self.termenv.terms[term_id.index()].name;
                let name = self.tyenv.syms[sym.index()].clone();

                let mut var = format!(
                    "[{}|{}]",
                    name,
                    self.get_result_from_term_id(calls, term_id)
                );

                let mut sexprs: Vec<SExpr> = args
                    .iter()
                    .map(|a| self.to_sexpr(calls, model, rule, smt, a))
                    .collect::<Vec<SExpr>>();

                sexprs.insert(0, smt.atom(var));
                smt.list(sexprs)
            }

            sema::Pattern::Var(_, var_id) => {
                let sym = rule.vars[var_id.index()].name;
                let ident = self.tyenv.syms[sym.index()].clone();

                let mut var = " ".to_string();

                smt.atom(var)
            }
            sema::Pattern::BindPattern(_, var_id, subpat) => {
                let sym = rule.vars[var_id.index()].name;
                let ident = &self.tyenv.syms[sym.index()];
                let subpat_node = self.to_sexpr(calls, model, rule, smt, subpat);

                let mut var = " ".to_string();

                // Special case: elide bind patterns to wildcars
                if matches!(**subpat, sema::Pattern::Wildcard(_)) {
                    smt.atom(&var)
                } else {
                    smt.list(vec![smt.atom(&var), smt.atom("@"), subpat_node])
                }
            }
            sema::Pattern::Wildcard(_) => smt.list(vec![smt.atom("_")]),
            sema::Pattern::ConstPrim(_, sym) => {
                let name = self.tyenv.syms[sym.index()].clone();
                smt.list(vec![smt.atom(name)])
            }
            sema::Pattern::ConstInt(_, num) => {
                let _smt_name_prefix = format!("{}__", num);
                // TODO: look up BV vs int
                smt.list(vec![smt.atom(num.to_string())])
            }
            sema::Pattern::And(_, subpats) => {
                let mut sexprs = subpats
                    .iter()
                    .map(|a| self.to_sexpr(calls, model, rule, smt, a))
                    .collect::<Vec<SExpr>>();

                sexprs.insert(0, smt.atom("and"));
                smt.list(sexprs)
            }
        }
    }

    pub fn get_rule_patterns(&self, rule_id: RuleId) -> &Vec<sema::Pattern> {
        return &self.rule(rule_id).args;
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
