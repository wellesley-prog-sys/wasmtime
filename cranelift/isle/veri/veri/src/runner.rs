use std::{cmp::max, collections::HashMap, path::PathBuf, time::Duration};

use anyhow::{bail, format_err, Result};
use cranelift_isle::{
    sema::{RuleId, TermId},
    trie_again::RuleSet,
};

use crate::{
    debug::print_expansion,
    expand::{Chaining, Expander, Expansion},
    program::Program,
    solver::{Applicability, Solver, Verification},
    type_inference::{self, type_constraint_system, Assignment, Choice},
    veri::Conditions,
};

pub enum SolverBackend {
    Z3,
    CVC5,
}

impl SolverBackend {
    fn prog(&self) -> &str {
        match self {
            SolverBackend::Z3 => "z3",
            SolverBackend::CVC5 => "cvc5",
        }
    }

    fn args(&self, timeout: Duration) -> Vec<String> {
        match self {
            SolverBackend::Z3 => vec![
                "-smt2".to_string(),
                "-in".to_string(),
                format!("-t:{}", timeout.as_millis()),
            ],
            SolverBackend::CVC5 => vec![
                "--incremental".to_string(),
                "--print-success".to_string(),
                format!("--tlimit-per={ms}", ms = timeout.as_millis()),
                "-".to_string(),
            ],
        }
    }
}

enum ExpansionPredicate {
    FirstRuleNamed,
    SkipTag(String),
    ContainsRule(RuleId),
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
enum ShouldVerify {
    Unspecified,
    Yes,
    No,
}

impl From<bool> for ShouldVerify {
    fn from(b: bool) -> Self {
        if b {
            ShouldVerify::Yes
        } else {
            ShouldVerify::No
        }
    }
}

/// Runner orchestrates execution of the verification process over a set of
/// expansions.
pub struct Runner {
    prog: Program,
    term_rule_sets: HashMap<TermId, RuleSet>,

    root_term: String,
    expansion_predicates: Vec<ExpansionPredicate>,
    solver_backend: SolverBackend,
    timeout: Duration,
    skip_solver: bool,
    debug: bool,
}

impl Runner {
    pub fn from_files(inputs: &Vec<PathBuf>) -> Result<Self> {
        let expand_internal_extractors = false;
        let prog = Program::from_files(inputs, expand_internal_extractors)?;
        let term_rule_sets: HashMap<_, _> = prog.build_trie()?.into_iter().collect();
        Ok(Self {
            prog,
            term_rule_sets,
            root_term: "lower".to_string(),
            expansion_predicates: Vec::new(),
            solver_backend: SolverBackend::CVC5,
            timeout: Duration::from_secs(5),
            skip_solver: false,
            debug: false,
        })
    }

    pub fn set_root_term(&mut self, term: &str) {
        self.root_term = term.to_string();
    }

    pub fn include_first_rule_named(&mut self) {
        self.expansion_predicates
            .push(ExpansionPredicate::FirstRuleNamed);
    }

    pub fn skip_tag(&mut self, tag: String) {
        self.expansion_predicates
            .push(ExpansionPredicate::SkipTag(tag));
    }

    pub fn target_rule(&mut self, id: &str) -> Result<()> {
        let rule = self
            .prog
            .get_rule_by_identifier(id)
            .ok_or(format_err!("unknown rule {id}"))?;
        self.expansion_predicates
            .push(ExpansionPredicate::ContainsRule(rule.id));
        Ok(())
    }

    pub fn set_solver_backend(&mut self, backend: SolverBackend) {
        self.solver_backend = backend;
    }

    pub fn set_timeout(&mut self, timeout: Duration) {
        self.timeout = timeout;
    }

    pub fn skip_solver(&mut self, skip: bool) {
        self.skip_solver = skip;
    }

    pub fn debug(&mut self, debug: bool) {
        self.debug = debug;
    }

    pub fn run(&self) -> Result<()> {
        // Generate expansions.
        // TODO(mbm): don't hardcode the expansion configuration
        let chaining = Chaining::new(&self.prog, &self.term_rule_sets)?;
        let mut expander = Expander::new(&self.prog, &self.term_rule_sets, chaining);
        expander.add_root_term_name(&self.root_term)?;
        expander.set_prune_infeasible(true);
        expander.expand();

        // Process expansions.
        let expansions = expander.expansions();
        log::info!("expansions: {n}", n = expansions.len());
        for (i, expansion) in expansions.iter().enumerate() {
            if self.should_verify(expansion)? != ShouldVerify::Yes {
                continue;
            }

            // Report.
            println!("#{i}\t{}", self.expansion_description(expansion)?);
            if self.debug {
                print_expansion(&self.prog, expansion);
            }

            self.verify_expansion(expansion)?;
        }

        Ok(())
    }

    fn should_verify(&self, expansion: &Expansion) -> Result<ShouldVerify> {
        self.expansion_predicates
            .iter()
            .try_fold(ShouldVerify::Unspecified, |acc, p| {
                Ok(max(acc, self.predicate(p, expansion)?))
            })
    }

    fn predicate(
        &self,
        predicate: &ExpansionPredicate,
        expansion: &Expansion,
    ) -> Result<ShouldVerify> {
        match predicate {
            ExpansionPredicate::FirstRuleNamed => {
                let rule_id = expansion
                    .rules
                    .first()
                    .ok_or(format_err!("expansion should have at least one rule"))?;
                let rule = self.prog.rule(*rule_id);
                Ok(rule.name.is_some().into())
            }
            ExpansionPredicate::SkipTag(tag) => {
                let tags = expansion.term_tags(&self.prog);
                Ok(if tags.contains(tag) {
                    log::debug!("skip expansion with tag: {}", tag);
                    ShouldVerify::No
                } else {
                    ShouldVerify::Unspecified
                })
            }
            ExpansionPredicate::ContainsRule(rule_id) => {
                Ok(expansion.rules.contains(rule_id).into())
            }
        }
    }

    fn verify_expansion(&self, expansion: &Expansion) -> Result<()> {
        // Verification conditions.
        let conditions = Conditions::from_expansion(expansion, &self.prog)?;
        if self.debug {
            conditions.pretty_print(&self.prog);
        }

        // Type constraints.
        let system = type_constraint_system(&conditions);
        if self.debug {
            system.pretty_print();
        }

        // Infer types.
        let type_solver = type_inference::Solver::new();
        let solutions = type_solver.solve(&system);

        for solution in &solutions {
            // Show type assignment.
            for choice in &solution.choices {
                match choice {
                    Choice::TermInstantiation(term_id, sig) => {
                        println!("\t{term}{sig}", term = self.prog.term_name(*term_id));
                    }
                }
            }
            println!("\t\ttype solution status = {}", solution.status);
            if self.debug {
                println!("type assignment:");
                solution.assignment.pretty_print(&conditions);
            }

            match solution.status {
                type_inference::Status::Solved => (),
                type_inference::Status::Inapplicable => continue,
                type_inference::Status::Underconstrained => {
                    bail!("underconstrained type inference")
                }
            }

            // Verify.
            if self.skip_solver {
                println!("skip solver");
                continue;
            }

            self.verify_expansion_type_instantiation(&conditions, &solution.assignment)?;
        }

        Ok(())
    }

    fn verify_expansion_type_instantiation(
        &self,
        conditions: &Conditions,
        assignment: &Assignment,
    ) -> Result<()> {
        // Solve.
        let binary = self.solver_backend.prog();
        let args = self.solver_backend.args(self.timeout);
        let smt = easy_smt::ContextBuilder::new()
            .solver(binary, &args)
            .build()?;

        let mut solver = Solver::new(smt, conditions, assignment)?;
        solver.encode()?;

        let applicability = solver.check_assumptions_feasibility()?;
        println!("\t\tapplicability = {applicability}");
        match applicability {
            Applicability::Applicable => (),
            Applicability::Inapplicable => return Ok(()),
            Applicability::Unknown => bail!("could not prove applicability"),
        };

        let verification = solver.check_verification_condition()?;
        println!("\t\tverification = {verification}");
        match verification {
            Verification::Failure(model) => {
                println!("model:");
                conditions.print_model(&model, &self.prog)?;
                bail!("verification failed");
            }
            Verification::Success | Verification::Unknown => (),
        };

        Ok(())
    }

    /// Human-readable description of an expansion.
    fn expansion_description(&self, expansion: &Expansion) -> Result<String> {
        let rule_id = expansion
            .rules
            .first()
            .ok_or(format_err!("expansion should have at least one rule"))?;
        let rule = self.prog.rule(*rule_id);
        Ok(rule.identifier(&self.prog.tyenv, &self.prog.files))
    }
}
