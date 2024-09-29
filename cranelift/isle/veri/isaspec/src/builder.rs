//! Construction of VeriISLE specifications from ASLp semantics.

use std::collections::{HashMap, HashSet};

use anyhow::{bail, Result};
use itertools::Itertools;

use cranelift_codegen::isa::aarch64::inst::Inst;
use cranelift_isle::ast::{Def, Spec, SpecExpr};
use cranelift_isle::lexer::Pos;
use cranelift_isle_veri_aslp::client::Client;

use crate::constraints::{Binding, Target, Translator};
use crate::semantics::inst_semantics;
use crate::{
    aarch64,
    spec::{spec_all, spec_ident, spec_idents, spec_with, substitute, Conditions},
};

pub struct SpecConfig {
    pub term: String,
    pub args: Vec<String>,
    pub cases: Cases,
}

pub enum Cases {
    Instruction(InstConfig),
    Cases(Vec<Case>),
}

pub struct Case {
    pub conds: Vec<SpecExpr>,
    pub cases: Cases,
}

#[derive(Clone)]
pub enum Expectation {
    Require,
    Allow,
}

#[derive(Clone)]
pub struct Mapping {
    expr: SpecExpr,
    expect: Expectation,
}

impl Mapping {
    pub fn new(expr: SpecExpr, expect: Expectation) -> Self {
        Self { expr, expect }
    }

    pub fn require(expr: SpecExpr) -> Self {
        Self::new(expr, Expectation::Require)
    }

    pub fn allow(expr: SpecExpr) -> Self {
        Self::new(expr, Expectation::Allow)
    }
}

#[derive(Clone, Default)]
pub struct Mappings {
    pub reads: HashMap<Target, Mapping>,
    pub writes: HashMap<Target, Mapping>,
}

impl Mappings {
    fn required_reads(&self) -> HashSet<Target> {
        Self::required_targets(&self.reads)
    }

    fn required_writes(&self) -> HashSet<Target> {
        Self::required_targets(&self.writes)
    }

    fn required_targets(target_mapping: &HashMap<Target, Mapping>) -> HashSet<Target> {
        target_mapping
            .iter()
            .filter_map(|(target, mapping)| match mapping.expect {
                Expectation::Require => Some(target.clone()),
                Expectation::Allow => None,
            })
            .collect()
    }
}

pub struct InstConfig {
    pub inst: Inst,
    pub mappings: Mappings,
    pub index: usize,
}

pub struct Builder<'a> {
    cfg: SpecConfig,
    client: &'a Client<'a>,
}

impl<'a> Builder<'a> {
    pub fn new(cfg: SpecConfig, client: &'a Client<'a>) -> Self {
        Self { cfg, client }
    }

    pub fn build(&self) -> Result<Def> {
        let spec = self.spec()?;
        let def = Def::Spec(spec);
        Ok(def)
    }

    fn spec(&self) -> Result<Spec> {
        let cond = self.cases(&self.cfg.cases)?;
        let spec = Spec {
            term: spec_ident(self.cfg.term.clone()),
            args: spec_idents(&self.cfg.args),
            requires: cond.requires,
            provides: cond.provides,
            pos: Pos::default(),
        };
        Ok(spec)
    }

    fn cases(&self, cases: &Cases) -> Result<Conditions> {
        match cases {
            Cases::Instruction(case) => self.case(case),
            Cases::Cases(cases) => {
                let conds = cases
                    .iter()
                    .map(|case| {
                        let mut cond = self.cases(&case.cases)?;
                        cond.requires.extend(case.conds.clone());
                        Ok(cond)
                    })
                    .collect::<Result<Vec<_>>>()?;
                Ok(Conditions::merge(conds))
            }
        }
    }

    fn case(&self, case: &InstConfig) -> Result<Conditions> {
        // Semantics.
        let block = inst_semantics(&case.inst, self.client)?;

        // Translation.
        let prefix = format!("v{}_", case.index);
        let mut translator = Translator::new(aarch64::state(), prefix);
        translator.translate(&block)?;

        let global = translator.global();

        // Reads mapping.
        let mut substitutions = HashMap::new();
        let reads = global.reads();
        let init = global.init();
        for target in reads.iter().sorted() {
            // Expect mapping for the read.
            let Some(mapping) = case.mappings.reads.get(target) else {
                bail!("read of {target} is unmapped");
            };

            // Lookup variable holding the initial read value.
            let v = &init[target];

            // Substitute variable for mapped expression.
            substitutions.insert(v.clone(), mapping.expr.clone());
        }

        if let Some(target) = case.mappings.required_reads().difference(reads).next() {
            bail!("{target} should have been read");
        }

        // Writes mapping.
        let writes = global.writes();
        let bindings = global.bindings();
        for target in writes.iter().sorted() {
            // Expect mapping for the write.
            let Some(mapping) = case.mappings.writes.get(target) else {
                bail!("write to {target} is unmapped");
            };

            // Lookup bound variable.
            let Some(Binding::Var(v)) = bindings.get(target) else {
                bail!("{target} not bound to variable");
            };

            // Substitute variable for mapped expression.
            substitutions.insert(v.clone(), mapping.expr.clone());
        }

        if let Some(target) = case.mappings.required_writes().difference(writes).next() {
            bail!("{target} should have been written");
        }

        // Finalize provided constraints.
        let mut provides = Vec::new();
        for constraint in global.constraints() {
            provides.push(substitute(constraint.clone(), &substitutions)?);
        }

        // Determine remaining temporaries and encapsulate in a scope.
        let temporaries: Vec<_> = global
            .vars()
            .iter()
            .filter(|v| !substitutions.contains_key(*v))
            .sorted()
            .cloned()
            .collect();
        if !temporaries.is_empty() {
            let with_scope = spec_with(spec_idents(&temporaries), spec_all(provides));
            provides = vec![with_scope];
        }

        // Conditions.
        Ok(Conditions {
            requires: Vec::new(),
            provides,
        })
    }
}
