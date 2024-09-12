//! Construction of VeriISLE specifications from ASLp semantics.

use std::collections::{HashMap, HashSet};

use cranelift_codegen::isa::aarch64::inst::Inst;
use cranelift_isle::ast::{Def, Spec, SpecExpr};
use cranelift_isle::lexer::Pos;
use cranelift_isle_veri_aslp::client::Client;

use crate::constraints::{Binding, Target, Translator};
use crate::semantics::inst_semantics;
use crate::{aarch64, spec::*};

pub struct SpecConfig {
    pub term: String,
    pub args: Vec<String>,
    pub cases: Vec<InstConfig>,
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
    pub require: Vec<SpecExpr>,
    pub mappings: Mappings,
}

pub struct Builder {
    cfg: SpecConfig,
    client: Client,
}

impl Builder {
    pub fn new(cfg: SpecConfig, client: Client) -> Self {
        Self { cfg, client }
    }

    pub fn build(&self) -> anyhow::Result<Vec<Def>> {
        let spec = self.spec(&self.cfg)?;
        let def = Def::Spec(spec);
        let defs = vec![def];
        Ok(defs)
    }

    fn spec(&self, cfg: &SpecConfig) -> anyhow::Result<Spec> {
        // Derive conditions for each case.
        let conds: Vec<Conditions> = cfg
            .cases
            .iter()
            .enumerate()
            .map(|(i, c)| self.case(i, c))
            .collect::<Result<_, _>>()?;
        let mut cond = Conditions::merge(conds);

        // Assert the result is fixed 1-bit vector.
        // TODO(mbm): decide on verification model for MInst, or explicitly model as void or Unit
        cond.provides.insert(
            0,
            spec_eq(spec_var("result".to_string()), spec_const_bit_vector(1, 1)),
        );

        let spec = Spec {
            term: spec_ident(cfg.term.clone()),
            args: cfg.args.iter().cloned().map(spec_ident).collect(),
            requires: cond.requires,
            provides: cond.provides,
            pos: Pos::default(),
        };

        Ok(spec)
    }

    fn case(&self, i: usize, case: &InstConfig) -> anyhow::Result<Conditions> {
        let mut conds = Conditions {
            requires: case.require.clone(),
            provides: Vec::new(),
        };

        // Semantics.
        let block = inst_semantics(&case.inst, &self.client)?;

        // Translation.
        let prefix = format!("v{i}_");
        let mut translator = Translator::new(aarch64::state(), prefix);
        translator.translate(&block)?;

        let global = translator.global();

        // Reads mapping.
        let reads = global.reads();
        let init = global.init();
        for target in reads {
            // Expect mapping for the read.
            let Some(mapping) = case.mappings.reads.get(target) else {
                anyhow::bail!("read of {target} is unmapped");
            };

            // Lookup variable holding the initial read value.
            let v = &init[target];

            // Bind to mapped expression.
            conds
                .provides
                .push(spec_eq(mapping.expr.clone(), spec_var(v.clone())));
        }

        if let Some(target) = case.mappings.required_reads().difference(reads).next() {
            anyhow::bail!("{target} should have been read");
        }

        // Writes mapping.
        let writes = global.writes();
        let bindings = global.bindings();
        for target in writes {
            // Expect mapping for the write.
            let Some(mapping) = case.mappings.writes.get(target) else {
                anyhow::bail!("write to {target} is unmapped");
            };

            // Lookup bound variable.
            let Some(Binding::Var(v)) = bindings.get(target) else {
                anyhow::bail!("{target} not bound to variable");
            };

            // Bind to mapped expression.
            conds
                .provides
                .push(spec_eq(mapping.expr.clone(), spec_var(v.clone())));
        }

        if let Some(target) = case.mappings.required_writes().difference(writes).next() {
            anyhow::bail!("{target} should have been written");
        }

        // Conditions.
        conds.provides.extend(global.constraints().iter().cloned());

        Ok(conds)
    }
}
