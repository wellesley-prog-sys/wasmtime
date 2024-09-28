//! Construction of VeriISLE specifications from ASLp semantics.

use std::collections::{HashMap, HashSet};

use anyhow::{bail, Result};
use cranelift_codegen::isa::aarch64::inst::Inst;
use cranelift_isle::ast::{Def, Spec, SpecExpr};
use cranelift_isle::lexer::Pos;
use cranelift_isle_veri_aslp::client::Client;
use itertools::Itertools;

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

pub struct Builder<'a> {
    cfg: SpecConfig,
    client: &'a Client<'a>,
}

impl<'a> Builder<'a> {
    pub fn new(cfg: SpecConfig, client: &'a Client<'a>) -> Self {
        Self { cfg, client }
    }

    pub fn build(&self) -> Result<Def> {
        let spec = self.spec(&self.cfg)?;
        let def = Def::Spec(spec);
        Ok(def)
    }

    fn spec(&self, cfg: &SpecConfig) -> Result<Spec> {
        // Derive conditions for each case.
        let conds: Vec<Conditions> = cfg
            .cases
            .iter()
            .enumerate()
            .map(|(i, c)| self.case(i, c))
            .collect::<Result<_, _>>()?;
        let cond = Conditions::merge(conds);
        let spec = Spec {
            term: spec_ident(cfg.term.clone()),
            args: spec_idents(&cfg.args),
            requires: cond.requires,
            provides: cond.provides,
            pos: Pos::default(),
        };

        Ok(spec)
    }

    fn case(&self, i: usize, case: &InstConfig) -> Result<Conditions> {
        // Semantics.
        let block = inst_semantics(&case.inst, self.client)?;

        // Translation.
        let prefix = format!("v{i}_");
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
            requires: case.require.clone(),
            provides,
        })
    }
}

fn substitute(expr: SpecExpr, substitutions: &HashMap<String, SpecExpr>) -> Result<SpecExpr> {
    Ok(match expr {
        // Variable
        SpecExpr::Var { ref var, pos: _ } => {
            if let Some(substitution) = substitutions.get(&var.0) {
                substitution.clone()
            } else {
                expr
            }
        }

        // Constants are unchanged.
        SpecExpr::ConstInt { .. } | SpecExpr::ConstBitVec { .. } | SpecExpr::ConstBool { .. } => {
            expr
        }

        // Scopes require care to ensure we are not replacing introduced variables.
        SpecExpr::Let { defs, body, pos } => SpecExpr::Let {
            defs: defs
                .into_iter()
                .map(|(var, expr)| {
                    if substitutions.contains_key(&var.0) {
                        bail!("substituted variable collides with let binding");
                    }
                    Ok((var, substitute(expr, substitutions)?))
                })
                .collect::<Result<_>>()?,
            body: Box::new(substitute(*body, substitutions)?),
            pos,
        },
        SpecExpr::With { decls, body, pos } => {
            for decl in &decls {
                if substitutions.contains_key(&decl.0) {
                    bail!("substituted variable collides with with scope");
                }
            }
            SpecExpr::With {
                decls,
                body: Box::new(substitute(*body, substitutions)?),
                pos,
            }
        }

        // Recurse into child expressions.
        SpecExpr::Field { field, x, pos } => SpecExpr::Field {
            field,
            x: Box::new(substitute(*x, substitutions)?),
            pos,
        },
        SpecExpr::Discriminator { variant, x, pos } => SpecExpr::Discriminator {
            variant,
            x: Box::new(substitute(*x, substitutions)?),
            pos,
        },
        SpecExpr::Op { op, args, pos } => SpecExpr::Op {
            op,
            args: args
                .into_iter()
                .map(|arg| substitute(arg, substitutions))
                .collect::<Result<_>>()?,
            pos,
        },
        SpecExpr::Pair { l, r, pos } => SpecExpr::Pair {
            l: Box::new(substitute(*l, substitutions)?),
            r: Box::new(substitute(*r, substitutions)?),
            pos,
        },
        SpecExpr::Enum {
            name,
            variant,
            args,
            pos,
        } => SpecExpr::Enum {
            name,
            variant,
            args: args
                .into_iter()
                .map(|arg| substitute(arg, substitutions))
                .collect::<Result<_>>()?,
            pos,
        },
    })
}
