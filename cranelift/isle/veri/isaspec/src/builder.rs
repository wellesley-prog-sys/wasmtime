//! Construction of VeriISLE specifications from ASLp semantics.

use std::collections::HashMap;

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

pub enum Action {
    Read,
    Write,
}

pub struct Mapping {
    pub expected_action: Action,
    pub variable: String,
}

impl Mapping {
    pub fn new(expected_action: Action, variable: String) -> Self {
        Self {
            expected_action,
            variable,
        }
    }

    pub fn read(variable: String) -> Self {
        Self::new(Action::Read, variable)
    }

    pub fn write(variable: String) -> Self {
        Self::new(Action::Write, variable)
    }
}

pub struct InstConfig {
    pub inst: Inst,
    pub require: Vec<SpecExpr>,
    pub mappings: HashMap<Target, Mapping>,
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

        // Binding mapping.
        let bindings = global.bindings();
        let reads = global.reads();
        let writes = global.writes();
        for (target, mapping) in &case.mappings {
            // Confirm expected action.
            match mapping.expected_action {
                Action::Read => assert!(reads.contains(target)),
                Action::Write => assert!(writes.contains(target)),
            }

            // Confirm target is bound to a variable in the constraints.
            let v = match bindings.get(target) {
                Some(Binding::Var(v)) => v,
                _ => anyhow::bail!("{target} not bound to variable"),
            };

            // Bind to mapped variable.
            conds.provides.push(spec_eq(
                spec_var(mapping.variable.clone()),
                spec_var(v.clone()),
            ));
        }

        // Confirm all actions are mapped.
        if reads.len() + writes.len() != case.mappings.len() {
            anyhow::bail!("unmapped bindings");
        }

        // Conditions.
        conds.provides.extend(global.constraints().iter().cloned());

        Ok(conds)
    }
}
