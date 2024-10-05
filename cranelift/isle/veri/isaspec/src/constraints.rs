//! Translation of ASLp semantics to constraints.

use core::fmt;
use std::collections::{HashMap, HashSet};
use std::vec;

use anyhow::{bail, format_err, Result};
use cranelift_isle::ast::{SpecExpr, SpecOp};
use cranelift_isle_veri_aslp::ast::{Block, Expr, Func, LExpr, Slice, Stmt};
use tracing::debug;

use crate::memory::ReadEffect;
use crate::spec::*;

#[derive(Debug, PartialEq, Eq, Hash, Clone, PartialOrd, Ord)]
pub enum Target {
    Var(String),
    Index(Box<Target>, usize),
    Field(Box<Target>, String),
}

impl fmt::Display for Target {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Var(v) => write!(f, "{v}"),
            Self::Index(a, i) => write!(f, "{a}[{i}]"),
            Self::Field(s, field) => write!(f, "{s}.{field}"),
        }
    }
}

impl TryFrom<&LExpr> for Target {
    type Error = anyhow::Error;

    fn try_from(lexpr: &LExpr) -> Result<Self> {
        match lexpr {
            LExpr::Var(v) => Ok(Target::Var(v.clone())),
            LExpr::ArrayIndex { array, index } => {
                let array = Box::new(array.as_ref().try_into()?);
                let index = index
                    .as_lit_int()
                    .ok_or(format_err!("array index must be literal integer"))?
                    .parse()?;
                Ok(Target::Index(array, index))
            }
            LExpr::Field { x, name } => {
                let x = Box::new(x.as_ref().try_into()?);
                Ok(Target::Field(x, name.clone()))
            }
        }
    }
}

impl TryFrom<&Expr> for Target {
    type Error = anyhow::Error;

    fn try_from(expr: &Expr) -> Result<Self> {
        match expr {
            Expr::Var(v) => Ok(Target::Var(v.clone())),
            Expr::ArrayIndex { array, index } => {
                let array = Box::new(array.as_ref().try_into()?);
                let index = index
                    .as_lit_int()
                    .ok_or(format_err!("array index must be literal integer"))?
                    .parse()?;
                Ok(Target::Index(array, index))
            }
            Expr::Field { x, name } => {
                let x = Box::new(x.as_ref().try_into()?);
                Ok(Target::Field(x, name.clone()))
            }
            _ => todo!("target expr: {expr:?}"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Binding {
    Uninitialized,
    Global,
    Var(String),
}

impl Binding {
    pub fn as_var(&self) -> Option<&String> {
        match self {
            Binding::Var(v) => Some(v),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Scope {
    constraints: Vec<SpecExpr>,
    vars: HashSet<String>,
    decls: HashSet<Target>,
    bindings: HashMap<Target, Binding>,
    init: HashMap<Target, String>,
    reads: HashSet<Target>,
    writes: HashSet<Target>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            constraints: Vec::new(),
            vars: HashSet::new(),
            decls: HashSet::new(),
            bindings: HashMap::new(),
            init: HashMap::new(),
            reads: HashSet::new(),
            writes: HashSet::new(),
        }
    }

    pub fn constraints(&self) -> &Vec<SpecExpr> {
        &self.constraints
    }

    fn constrain(&mut self, constraint: SpecExpr) {
        self.constraints.push(constraint);
    }

    pub fn vars(&self) -> &HashSet<String> {
        &self.vars
    }

    pub fn reads(&self) -> &HashSet<Target> {
        &self.reads
    }

    pub fn writes(&self) -> &HashSet<Target> {
        &self.writes
    }

    pub fn init(&self) -> &HashMap<Target, String> {
        &self.init
    }

    pub fn bindings(&self) -> &HashMap<Target, Binding> {
        &self.bindings
    }

    fn bind(&mut self, target: Target, b: Binding) {
        self.bindings.insert(target.clone(), b);
    }

    fn decl(&mut self, target: Target) {
        self.decls.insert(target.clone());
        self.bind(target, Binding::Uninitialized);
    }

    pub fn global(&mut self, target: Target) {
        self.decls.insert(target.clone());
        self.bind(target, Binding::Global);
    }

    fn add_var(&mut self, v: String) {
        self.vars.insert(v);
    }

    fn bind_var(&mut self, target: Target, v: String) {
        self.add_var(v.clone());
        self.bind(target, Binding::Var(v));
    }

    fn init_var(&mut self, target: Target, v: String) {
        assert!(!self.init.contains_key(&target));
        self.init.insert(target.clone(), v.clone());
        self.bind_var(target, v);
    }

    fn write(&mut self, target: Target, v: String) {
        self.writes.insert(target.clone());
        self.bind_var(target, v);
    }

    fn read(&mut self, target: &Target) -> Option<&Binding> {
        if let Some(b) = self.bindings.get(target) {
            debug!(?target, "scope read");
            self.reads.insert(target.clone());
            return Some(b);
        }
        None
    }

    fn update(&mut self, child: &Self) {
        self.constraints.extend(child.constraints.iter().cloned());
        self.vars.extend(child.vars.iter().cloned());
        for target in &child.writes {
            if !child.decls.contains(target) {
                self.bind(target.clone(), child.bindings[target].clone());
                self.writes.insert(target.clone());
            }
        }
    }
}

struct VariableAllocator {
    index: usize,
    prefix: String,
}

impl VariableAllocator {
    fn new(prefix: String) -> Self {
        Self { index: 0, prefix }
    }

    fn alloc(&mut self) -> String {
        let index = self.index;
        self.index += 1;
        format!("{}{}", self.prefix, index)
    }
}

pub struct Translator {
    stack: Vec<Scope>,
    vars: VariableAllocator,
}

impl Translator {
    pub fn new(global: Scope, prefix: String) -> Self {
        Self {
            stack: vec![global],
            vars: VariableAllocator::new(prefix),
        }
    }

    pub fn global(&self) -> &Scope {
        self.stack.first().expect("stack must be non-empty")
    }

    fn enter(&mut self) {
        debug!("enter scope");
        self.stack.push(Scope::new())
    }

    fn exit(&mut self) {
        let scope = self.pop();
        debug!(?scope, "exit scope");
        self.scope_mut().update(&scope);
    }

    fn pop(&mut self) -> Scope {
        self.stack.pop().expect("stack must be non-empty")
    }

    fn scope_mut(&mut self) -> &mut Scope {
        self.stack.last_mut().expect("stack must be non-empty")
    }

    fn constrain(&mut self, constraint: SpecExpr) {
        self.scope_mut().constrain(constraint)
    }

    fn write(&mut self, target: &Target, v: &str) {
        self.scope_mut().write(target.clone(), v.to_string());
    }

    fn read(&mut self, target: &Target) -> Result<String> {
        // Read from innermost scope.
        for scope in self.stack.iter_mut().rev() {
            match scope.read(target) {
                None => continue,
                Some(Binding::Var(v)) => return Ok(v.clone()),
                Some(Binding::Uninitialized) => bail!("uninitialized read: {target}"),
                Some(Binding::Global) => {
                    let v = self.vars.alloc();
                    scope.init_var(target.clone(), v.clone());
                    return Ok(v);
                }
            };
        }
        let scope = self.scope_mut();
        debug!(?scope, "scope");
        bail!("undefined read: {target}")
    }

    pub fn translate(&mut self, block: &Block) -> Result<()> {
        self.enter();
        self.block(block)?;
        self.exit();
        Ok(())
    }

    fn block(&mut self, block: &Block) -> Result<()> {
        for stmt in &block.stmts {
            self.stmt(stmt)?;
        }
        Ok(())
    }

    fn stmt(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::Assign { lhs, rhs } => {
                let target = lhs.try_into()?;
                let rhs = self.expr(rhs)?;
                self.assign(&target, rhs)
            }
            Stmt::ConstDecl { name, rhs, .. } => {
                let target = Target::Var(name.clone());
                self.scope_mut().decl(target.clone());
                let rhs = self.expr(rhs)?;
                self.assign(&target, rhs)
            }
            Stmt::VarDeclsNoInit { names, .. } => {
                for name in names {
                    let target = Target::Var(name.clone());
                    self.scope_mut().decl(target);
                }
                Ok(())
            }
            Stmt::Assert { cond } => {
                let constraint = self.expr(cond)?;
                self.constrain(constraint);
                Ok(())
            }
            Stmt::If {
                cond,
                then_block,
                else_block,
            } => {
                self.enter();

                // Assign the conditional to a variable.
                let cond = self.expr(cond)?;
                let c = self.bind(cond)?;
                let cond = spec_var(c);

                // Execute then. Pop off the scope.
                self.enter();
                self.block(then_block)?;
                let then_scope = self.pop();

                // Execute else. Pop off the scope.
                self.enter();
                self.block(else_block)?;
                let else_scope = self.pop();

                // Join blocks.
                self.constrain(spec_if(
                    cond.clone(),
                    spec_all(then_scope.constraints),
                    spec_all(else_scope.constraints),
                ));

                // Merge bindings. Expect both sides to have the same bindings.
                if then_scope.bindings.len() != else_scope.bindings.len() {
                    // TODO(mbm): handle distinct bindings at joins.
                    todo!("joining distinct bindings");
                }
                for (target, then_binding) in then_scope.bindings {
                    // Lookup binding on the else branch.
                    let else_binding = match else_scope.bindings.get(&target) {
                        Some(b) => b,
                        None => todo!("joining distinct bindings"),
                    };

                    // Joined "phi node" expression.
                    let then_var = then_binding
                        .as_var()
                        .ok_or(format_err!("expected variable"))?;
                    let else_var = else_binding
                        .as_var()
                        .ok_or(format_err!("expected variable"))?;
                    let phi = spec_if(
                        cond.clone(),
                        spec_var(then_var.clone()),
                        spec_var(else_var.clone()),
                    );
                    self.assign(&target, phi)?;
                }

                // Merge additional scope metadata.
                let joined = self.scope_mut();
                joined.vars.extend(then_scope.vars.iter().cloned());
                joined.vars.extend(else_scope.vars.iter().cloned());

                joined.reads.extend(then_scope.reads.iter().cloned());
                joined.reads.extend(else_scope.reads.iter().cloned());

                joined.writes.extend(then_scope.writes.iter().cloned());
                joined.writes.extend(else_scope.writes.iter().cloned());

                // Exit if scope.
                self.exit();

                Ok(())
            }
            _ => todo!("statement: {stmt:?}"),
        }
    }

    fn assign(&mut self, target: &Target, rhs: SpecExpr) -> Result<()> {
        // Bind the expression to a variable.
        let v = self.bind(rhs)?;

        // Write variable to the target.
        self.write(target, &v);

        Ok(())
    }

    // Bind expression to a variable and return it.
    fn bind(&mut self, expr: SpecExpr) -> Result<String> {
        let v = self.vars.alloc();
        self.scope_mut().add_var(v.clone());
        let lhs = spec_var(v.clone());
        self.constrain(spec_eq(lhs, expr));
        Ok(v)
    }

    fn expr(&mut self, expr: &Expr) -> Result<SpecExpr> {
        match expr {
            Expr::Apply {
                func,
                types: _,
                args,
            } => self.func(func, args),
            Expr::Var(..) | Expr::ArrayIndex { .. } | Expr::Field { .. } => {
                let target: Target = expr.try_into()?;
                Ok(spec_var(self.read(&target)?))
            }
            Expr::Slices { x, slices } => {
                let slice = expect_unary(slices)?;
                Ok(self.slice(x, slice)?)
            }
            Expr::LitBits(bits) => {
                let val = u128::from_str_radix(bits, 2)?;
                let width = bits.len();
                Ok(spec_const_bit_vector(val, width))
            }
            _ => todo!("expr: {expr:?}"),
        }
    }

    fn func(&mut self, func: &Func, args: &[Expr]) -> Result<SpecExpr> {
        match func.name.as_str() {
            "ZeroExtend" => {
                let (x, w) = expect_binary(args)?;
                let x = self.expr(x)?;
                let w = spec_const_int(expect_lit_int_as_usize(w)?.try_into()?);
                Ok(spec_binary(SpecOp::ZeroExt, w, x))
            }
            "SignExtend" => {
                let (x, w) = expect_binary(args)?;
                let x = self.expr(x)?;
                let w = spec_const_int(expect_lit_int_as_usize(w)?.try_into()?);
                Ok(spec_binary(SpecOp::SignExt, w, x))
            }
            "not_bool" => {
                let x = expect_unary(args)?;
                let x = self.expr(x)?;
                Ok(spec_unary(SpecOp::Not, x))
            }
            "append_bits" => {
                let (x, y) = expect_binary(args)?;
                let x = self.expr(x)?;
                let y = self.expr(y)?;
                Ok(spec_binary(SpecOp::Concat, x, y))
            }
            "not_bits" => {
                let x = expect_unary(args)?;
                let x = self.expr(x)?;
                Ok(spec_unary(SpecOp::BVNot, x))
            }
            "cvt_bool_bv" => {
                let b = expect_unary(args)?;
                let b = self.expr(b)?;
                Ok(spec_if(
                    b,
                    spec_const_bit_vector(1, 1),
                    spec_const_bit_vector(0, 1),
                ))
            }
            "ite" => {
                let (c, t, e) = expect_ternary(args)?;
                Ok(spec_if(self.expr(c)?, self.expr(t)?, self.expr(e)?))
            }
            "and_bool" => {
                // TODO(mbm): binary op helper
                let (lhs, rhs) = expect_binary(args)?;
                let lhs = self.expr(lhs)?;
                let rhs = self.expr(rhs)?;
                Ok(spec_binary(SpecOp::And, lhs, rhs))
            }
            "eq_bits" => {
                // TODO(mbm): binary op helper
                let (lhs, rhs) = expect_binary(args)?;
                let lhs = self.expr(lhs)?;
                let rhs = self.expr(rhs)?;
                Ok(spec_binary(SpecOp::Eq, lhs, rhs))
            }
            "ne_bits" => {
                // TODO(mbm): binary op helper
                let (lhs, rhs) = expect_binary(args)?;
                let lhs = self.expr(lhs)?;
                let rhs = self.expr(rhs)?;
                Ok(spec_unary(SpecOp::Not, spec_binary(SpecOp::Eq, lhs, rhs)))
            }
            "add_bits" => {
                // TODO(mbm): binary op helper
                // TODO(mbm): check type annotation on function matches type of operands?
                let (lhs, rhs) = expect_binary(args)?;
                let lhs = self.expr(lhs)?;
                let rhs = self.expr(rhs)?;
                Ok(spec_binary(SpecOp::BVAdd, lhs, rhs))
            }
            "sub_bits" => {
                // TODO(mbm): binary op helper
                // TODO(mbm): check type annotation on function matches type of operands?
                let (lhs, rhs) = expect_binary(args)?;
                let lhs = self.expr(lhs)?;
                let rhs = self.expr(rhs)?;
                Ok(spec_binary(SpecOp::BVSub, lhs, rhs))
            }
            "or_bits" => {
                // TODO(mbm): binary op helper
                let (lhs, rhs) = expect_binary(args)?;
                let lhs = self.expr(lhs)?;
                let rhs = self.expr(rhs)?;
                Ok(spec_binary(SpecOp::BVOr, lhs, rhs))
            }
            "and_bits" => {
                // TODO(mbm): binary op helper
                let (lhs, rhs) = expect_binary(args)?;
                let lhs = self.expr(lhs)?;
                let rhs = self.expr(rhs)?;
                Ok(spec_binary(SpecOp::BVAnd, lhs, rhs))
            }
            "eor_bits" => {
                // TODO(mbm): binary op helper
                let (lhs, rhs) = expect_binary(args)?;
                let lhs = self.expr(lhs)?;
                let rhs = self.expr(rhs)?;
                Ok(spec_binary(SpecOp::BVXor, lhs, rhs))
            }
            "mul_bits" => {
                // TODO(mbm): binary op helper
                let (lhs, rhs) = expect_binary(args)?;
                let lhs = self.expr(lhs)?;
                let rhs = self.expr(rhs)?;
                Ok(spec_binary(SpecOp::BVMul, lhs, rhs))
            }
            "sdiv_bits" => {
                // TODO(mbm): binary op helper
                let (lhs, rhs) = expect_binary(args)?;
                let lhs = self.expr(lhs)?;
                let rhs = self.expr(rhs)?;
                Ok(spec_binary(SpecOp::BVSdiv, lhs, rhs))
            }
            "lsr_bits" => {
                // TODO(mbm): binary op helper
                // TODO(mbm): ensure correct bitwidth of shift value
                let (x, s) = expect_binary(args)?;
                let x = self.expr(x)?;
                let s = self.expr(s)?;
                Ok(spec_binary(SpecOp::BVLshr, x, s))
            }
            "asr_bits" => {
                // TODO(mbm): binary op helper
                // TODO(mbm): ensure correct bitwidth of shift value
                let (x, s) = expect_binary(args)?;
                let x = self.expr(x)?;
                let s = self.expr(s)?;
                Ok(spec_binary(SpecOp::BVAshr, x, s))
            }
            "lsl_bits" => {
                // TODO(mbm): binary op helper
                // TODO(mbm): ensure correct bitwidth of shift value
                let (x, s) = expect_binary(args)?;
                let x = self.expr(x)?;
                let s = self.expr(s)?;
                Ok(spec_binary(SpecOp::BVShl, x, s))
            }
            "sle_bits" => {
                // TODO(mbm): binary op helper
                let (lhs, rhs) = expect_binary(args)?;
                let lhs = self.expr(lhs)?;
                let rhs = self.expr(rhs)?;
                Ok(spec_binary(SpecOp::BVSle, lhs, rhs))
            }
            "slt_bits" => {
                // TODO(mbm): binary op helper
                let (lhs, rhs) = expect_binary(args)?;
                let lhs = self.expr(lhs)?;
                let rhs = self.expr(rhs)?;
                Ok(spec_binary(SpecOp::BVSlt, lhs, rhs))
            }
            "Mem.read" => {
                let (addr, size, access) = expect_ternary(args)?;
                self.mem_read(addr, size, access)
            }
            unexpected => todo!("func: {unexpected}"),
        }
    }

    fn slice(&mut self, x: &Expr, slice: &Slice) -> Result<SpecExpr> {
        match slice {
            Slice::LowWidth(l, w) => {
                let l = expect_lit_int_as_usize(l)?;
                let w = expect_lit_int_as_usize(w)?;
                let h = l + w - 1;
                let x = self.expr(x)?;
                Ok(spec_ternary(
                    SpecOp::Extract,
                    spec_const_int(h.try_into()?),
                    spec_const_int(l.try_into()?),
                    x,
                ))
            }
        }
    }

    fn mem_read(&mut self, addr: &Expr, size: &Expr, access: &Expr) -> Result<SpecExpr> {
        // Map parameters to spec expressions.
        let addr = self.expr(addr)?;
        let size_bytes = expect_lit_int_as_usize(size)?;
        let size_bits = 8 * size_bytes;

        // Access flags not implemented: error on unexpected non-zero value.
        let access = expect_lit_int_as_usize(access)?;
        if access != 0 {
            bail!("non-zero memory read access flags");
        }

        // Memory read operation modifies read effect variables.
        let read_effect = ReadEffect::new();
        self.assign(&read_effect.active, spec_true())?;
        self.assign(
            &read_effect.size_bits,
            spec_const_int(size_bits.try_into()?),
        )?;
        self.assign(&read_effect.addr, addr)?;

        let value = self.read(&read_effect.value)?;
        Ok(spec_var(value))
    }
}

fn expect_unary<T>(xs: &[T]) -> Result<&T> {
    if xs.len() != 1 {
        bail!("expected unary");
    }
    Ok(&xs[0])
}

fn expect_binary<T>(xs: &[T]) -> Result<(&T, &T)> {
    if xs.len() != 2 {
        bail!("expected binary");
    }
    Ok((&xs[0], &xs[1]))
}

fn expect_ternary<T>(xs: &[T]) -> Result<(&T, &T, &T)> {
    if xs.len() != 3 {
        bail!("expected ternary");
    }
    Ok((&xs[0], &xs[1], &xs[2]))
}

fn expect_lit_int_as_usize(expr: &Expr) -> Result<usize> {
    Ok(expr
        .as_lit_int()
        .ok_or(format_err!("expected literal integer"))?
        .parse()?)
}
