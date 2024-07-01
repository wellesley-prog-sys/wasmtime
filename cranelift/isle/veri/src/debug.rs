use crate::{
    expand::Expansion,
    program::Program,
    trie_again::{binding_type, BindingType},
};
use cranelift_isle::{
    sema::{TermId, Type, TypeEnv},
    trie_again::{Binding, BindingId, Constraint, RuleSet},
};

pub fn print_expansion(prog: &Program, expansion: &Expansion) {
    println!("expansion {{");

    // Term.
    println!("\tterm = {}", prog.term_name(expansion.term));

    // Rules.
    println!("\trules = [");
    for rule_id in &expansion.rules {
        let rule = &prog.termenv.rules[rule_id.index()];
        println!("\t\t{}", rule.identifier(&prog.tyenv));
    }
    println!("\t]");

    // Bindings.
    let lookup_binding =
        |binding_id: BindingId| expansion.bindings[binding_id.index()].clone().unwrap();
    println!("\tbindings = [");
    for (i, binding) in expansion.bindings.iter().enumerate() {
        if let Some(binding) = binding {
            let ty = binding_type(binding, expansion.term, &prog, lookup_binding);
            println!(
                "\t\t{i}: {}\t{}",
                ty.display(&prog.tyenv),
                binding_string(binding, expansion.term, &prog, lookup_binding),
            );
        }
    }
    println!("\t]");

    // Constraints.
    println!("\tconstraints = [");
    let mut constrained_binding_ids: Vec<_> = expansion.constraints.keys().collect();
    constrained_binding_ids.sort();
    for binding_id in &constrained_binding_ids {
        for constraint in &expansion.constraints[binding_id] {
            println!(
                "\t\t{}:\t{}",
                binding_id.index(),
                constraint_string(&constraint, &prog.tyenv)
            );
        }
    }
    println!("\t]");

    // Equals.
    if !expansion.equals.is_empty() {
        println!("\tequals = [");
        for (left, right) in expansion.equalities() {
            println!("\t\t{} == {}", left.index(), right.index());
        }
        println!("\t]");
    }

    // Parameters.
    println!("\tparameters = [");
    for binding_id in &expansion.parameters {
        println!("\t\t{}", binding_id.index());
    }
    println!("\t]");

    // Result.
    println!("\tresult = {}", expansion.result.index());

    // Feasibility.
    println!("\tfeasible = {}", expansion.is_feasible());

    println!("}}");
}

pub fn print_rule_set(prog: &Program, term_id: &TermId, rule_set: &RuleSet) {
    println!("term {{");
    println!("\tname = {}", prog.term_name(*term_id));

    // Bindings.
    let lookup_binding = |binding_id: BindingId| rule_set.bindings[binding_id.index()].clone();
    println!("\tbindings = [");
    for (i, binding) in rule_set.bindings.iter().enumerate() {
        let ty = binding_type(binding, *term_id, &prog, lookup_binding);
        println!(
            "\t\t{i}: {}\t{}",
            ty.display(&prog.tyenv),
            binding_string(binding, *term_id, &prog, lookup_binding),
        );
    }
    println!("\t]");

    // Rules.
    println!("\trules = [");
    for rule in &rule_set.rules {
        assert_eq!(rule.iterators.len(), 0);
        println!("\t\t{{");
        println!(
            "\t\t\tpos = {}",
            rule.pos.pretty_print_line(&prog.tyenv.filenames[..])
        );
        println!("\t\t\tconstraints = [");
        for i in 0..rule_set.bindings.len() {
            if let Some(constraint) = rule.get_constraint(i.try_into().unwrap()) {
                println!(
                    "\t\t\t\t{}:\t{}",
                    i,
                    constraint_string(&constraint, &prog.tyenv)
                );
            }
        }
        println!("\t\t\t]");
        if !rule.equals.is_empty() {
            println!("\t\t\tequals = [");
            for i in 0..rule_set.bindings.len() {
                let binding_id = i.try_into().unwrap();
                if let Some(eq) = rule.equals.find(binding_id) {
                    if eq != binding_id {
                        println!("\t\t\t\t{} == {}", binding_id.index(), eq.index());
                    }
                }
            }
            println!("\t\t\t]");
        }
        println!("\t\t\tprio = {}", rule.prio);
        println!("\t\t\tresult = {}", rule.result.index());
        if !rule.impure.is_empty() {
            println!(
                "\t\t\timpure = {impure:?}",
                impure = rule
                    .impure
                    .iter()
                    .copied()
                    .map(BindingId::index)
                    .collect::<Vec<_>>()
            );
        }
        println!("\t\t}}");
    }
    println!("\t]");

    println!("}}");
}

pub fn binding_string(
    binding: &Binding,
    term_id: TermId,
    prog: &Program,
    lookup_binding: impl Fn(BindingId) -> Binding,
) -> String {
    match binding {
        Binding::Argument { index } => format!("argument({})", index.index()),
        Binding::ConstInt { val, ty } => {
            let ty = &prog.tyenv.types[ty.index()];
            format!("const_int({val}, {name})", name = ty.name(&prog.tyenv))
        }
        Binding::ConstPrim { val } => format!("const_prim({})", prog.tyenv.syms[val.index()]),
        Binding::Constructor {
            term,
            parameters,
            instance,
        } => {
            let name = prog.term_name(*term);
            format!(
                "constructor({name}, {parameters:?}, {instance})",
                parameters = parameters
                    .iter()
                    .copied()
                    .map(BindingId::index)
                    .collect::<Vec<_>>()
            )
        }
        Binding::Extractor { term, parameter } => {
            let name = prog.term_name(*term);
            format!(
                "extractor({name}, {parameter})",
                parameter = parameter.index()
            )
        }
        Binding::MatchVariant {
            source,
            variant,
            field,
        } => {
            let source_binding = lookup_binding(*source);
            let source_type = binding_type(&source_binding, term_id, prog, lookup_binding);
            let source_type_id = match source_type {
                BindingType::Base(type_id) => type_id,
                _ => unreachable!("source of match variant should be a base type"),
            };

            // Lookup variant.
            let enum_ty = &prog.tyenv.types[source_type_id.index()];
            let enum_name = enum_ty.name(&prog.tyenv);
            let variant = match enum_ty {
                Type::Enum { variants, .. } => &variants[variant.index()],
                _ => unreachable!("source match variant should be an enum"),
            };
            let variant_name = &prog.tyenv.syms[variant.name.index()];

            // Field.
            let field = &variant.fields[field.index()];
            let field_name = &prog.tyenv.syms[field.name.index()];

            format!(
                "match_variant({source}, {enum_name}::{variant_name}, {field_name})",
                source = source.index(),
            )
        }
        Binding::MakeVariant {
            ty,
            variant,
            fields,
        } => {
            let ty = &prog.tyenv.types[ty.index()];
            let variant = match ty {
                Type::Enum { variants, .. } => &variants[variant.index()],
                _ => unreachable!("source match variant should be an enum"),
            };
            let variant_name = &prog.tyenv.syms[variant.name.index()];
            format!(
                "make_variant({ty}::{variant_name}, {fields:?})",
                ty = ty.name(&prog.tyenv),
                fields = fields
                    .iter()
                    .copied()
                    .map(BindingId::index)
                    .collect::<Vec<_>>()
            )
        }
        Binding::MakeSome { inner } => format!("some({inner})", inner = inner.index()),
        Binding::MatchSome { source } => format!("match_some({source})", source = source.index()),
        Binding::MatchTuple { source, field } => format!(
            "match_tuple({source}, {field})",
            source = source.index(),
            field = field.index()
        ),
        _ => todo!("binding: {binding:?}"),
    }
}

pub fn constraint_string(constraint: &Constraint, tyenv: &TypeEnv) -> String {
    match constraint {
        Constraint::Variant { ty, variant, .. } => {
            let ty = &tyenv.types[ty.index()];
            match ty {
                Type::Primitive(_, sym, _) => {
                    format!("variant({})", tyenv.syms[sym.index()].clone())
                }
                Type::Enum { name, variants, .. } => {
                    let name = &tyenv.syms[name.index()];
                    let variant = &variants[variant.index()];
                    let variant_name = &tyenv.syms[variant.name.index()];
                    format!("variant({name}::{variant_name})")
                }
            }
        }
        Constraint::ConstInt { val, .. } => format!("const_int({})", val),
        Constraint::ConstPrim { val } => format!("const_prim({})", tyenv.syms[val.index()]),
        Constraint::Some => "some".to_string(),
    }
}
