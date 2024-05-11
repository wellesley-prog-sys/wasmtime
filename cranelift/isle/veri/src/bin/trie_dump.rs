use clap::Parser;
use cranelift_codegen_meta::{generate_isle, isle::get_isle_compilations};
use cranelift_isle::{
    overlap,
    sema::{ExternalSig, ReturnKind, TermId, Type, TypeEnv, TypeId},
    trie_again::{Binding, BindingId, Constraint, RuleSet},
};
use cranelift_isle_veri::program::Program;

#[derive(Parser)]
struct Opts {
    /// Name of the ISLE compilation.
    #[arg(long, required = true)]
    name: String,

    /// Path to codegen crate directory.
    #[arg(long, required = true)]
    codegen_crate_dir: std::path::PathBuf,

    /// Working directory.
    #[arg(long, required = true)]
    work_dir: std::path::PathBuf,
}

impl Opts {
    fn isle_input_files(&self) -> anyhow::Result<Vec<std::path::PathBuf>> {
        // Generate ISLE files.
        let gen_dir = &self.work_dir;
        generate_isle(gen_dir)?;

        // Lookup ISLE compilations.
        let compilations = get_isle_compilations(&self.codegen_crate_dir, gen_dir);

        // Return inputs from the matching compilation, if any.
        Ok(compilations
            .lookup(&self.name)
            .ok_or(anyhow::format_err!(
                "unknown ISLE compilation: {}",
                self.name
            ))?
            .inputs())
    }
}

fn main() -> anyhow::Result<()> {
    let opts = Opts::parse();

    // Read ISLE inputs.
    let inputs = opts.isle_input_files()?;
    let expand_internal_extractors = true;
    let prog = Program::from_files(&inputs, expand_internal_extractors)?;

    // Dump rule sets.
    let term_rule_sets = overlap::check(&prog.tyenv, &prog.termenv)?;
    for (term_id, rule_set) in &term_rule_sets {
        println!("term {{");
        println!("\tname = {}", prog.term_name(*term_id));

        // Bindings.
        println!("\tbindings = [");
        for (i, binding) in rule_set.bindings.iter().enumerate() {
            println!(
                "\t\t{i}:\t{}",
                binding_string(binding, *term_id, &prog, rule_set)
            );
        }
        println!("\t]");

        // Rules.
        println!("\trules = [");
        for rule in &rule_set.rules {
            assert_eq!(rule.iterators.len(), 0);
            // TODO(mbm): how to handle impure?

            println!("\t\t{{");
            println!(
                "\t\t\tpos = {}",
                rule.pos.pretty_print_line(&prog.tyenv.filenames[..])
            );
            println!("\t\t\tconstraints = [");
            for i in 0..rule_set.bindings.len() {
                if let Some(constraint) = rule.get_constraint(i.try_into().unwrap()) {
                    println!(
                        "\t\t\t\t{}: {}",
                        i,
                        constraint_string(&constraint, &prog.tyenv)
                    );
                }
            }
            println!("\t\t\t]");
            if !rule.equals.is_empty() {
                println!("\t\t\tequals = {:?}", rule.equals);
            }
            println!("\t\t\tprio = {}", rule.prio);
            println!("\t\t\tresult = {}", rule.result.index());
            println!("\t\t}}");
        }
        println!("\t]");

        println!("}}");
    }

    Ok(())
}

fn binding_string(
    binding: &Binding,
    term_id: TermId,
    prog: &Program,
    rule_set: &RuleSet,
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
            let source_binding = &rule_set.bindings[source.index()];
            let source_type = binding_type(source_binding, term_id, prog, rule_set);
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
        Binding::MatchSome { source } => format!("match_some({source})", source = source.index()),
        Binding::MatchTuple { source, field } => format!(
            "match_tuple({source}, {field})",
            source = source.index(),
            field = field.index()
        ),
        _ => todo!("binding: {binding:?}"),
    }
}

fn constraint_string(constraint: &Constraint, tyenv: &TypeEnv) -> String {
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

#[derive(Clone, Debug)]
enum BindingType {
    Base(TypeId),
    Option(Box<BindingType>),
    Tuple(Vec<BindingType>),
}

// Determine the type of a given binding.
fn binding_type(
    binding: &Binding,
    term_id: TermId,
    prog: &Program,
    rule_set: &RuleSet,
) -> BindingType {
    match binding {
        Binding::Argument { index } => {
            let term = &prog.termenv.terms[term_id.index()];
            BindingType::Base(term.arg_tys[index.index()])
        }
        Binding::Extractor { term, .. } => {
            // Determine the extractor signature.
            let term = &prog.termenv.terms[term.index()];
            let sig = term
                .extractor_sig(&prog.tyenv)
                .expect("term should have extractor signature");
            external_sig_return_type(&sig)
        }

        Binding::Constructor { term, .. } => {
            // Determine the constructor signature.
            let term = &prog.termenv.terms[term.index()];
            let sig = term
                .constructor_sig(&prog.tyenv)
                .expect("term should have constructor signature");
            external_sig_return_type(&sig)
        }

        Binding::MatchSome { source } => {
            let source_binding = &rule_set.bindings[source.index()];
            let source_ty = binding_type(source_binding, term_id, prog, rule_set);
            match source_ty {
                BindingType::Option(ty) => *ty,
                _ => unreachable!("source of match some should be an option"),
            }
        }

        Binding::MatchVariant {
            source,
            variant,
            field,
        } => {
            // Lookup type ID for the underlying enum.
            let source_binding = &rule_set.bindings[source.index()];
            let source_ty = binding_type(source_binding, term_id, prog, rule_set);
            let source_type_id = match source_ty {
                BindingType::Base(type_id) => type_id,
                _ => unreachable!("source of match variant should be a base type"),
            };

            // Lookup variant.
            let enum_ty = &prog.tyenv.types[source_type_id.index()];
            let variant = match enum_ty {
                Type::Enum { variants, .. } => &variants[variant.index()],
                _ => unreachable!("source match variant should be an enum"),
            };

            // Lookup field type.
            let field = &variant.fields[field.index()];
            BindingType::Base(field.ty)
        }

        _ => todo!("binding type: {binding:?}"),
    }
}

fn external_sig_return_type(sig: &ExternalSig) -> BindingType {
    // Multiple return types are represented as a tuple.
    let ty = if sig.ret_tys.len() == 1 {
        BindingType::Base(sig.ret_tys[0].clone())
    } else {
        BindingType::Tuple(
            sig.ret_tys
                .iter()
                .copied()
                .map(|type_id| BindingType::Base(type_id))
                .collect(),
        )
    };

    // Fallible terms return option type.
    match sig.ret_kind {
        ReturnKind::Option => BindingType::Option(Box::new(ty)),
        ReturnKind::Plain => ty,
        ReturnKind::Iterator => unimplemented!("extractor iterator return"),
    }
}
