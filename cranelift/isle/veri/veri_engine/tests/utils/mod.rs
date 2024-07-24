use cranelift_isle::compile::create_envs;
use std::env;
use std::path::PathBuf;
use std::time::Duration;
use strum::IntoEnumIterator;
use strum_macros::EnumIter;
use veri_engine_lib::annotations::parse_annotations;
use veri_engine_lib::build_clif_lower_isle;
use veri_engine_lib::type_inference::type_rules_with_term_and_types;
use veri_engine_lib::verify::verify_rules_for_term;
use veri_engine_lib::Config;
use veri_ir::{ConcreteTest, Counterexample, TermSignature, VerificationResult};

#[derive(Debug, EnumIter, PartialEq, Eq, PartialOrd, Ord, Copy, Clone)]
#[repr(usize)]
pub enum Bitwidth {
    I8 = 8,
    I16 = 16,
    I32 = 32,
    I64 = 64,
}

pub enum TestResult {
    Simple(Vec<(Bitwidth, VerificationResult)>),
    Expect(fn(&TermSignature) -> VerificationResult),
}

type TestResultBuilder = dyn Fn(Bitwidth) -> (Bitwidth, VerificationResult);

// Some examples of functions we might need
#[allow(dead_code)]
pub fn just_8_result() -> TestResult {
    TestResult::Simple(vec![(Bitwidth::I8, VerificationResult::Success)])
}

#[allow(dead_code)]
pub fn just_16_result() -> TestResult {
    TestResult::Simple(vec![(Bitwidth::I16, VerificationResult::Success)])
}

#[allow(dead_code)]
pub fn just_32_result() -> TestResult {
    TestResult::Simple(vec![(Bitwidth::I32, VerificationResult::Success)])
}

#[allow(dead_code)]
pub fn just_64_result() -> TestResult {
    TestResult::Simple(vec![(Bitwidth::I64, VerificationResult::Success)])
}

/// All bitwidths verify
#[allow(dead_code)]
pub fn all_success_result() -> Vec<(Bitwidth, VerificationResult)> {
    custom_result(&|w| (w, VerificationResult::Success))
}

/// All bitwidths fail
#[allow(dead_code)]
pub fn all_failure_result() -> Vec<(Bitwidth, VerificationResult)> {
    custom_result(&|w| (w, VerificationResult::Failure(Counterexample {})))
}

/// Specify a custom set expected result (helpful if you want to test all the bitwidths and expect
/// a range of different success, failure, and inapplicable outcomes)
pub fn custom_result(f: &TestResultBuilder) -> Vec<(Bitwidth, VerificationResult)> {
    Bitwidth::iter().map(|w| f(w)).collect()
}

/// Run the test with a 4 minute timeout, retrying 5 times if timeout hit, waiting 1ms between tries
pub fn run_and_retry<F>(f: F) -> ()
where
    F: Fn() -> (),
    F: Send + 'static + Copy,
{
    let delay_before_retrying = retry::delay::Fixed::from_millis(1);
    let num_retries = 5;
    let timeout_per_try = Duration::from_secs(4 * 60);

    use std::{sync::mpsc, thread};
    let result = retry::retry_with_index(delay_before_retrying, |current_try| {
        if current_try > num_retries {
            return retry::OperationResult::Err(format!(
                "Test did not succeed within {} tries",
                num_retries
            ));
        }
        if current_try > 1 {
            println!("Retrying test that timed out, try #{}", current_try);
        }

        // From: https://github.com/rust-lang/rfcs/issues/2798
        let (done_tx, done_rx) = mpsc::channel();
        let handle = thread::spawn(move || {
            f();
            done_tx.send(()).expect("Unable to send completion signal");
        });

        match done_rx.recv_timeout(timeout_per_try) {
            Ok(_) => match handle.join() {
                Ok(_) => retry::OperationResult::Ok("Test thread succeeded"),
                Err(e) => retry::OperationResult::Err(format!("Test thread panicked {:?}", e)),
            },
            Err(_) => match handle.join() {
                Ok(_) => retry::OperationResult::Retry("Test thread took too long".to_string()),
                Err(e) => retry::OperationResult::Err(format!("Test thread panicked {:?}", e)),
            },
        }
    });
    result.unwrap();
}

fn test_rules_with_term(inputs: Vec<PathBuf>, tr: TestResult, config: Config) -> () {
    let lexer = cranelift_isle::lexer::Lexer::from_files(&inputs).unwrap();
    let defs = cranelift_isle::parser::parse(lexer).expect("should parse");
    let (typeenv, termenv) = create_envs(&defs).unwrap();
    let annotation_env = parse_annotations(&defs, &termenv, &typeenv);

    let term_signatures = annotation_env
        .get_term_signatures_by_name(&termenv, &typeenv)
        .get(config.term.as_str())
        .expect(format!("Missing term width for {}", config.term).as_str())
        .clone();
    let instantiations = match tr {
        TestResult::Simple(s) => {
            let mut res = vec![];
            for (width, result) in s {
                let ty = match width {
                    Bitwidth::I8 => veri_ir::Type::BitVector(Some(8)),
                    Bitwidth::I16 => veri_ir::Type::BitVector(Some(16)),
                    Bitwidth::I32 => veri_ir::Type::BitVector(Some(32)),
                    Bitwidth::I64 => veri_ir::Type::BitVector(Some(64)),
                };
                // Find the type instantiations with this as the canonical type
                let all_instantiations: Vec<&TermSignature> = term_signatures
                    .iter()
                    .filter(|sig| sig.canonical_type.unwrap() == ty)
                    .collect();
                if all_instantiations.len() < 1 {
                    panic!("Missing type instantiation for width {:?}", width);
                }
                for i in all_instantiations {
                    res.push((i.clone(), result.clone()));
                }
            }
            res
        }
        TestResult::Expect(expect) => term_signatures
            .iter()
            .map(|sig| (sig.clone(), expect(sig)))
            .collect(),
    };

    for (type_instantiation, expected_result) in instantiations {
        println!("Expected result: {:?}", expected_result);
        let type_sols = type_rules_with_term_and_types(
            &termenv,
            &typeenv,
            &annotation_env,
            &config,
            &type_instantiation,
            &None,
        );
        let result = verify_rules_for_term(
            &termenv,
            &typeenv,
            &type_sols,
            type_instantiation,
            &None,
            &config,
        );
        assert_eq!(result, expected_result);
    }
}

pub fn test_from_file_with_lhs_termname_simple(
    file: &str,
    termname: String,
    tr: Vec<(Bitwidth, VerificationResult)>,
) -> () {
    test_from_file_with_lhs_termname(file, termname, TestResult::Simple(tr))
}

pub fn test_from_file_with_lhs_termname(file: &str, termname: String, tr: TestResult) -> () {
    println!("Verifying {} rules in file: {}", termname, file);
    // TODO: clean up path logic
    let cur_dir = env::current_dir().expect("Can't access current working directory");
    let clif_isle = cur_dir.join("../../../codegen/src").join("inst_specs.isle");
    let prelude_isle = cur_dir.join("../../../codegen/src").join("prelude.isle");
    let prelude_lower_isle = cur_dir
        .join("../../../codegen/src")
        .join("prelude_lower.isle");
    let mut inputs = vec![prelude_isle, prelude_lower_isle, clif_isle];
    inputs.push(build_clif_lower_isle());
    inputs.push(PathBuf::from(file));
    let config = Config {
        term: termname,
        distinct_check: true,
        custom_verification_condition: None,
        custom_assumptions: None,
        names: None,
    };
    test_rules_with_term(inputs, tr, config);
}

pub fn test_aarch64_rule_with_lhs_termname_simple(
    rulename: &str,
    termname: &str,
    tr: Vec<(Bitwidth, VerificationResult)>,
) -> () {
    test_aarch64_rule_with_lhs_termname(rulename, termname, TestResult::Simple(tr))
}

pub fn test_aarch64_rule_with_lhs_termname(rulename: &str, termname: &str, tr: TestResult) -> () {
    println!("Verifying rule `{}` with termname {} ", rulename, termname);
    // TODO: clean up path logic
    let cur_dir = env::current_dir().expect("Can't access current working directory");
    let clif_isle = cur_dir.join("../../../codegen/src").join("inst_specs.isle");
    let prelude_isle = cur_dir.join("../../../codegen/src").join("prelude.isle");
    let prelude_lower_isle = cur_dir
        .join("../../../codegen/src")
        .join("prelude_lower.isle");
    let mut inputs = vec![
        build_clif_lower_isle(),
        prelude_isle,
        prelude_lower_isle,
        clif_isle,
    ];
    inputs.push(
        cur_dir
            .join("../../../codegen/src/isa/aarch64")
            .join("inst.isle"),
    );
    inputs.push(
        cur_dir
            .join("../../../codegen/src/isa/aarch64")
            .join("lower.isle"),
    );
    let config = Config {
        term: termname.to_string(),
        distinct_check: true,
        custom_verification_condition: None,
        custom_assumptions: None,
        names: Some(vec![rulename.to_string()]),
    };
    test_rules_with_term(inputs, tr, config);
}

pub fn test_x64_rule_with_lhs_termname_simple(
    rulename: &str,
    termname: &str,
    tr: Vec<(Bitwidth, VerificationResult)>,
) -> () {
    test_x64_rule_with_lhs_termname(rulename, termname, TestResult::Simple(tr))
}

pub fn test_x64_rule_with_lhs_termname(rulename: &str, termname: &str, tr: TestResult) -> () {
    println!("Verifying rule `{}` with termname {} ", rulename, termname);
    // TODO(mbm): dedupe with aarch64
    // TODO(mbm): share configuration with cranelift/codegen/build.rs (or export it as part of build)
    let cur_dir = env::current_dir().expect("Can't access current working directory");
    let clif_isle = cur_dir.join("../../../codegen/src").join("inst_specs.isle");
    let prelude_isle = cur_dir.join("../../../codegen/src").join("prelude.isle");
    let prelude_lower_isle = cur_dir
        .join("../../../codegen/src")
        .join("prelude_lower.isle");
    let mut inputs = vec![
        build_clif_lower_isle(),
        prelude_isle,
        prelude_lower_isle,
        clif_isle,
    ];
    inputs.push(
        cur_dir
            .join("../../../codegen/src/isa/x64")
            .join("inst.isle"),
    );
    inputs.push(
        cur_dir
            .join("../../../codegen/src/isa/x64")
            .join("lower.isle"),
    );
    let config = Config {
        term: termname.to_string(),
        distinct_check: true,
        custom_verification_condition: None,
        custom_assumptions: None,
        names: Some(vec![rulename.to_string()]),
    };
    test_rules_with_term(inputs, tr, config);
}

pub fn test_from_file_with_config_simple(
    file: &str,
    config: Config,
    tr: Vec<(Bitwidth, VerificationResult)>,
) -> () {
    test_from_file_with_config(file, config, TestResult::Simple(tr))
}
pub fn test_from_file_with_config(file: &str, config: Config, tr: TestResult) -> () {
    println!("Verifying {} rules in file: {}", config.term, file);
    // TODO: clean up path logic
    let cur_dir = env::current_dir().expect("Can't access current working directory");
    let clif_isle = cur_dir.join("../../../codegen/src").join("inst_specs.isle");
    let prelude_isle = cur_dir.join("../../../codegen/src").join("prelude.isle");
    let prelude_lower_isle = cur_dir
        .join("../../../codegen/src")
        .join("prelude_lower.isle");
    let mut inputs = vec![
        build_clif_lower_isle(),
        prelude_isle,
        prelude_lower_isle,
        clif_isle,
    ];
    inputs.push(PathBuf::from(file));
    test_rules_with_term(inputs, tr, config);
}

pub fn test_aarch64_with_config_simple(
    config: Config,
    tr: Vec<(Bitwidth, VerificationResult)>,
) -> () {
    test_aarch64_with_config(config, TestResult::Simple(tr))
}

pub fn test_aarch64_with_config(config: Config, tr: TestResult) -> () {
    println!(
        "Verifying rules {:?} with termname {}",
        config.names, config.term
    );
    // TODO: clean up path logic
    let cur_dir = env::current_dir().expect("Can't access current working directory");
    let clif_isle = cur_dir.join("../../../codegen/src").join("inst_specs.isle");
    let prelude_isle = cur_dir.join("../../../codegen/src").join("prelude.isle");
    let prelude_lower_isle = cur_dir
        .join("../../../codegen/src")
        .join("prelude_lower.isle");
    let mut inputs = vec![
        build_clif_lower_isle(),
        prelude_isle,
        prelude_lower_isle,
        clif_isle,
    ];
    inputs.push(
        cur_dir
            .join("../../../codegen/src/isa/aarch64")
            .join("inst.isle"),
    );
    inputs.push(
        cur_dir
            .join("../../../codegen/src/isa/aarch64")
            .join("lower.isle"),
    );
    test_rules_with_term(inputs, tr, config);
}

pub fn test_concrete_aarch64_rule_with_lhs_termname(
    rulename: &str,
    termname: &str,
    concrete: ConcreteTest,
) -> () {
    println!(
        "Verifying concrete input rule `{}` with termname {} ",
        rulename, termname
    );
    // TODO: clean up path logic
    let cur_dir = env::current_dir().expect("Can't access current working directory");
    let clif_isle = cur_dir.join("../../../codegen/src").join("inst_specs.isle");
    let prelude_isle = cur_dir.join("../../../codegen/src").join("prelude.isle");
    let prelude_lower_isle = cur_dir
        .join("../../../codegen/src")
        .join("prelude_lower.isle");
    let mut inputs = vec![
        build_clif_lower_isle(),
        prelude_isle,
        prelude_lower_isle,
        clif_isle,
    ];
    inputs.push(
        cur_dir
            .join("../../../codegen/src/isa/aarch64")
            .join("inst.isle"),
    );
    inputs.push(
        cur_dir
            .join("../../../codegen/src/isa/aarch64")
            .join("lower.isle"),
    );

    let lexer = cranelift_isle::lexer::Lexer::from_files(&inputs).unwrap();
    let defs = cranelift_isle::parser::parse(lexer).expect("should parse");
    let (typeenv, termenv) = create_envs(&defs).unwrap();
    let annotation_env = parse_annotations(&defs, &termenv, &typeenv);

    let config = Config {
        term: termname.to_string(),
        distinct_check: false,
        custom_verification_condition: None,
        custom_assumptions: None,
        names: Some(vec![rulename.to_string()]),
    };

    // Get the types/widths for this particular term
    let args = concrete.args.iter().map(|i| i.ty.clone()).collect();
    let ret = concrete.output.ty;
    let t = TermSignature {
        args,
        ret,
        canonical_type: None,
    };

    let type_sols = type_rules_with_term_and_types(
        &termenv,
        &typeenv,
        &annotation_env,
        &config,
        &t,
        &Some(concrete.clone()),
    );
    let result = verify_rules_for_term(&termenv, &typeenv, &type_sols, t, &Some(concrete), &config);
    assert_eq!(result, VerificationResult::Success);
}

pub fn test_concrete_input_from_file_with_lhs_termname(
    file: &str,
    termname: String,
    concrete: ConcreteTest,
) -> () {
    println!(
        "Verifying concrete input {} rule in file: {}",
        termname, file
    );
    // TODO: clean up path logic
    let cur_dir = env::current_dir().expect("Can't access current working directory");
    let clif_isle = cur_dir.join("../../../codegen/src").join("inst_specs.isle");
    let prelude_isle = cur_dir.join("../../../codegen/src").join("prelude.isle");
    let prelude_lower_isle = cur_dir
        .join("../../../codegen/src")
        .join("prelude_lower.isle");
    let mut inputs = vec![
        build_clif_lower_isle(),
        prelude_isle,
        prelude_lower_isle,
        clif_isle,
    ];
    inputs.push(PathBuf::from(file));

    let lexer = cranelift_isle::lexer::Lexer::from_files(&inputs).unwrap();
    let defs = cranelift_isle::parser::parse(lexer).expect("should parse");
    let (typeenv, termenv) = create_envs(&defs).unwrap();
    let annotation_env = parse_annotations(&defs, &termenv, &typeenv);

    let config = Config {
        term: termname.clone(),
        distinct_check: false,
        custom_verification_condition: None,
        custom_assumptions: None,
        names: None,
    };

    // Get the types/widths for this particular term
    let args = concrete.args.iter().map(|i| i.ty.clone()).collect();
    let ret = concrete.output.ty;
    let t = TermSignature {
        args,
        ret,
        canonical_type: None,
    };

    let type_sols = type_rules_with_term_and_types(
        &termenv,
        &typeenv,
        &annotation_env,
        &config,
        &t,
        &Some(concrete.clone()),
    );
    let result = verify_rules_for_term(&termenv, &typeenv, &type_sols, t, &Some(concrete), &config);
    assert_eq!(result, VerificationResult::Success);
}
