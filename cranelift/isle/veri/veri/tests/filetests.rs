use std::path::PathBuf;

use cranelift_isle_veri::runner::Runner;
use cranelift_isle_veri_test_macros::file_tests;

#[file_tests(path = "filetests/pass", ext = "isle")]
fn pass(test_file: &str) {
    let inputs = vec![PathBuf::from(test_file)];
    let mut runner = Runner::from_files(&inputs).expect("should be able to create runner");
    runner.include_first_rule_named();
    runner.set_root_term("test");
    runner.run().expect("verification should pass");
}

#[file_tests(path = "filetests/broken", ext = "isle")]
fn broken(test_file: &str) {
    let inputs = vec![PathBuf::from(test_file)];
    let mut runner = Runner::from_files(&inputs).expect("should be able to create runner");
    runner.include_first_rule_named();
    runner.set_root_term("test");
    runner.run().expect_err("verification should fail");
}
