//! Helper for autogenerated unit tests.

use cranelift_isle::compile;
use cranelift_isle::error::Errors;

fn build(filename: &str) -> Result<String, Errors> {
    compile::from_files(&[filename], &Default::default())
}

pub fn run_pass(filename: &str) {
    if let Err(err) = build(filename) {
        panic!("pass test failed:\n{err:?}");
    }
}

pub fn run_fail(filename: &str) {
    match build(filename) {
        Ok(_) => panic!("test {filename} passed unexpectedly"),
        Err(err) => {
            // Log the actual errors for use with `cargo test -- --nocapture`
            println!("failed, as expected:\n{err:?}");
        }
    }
}

fn build_and_link_isle(isle_filename: &str) -> (tempfile::TempDir, std::path::PathBuf) {
    let tempdir = tempfile::tempdir().unwrap();
    let code = build(isle_filename).unwrap();

    let isle_filename_base = std::path::Path::new(isle_filename)
        .file_stem()
        .unwrap()
        .to_str()
        .unwrap()
        .to_string();
    let isle_generated_code = tempdir
        .path()
        .to_path_buf()
        .join(isle_filename_base.clone() + ".rs");
    std::fs::write(isle_generated_code, code).unwrap();

    let rust_filename = isle_filename.replace(".isle", "").to_string() + "_main.rs";
    let rust_filename_base = std::path::Path::new(&rust_filename).file_name().unwrap();
    let rust_driver = tempdir.path().to_path_buf().join(&rust_filename_base);
    println!("copying {rust_filename} to {rust_driver:?}");
    std::fs::copy(&rust_filename, &rust_driver).unwrap();

    let output = tempdir.path().to_path_buf().join("out");

    let mut rustc = std::process::Command::new("rustc")
        .arg(&rust_driver)
        .arg("-o")
        .arg(output.clone())
        .spawn()
        .unwrap();
    assert!(rustc.wait().unwrap().success());

    (tempdir, output)
}

pub fn run_link(isle_filename: &str) {
    build_and_link_isle(isle_filename);
}

pub fn run_run(isle_filename: &str) {
    let (_tempdir, exe) = build_and_link_isle(isle_filename);

    assert!(std::process::Command::new(exe)
        .spawn()
        .unwrap()
        .wait()
        .unwrap()
        .success());
}

// Generated by build.rs.
include!(concat!(env!("OUT_DIR"), "/isle_tests.rs"));
