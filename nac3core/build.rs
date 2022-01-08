use regex::Regex;
use std::{
    env,
    io::Write,
    process::{Command, Stdio},
};

fn main() {
    let out_dir = env::var("OUT_DIR").unwrap();
    const FILE: &str = "src/codegen/irrt/irrt.c";
    println!("cargo:rerun-if-changed={}", FILE);
    const FLAG: &[&str] = &[
        FILE,
        "-O3",
        "-emit-llvm",
        "-S",
        "-Wall",
        "-Wextra",
        "-Wno-implicit-function-declaration",
        "-o",
        "-",
    ];
    let output = Command::new("clang")
        .args(FLAG)
        .output()
        .map(|o| {
            assert!(o.status.success(), "{}", std::str::from_utf8(&o.stderr).unwrap());
            o
        })
        .unwrap();

    let output = std::str::from_utf8(&output.stdout).unwrap();
    let mut filtered_output = String::with_capacity(output.len());

    let regex_filter = regex::Regex::new(r"(?ms:^define.*?\}$)|(?m:^declare.*?$)").unwrap();
    for f in regex_filter.captures_iter(output) {
        assert!(f.len() == 1);
        filtered_output.push_str(&f[0]);
        filtered_output.push('\n');
    }

    let filtered_output = Regex::new("(#\\d+)|(, *![0-9A-Za-z.]+)|(![0-9A-Za-z.]+)|(!\".*?\")")
        .unwrap()
        .replace_all(&filtered_output, "");

    let mut llvm_as = Command::new("llvm-as")
        .stdin(Stdio::piped())
        .arg("-o")
        .arg(&format!("{}/irrt.bc", out_dir))
        .spawn()
        .unwrap();
    llvm_as.stdin.as_mut().unwrap().write_all(filtered_output.as_bytes()).unwrap();
    assert!(llvm_as.wait().unwrap().success())
}
