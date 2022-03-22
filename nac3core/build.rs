use regex::Regex;
use std::{
    env,
    fs::File,
    io::Write,
    path::Path,
    process::{Command, Stdio},
};

fn main() {
    const FILE: &str = "src/codegen/irrt/irrt.c";
    println!("cargo:rerun-if-changed={}", FILE);
    let out_dir = env::var("OUT_DIR").unwrap();
    let out_path = Path::new(&out_dir);

    /*
     * HACK: Sadly, clang doesn't let us emit generic LLVM bitcode.
     * Compiling for WASM32 and filtering the output with regex is the closest we can get.
     */

    const FLAG: &[&str] = &[
        "--target=wasm32",
        FILE,
        "-O3",
        "-emit-llvm",
        "-S",
        "-Wall",
        "-Wextra",
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

    println!("cargo:rerun-if-env-changed=DEBUG_DUMP_IRRT");
    if env::var("DEBUG_DUMP_IRRT").is_ok() {
        let mut file = File::create(out_path.join("irrt.ll")).unwrap();
        file.write_all(output.as_bytes()).unwrap();
        let mut file = File::create(out_path.join("irrt-filtered.ll")).unwrap();
        file.write_all(filtered_output.as_bytes()).unwrap();
    }

    let mut llvm_as = Command::new("llvm-as")
        .stdin(Stdio::piped())
        .arg("-o")
        .arg(out_path.join("irrt.bc"))
        .spawn()
        .unwrap();
    llvm_as.stdin.as_mut().unwrap().write_all(filtered_output.as_bytes()).unwrap();
    assert!(llvm_as.wait().unwrap().success())
}
