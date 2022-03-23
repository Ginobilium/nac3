fn main() {
	#[cfg(not(windows))]
    println!("cargo:rustc-link-arg=-rdynamic");
}
