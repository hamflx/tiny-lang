use std::path::PathBuf;

fn main() {
    let manifest_dir: PathBuf = std::env::var("CARGO_MANIFEST_DIR").unwrap().into();
    let workspace_dir = manifest_dir.parent().unwrap();
    println!("cargo:rustc-link-search={}", workspace_dir.display());
    println!("cargo:rustc-link-lib=static=machine_code");
}
