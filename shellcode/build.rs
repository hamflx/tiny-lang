use std::path::PathBuf;

fn main() {
    let manifest_dir: PathBuf = std::env::var("CARGO_MANIFEST_DIR").unwrap().into();
    let workspace_dir = manifest_dir.parent().unwrap();
    let lib_path = workspace_dir.join("machine_code.lib");
    println!("cargo:rerun-if-changed={}", lib_path.display());
    println!("cargo:rustc-link-search={}", workspace_dir.display());
    println!("cargo:rustc-link-lib=static=machine_code");
}
