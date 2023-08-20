fn main() {
  println!("cargo:rustc-link-search=/usr/include");
  println!("cargo:rerun-if-changed=wrapper.h");
  let bindings = bindgen::Builder::default()
    .header("wrapper.h")
    .prepend_enum_name(false)
    .derive_default(true)
    .generate_comments(true)
    .allowlist_type("btrfs[_]util.*")
    .allowlist_var("BTRFS[_]UTIL.*")
    .parse_callbacks(Box::new(bindgen::CargoCallbacks))
    .generate()
    .expect("Unable to generate bindings");
  bindings
    .write_to_file("bindings.rs")
    .expect("Couldn't write bindings!");
}
