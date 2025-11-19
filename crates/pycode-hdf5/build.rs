use std::env;
use std::path::PathBuf;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());

    // **********************************************************************
    // compile the C part of the library and link it with the hdf5 library
    // **********************************************************************
    println!("cargo:rerun-if-changed=../src/pycode_h5.c");
    println!("cargo:rerun-if-changed=../src/pycode_h5.h");

    let hdf5_include_dir = PathBuf::from(env::var("HDF5_INCLUDE_DIR")
        .expect("Please set the `HDF5_INCLUDE_DIR` environment variable"));
    let hdf5_lib_dir =
        PathBuf::from(env::var("HDF5_LIB_DIR")
            .expect("Please set the `HDF5_LIB_DIR` environment variable"));
    // let hdf5_static_lib = hdf5_lib_dir.join("libhdf5.a");

    cc::Build::new()
        .file("src/pycode_h5.c")
        .include(hdf5_include_dir.clone())
        //.object(hdf5_static_lib)
        .compile("pycode_h5");

    
    // **********************************************************************
    // use BINDGEN to generate binding to c_pycode
    // **********************************************************************
    let bindings = bindgen::Builder::default()
        .header("src/pycode_h5.h")
        .clang_arg(format!("-I{}", hdf5_include_dir.display()))
        .parse_callbacks(Box::new(bindgen::CargoCallbacks::new()))
        .generate()
        .expect("Unable to generate bindings");

    bindings
        .write_to_file(out_path.join("bindings.rs"))
        .expect("Couldn't write bindings");

    
    // **********************************************************************
    // tell the compiler what to link and where it is 
    // **********************************************************************
    println!(
        "cargo:rustc-link-search=native={}/lib",
        out_path.display()
    );
    println!("cargo:rustc-link-lib=static=pycode_h5");
    println!("cargo:rustc-link-search=native={}", hdf5_lib_dir.display());
    println!("cargo:rustc-link-lib=static=hdf5");
    println!("cargo:rustc-link-lib=static=zlib-static");
    #[cfg(target_os = "windows")]
    println!("cargo:rustc-link-lib=static=szaec");
    #[cfg(target_os = "windows")]
    println!("cargo:rustc-link-lib=static=aec");
    #[cfg(not(target_os = "windows"))]
    println!("cargo:rustc-link-lib=static=szaec");
    #[cfg(not(target_os = "windows"))]
    println!("cargo:rustc-link-lib=static=aec");
    Ok(())
}
