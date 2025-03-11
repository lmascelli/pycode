mod sys {
    #![allow(non_upper_case_globals)]
    #![allow(non_camel_case_types)]
    #![allow(non_snake_case)]
    #![allow(dead_code)]
    #![allow(unsafe_op_in_unsafe_fn)]
    include!(concat!(env!("OUT_DIR"), "/bindings.rs"));
}

pub mod error;
use error::Error;

pub mod channel;
#[macro_use]
pub mod peak_train;
#[macro_use]
pub mod phase;

/// This function must be called before using the any other function of this
/// library. It initialize the hdf5 library and create some custom types that
/// MultiChannel Systems use in its recordings
pub fn spike_c_init() -> Result<(), Error> {
    Ok(Error::from_phaseh5_error(unsafe { sys::pycodeh5_init() })?)
}

/// This function must be called after using the any other function of this
/// library. It releases the memory bounded to the custom types created at
/// the initialization of the library
pub fn spike_c_close() {
    unsafe { sys::pycodeh5_close() };
}
