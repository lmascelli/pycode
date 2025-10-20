use pycode_hdf5::channel::Channel;
use pycode_hdf5::phase::{Phase, PhaseTrait};
use std::ffi::{c_char, c_void};
use std::ffi::{CStr, CString};
use std::ptr::null_mut;

#[unsafe(no_mangle)]
pub extern "C" fn cpycode_init() -> i64 {
    match pycode_hdf5::spike_c_init() {
        Ok(()) => 1,
        Err(err) => {
            println!("{err:?}");
            0
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn cpycode_close() {
    pycode_hdf5::spike_c_close();
}

#[unsafe(no_mangle)]
pub extern "C" fn cpycode_phase_open(filename: *const c_char) -> *mut c_void {
    let phase = Phase::open(
        unsafe { CStr::from_ptr(filename.cast()) }
            .to_str()
            .expect("Failed to parse the filename"),
    );
    match phase {
        Ok(phase) => Box::into_raw(Box::new(phase)).cast(),
        Err(_) => null_mut(),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn cpycode_phase_close(phase: *const c_void) {
    let _ = unsafe { Box::from_raw(phase as *const Phase as *mut Phase) };
}

#[unsafe(no_mangle)]
pub extern "C" fn cpycode_phase_print(phase: *const c_void) -> *const c_char {
    let phase = unsafe { &*(phase as *const Phase) };
    let phase_description = format!("{phase:?}");
    let phase_cstring =
        CString::new(phase_description).expect("Failed to convert the Phase description");
    phase_cstring.into_raw().cast()
}

#[unsafe(no_mangle)]
pub extern "C" fn cpycode_phase_sampling_frequency(phase: *const c_void) -> f32 {
    let phase = unsafe { &*(phase as *const Phase) };
    phase.sampling_frequency()
}

#[unsafe(no_mangle)]
pub extern "C" fn cpycode_phase_datalen(phase: *const c_void) -> usize {
    let phase = unsafe { &*(phase as *const Phase) };
    phase.datalen()
}

#[unsafe(no_mangle)]
pub extern "C" fn cpycode_phase_n_channels(phase: *const c_void) -> usize {
    let phase = unsafe { &*(phase as *const Phase) };
    phase.channels().len()
}

#[unsafe(no_mangle)]
pub extern "C" fn cpycode_phase_channel_label(phase: *const c_void, index: usize) -> *const c_char {
    let phase = unsafe { &*(phase as *const Phase) };
    CString::new(phase.channels()[index].label.clone())
        .expect("Failed to parse the channel label")
        .into_raw()
        .cast()
}

#[unsafe(no_mangle)]
pub extern "C" fn cpycode_phase_channel_group(phase: *const c_void, index: usize) -> usize {
    let phase = unsafe { &*(phase as *const Phase) };
    phase.channels()[index].group
}

#[unsafe(no_mangle)]
#[allow(clippy::not_unsafe_ptr_arg_deref)]
pub extern "C" fn cpycode_phase_raw_data(
    phase: *const c_void,
    group: usize,
    label: *const c_char,
    data: *mut *const f32,
    start: usize,
    end: usize,
) -> usize {
    // SAFETY:
    // the phase pointer should be guaranteed to be a valid pointer
    let phase = unsafe { &*(phase as *const Phase) };
    let raw_data = phase
        .raw_data(
            &Channel {
                index: 0,
                label: String::from(
                    // SAFETY: the program will panic with an explaination if the conversion 
                    // of the label pointer is not valid
                    unsafe { CStr::from_ptr(label) }
                        .to_str()
                        .expect("Failed to convert the label string"),
                ),
                group,
            },
            Some(start),
            if end == 0 { None } else { Some(end) },
        )
        .expect("Failed to get raw_data");
    unsafe { *data = raw_data.as_ptr() };
    let len = raw_data.len();
    std::mem::forget(raw_data);
    len
}
