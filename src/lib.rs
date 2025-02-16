// Pycode. A native python library for the analysis of MEA recordings
// Corporation: DIBRIS University of Genoa
// Author:      Leonardo Mascelli

use pyo3::prelude::*;
use spike_rs::analysis;

// mod sys {
//     #![allow(non_upper_case_globals)]
//     #![allow(non_camel_case_types)]
//     #![allow(non_snake_case)]
//     #![allow(dead_code)]
//     include!(concat!(env!("OUT_DIR"), "/bindings.rs"));
// }

mod sys;

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

#[pyfunction(name = "init")]
fn py_init() -> bool {
    match spike_c_init() {
        Ok(()) => true,
        Err(err) => {
            println!("{err:?}");
            false
        }
    }
}

#[pyfunction(name = "close")]
fn py_close() {
    spike_c_close();
}

//        compute_threshold,
//        spike_detection,
//        get_digital_intervals,
//        subsample_range,

#[pyfunction]
fn compute_threshold(range: Vec<f32>, sampling_frequency: f32, multiplier: f32) -> Option<f32> {
    match analysis::spike_detection::compute_threshold(
        range[..].as_ref(),
        sampling_frequency,
        multiplier,
    ) {
        Ok(ret) => Some(ret),
        Err(err) => {
            eprintln!("compute_threshold: {err:?}");
            None
        }
    }
}

#[pyfunction]
fn spike_detection(
    data: Vec<f32>,
    sampling_frequency: f32,
    threshold: f32,
    peak_duration: f32,
    refractory_time: f32,
) -> Option<(Vec<usize>, Vec<f32>)> {
    match analysis::spike_detection::spike_detection(
        data[..].as_ref(),
        sampling_frequency,
        threshold,
        peak_duration,
        refractory_time,
    ) {
        Ok(ret) => Some(ret),
        Err(err) => {
            eprintln!("spike_detection: {err:?}");
            None
        }
    }
}

#[pyfunction]
fn get_digital_intervals(digital: Vec<f32>) -> Option<Vec<(usize, usize)>> {
    Some(analysis::digital::get_digital_intervals(
        digital[..].as_ref(),
    ))
}

#[pyfunction]
pub fn subsample_range(
    peak_times: Vec<usize>,
    starting_sample: usize,
    bin_size: usize,
    n_bins: usize,
) -> Option<Vec<usize>> {
    Some(analysis::subsampling::subsample_range(
        peak_times[..].as_ref(),
        starting_sample,
        bin_size,
        n_bins,
    ))
}

#[pyfunction]
pub fn logspace(start: f32, end: f32, n_points: usize) -> Vec<f32> {
    return spike_rs::operations::math::logspace(start, end, n_points);
}

#[pyfunction]
pub fn lowess(data: Vec<f32>, span: f32) -> Vec<f32> {
    return spike_rs::operations::math::lowess(data[..].as_ref(), span);
}

#[pyfunction]
pub fn burst_detection(
    peak_train: Vec<usize>,
    sampling_frequency: f32,
    cutoff: f32,
) -> Option<(Vec<usize>, Vec<usize>, Vec<usize>)> {
    match analysis::spike_analysis::logisi::burst_detection(
        peak_train[..].as_ref(),
        sampling_frequency,
        cutoff,
    ) {
        Ok(ret) => Some(ret),
        Err(err) => {
            eprintln!("burst_detection: {err:?}");
            None
        }
    }
}

#[pymodule(name = "pycode")]
fn pycode_rs_module(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_class::<channel::PyChannel>()?;
    m.add_class::<phase::PyPhase>()?;
    m.add_function(wrap_pyfunction!(py_init, m)?)?;
    m.add_function(wrap_pyfunction!(py_close, m)?)?;
    m.add_function(wrap_pyfunction!(compute_threshold, m)?)?;
    m.add_function(wrap_pyfunction!(spike_detection, m)?)?;
    m.add_function(wrap_pyfunction!(get_digital_intervals, m)?)?;
    m.add_function(wrap_pyfunction!(subsample_range, m)?)?;
    m.add_function(wrap_pyfunction!(logspace, m)?)?;
    m.add_function(wrap_pyfunction!(lowess, m)?)?;
    m.add_function(wrap_pyfunction!(burst_detection, m)?)?;
    Ok(())
}
