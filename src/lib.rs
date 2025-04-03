// Pycode. A native python library for the analysis of MEA recordings
// Corporation: DIBRIS University of Genoa
// Author:      Leonardo Mascelli

use pycode_hdf5::{channel::Channel, phase::Phase, spike_c_close, spike_c_init};
use pyo3::prelude::*;
use spike_rs::{
    operations::filter::{ButterworthFilter, ButterworthFilterType, filtfilt},
    types::PhaseTrait,
};
use std::hash::{DefaultHasher, Hash, Hasher};

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

#[pyfunction]
fn compute_threshold(
    range: Vec<f32>,
    sampling_frequency: f32,
    multiplier: f32,
    min_threshold: f32,
) -> Option<f32> {
    match spike_rs::analysis::spike_detection::compute_threshold(
        range[..].as_ref(),
        sampling_frequency,
        multiplier,
        min_threshold,
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
    match spike_rs::analysis::spike_detection::spike_detection(
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
fn spike_detection_new(
    data: Vec<f32>,
    threshold: f32,
    peak_duration: usize,
    peak_distance: usize,
) -> Option<(Vec<usize>, Vec<usize>, Vec<f32>, Vec<f32>)> {
    match spike_rs::analysis::spike_detection::spike_detection_new(
        data[..].as_ref(),
        threshold,
        peak_duration,
        peak_distance,
        None,
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
    Some(spike_rs::analysis::digital::get_digital_intervals(
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
    Some(spike_rs::analysis::subsampling::subsample_range(
        peak_times[..].as_ref(),
        starting_sample,
        bin_size,
        n_bins,
    ))
}

#[pyfunction]
pub fn count_peaks_in_intervals(
    peak_times: Vec<usize>,
    intervals: Vec<(usize, usize)>,
) -> Vec<usize> {
    return spike_rs::analysis::spike_analysis::count_peaks_in_intervals(
        peak_times[..].as_ref(),
        intervals[..].as_ref(),
    );
}

#[pyfunction]
pub fn find_peaks_around_points(
    peak_times: Vec<usize>,
    points: Vec<usize>,
    window_half_size: usize,
) -> Vec<usize> {
    return spike_rs::analysis::cleaning::find_peaks_around_points(
        &peak_times[..],
        &points[..],
        window_half_size,
    );
}

#[pyfunction]
pub fn high_pass_filter(input: Vec<f32>, sampling_frequency: f32, cutoff: f32) -> Vec<f32> {
    let filter = ButterworthFilter::create(
        ButterworthFilterType::HighPass,
        3,
        cutoff,
        sampling_frequency,
    );
    println!("FILTER: {filter:?}");
    return filtfilt(&filter, input[..].as_ref());
}

#[pyfunction]
pub fn logspace(start: f32, end: f32, n_points: usize) -> Vec<f32> {
    return spike_rs::operations::math::logspace(start, end, n_points);
}

#[pyfunction]
pub unsafe fn lowess(data: Vec<f32>, span: f32) -> Vec<f32> {
    return spike_rs::operations::filter::lowess(data[..].as_ref(), span);
}

#[pyfunction]
pub fn burst_detection(
    peak_train: Vec<usize>,
    sampling_frequency: f32,
    cutoff: f32,
) -> Option<(Vec<usize>, Vec<usize>, Vec<usize>)> {
    match spike_rs::analysis::spike_analysis::logisi::burst_detection(
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

#[pyclass]
pub struct PyChannel {
    pub channel: Channel,
}

#[pymethods]
impl PyChannel {
    pub fn __str__(&self) -> String {
        return format!(
            "well: {}, label: {}",
            self.channel.group, self.channel.label
        );
    }

    pub fn __eq__(&self, other: &Self) -> bool {
        self.group() == other.group() && self.label() == other.label()
    }

    pub fn __key__(&self) -> (usize, String) {
        (self.group(), self.label())
    }

    pub fn __hash__(&self) -> usize {
        let mut s = DefaultHasher::new();
        format!("{}-{}", self.group(), self.label()).hash(&mut s);
        s.finish() as usize
    }

    fn group(&self) -> usize {
        self.channel.group
    }

    fn label(&self) -> String {
        self.channel.label.clone()
    }
}

#[pyclass(unsendable)]
pub struct PyPhase {
    phase: Option<Phase>,
}

#[pymethods]
impl PyPhase {
    #[new]
    pub fn new(filename: &str) -> Self {
        PyPhase {
            phase: Some(Phase::open(filename).expect(&format!("Failed to open {filename}"))),
        }
    }

    pub fn close(&mut self) {
        if self.phase.is_some() {
            println!("Closing phase");
            self.phase.take();
        }
    }

    pub fn date(&self) -> Option<String> {
        match &self.phase {
            Some(phase) => unsafe {
                Some(
                    std::ffi::CStr::from_ptr(phase.phase.date.as_ptr())
                        .to_str()
                        .expect("Failed to read the phase date")
                        .to_owned(),
                )
            },
            None => None,
        }
    }

    pub fn datalen(&self) -> Option<usize> {
        match &self.phase {
            None => None,
            Some(phase) => Some(phase.datalen()),
        }
    }

    pub fn sampling_frequency(&self) -> Option<f32> {
        match &self.phase {
            None => None,
            Some(phase) => Some(phase.sampling_frequency()),
        }
    }

    pub fn channels(&self) -> Option<Vec<PyChannel>> {
        match &self.phase {
            None => None,
            Some(phase) => Some(
                phase
                    .channels()
                    .iter()
                    .map(|c| PyChannel { channel: c.clone() })
                    .collect(),
            ),
        }
    }

    #[pyo3(signature = (channel, start=None, end=None))]
    pub fn raw_data(
        &self,
        channel: &PyChannel,
        start: Option<usize>,
        end: Option<usize>,
    ) -> Option<Vec<f32>> {
        match &self.phase {
            None => None,
            Some(phase) => match phase.raw_data(&channel.channel, start, end) {
                Ok(res) => Some(res),
                Err(err) => {
                    println!("{err:?}");
                    None
                }
            },
        }
    }
    #[pyo3(signature = (channel, data, start=None))]
    pub fn set_raw_data(
        &mut self,
        channel: &PyChannel,
        data: Vec<f32>,
        start: Option<usize>,
    ) -> Option<bool> {
        match &mut self.phase {
            None => None,
            Some(phase) => match phase.set_raw_data(&channel.channel, start, data[..].as_ref()) {
                Ok(()) => Some(true),
                Err(err) => {
                    println!("{err:?}");
                    Some(false)
                }
            },
        }
    }

    pub fn n_digitals(&self) -> Option<usize> {
        match &self.phase {
            None => None,
            Some(phase) => Some(phase.n_digitals()),
        }
    }

    #[pyo3(signature = (index, start=None, end=None))]
    pub fn digital(
        &self,
        index: usize,
        start: Option<usize>,
        end: Option<usize>,
    ) -> Option<Vec<f32>> {
        match &self.phase {
            None => None,
            Some(phase) => match phase.digital(index, start, end) {
                Ok(ret) => Some(ret),
                Err(err) => {
                    println!("{err:?}");
                    None
                }
            },
        }
    }

    #[pyo3(signature = (index, data, start=None))]
    pub fn set_digital(
        &mut self,
        index: usize,
        data: Vec<f32>,
        start: Option<usize>,
    ) -> Option<bool> {
        match &mut self.phase {
            None => None,
            Some(phase) => match phase.set_digital(index, start, data[..].as_ref()) {
                Ok(()) => Some(true),
                Err(err) => {
                    println!("{err:?}");
                    Some(false)
                }
            },
        }
    }

    pub fn n_events(&self) -> Option<usize> {
        match &self.phase {
            None => None,
            Some(phase) => Some(phase.n_events()),
        }
    }

    #[pyo3(signature = (channel, start=None, end=None))]
    pub fn peak_train(
        &self,
        channel: &PyChannel,
        start: Option<usize>,
        end: Option<usize>,
    ) -> Option<(Vec<usize>, Vec<f32>)> {
        match &self.phase {
            None => None,
            Some(phase) => match phase.peak_train(&channel.channel, start, end) {
                Ok(ret) => Some(ret),
                Err(err) => {
                    println!("{err:?}");
                    None
                }
            },
        }
    }

    #[pyo3(signature = (channel, data))]
    pub fn set_peak_train(
        &mut self,
        channel: &PyChannel,
        data: (Vec<usize>, Vec<f32>),
    ) -> Option<bool> {
        match &mut self.phase {
            None => None,
            Some(phase) => match phase.set_peak_train(&channel.channel, data) {
                Ok(()) => Some(true),
                Err(err) => {
                    println!("{err:?}");
                    Some(false)
                }
            },
        }
    }
}

#[pymodule(name = "pycode")]
fn pycode_rs_module(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_class::<PyChannel>()?;
    m.add_class::<PyPhase>()?;
    m.add_function(wrap_pyfunction!(py_init, m)?)?;
    m.add_function(wrap_pyfunction!(py_close, m)?)?;
    m.add_function(wrap_pyfunction!(compute_threshold, m)?)?;
    m.add_function(wrap_pyfunction!(spike_detection, m)?)?;
    m.add_function(wrap_pyfunction!(spike_detection_new, m)?)?;
    m.add_function(wrap_pyfunction!(get_digital_intervals, m)?)?;
    m.add_function(wrap_pyfunction!(count_peaks_in_intervals, m)?)?;
    m.add_function(wrap_pyfunction!(subsample_range, m)?)?;
    m.add_function(wrap_pyfunction!(find_peaks_around_points, m)?)?;
    m.add_function(wrap_pyfunction!(high_pass_filter, m)?)?;
    m.add_function(wrap_pyfunction!(logspace, m)?)?;
    m.add_function(wrap_pyfunction!(lowess, m)?)?;
    m.add_function(wrap_pyfunction!(burst_detection, m)?)?;
    Ok(())
}

#[cfg(test)]
mod tests;
