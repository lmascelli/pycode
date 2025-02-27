use super::channel::Channel;
use super::channel::PyChannel;
use super::error::Error;
use super::peak_train::PeakTrain;
use super::sys;
use pyo3::prelude::*;
use spike_rs::error::SpikeError;
use spike_rs::types::PhaseTrait;
use std::ffi::CString;

pub struct Phase {
    phase: sys::PhaseH5,
    pub filename: String,
    pub channels: Vec<Channel>,
}

macro_rules! phase_ptr {
    ($p:ident) => {
        &$p.phase as *const sys::PhaseH5 as *mut sys::PhaseH5
    };
}

impl Drop for Phase {
    fn drop(&mut self) {
        unsafe {
            sys::phase_close(phase_ptr!(self));
        };
    }
}

impl std::fmt::Debug for Phase {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        writeln!(f, "{{")?;
        writeln!(f, "  file: {},", self.filename)?;
        writeln!(f, "  datalen: {},", self.datalen())?;
        writeln!(f, "  sampling frequency: {},", self.sampling_frequency())?;
        writeln!(f, "  channels:")?;
        for channel in &self.channels {
            writeln!(f, "    ({}, {})", channel.group, channel.label)?;
        }
        writeln!(f, "}}")?;
        Ok(())
    }
}

impl std::default::Default for Phase {
    fn default() -> Self {
        Self {
            filename: String::new(),
            channels: vec![],
            phase: sys::PhaseH5 {
                fid: 0,
                date: [0; sys::DATE_STRING_LEN as usize],
                datalen: 0,
                sampling_frequency: 0f32,
                raw_data: sys::AnalogStream {
                    label: [0; sys::ANALOG_LABEL_STRING_LEN as usize],
                    n_channels: 0,
                    channel_data_dataset: 0,
                    datalen: 0,
                    info_channels: [sys::InfoChannel {
                        channel_id: 0,
                        row_index: 0,
                        group_id: 0,
                        electrode_group: 0,
                        label: std::ptr::null(),
                        raw_data_type: std::ptr::null(),
                        unit: std::ptr::null(),
                        exponent: 0,
                        ad_zero: 0,
                        tick: 0,
                        conversion_factor: 0,
                        adc_bits: 0,
                        high_pass_filter_type: std::ptr::null(),
                        high_pass_filter_cutoff: std::ptr::null(),
                        high_pass_filter_order: 0,
                        low_pass_filter_type: std::ptr::null(),
                        low_pass_filter_cutoff: std::ptr::null(),
                        low_pass_filter_order: 0,
                    }; sys::MAX_CHANNELS as usize],
                },
                has_digital: false,
                digital: sys::AnalogStream {
                    label: [0; sys::ANALOG_LABEL_STRING_LEN as usize],
                    n_channels: 0,
                    channel_data_dataset: 0,
                    datalen: 0,
                    info_channels: [sys::InfoChannel {
                        channel_id: 0,
                        row_index: 0,
                        group_id: 0,
                        electrode_group: 0,
                        label: std::ptr::null(),
                        raw_data_type: std::ptr::null(),
                        unit: std::ptr::null(),
                        exponent: 0,
                        ad_zero: 0,
                        tick: 0,
                        conversion_factor: 0,
                        adc_bits: 0,
                        high_pass_filter_type: std::ptr::null(),
                        high_pass_filter_cutoff: std::ptr::null(),
                        high_pass_filter_order: 0,
                        low_pass_filter_type: std::ptr::null(),
                        low_pass_filter_cutoff: std::ptr::null(),
                        low_pass_filter_order: 0,
                    }; sys::MAX_CHANNELS as usize],
                },
                n_events: 0,
                event_entities: [0; sys::MAX_EVENT_STREAMS as usize],
                peaks_group: 0,
            },
        }
    }
}

impl Phase {
    pub fn open(filename: &str) -> Result<Self, Error> {
        let mut phase = Self::default();
        phase.filename = filename.to_string();
        let cfilename = CString::new(filename).unwrap();

        let res = unsafe { sys::phase_open(phase_ptr!(phase), cfilename.as_ptr()) };

        match Error::from_phaseh5_error(res) {
            Ok(()) => {
                for i in 0..phase.phase.raw_data.n_channels as usize {
                    phase
                        .channels
                        .push(Channel::from(&phase.phase.raw_data.info_channels[i]));
                }
                Ok(phase)
            }
            Err(err) => {
                eprintln!("{err:?}");
                Err(err)
            }
        }
    }

    pub fn events_len(&self, index: usize) -> usize {
        let mut dims = 0u64;
        unsafe {
            sys::events_len(phase_ptr!(self), index, &mut dims as *mut _);
        }
        dims as usize
    }

    pub fn peak_train_len(&self, channel: &Channel) -> usize {
        let label_c =
            CString::new(channel.label.clone()).expect("peak_train_len: Failed to convert the CStr");
        let mut len = 0usize;
        let res = unsafe {
            sys::peak_train_len(
                phase_ptr!(self),
                channel.group,
                label_c.as_ptr(),
                &mut len as *mut _,
            )
        };

        match Error::from_phaseh5_error(res) {
            Ok(()) => len,
            Err(Error::PeakTrainNoPeakGroup) => 0,
            Err(err) => {
                panic!("peak_train_len: {err:?}");
            }
        }
    }
}

impl PhaseTrait<Channel> for Phase {
    fn sampling_frequency(&self) -> f32 {
        return self.phase.sampling_frequency;
    }

    fn datalen(&self) -> usize {
        return self.phase.datalen;
    }

    fn channels(&self) -> Vec<Channel> {
        self.channels.iter().cloned().collect()
    }

    fn raw_data(
        &self,
        channel: &Channel,
        start: Option<usize>,
        end: Option<usize>,
    ) -> Result<Vec<f32>, SpikeError> {
        let actual_start = match start {
            Some(val) => val,
            None => 0,
        };
        let actual_end = match end {
            Some(val) => val,
            None => self.datalen(),
        };

        if actual_start >= actual_end {
            return Err(SpikeError::RawDataStartIsAfterEnd);
        }

        if actual_end > self.datalen() {
            return Err(SpikeError::RawDataOutOfBounds);
        }

        let index = match self
            .channels
            .iter()
            .filter(|c| c.label == channel.label && c.group == channel.group)
            .next()
        {
            Some(c) => c.index,
            None => {
                return Err(SpikeError::RawDataLabelNotFound);
            }
        };

        let mut ret = vec![0; actual_end - actual_start];

        let res = Error::from_phaseh5_error(unsafe {
            sys::raw_data(
                phase_ptr!(self),
                index,
                actual_start,
                actual_end,
                ret.as_mut_ptr().cast(),
            )
        });

        match res {
            Ok(()) => {
                let conversion_factor = self.phase.raw_data.info_channels[index].conversion_factor
                    as f32
                    * f32::powf(
                        10f32,
                        self.phase.raw_data.info_channels[index].exponent as f32,
                    );
                let offset = self.phase.raw_data.info_channels[index].ad_zero;

                Ok(ret
                    .iter()
                    .map(|x| (*x - offset) as f32 * conversion_factor)
                    .collect())
            }
            Err(err) => Err(err.into()),
        }
    }

    fn set_raw_data(
        &mut self,
        channel: &Channel,
        start: Option<usize>,
        data: &[f32],
    ) -> Result<(), SpikeError> {
        let actual_start = match start {
            Some(val) => val,
            None => 0,
        };

        let actual_end = actual_start + data.len();

        if actual_end >= self.datalen() {
            return Err(SpikeError::SetRawDataOutOfBounds);
        }

        let index = match self
            .channels
            .iter()
            .filter(|c| c.group == channel.group && c.label == channel.label)
            .next()
        {
            Some(c) => c.index,
            None => {
                return Err(SpikeError::SetRawDataLabelNotFound);
            }
        };

        let conversion_factor = self.phase.raw_data.info_channels[index].conversion_factor as f32
            * f32::powf(
                10f32,
                self.phase.raw_data.info_channels[index].exponent as f32,
            );
        let offset = self.phase.raw_data.info_channels[index].ad_zero;
        let buf: Vec<i32> = data
            .iter()
            .map(|x| (*x / conversion_factor) as i32 + offset)
            .collect();

        let res = unsafe {
            sys::set_raw_data(
                phase_ptr!(self),
                index,
                actual_start,
                actual_end,
                buf.as_ptr(),
            )
        };

        Ok(Error::from_phaseh5_error(res)?)
    }

    fn n_digitals(&self) -> usize {
        if self.phase.has_digital {
            1
        } else {
            0
        }
    }

    fn digital(
        &self,
        index: usize,
        start: Option<usize>,
        end: Option<usize>,
    ) -> Result<Vec<f32>, SpikeError> {
        if index > 0 {
            panic!("digital: no more than one index can be processed atm");
        }

        if self.phase.has_digital == false {
            return Err(SpikeError::DigitalNoDigitalPresent);
        }

        let actual_start = match start {
            Some(val) => val,
            None => 0,
        };

        let actual_end = match end {
            Some(val) => val,
            None => self.datalen() - 1,
        };

        if actual_start >= actual_end {
            return Err(SpikeError::DigitalStartIsAfterEnd);
        }

        let mut buf = vec![0f32; actual_end - actual_start];

        let res = unsafe {
            sys::digital(
                phase_ptr!(self),
                actual_start,
                actual_end,
                buf.as_mut_ptr().cast(),
            )
        };

        match Error::from_phaseh5_error(res) {
            Ok(()) => Ok(buf),
            Err(err) => Err(err.into()),
        }
    }

    fn set_digital(
        &mut self,
        index: usize,
        start: Option<usize>,
        data: &[f32],
    ) -> Result<(), SpikeError> {
        if index > 0 {
            panic!("set_digital: no more than one index can be processed atm");
        }

        if self.phase.has_digital == false {
            panic!("set_digital: no digital present");
        }

        let actual_start = match start {
            Some(val) => val,
            None => 0,
        };
        let actual_end = actual_start + data.len();

        if actual_end >= self.datalen() {
            panic!("set_digital: [end] is greater than [datalen]");
        }

        if actual_start >= actual_end {
            panic!("set_digital: [start] is not before [end]");
        }

        let res = unsafe {
            sys::set_digital(
                phase_ptr!(self),
                actual_start,
                actual_end,
                data.as_ptr().cast(),
            )
        };

        match Error::from_phaseh5_error(res) {
            Ok(()) => Ok(()),
            Err(err) => Err(err.into()),
        }
    }

    fn n_events(&self) -> usize {
        self.phase.n_events as usize
    }

    fn events(&self, index: usize) -> Result<Vec<i64>, SpikeError> {
        let len = self.events_len(index);
        let mut data = vec![0i64; len];

        let res = unsafe { sys::events(phase_ptr!(self), index, data.as_mut_ptr()) };

        match Error::from_phaseh5_error(res) {
            Ok(()) => Ok(data),
            Err(err) => Err(err.into()),
        }
    }

    fn peak_train(
        &self,
        channel: &Channel,
        start: Option<usize>,
        end: Option<usize>,
    ) -> Result<(Vec<usize>, Vec<f32>), SpikeError> {
        let label_c = CString::new(channel.label.clone())
            .expect("peak_train_len: Failed to convert the CStr");
        let peak_train_len = self.peak_train_len(channel);
        if peak_train_len == 0 {
            return Ok((vec![], vec![]));
        }
        let mut peak_train = PeakTrain::new(peak_train_len);
        let mut peak_train_c = peak_train.as_c_repr();
        let res = unsafe {
            sys::peak_train(
                phase_ptr!(self),
                channel.group,
                label_c.as_ptr(),
                peak_train_ptr!(peak_train_c),
            )
        };

        match Error::from_phaseh5_error(res) {
            Ok(()) => {
                let (samples, values) = (peak_train.samples, peak_train.values);
                if start.is_none() && end.is_none() {
                    return Ok((samples, values));
                } else {
                    let start = start.unwrap_or(samples[0]);
                    let end = end.unwrap_or(samples[samples.len() - 1]);
                    let mut i_start = 0;
                    let mut i_end = samples.len() - 1;

                    for (i, val) in samples.iter().enumerate() {
                        if *val >= start {
                            i_start = i;
                            break;
                        }
                    }
                    for (i, val) in samples.iter().enumerate() {
                        if *val >= end {
                            i_end = i;
                            break;
                        }
                    }
                    Ok((
                        samples[i_start..i_end].iter().map(|x| *x).collect(),
                        values[i_start..i_end].iter().map(|x| *x).collect(),
                    ))
                }
            }
            Err(err) => Err(err.into()),
        }
    }

    fn set_peak_train(
        &mut self,
        channel: &Channel,
        data: (Vec<usize>, Vec<f32>),
    ) -> Result<(), SpikeError> {
        let label_c = CString::new(channel.label.clone())
            .expect("peak_train_len: Failed to convert the CStr");
        // there is no group yet. Create a peak_train and set the data to it

        let mut peak_train = match PeakTrain::from(data.0, data.1) {
            Ok(peak_train) => peak_train,
            Err(err) => {
                return Err(err.into());
            }
        };

        let mut peak_train_c = peak_train.as_c_repr();

        let res = unsafe {
            sys::set_peak_train(
                phase_ptr!(self),
                channel.group,
                label_c.as_ptr(),
                peak_train_ptr!(peak_train_c),
            )
        };
        match Error::from_phaseh5_error(res) {
            Ok(()) => Ok(()),
            Err(err) => Err(err.into()),
        }
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
