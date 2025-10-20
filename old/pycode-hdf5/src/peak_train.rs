use super::error::Error;
use super::sys;

pub struct PeakTrain {
    pub samples: Vec<usize>,
    pub values: Vec<f32>,
}

impl PeakTrain {
    pub fn new(len: usize) -> Self {
        PeakTrain {
            samples: vec![0; len],
            values: vec![0f32; len],
        }
    }

    pub fn from(samples: Vec<usize>, values: Vec<f32>) -> Result<Self, Error> {
        if samples.len() != values.len() {
            Err(Error::CreatePeakTrainSamplesValuesDifferentLen)
        } else {
            Ok(Self { samples, values })
        }
    }

    pub fn as_c_repr(&mut self) -> sys::PeakTrain {
        sys::PeakTrain {
            n_peaks: self.samples.len(),
            samples: self.samples.as_mut_ptr().cast(),
            values: self.values.as_mut_ptr(),
        }
    }
}

macro_rules! peak_train_ptr {
    ($p:ident) => {
        &mut$p as *mut sys::PeakTrain
    };
}
