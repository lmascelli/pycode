use super::sys::InfoChannel;
use pyo3::*;
use spike_rs::types::ChannelTrait;
use std::ffi::CStr;
use std::hash::{DefaultHasher, Hash, Hasher};

#[derive(Clone)]
#[pyclass]
pub struct Channel {
    pub group: usize,
    pub label: String,
    pub index: usize,
}

impl Channel {
    pub fn from(info_channel: &InfoChannel) -> Self {
        Self {
            group: info_channel.group_id as usize,
            label: unsafe {
                CStr::from_ptr(info_channel.label)
                    .to_str()
                    .expect("Failed to convert CStr of label")
                    .to_string()
            },
            index: info_channel.row_index as usize,
        }
    }
}

impl ChannelTrait for Channel {
    fn group(&self) -> usize {
        self.group
    }

    fn label(&self) -> String {
        self.label.clone()
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
