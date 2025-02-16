use super::sys::InfoChannel;
use spike_rs::types::ChannelTrait;
use std::ffi::CStr;
use pyo3::*;

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
    #[new]
    pub fn new() -> Self {
        Self {
            channel: Channel {
                group: 0,
                label: "E-00155 21".to_string(),
                index: 0,
            }
        }
    }

    pub fn __str__(&self) -> String {
        return format!("wheel: {}, label: {}", self.channel.group, self.channel.label);
    }

    fn group(&self) -> usize {
        self.channel.group
    }

    fn label(&self) -> String {
        self.channel.label.clone()
    }
}
