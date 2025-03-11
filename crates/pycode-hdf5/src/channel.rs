use super::sys::InfoChannel;
use spike_rs::types::ChannelTrait;
use std::ffi::CStr;

#[derive(Clone)]
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
