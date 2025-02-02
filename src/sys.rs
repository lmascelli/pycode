#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(dead_code)]

pub type hsize_t = u64;
pub type hid_t = i64;

pub const DATE_STRING_LEN: u32 = 32;
pub const ANALOG_LABEL_STRING_LEN: u32 = 64;
pub const CHANNEL_LABEL_STRING_LEN: u32 = 32;
pub const MAX_EVENT_STREAMS: u32 = 16;
pub const MAX_CHANNELS: u32 = 60;
pub const MAX_ANALOG_STREAMS: u32 = 4;
pub const MAX_GROUP_STRING_LEN: u32 = 256;

pub const phaseh5_error_OK: phaseh5_error = 0;
pub const phaseh5_error_INIT_CREATE_STRING_TYPE_FAIL: phaseh5_error = 1;
pub const phaseh5_error_OPEN_FAIL: phaseh5_error = 2;
pub const phaseh5_error_CLOSE_FILE_FAIL: phaseh5_error = 3;
pub const phaseh5_error_OPEN_DATA_GROUP_FAIL: phaseh5_error = 4;
pub const phaseh5_error_OPEN_DATE_ATTRIBUTE_FAIL: phaseh5_error = 5;
pub const phaseh5_error_READ_DATE_ATTRIBUTE_FAIL: phaseh5_error = 6;
pub const phaseh5_error_OPEN_DATE_DATATYPE_FAIL: phaseh5_error = 7;
pub const phaseh5_error_OPEN_ALLOCATE_ANALOGS_FAIL: phaseh5_error = 8;
pub const phaseh5_error_OPEN_ANALOG_GROUP_FAIL: phaseh5_error = 9;
pub const phaseh5_error_OPEN_INFO_CHANNEL_DATASET_FAIL: phaseh5_error = 10;
pub const phaseh5_error_OPEN_INFO_CHANNEL_DATASPACE_FAIL: phaseh5_error = 11;
pub const phaseh5_error_OPEN_INFO_CHANNEL_DATATYPE_FAIL: phaseh5_error = 12;
pub const phaseh5_error_OPEN_ANALOG_DATASET_FAIL: phaseh5_error = 13;
pub const phaseh5_error_OPEN_LABEL_ATTRIBUTE_FAIL: phaseh5_error = 14;
pub const phaseh5_error_READ_LABEL_ATTRIBUTE_FAIL: phaseh5_error = 15;
pub const phaseh5_error_OPEN_LABEL_DATATYPE_FAIL: phaseh5_error = 16;
pub const phaseh5_error_READ_INFO_CHANNELS_FAIL: phaseh5_error = 17;
pub const phaseh5_error_PARSE_ANALOG_STREAM_DIFFERENT_TICK: phaseh5_error = 18;
pub const phaseh5_error_MULTIPLE_DIGITAL_STREAMS: phaseh5_error = 19;
pub const phaseh5_error_MULTIPLE_RAW_DATA_STREAMS: phaseh5_error = 20;
pub const phaseh5_error_MULTIPLE_SAMPLING_FREQUENCIES: phaseh5_error = 21;
pub const phaseh5_error_MULTIPLE_DATALENS: phaseh5_error = 22;
pub const phaseh5_error_OPEN_CHANNEL_DATA_FAIL: phaseh5_error = 23;
pub const phaseh5_error_OPEN_CHANNEL_DATA_DATASPACE_FAIL: phaseh5_error = 24;
pub const phaseh5_error_GET_CHANNEL_DATA_DIMS_FAIL: phaseh5_error = 25;
pub const phaseh5_error_NO_RAW_DATA_STREAM: phaseh5_error = 26;
pub const phaseh5_error_OPEN_EVENT_STREAM_GROUP_LINK_FAIL: phaseh5_error = 27;
pub const phaseh5_error_OPEN_EVENT_STREAM_GROUP_FAIL: phaseh5_error = 28;
pub const phaseh5_error_OPEN_EVENT_STREAM_STREAM_0_GROUP_LINK_FAIL: phaseh5_error = 29;
pub const phaseh5_error_MAX_EVENT_STREAMS_EXCEEDED: phaseh5_error = 30;
pub const phaseh5_error_OPEN_ENTITY_DATASET_FAIL: phaseh5_error = 31;
pub const phaseh5_error_EVENT_ENTITY_DATASET_CLOSE_FAIL: phaseh5_error = 32;
pub const phaseh5_error_OPEN_PEAK_TRAIN_GROUP_FAIL: phaseh5_error = 33;
pub const phaseh5_error_CREATE_PEAK_GROUP_FAIL: phaseh5_error = 34;
pub const phaseh5_error_RAW_DATA_END_BEFORE_START: phaseh5_error = 35;
pub const phaseh5_error_RAW_DATA_END_OUT_OF_BOUNDS: phaseh5_error = 36;
pub const phaseh5_error_RAW_DATA_GET_DATASPACE_FAIL: phaseh5_error = 37;
pub const phaseh5_error_RAW_DATA_SELECT_HYPERSLAB_FAIL: phaseh5_error = 38;
pub const phaseh5_error_RAW_DATA_CREATE_MEMORY_DATASPACE_FAIL: phaseh5_error = 39;
pub const phaseh5_error_RAW_DATA_READ_DATA_FAIL: phaseh5_error = 40;
pub const phaseh5_error_SET_RAW_DATA_END_BEFORE_START: phaseh5_error = 41;
pub const phaseh5_error_SET_RAW_DATA_END_OUT_OF_BOUNDS: phaseh5_error = 42;
pub const phaseh5_error_SET_RAW_DATA_GET_DATASPACE_FAIL: phaseh5_error = 43;
pub const phaseh5_error_SET_RAW_DATA_SELECT_HYPERSLAB_FAIL: phaseh5_error = 44;
pub const phaseh5_error_SET_RAW_DATA_CREATE_MEMORY_DATASPACE_FAIL: phaseh5_error = 45;
pub const phaseh5_error_SET_RAW_DATA_WRITE_DATASET_FAIL: phaseh5_error = 46;
pub const phaseh5_error_DIGITAL_NO_DIGITAL: phaseh5_error = 47;
pub const phaseh5_error_DIGITAL_END_BEFORE_START: phaseh5_error = 48;
pub const phaseh5_error_DIGITAL_END_OUT_OF_BOUNDS: phaseh5_error = 49;
pub const phaseh5_error_DIGITAL_GET_DATASPACE_FAIL: phaseh5_error = 50;
pub const phaseh5_error_DIGITAL_SELECT_HYPERSLAB_FAIL: phaseh5_error = 51;
pub const phaseh5_error_DIGITAL_CREATE_MEMORY_DATASPACE_FAIL: phaseh5_error = 52;
pub const phaseh5_error_DIGITAL_READ_DATA_FAIL: phaseh5_error = 53;
pub const phaseh5_error_SET_DIGITAL_NO_DIGITAL: phaseh5_error = 54;
pub const phaseh5_error_SET_DIGITAL_END_BEFORE_START: phaseh5_error = 55;
pub const phaseh5_error_SET_DIGITAL_END_OUT_OF_BOUNDS: phaseh5_error = 56;
pub const phaseh5_error_SET_DIGITAL_GET_DATASPACE_FAIL: phaseh5_error = 57;
pub const phaseh5_error_SET_DIGITAL_SELECT_HYPERSLAB_FAIL: phaseh5_error = 58;
pub const phaseh5_error_SET_DIGITAL_CREATE_MEMORY_DATASPACE_FAIL: phaseh5_error = 59;
pub const phaseh5_error_SET_DIGITAL_WRITE_DATA_FAIL: phaseh5_error = 60;
pub const phaseh5_error_EVENTS_LEN_INDEX_OUT_OF_BOUNDS: phaseh5_error = 61;
pub const phaseh5_error_EVENTS_LEN_OPEN_EVENT_DATASPACE_FAIL: phaseh5_error = 62;
pub const phaseh5_error_EVENTS_INDEX_OUT_OF_BOUNDS: phaseh5_error = 63;
pub const phaseh5_error_EVENTS_LEN_GET_DIMS_FAIL: phaseh5_error = 64;
pub const phaseh5_error_EVENTS_GET_EVENTS_DATASPACE_FAIL: phaseh5_error = 65;
pub const phaseh5_error_EVENTS_SELECT_DATASPACE_HYPERSLAB_FAIL: phaseh5_error = 66;
pub const phaseh5_error_EVENTS_CREATE_MEMORY_DATASPACE_FAIL: phaseh5_error = 67;
pub const phaseh5_error_EVENTS_READ_DATASET_FAIL: phaseh5_error = 68;
pub const phaseh5_error_PEAK_TRAIN_NO_PEAK_GROUP: phaseh5_error = 69;
pub const phaseh5_error_PEAK_TRAIN_GROUP_LINK_FAIL: phaseh5_error = 70;
pub const phaseh5_error_PEAK_TRAIN_VALUES_DATASET_LINK_FAIL: phaseh5_error = 71;
pub const phaseh5_error_PEAK_TRAIN_NO_VALUES_DATASET: phaseh5_error = 72;
pub const phaseh5_error_PEAK_TRAIN_SAMPLES_DATASET_LINK_FAIL: phaseh5_error = 73;
pub const phaseh5_error_PEAK_TRAIN_NO_SAMPLES_DATASET: phaseh5_error = 74;
pub const phaseh5_error_PEAK_TRAIN_OPEN_VALUES_DATASET_FAIL: phaseh5_error = 75;
pub const phaseh5_error_PEAK_TRAIN_OPEN_SAMPLES_DATASET_FAIL: phaseh5_error = 76;
pub const phaseh5_error_DELETE_PEAK_TRAIN_VALUES_DATASET_LINK_FAIL: phaseh5_error = 77;
pub const phaseh5_error_DELETE_PEAK_TRAIN_NO_VALUES_DATASET: phaseh5_error = 78;
pub const phaseh5_error_DELETE_PEAK_TRAIN_SAMPLES_DATASET_LINK_FAIL: phaseh5_error = 79;
pub const phaseh5_error_DELETE_PEAK_TRAIN_NO_SAMPLES_DATASET: phaseh5_error = 80;
pub const phaseh5_error_DELETE_PEAK_TRAIN_VALUES_DATASET_FAIL: phaseh5_error = 81;
pub const phaseh5_error_DELETE_PEAK_TRAIN_SAMPLES_DATASET_FAIL: phaseh5_error = 82;
pub const phaseh5_error_PEAK_TRAIN_LEN_OPEN_VALUES_DATASPACE_FAIL: phaseh5_error = 83;
pub const phaseh5_error_PEAK_TRAIN_LEN_GET_VALUES_DATASPACE_DIM_FAIL: phaseh5_error = 84;
pub const phaseh5_error_PEAK_TRAIN_CLOSE_MEMORY_DATASPACE_FAIL: phaseh5_error = 85;
pub const phaseh5_error_PEAK_TRAIN_LEN_CLOSE_VALUES_DATASPACE_FAIL: phaseh5_error = 86;
pub const phaseh5_error_PEAK_TRAIN_LEN_CLOSE_VALUES_DATASET_FAIL: phaseh5_error = 87;
pub const phaseh5_error_PEAK_TRAIN_LEN_CLOSE_SAMPLES_DATASPACE_FAIL: phaseh5_error = 88;
pub const phaseh5_error_PEAK_TRAIN_LEN_CLOSE_SAMPLES_DATASET_FAIL: phaseh5_error = 89;
pub const phaseh5_error_PEAK_TRAIN_LEN_OPEN_SAMPLES_DATASPACE_FAIL: phaseh5_error = 90;
pub const phaseh5_error_PEAK_TRAIN_LEN_GET_SAMPLES_DATASPACE_DIM_FAIL: phaseh5_error = 91;
pub const phaseh5_error_PEAK_TRAIN_LEN_VALUES_SAMPLES_DIFFERENT: phaseh5_error = 92;
pub const phaseh5_error_PEAK_TRAIN_CREATE_MEMORY_DATASPACE_FAIL: phaseh5_error = 93;
pub const phaseh5_error_PEAK_TRAIN_READ_VALUES_DATASET_FAIL: phaseh5_error = 94;
pub const phaseh5_error_PEAK_TRAIN_READ_SAMPLES_DATASET_FAIL: phaseh5_error = 95;
pub const phaseh5_error_PEAK_TRAIN_CLOSE_VALUES_DATASET_FAIL: phaseh5_error = 96;
pub const phaseh5_error_PEAK_TRAIN_CLOSE_SAMPLES_DATASET_FAIL: phaseh5_error = 97;
pub const phaseh5_error_SET_PEAK_TRAIN_CHECK_LABEL_GROUP_FAIL: phaseh5_error = 98;
pub const phaseh5_error_SET_PEAK_TRAIN_CREATE_GROUP_FAIL: phaseh5_error = 99;
pub const phaseh5_error_SET_PEAK_TRAIN_CLOSE_DELETED_VALUES_DATASET_FAIL: phaseh5_error = 100;
pub const phaseh5_error_SET_PEAK_TRAIN_CLOSE_DELETED_SAMPLES_DATASET_FAIL: phaseh5_error = 101;
pub const phaseh5_error_SET_PEAK_TRAIN_CLOSE_SAMPLES_FILE_DATASPACE_FAIL: phaseh5_error = 102;
pub const phaseh5_error_SET_PEAK_TRAIN_CLOSE_VALUES_FILE_DATASPACE_FAIL: phaseh5_error = 103;
pub const phaseh5_error_SET_PEAK_TRAIN_CREATE_SAMPLES_MEMORY_DATASPACE_FAIL: phaseh5_error = 104;
pub const phaseh5_error_SET_PEAK_TRAIN_CREATE_VALUES_MEMORY_DATASPACE_FAIL: phaseh5_error = 105;
pub const phaseh5_error_SET_PEAK_TRAIN_CREATE_SAMPLES_FILE_DATASPACE_FAIL: phaseh5_error = 106;
pub const phaseh5_error_SET_PEAK_TRAIN_CREATE_VALUES_FILE_DATASPACE_FAIL: phaseh5_error = 107;
pub const phaseh5_error_SET_PEAK_TRAIN_CREATE_SAMPLES_FILE_DATASET_FAIL: phaseh5_error = 108;
pub const phaseh5_error_SET_PEAK_TRAIN_CREATE_VALUES_FILE_DATASET_FAIL: phaseh5_error = 109;
pub const phaseh5_error_SET_PEAK_TRAIN_WRITE_SAMPLES_DATASET_FAIL: phaseh5_error = 110;
pub const phaseh5_error_SET_PEAK_TRAIN_WRITE_VALUES_DATASET_FAIL: phaseh5_error = 111;
pub const phaseh5_error_SET_PEAK_TRAIN_CLOSE_SAMPLES_MEMORY_DATASPACE_FAIL: phaseh5_error = 112;
pub const phaseh5_error_SET_PEAK_TRAIN_CLOSE_VALUES_MEMORY_DATASPACE_FAIL: phaseh5_error = 113;
pub const phaseh5_error_SET_PEAK_TRAIN_CLOSE_SAMPLES_DATASET_FAIL: phaseh5_error = 114;
pub const phaseh5_error_SET_PEAK_TRAIN_CLOSE_VALUES_DATASET_FAIL: phaseh5_error = 115;
pub type phaseh5_error = ::std::os::raw::c_uint;

#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct InfoChannel {
    pub channel_id: ::std::os::raw::c_int,
    pub row_index: ::std::os::raw::c_int,
    pub group_id: ::std::os::raw::c_int,
    pub electrode_group: ::std::os::raw::c_int,
    pub label: *const ::std::os::raw::c_char,
    pub raw_data_type: *const ::std::os::raw::c_char,
    pub unit: *const ::std::os::raw::c_char,
    pub exponent: ::std::os::raw::c_int,
    pub ad_zero: ::std::os::raw::c_int,
    pub tick: ::std::os::raw::c_long,
    pub conversion_factor: ::std::os::raw::c_long,
    pub adc_bits: ::std::os::raw::c_int,
    pub high_pass_filter_type: *const ::std::os::raw::c_char,
    pub high_pass_filter_cutoff: *const ::std::os::raw::c_char,
    pub high_pass_filter_order: ::std::os::raw::c_int,
    pub low_pass_filter_type: *const ::std::os::raw::c_char,
    pub low_pass_filter_cutoff: *const ::std::os::raw::c_char,
    pub low_pass_filter_order: ::std::os::raw::c_int,
}


#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct AnalogStream {
    pub label: [::std::os::raw::c_char; 64usize],
    pub n_channels: hsize_t,
    pub channel_data_dataset: hid_t,
    pub datalen: usize,
    pub info_channels: [InfoChannel; 60usize],
}

#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct PeakTrain {
    pub n_peaks: usize,
    pub values: *mut f32,
    pub samples: *mut ::std::os::raw::c_long,
}


#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct PhaseH5 {
    pub fid: hid_t,
    pub date: [::std::os::raw::c_char; 32usize],
    pub datalen: usize,
    pub sampling_frequency: f32,
    pub raw_data: AnalogStream,
    pub has_digital: bool,
    pub digital: AnalogStream,
    pub n_events: ::std::os::raw::c_int,
    pub event_entities: [hid_t; 16usize],
    pub peaks_group: hid_t,
}

extern "C" {
    pub fn pycodeh5_init() -> phaseh5_error;
}
extern "C" {
    pub fn pycodeh5_close();
}
extern "C" {
    pub fn init_phase(phase: *mut PhaseH5);
}
extern "C" {
    pub fn phase_open(
        phase: *mut PhaseH5,
        filename: *const ::std::os::raw::c_char,
    ) -> phaseh5_error;
}
extern "C" {
    pub fn phase_close(phase: *mut PhaseH5) -> phaseh5_error;
}
extern "C" {
    pub fn raw_data(
        phase: *mut PhaseH5,
        index: usize,
        start: usize,
        end: usize,
        buf: *mut ::std::os::raw::c_int,
    ) -> phaseh5_error;
}
extern "C" {
    pub fn set_raw_data(
        phase: *mut PhaseH5,
        index: usize,
        start: usize,
        end: usize,
        buf: *const ::std::os::raw::c_int,
    ) -> phaseh5_error;
}
extern "C" {
    pub fn digital(
        phase: *mut PhaseH5,
        start: usize,
        end: usize,
        buf: *mut ::std::os::raw::c_int,
    ) -> phaseh5_error;
}
extern "C" {
    pub fn set_digital(
        phase: *mut PhaseH5,
        start: usize,
        end: usize,
        buf: *const ::std::os::raw::c_int,
    ) -> phaseh5_error;
}
extern "C" {
    pub fn events_len(phase: *mut PhaseH5, index: usize, len: *mut hsize_t) -> phaseh5_error;
}
extern "C" {
    pub fn events(
        phase: *mut PhaseH5,
        index: usize,
        buf: *mut ::std::os::raw::c_long,
    ) -> phaseh5_error;
}
extern "C" {
    pub fn peak_train_len(
        arg1: *mut PhaseH5,
        label: *const ::std::os::raw::c_char,
        len: *mut usize,
    ) -> phaseh5_error;
}
extern "C" {
    pub fn peak_train(
        phase: *mut PhaseH5,
        label: *const ::std::os::raw::c_char,
        peak_train: *mut PeakTrain,
    ) -> phaseh5_error;
}
extern "C" {
    pub fn set_peak_train(
        phase: *mut PhaseH5,
        label: *const ::std::os::raw::c_char,
        peak_train: *const PeakTrain,
    ) -> phaseh5_error;
}
