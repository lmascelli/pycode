#include <stddef.h>
#ifdef _WIN32
#define H5_BUILT_AS_DYNAMIC_LIB
#endif

#include <hdf5.h>
#define DATE_STRING_LEN 32
#define ANALOG_LABEL_STRING_LEN 64
#define CHANNEL_LABEL_STRING_LEN 32
#define MAX_EVENT_STREAMS 32
#define MAX_CHANNELS 512
#define MAX_ANALOG_STREAMS 4
#define MAX_GROUP_STRING_LEN 256

#ifdef _WIN32
#define LLONG_TYPE long long int
#elif __linux__
#define LLONG_TYPE long int
#elif __APPLE__
#define LLONG_TYPE long int
#endif

typedef enum phaseh5_error {
    OK = 0,

    // pycodeh5_init

    INIT_CREATE_STRING_TYPE_FAIL,

    OPEN_FAIL,
    CLOSE_FILE_FAIL,
    FLUSH_FAIL,
    OPEN_DATA_GROUP_FAIL,
    OPEN_DATE_ATTRIBUTE_FAIL,
    READ_DATE_ATTRIBUTE_FAIL,
    OPEN_DATE_DATATYPE_FAIL,
    OPEN_ALLOCATE_ANALOGS_FAIL,
    OPEN_ANALOG_GROUP_FAIL,
    OPEN_INFO_CHANNEL_DATASET_FAIL,
    OPEN_INFO_CHANNEL_DATASPACE_FAIL,
    OPEN_INFO_CHANNEL_DATATYPE_FAIL,
    OPEN_ANALOG_DATASET_FAIL,
    OPEN_LABEL_ATTRIBUTE_FAIL,
    READ_LABEL_ATTRIBUTE_FAIL,
    OPEN_LABEL_DATATYPE_FAIL,
    READ_INFO_CHANNELS_FAIL,
    PARSE_ANALOG_STREAM_DIFFERENT_TICK,
    MULTIPLE_DIGITAL_STREAMS,
    MULTIPLE_RAW_DATA_STREAMS,
    MULTIPLE_SAMPLING_FREQUENCIES,
    MULTIPLE_DATALENS,
    OPEN_CHANNEL_DATA_FAIL,
    OPEN_CHANNEL_DATA_DATASPACE_FAIL,
    GET_CHANNEL_DATA_DIMS_FAIL,
    NO_RAW_DATA_STREAM,
    OPEN_EVENT_STREAM_GROUP_LINK_FAIL,
    OPEN_EVENT_STREAM_GROUP_FAIL,
    OPEN_EVENT_STREAM_STREAM_0_GROUP_LINK_FAIL,
    MAX_EVENT_STREAMS_EXCEEDED,
    OPEN_ENTITY_DATASET_FAIL,
    EVENT_ENTITY_DATASET_CLOSE_FAIL,
    OPEN_PEAK_TRAIN_GROUP_FAIL,
    CREATE_PEAK_GROUP_FAIL,
    RAW_DATA_END_BEFORE_START,
    RAW_DATA_END_OUT_OF_BOUNDS,
    RAW_DATA_GET_DATASPACE_FAIL,
    RAW_DATA_SELECT_HYPERSLAB_FAIL,
    RAW_DATA_CREATE_MEMORY_DATASPACE_FAIL,
    RAW_DATA_READ_DATA_FAIL,
    SET_RAW_DATA_END_BEFORE_START,
    SET_RAW_DATA_END_OUT_OF_BOUNDS,
    SET_RAW_DATA_GET_DATASPACE_FAIL,
    SET_RAW_DATA_SELECT_HYPERSLAB_FAIL,
    SET_RAW_DATA_CREATE_MEMORY_DATASPACE_FAIL,
    SET_RAW_DATA_WRITE_DATASET_FAIL,
    DIGITAL_NO_DIGITAL,
    DIGITAL_END_BEFORE_START,
    DIGITAL_END_OUT_OF_BOUNDS,
    DIGITAL_GET_DATASPACE_FAIL,
    DIGITAL_SELECT_HYPERSLAB_FAIL,
    DIGITAL_CREATE_MEMORY_DATASPACE_FAIL,
    DIGITAL_READ_DATA_FAIL,
    SET_DIGITAL_NO_DIGITAL,
    SET_DIGITAL_END_BEFORE_START,
    SET_DIGITAL_END_OUT_OF_BOUNDS,
    SET_DIGITAL_GET_DATASPACE_FAIL,
    SET_DIGITAL_SELECT_HYPERSLAB_FAIL,
    SET_DIGITAL_CREATE_MEMORY_DATASPACE_FAIL,
    SET_DIGITAL_WRITE_DATA_FAIL,
    EVENTS_LEN_INDEX_OUT_OF_BOUNDS,
    EVENTS_LEN_OPEN_EVENT_DATASPACE_FAIL,
    EVENTS_INDEX_OUT_OF_BOUNDS,
    EVENTS_LEN_GET_DIMS_FAIL,
    EVENTS_GET_EVENTS_DATASPACE_FAIL,
    EVENTS_SELECT_DATASPACE_HYPERSLAB_FAIL,
    EVENTS_CREATE_MEMORY_DATASPACE_FAIL,
    EVENTS_READ_DATASET_FAIL,
    PEAK_TRAIN_NO_PEAK_GROUP,
    PEAK_TRAIN_GROUP_LINK_FAIL,
    PEAK_TRAIN_VALUES_DATASET_LINK_FAIL,
    PEAK_TRAIN_NO_VALUES_DATASET,
    PEAK_TRAIN_SAMPLES_DATASET_LINK_FAIL,
    PEAK_TRAIN_NO_SAMPLES_DATASET,
    PEAK_TRAIN_OPEN_VALUES_DATASET_FAIL,
    PEAK_TRAIN_OPEN_SAMPLES_DATASET_FAIL,
    DELETE_PEAK_TRAIN_VALUES_DATASET_LINK_FAIL,
    DELETE_PEAK_TRAIN_NO_VALUES_DATASET,
    DELETE_PEAK_TRAIN_SAMPLES_DATASET_LINK_FAIL,
    DELETE_PEAK_TRAIN_NO_SAMPLES_DATASET,
    DELETE_PEAK_TRAIN_VALUES_DATASET_FAIL,
    DELETE_PEAK_TRAIN_SAMPLES_DATASET_FAIL,
    PEAK_TRAIN_LEN_OPEN_VALUES_DATASPACE_FAIL,
    PEAK_TRAIN_LEN_GET_VALUES_DATASPACE_DIM_FAIL,
    PEAK_TRAIN_CLOSE_MEMORY_DATASPACE_FAIL,
    PEAK_TRAIN_LEN_CLOSE_VALUES_DATASPACE_FAIL,
    PEAK_TRAIN_LEN_CLOSE_VALUES_DATASET_FAIL,
    PEAK_TRAIN_LEN_CLOSE_SAMPLES_DATASPACE_FAIL,
    PEAK_TRAIN_LEN_CLOSE_SAMPLES_DATASET_FAIL,
    PEAK_TRAIN_LEN_OPEN_SAMPLES_DATASPACE_FAIL,
    PEAK_TRAIN_LEN_GET_SAMPLES_DATASPACE_DIM_FAIL,
    PEAK_TRAIN_LEN_VALUES_SAMPLES_DIFFERENT,
    PEAK_TRAIN_CREATE_MEMORY_DATASPACE_FAIL,
    PEAK_TRAIN_READ_VALUES_DATASET_FAIL,
    PEAK_TRAIN_READ_SAMPLES_DATASET_FAIL,
    PEAK_TRAIN_CLOSE_VALUES_DATASET_FAIL,
    PEAK_TRAIN_CLOSE_SAMPLES_DATASET_FAIL,
    SET_PEAK_TRAIN_CHECK_LABEL_GROUP_FAIL,
    SET_PEAK_TRAIN_CREATE_GROUP_FAIL,
    SET_PEAK_TRAIN_CLOSE_DELETED_VALUES_DATASET_FAIL,
    SET_PEAK_TRAIN_CLOSE_DELETED_SAMPLES_DATASET_FAIL,
    SET_PEAK_TRAIN_CLOSE_SAMPLES_FILE_DATASPACE_FAIL,
    SET_PEAK_TRAIN_CLOSE_VALUES_FILE_DATASPACE_FAIL,
    SET_PEAK_TRAIN_CREATE_SAMPLES_MEMORY_DATASPACE_FAIL,
    SET_PEAK_TRAIN_CREATE_VALUES_MEMORY_DATASPACE_FAIL,
    SET_PEAK_TRAIN_CREATE_SAMPLES_FILE_DATASPACE_FAIL,
    SET_PEAK_TRAIN_CREATE_VALUES_FILE_DATASPACE_FAIL,
    SET_PEAK_TRAIN_CREATE_SAMPLES_FILE_DATASET_FAIL,
    SET_PEAK_TRAIN_CREATE_VALUES_FILE_DATASET_FAIL,
    SET_PEAK_TRAIN_WRITE_SAMPLES_DATASET_FAIL,
    SET_PEAK_TRAIN_WRITE_VALUES_DATASET_FAIL,
    SET_PEAK_TRAIN_CLOSE_SAMPLES_MEMORY_DATASPACE_FAIL,
    SET_PEAK_TRAIN_CLOSE_VALUES_MEMORY_DATASPACE_FAIL,
    SET_PEAK_TRAIN_CLOSE_SAMPLES_DATASET_FAIL,
    SET_PEAK_TRAIN_CLOSE_VALUES_DATASET_FAIL,
} phaseh5_error;

typedef struct InfoChannel {
    int channel_id;
    int row_index;
    int group_id;
    int electrode_group;
    const char* label;
    const char* raw_data_type;
    const char* unit;
    int exponent;
    int ad_zero;
    LLONG_TYPE tick;
    LLONG_TYPE conversion_factor;
    int adc_bits;
    const char* high_pass_filter_type;
    const char* high_pass_filter_cutoff;
    int high_pass_filter_order;
    const char* low_pass_filter_type;
    const char* low_pass_filter_cutoff;
    int low_pass_filter_order;
} InfoChannel;

typedef struct AnalogStream {
    const char label[ANALOG_LABEL_STRING_LEN];
    hsize_t n_channels;
    // ChannelData dataset
    hid_t channel_data_dataset;
    size_t datalen;
    // InfoChannel data
    InfoChannel info_channels[MAX_CHANNELS];
} AnalogStream;

typedef struct PeakTrain {
    size_t n_peaks;
    float* values;
    long int* samples;
} PeakTrain;

typedef struct PhaseH5 {
    hid_t fid;
    char date[DATE_STRING_LEN];
    size_t datalen;
    float sampling_frequency;
    AnalogStream raw_data;
    bool has_digital;
    AnalogStream digital;
    int n_events;
    hid_t event_entities[MAX_EVENT_STREAMS];
    hid_t peaks_group;
} PhaseH5;

/*
  To be called at the start of its use.
  Initialize the library. Create the InfoChannel type and the string type for
  the current system architecture.
 */
phaseh5_error pycodeh5_init();

/*
  To bel called at the end of its use.
  Finalize the library. Free the memory that handles the InfoChannel and string
  types created on init.
 */
void pycodeh5_close();

// Clear the fields of a PhaseH5 struct
void init_phase(PhaseH5* phase);

// Open a Phase from a .h5 file and parse its content
phaseh5_error phase_open(PhaseH5* phase, const char* filename);

// Close a PhaseH5 clearing the allocated resources
phaseh5_error phase_close(PhaseH5* phase);

// Flushes data to disk
phaseh5_error flush(PhaseH5* phase);
phaseh5_error raw_data(PhaseH5* phase, size_t index, size_t start, size_t end, int* buf);
phaseh5_error set_raw_data(PhaseH5* phase, size_t index, size_t start, size_t end, const int* buf);
phaseh5_error digital(PhaseH5* phase, size_t start, size_t end, int* buf);
phaseh5_error set_digital(PhaseH5* phase, size_t start, size_t end, const int* buf);
phaseh5_error events_len(PhaseH5* phase, size_t index, hsize_t* len);
phaseh5_error events(PhaseH5* phase, size_t index, LLONG_TYPE* buf);
phaseh5_error peak_train_len(PhaseH5*, size_t group, const char* label, size_t* len);
phaseh5_error peak_train(PhaseH5* phase, size_t group, const char* label, PeakTrain* peak_train);
phaseh5_error set_peak_train(PhaseH5* phase, size_t group, const char* label, const PeakTrain* peak_train);

