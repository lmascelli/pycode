use super::sys;
use spike_rs::error::SpikeError;
 
/// All the variant of the Error type represent one of the possible event that
/// can go wrong during the reading/writing of an hdf5 file containg data
/// recorded with the MultiChannel Experimenter or MC_Rack software.
#[derive(Debug)]
pub enum Error {
    ErrorNotYetConverted(i32),
    CreatePeakTrainSamplesValuesDifferentLen,
    OpenFile,
    CloseFile,
    OpenDataGroup,
    OpenDateAttribute,
    ReadDateAttribute,
    OpenDateDatatype,
    OpenAnalogGroup,
    OpenInfoChannelDataset,
    OpenInfoChannelDataspace,
    OpenInfoChannelDatatype,
    ReadInfoChannels,
    OpenAnalogDataset,
    OpenLabelAttribute,
    ReadLabelAttribute,
    OpenLabelDatatype,
    ParseAnalogStream,
    MultipleDigitalStreams,
    MultipleRawDataStreams,
    MultipleSamplingFrequencies,
    MultipleDatalens,
    OpenChannelData,
    OpenChannelDataDataspace,
    GetChannelDataDims,
    NoRawDataStream,
    OpenEventStreamGroupLink,
    OpenEventStreamGroup,
    OpenEventStreamStream0GroupLink,
    MaxEventStreamsExceeded,
    OpenEntityDataset,
    EventEntityDatasetClose,
    OpenPeakTrainGroup,
    CreatePeakGroup,
    RawDataEndBeforeStart,
    RawDataEndOutOfBounds,
    RawDataGetDataspace,
    RawDataSelectHyperslab,
    RawDataCreateMemoryDataspace,
    RawDataReadData,
    SetRawDataEndOutOfBounds,
    SetRawDataGetDataspace,
    SetRawDataSelectHyperslab,
    SetRawDataCreateMemoryDataspace,
    SetRawDataWriteDataset,
    DigitalNoDigital,
    DigitalEndBeforeStart,
    DigitalEndOutOfBounds,
    DigitalGetDataspaceFail,
    DigitalSelectHyperslabFail,
    DigitalCreateMemoryDataspaceFail,
    DigitalReadDataFail,
    SetDigitalNoDigital,
    SetDigitalEndBeforeStart,
    SetDigitalEndOutOfBounds,
    SetDigitalGetDataspaceFail,
    SetDigitalSelectHyperslabFail,
    SetDigitalCreateMemoryDataspaceFail,
    SetDigitalWriteDataFail,
    EventsLenIndexOutOfBounds,
    EventsLenOpenEventDataspace,
    EventsLenGetDims,
    EventsIndexOutOfBounds,
    EventsGetEventsDataspace,
    EventsSelectDataspaceHyperslab,
    EventsCreateMemoryDataspace,
    EventsReadDataset,
    PeakTrainNoPeakGroup,
    PeakTrainValuesDatasetLink,
    PeakTrainNoValuesDataset,
    PeakTrainSamplesDatasetLink,
    PeakTrainNoSamplesDataset,
    PeakTrainOpenValuesDataset,
    PeakTrainOpenSamplesDataset,
    DeletePeakTrainValuesDatasetLink,
    DeletePeakTrainNoValuesDataset,
    DeletePeakTrainSamplesDatasetLink,
    DeletePeakTrainNoSamplesDataset,
    DeletePeakTrainValuesDataset,
    DeletePeakTrainSamplesDataset,
    PeakTrainCloseMemoryDataspaceFail,
    PeakTrainLenOpenValuesDataset,
    PeakTrainLenOpenSamplesDataset,
    PeakTrainLenOpenSamplesDataspace,
    PeakTrainLenOpenValuesDataspace,
    PeakTrainLenGetValuesDataspace,
    PeakTrainLenGetSamplesDataspace,
    PeakTrainLenValuesSamplesDifferent,
    PeakTrainLenCloseSamplesDataset,
    PeakTrainLenCloseValuesDataset,
    PeakTrainLenCloseSamplesDataspace,
    PeakTrainLenCloseValuesDataspace,
    PeakTrainCloseValuesDataset,
    PeakTrainCloseSamplesDataset,
    PeakTrainCreateMemoryDataspace,
    PeakTrainReadValuesDataset,
    PeakTrainReadSamplesDataset,
    SetPeakTrainCheckLabelGroup,
    SetPeakTrainCloseDeletedValuesDataset,
    SetPeakTrainCloseDeletedSamplesDataset,
    SetPeakTrainCloseSamplesFileDataspace,
    SetPeakTrainCloseValuesFileDataspace,
    SetPeakTrainCreateSamplesMemoryDataspace,
    SetPeakTrainCreateValuesMemoryDataspace,
    SetPeakTrainCreateSamplesFileDataspace,
    SetPeakTrainCreateValuesFileDataspace,
    SetPeakTrainCreateSamplesMemoryDataset,
    SetPeakTrainCreateValuesMemoryDataset,
    SetPeakTrainWriteSamplesDataset,
    SetPeakTrainWriteValuesDataset,
    SetPeakTrainCloseSamplesMemoryDataspace,
    SetPeakTrainCloseValuesMemoryDataspace,
    SetPeakTrainCloseSamplesDataset,
    SetPeakTrainCloseValuesDataset,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        writeln!(f, "{:?}", self)?;
        Ok(())
    }
}
impl std::error::Error for Error {}

impl From<Error> for SpikeError {
    fn from(err: Error) -> Self {
        SpikeError::Implementation(format!("{:?}", err))
    }
}

impl Error {
    /// this method convert a C enum value returned from a function to its
    /// corresponding [`Error`] value
    pub fn from_phaseh5_error(code: sys::phaseh5_error) -> Result<(), Self> {
        match code {
            sys::phaseh5_error_OK => Ok(()),
            sys::phaseh5_error_OPEN_FAIL => Err(Error::OpenFile),
            sys::phaseh5_error_CLOSE_FILE_FAIL => Err(Error::CloseFile),
            sys::phaseh5_error_OPEN_DATA_GROUP_FAIL => Err(Error::OpenDataGroup),
            sys::phaseh5_error_OPEN_DATE_ATTRIBUTE_FAIL => Err(Error::OpenDateAttribute),
            sys::phaseh5_error_READ_DATE_ATTRIBUTE_FAIL => Err(Error::ReadDateAttribute),
            sys::phaseh5_error_OPEN_DATE_DATATYPE_FAIL => Err(Error::OpenDateDatatype),
            sys::phaseh5_error_OPEN_ANALOG_GROUP_FAIL => Err(Error::OpenAnalogGroup),
            sys::phaseh5_error_OPEN_INFO_CHANNEL_DATASET_FAIL => Err(Error::OpenInfoChannelDataset),
            sys::phaseh5_error_OPEN_INFO_CHANNEL_DATASPACE_FAIL => {
                Err(Error::OpenInfoChannelDataspace)
            }
            sys::phaseh5_error_OPEN_INFO_CHANNEL_DATATYPE_FAIL => {
                Err(Error::OpenInfoChannelDatatype)
            }
            sys::phaseh5_error_OPEN_ANALOG_DATASET_FAIL => Err(Error::OpenAnalogDataset),
            sys::phaseh5_error_OPEN_LABEL_ATTRIBUTE_FAIL => Err(Error::OpenLabelAttribute),
            sys::phaseh5_error_READ_LABEL_ATTRIBUTE_FAIL => Err(Error::ReadLabelAttribute),
            sys::phaseh5_error_OPEN_LABEL_DATATYPE_FAIL => Err(Error::OpenLabelDatatype),
            sys::phaseh5_error_READ_INFO_CHANNELS_FAIL => Err(Error::ReadInfoChannels),
            sys::phaseh5_error_PARSE_ANALOG_STREAM_DIFFERENT_TICK => Err(Error::ParseAnalogStream),
            sys::phaseh5_error_MULTIPLE_DIGITAL_STREAMS => Err(Error::MultipleDigitalStreams),
            sys::phaseh5_error_MULTIPLE_RAW_DATA_STREAMS => Err(Error::MultipleRawDataStreams),
            sys::phaseh5_error_MULTIPLE_SAMPLING_FREQUENCIES => {
                Err(Error::MultipleSamplingFrequencies)
            }
            sys::phaseh5_error_MULTIPLE_DATALENS => Err(Error::MultipleDatalens),
            sys::phaseh5_error_OPEN_CHANNEL_DATA_FAIL => Err(Error::OpenChannelData),
            sys::phaseh5_error_OPEN_CHANNEL_DATA_DATASPACE_FAIL => {
                Err(Error::OpenChannelDataDataspace)
            }
            sys::phaseh5_error_GET_CHANNEL_DATA_DIMS_FAIL => Err(Error::GetChannelDataDims),
            sys::phaseh5_error_NO_RAW_DATA_STREAM => Err(Error::NoRawDataStream),
            sys::phaseh5_error_OPEN_EVENT_STREAM_GROUP_FAIL => Err(Error::OpenEventStreamGroup),
            sys::phaseh5_error_OPEN_EVENT_STREAM_GROUP_LINK_FAIL => {
                Err(Error::OpenEventStreamGroupLink)
            }
            sys::phaseh5_error_OPEN_EVENT_STREAM_STREAM_0_GROUP_LINK_FAIL => {
                Err(Error::OpenEventStreamStream0GroupLink)
            }
            sys::phaseh5_error_MAX_EVENT_STREAMS_EXCEEDED => Err(Error::MaxEventStreamsExceeded),
            sys::phaseh5_error_OPEN_ENTITY_DATASET_FAIL => Err(Error::OpenEntityDataset),
            sys::phaseh5_error_EVENT_ENTITY_DATASET_CLOSE_FAIL => {
                Err(Error::EventEntityDatasetClose)
            }
            sys::phaseh5_error_OPEN_PEAK_TRAIN_GROUP_FAIL => Err(Error::OpenPeakTrainGroup),
            sys::phaseh5_error_CREATE_PEAK_GROUP_FAIL => Err(Error::CreatePeakGroup),
            sys::phaseh5_error_RAW_DATA_END_BEFORE_START => Err(Error::RawDataEndBeforeStart),
            sys::phaseh5_error_RAW_DATA_END_OUT_OF_BOUNDS => Err(Error::RawDataEndOutOfBounds),
            sys::phaseh5_error_RAW_DATA_GET_DATASPACE_FAIL => Err(Error::RawDataGetDataspace),
            sys::phaseh5_error_RAW_DATA_SELECT_HYPERSLAB_FAIL => Err(Error::RawDataSelectHyperslab),
            sys::phaseh5_error_RAW_DATA_CREATE_MEMORY_DATASPACE_FAIL => {
                Err(Error::RawDataCreateMemoryDataspace)
            }
            sys::phaseh5_error_RAW_DATA_READ_DATA_FAIL => Err(Error::RawDataReadData),
            sys::phaseh5_error_SET_RAW_DATA_GET_DATASPACE_FAIL => {
                Err(Error::SetRawDataGetDataspace)
            }
            sys::phaseh5_error_SET_RAW_DATA_SELECT_HYPERSLAB_FAIL => {
                Err(Error::SetRawDataSelectHyperslab)
            }
            sys::phaseh5_error_SET_RAW_DATA_CREATE_MEMORY_DATASPACE_FAIL => {
                Err(Error::SetRawDataCreateMemoryDataspace)
            }
            sys::phaseh5_error_SET_RAW_DATA_WRITE_DATASET_FAIL => {
                Err(Error::SetRawDataWriteDataset)
            }
            sys::phaseh5_error_DIGITAL_NO_DIGITAL => Err(Error::DigitalNoDigital),
            sys::phaseh5_error_DIGITAL_END_BEFORE_START => Err(Error::DigitalEndBeforeStart),
            sys::phaseh5_error_DIGITAL_END_OUT_OF_BOUNDS => Err(Error::DigitalEndOutOfBounds),
            sys::phaseh5_error_DIGITAL_GET_DATASPACE_FAIL => Err(Error::DigitalGetDataspaceFail),
            sys::phaseh5_error_DIGITAL_SELECT_HYPERSLAB_FAIL => {
                Err(Error::DigitalSelectHyperslabFail)
            }
            sys::phaseh5_error_DIGITAL_CREATE_MEMORY_DATASPACE_FAIL => {
                Err(Error::DigitalCreateMemoryDataspaceFail)
            }
            sys::phaseh5_error_DIGITAL_READ_DATA_FAIL => Err(Error::DigitalReadDataFail),
            sys::phaseh5_error_SET_DIGITAL_NO_DIGITAL => Err(Error::SetDigitalNoDigital),
            sys::phaseh5_error_SET_DIGITAL_END_BEFORE_START => Err(Error::SetDigitalEndBeforeStart),
            sys::phaseh5_error_SET_DIGITAL_END_OUT_OF_BOUNDS => {
                Err(Error::SetDigitalEndOutOfBounds)
            }
            sys::phaseh5_error_SET_DIGITAL_GET_DATASPACE_FAIL => {
                Err(Error::SetDigitalGetDataspaceFail)
            }
            sys::phaseh5_error_SET_DIGITAL_SELECT_HYPERSLAB_FAIL => {
                Err(Error::SetDigitalSelectHyperslabFail)
            }
            sys::phaseh5_error_SET_DIGITAL_CREATE_MEMORY_DATASPACE_FAIL => {
                Err(Error::SetDigitalCreateMemoryDataspaceFail)
            }
            sys::phaseh5_error_SET_DIGITAL_WRITE_DATA_FAIL => Err(Error::SetDigitalWriteDataFail),
            sys::phaseh5_error_EVENTS_LEN_INDEX_OUT_OF_BOUNDS => {
                Err(Error::EventsLenIndexOutOfBounds)
            }
            sys::phaseh5_error_EVENTS_LEN_OPEN_EVENT_DATASPACE_FAIL => {
                Err(Error::EventsLenOpenEventDataspace)
            }
            sys::phaseh5_error_EVENTS_INDEX_OUT_OF_BOUNDS => Err(Error::EventsIndexOutOfBounds),
            sys::phaseh5_error_EVENTS_LEN_GET_DIMS_FAIL => Err(Error::EventsLenGetDims),
            sys::phaseh5_error_EVENTS_GET_EVENTS_DATASPACE_FAIL => {
                Err(Error::EventsGetEventsDataspace)
            }
            sys::phaseh5_error_EVENTS_SELECT_DATASPACE_HYPERSLAB_FAIL => {
                Err(Error::EventsSelectDataspaceHyperslab)
            }
            sys::phaseh5_error_PEAK_TRAIN_CLOSE_MEMORY_DATASPACE_FAIL => {
                Err(Error::PeakTrainCloseMemoryDataspaceFail)
            }
            sys::phaseh5_error_EVENTS_CREATE_MEMORY_DATASPACE_FAIL => {
                Err(Error::EventsCreateMemoryDataspace)
            }
            sys::phaseh5_error_EVENTS_READ_DATASET_FAIL => Err(Error::EventsReadDataset),
            sys::phaseh5_error_SET_PEAK_TRAIN_CHECK_LABEL_GROUP_FAIL => {
                Err(Error::SetPeakTrainCheckLabelGroup)
            }
            sys::phaseh5_error_SET_PEAK_TRAIN_CLOSE_DELETED_VALUES_DATASET_FAIL => {
                Err(Error::SetPeakTrainCloseDeletedValuesDataset)
            }
            sys::phaseh5_error_SET_PEAK_TRAIN_CLOSE_DELETED_SAMPLES_DATASET_FAIL => {
                Err(Error::SetPeakTrainCloseDeletedSamplesDataset)
            }
            sys::phaseh5_error_SET_PEAK_TRAIN_CLOSE_SAMPLES_FILE_DATASPACE_FAIL => {
                Err(Error::SetPeakTrainCloseSamplesFileDataspace)
            }
            sys::phaseh5_error_SET_PEAK_TRAIN_CLOSE_VALUES_FILE_DATASPACE_FAIL => {
                Err(Error::SetPeakTrainCloseValuesFileDataspace)
            }
            sys::phaseh5_error_DELETE_PEAK_TRAIN_VALUES_DATASET_LINK_FAIL => {
                Err(Error::DeletePeakTrainValuesDatasetLink)
            }
            sys::phaseh5_error_DELETE_PEAK_TRAIN_NO_VALUES_DATASET => {
                Err(Error::DeletePeakTrainNoValuesDataset)
            }
            sys::phaseh5_error_DELETE_PEAK_TRAIN_SAMPLES_DATASET_LINK_FAIL => {
                Err(Error::DeletePeakTrainSamplesDatasetLink)
            }
            sys::phaseh5_error_DELETE_PEAK_TRAIN_NO_SAMPLES_DATASET => {
                Err(Error::DeletePeakTrainNoSamplesDataset)
            }
            sys::phaseh5_error_DELETE_PEAK_TRAIN_VALUES_DATASET_FAIL => {
                Err(Error::DeletePeakTrainValuesDataset)
            }
            sys::phaseh5_error_DELETE_PEAK_TRAIN_SAMPLES_DATASET_FAIL => {
                Err(Error::DeletePeakTrainSamplesDataset)
            }
            sys::phaseh5_error_PEAK_TRAIN_LEN_CLOSE_VALUES_DATASET_FAIL => {
                Err(Error::PeakTrainLenCloseValuesDataset)
            }
            sys::phaseh5_error_PEAK_TRAIN_LEN_CLOSE_SAMPLES_DATASET_FAIL => {
                Err(Error::PeakTrainLenCloseValuesDataset)
            }
            sys::phaseh5_error_PEAK_TRAIN_LEN_CLOSE_VALUES_DATASPACE_FAIL => {
                Err(Error::PeakTrainLenCloseValuesDataspace)
            }
            sys::phaseh5_error_PEAK_TRAIN_LEN_CLOSE_SAMPLES_DATASPACE_FAIL => {
                Err(Error::PeakTrainLenCloseSamplesDataspace)
            }
            sys::phaseh5_error_PEAK_TRAIN_LEN_GET_VALUES_DATASPACE_DIM_FAIL => {
                Err(Error::PeakTrainLenGetValuesDataspace)
            }
            sys::phaseh5_error_PEAK_TRAIN_LEN_OPEN_VALUES_DATASPACE_FAIL => {
                Err(Error::PeakTrainLenOpenValuesDataspace)
            }
            sys::phaseh5_error_PEAK_TRAIN_LEN_GET_SAMPLES_DATASPACE_DIM_FAIL => {
                Err(Error::PeakTrainLenGetSamplesDataspace)
            }
            sys::phaseh5_error_PEAK_TRAIN_LEN_VALUES_SAMPLES_DIFFERENT => {
                Err(Error::PeakTrainLenValuesSamplesDifferent)
            }
            sys::phaseh5_error_PEAK_TRAIN_NO_PEAK_GROUP => Err(Error::PeakTrainNoPeakGroup),
            sys::phaseh5_error_PEAK_TRAIN_VALUES_DATASET_LINK_FAIL => {
                Err(Error::PeakTrainValuesDatasetLink)
            }
            sys::phaseh5_error_PEAK_TRAIN_NO_VALUES_DATASET => Err(Error::PeakTrainNoValuesDataset),
            sys::phaseh5_error_PEAK_TRAIN_SAMPLES_DATASET_LINK_FAIL => {
                Err(Error::PeakTrainSamplesDatasetLink)
            }
            sys::phaseh5_error_PEAK_TRAIN_NO_SAMPLES_DATASET => {
                Err(Error::PeakTrainNoSamplesDataset)
            }
            sys::phaseh5_error_PEAK_TRAIN_OPEN_VALUES_DATASET_FAIL => {
                Err(Error::PeakTrainOpenValuesDataset)
            }
            sys::phaseh5_error_PEAK_TRAIN_OPEN_SAMPLES_DATASET_FAIL => {
                Err(Error::PeakTrainOpenSamplesDataset)
            }
            sys::phaseh5_error_PEAK_TRAIN_CLOSE_VALUES_DATASET_FAIL => {
                Err(Error::PeakTrainCloseValuesDataset)
            }
            sys::phaseh5_error_PEAK_TRAIN_CLOSE_SAMPLES_DATASET_FAIL => {
                Err(Error::PeakTrainCloseSamplesDataset)
            }
            sys::phaseh5_error_PEAK_TRAIN_CREATE_MEMORY_DATASPACE_FAIL => {
                Err(Error::PeakTrainCreateMemoryDataspace)
            }
            sys::phaseh5_error_PEAK_TRAIN_READ_VALUES_DATASET_FAIL => {
                Err(Error::PeakTrainReadValuesDataset)
            }
            sys::phaseh5_error_PEAK_TRAIN_READ_SAMPLES_DATASET_FAIL => {
                Err(Error::PeakTrainReadSamplesDataset)
            }
            sys::phaseh5_error_SET_PEAK_TRAIN_CREATE_SAMPLES_MEMORY_DATASPACE_FAIL => {
                Err(Error::SetPeakTrainCreateSamplesMemoryDataspace)
            }
            sys::phaseh5_error_SET_PEAK_TRAIN_CREATE_VALUES_MEMORY_DATASPACE_FAIL => {
                Err(Error::SetPeakTrainCreateValuesMemoryDataspace)
            }
            sys::phaseh5_error_SET_PEAK_TRAIN_CREATE_SAMPLES_FILE_DATASET_FAIL => {
                Err(Error::SetPeakTrainCreateSamplesMemoryDataset)
            }
            sys::phaseh5_error_SET_PEAK_TRAIN_CREATE_VALUES_FILE_DATASET_FAIL => {
                Err(Error::SetPeakTrainCreateValuesMemoryDataset)
            }
            sys::phaseh5_error_SET_PEAK_TRAIN_CREATE_SAMPLES_FILE_DATASPACE_FAIL => {
                Err(Error::SetPeakTrainCreateSamplesFileDataspace)
            }
            sys::phaseh5_error_SET_PEAK_TRAIN_CREATE_VALUES_FILE_DATASPACE_FAIL => {
                Err(Error::SetPeakTrainCreateValuesFileDataspace)
            }
            sys::phaseh5_error_SET_PEAK_TRAIN_WRITE_SAMPLES_DATASET_FAIL => {
                Err(Error::SetPeakTrainWriteSamplesDataset)
            }
            sys::phaseh5_error_SET_PEAK_TRAIN_WRITE_VALUES_DATASET_FAIL => {
                Err(Error::SetPeakTrainWriteValuesDataset)
            }
            sys::phaseh5_error_SET_PEAK_TRAIN_CLOSE_SAMPLES_MEMORY_DATASPACE_FAIL => {
                Err(Error::SetPeakTrainCloseSamplesMemoryDataspace)
            }
            sys::phaseh5_error_SET_PEAK_TRAIN_CLOSE_VALUES_MEMORY_DATASPACE_FAIL => {
                Err(Error::SetPeakTrainCloseValuesMemoryDataspace)
            }
            sys::phaseh5_error_SET_PEAK_TRAIN_CLOSE_SAMPLES_DATASET_FAIL => {
                Err(Error::SetPeakTrainCloseSamplesDataset)
            }
            sys::phaseh5_error_SET_PEAK_TRAIN_CLOSE_VALUES_DATASET_FAIL => {
                Err(Error::SetPeakTrainCloseValuesDataset)
            }
            _ => Err(Error::ErrorNotYetConverted(code.try_into().unwrap())),
        }
    }
}

