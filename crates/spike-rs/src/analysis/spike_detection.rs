use crate::{
    error::SpikeError,
    operations::math,
    types::{ChannelTrait, PhaseTrait},
};

pub fn compute_threshold(
    range: &[f32],
    sampling_frequency: f32,
    multiplier: f32,
    min_threshold: f32,
) -> Result<f32, SpikeError> {
    const WINDOW_DURATION_TIME: f32 = 200e-3; // s
    const START_THRESHOLD: f32 = 100e-6; // V

    let window_duration_sample: usize = (WINDOW_DURATION_TIME * sampling_frequency) as usize;
    let number_of_windows: usize = range.len() / window_duration_sample;
    let windows_distance: usize = range.len() / number_of_windows;

    if range.len() < (window_duration_sample * number_of_windows) {
        return Err(SpikeError::ComputeThresholdTooFewSamples(
            range.len(),
            window_duration_sample * number_of_windows,
        ));
    }

    let mut threshold = START_THRESHOLD;

    for i in 0..number_of_windows {
        let starting_point = windows_distance * i;
        let ending_point = starting_point + window_duration_sample;
        let new_threshold = math::stdev(&range[starting_point..ending_point]);

        if new_threshold < threshold && new_threshold > min_threshold {
            threshold = new_threshold;
        }
    }

    Ok(threshold * multiplier)
}

pub fn spike_detection(
    data: &[f32],
    sampling_frequency: f32,
    threshold: f32,
    peak_duration: f32,
    refractory_time: f32,
) -> Result<(Vec<usize>, Vec<f32>), SpikeError> {
    // TODO check if reserving space for the ret increases performances.
    let mut ret_values = Vec::new();
    let mut ret_times = Vec::new();

    const OVERLAP: usize = 10;
    let data_length = data.len();

    let peak_duration: usize = (peak_duration * sampling_frequency) as usize;
    let refractory_time: usize = (refractory_time * sampling_frequency) as usize;

    if data_length < 2 || data_length < peak_duration {
        return Err(SpikeError::SpikeDetectionTooFewSamples);
    }

    let mut index = 1usize;
    let mut interval;
    let mut in_interval_index;

    let mut peak_start_sample;
    let mut peak_start_value;
    let mut peak_end_sample;
    let mut peak_end_value;

    while index < data_length - 1 {
        // By the way the maximum and minimum that belongs to a peak are found
        // all the found i assume that all the maximum are positive and all the
        // minimum are negative so that an offset on the signal will invalidate
        // the peak detection. Otherwise a minimum can be in the form:
        //                 .
        //                   .        ...
        //                     .    .
        //                      . .
        //                       .
        // 
        // -------------------------------------------------------------
        //
        //

        // data[index - 1] > data[index] && data[index] < data[index + 1]
        // with data[index - 1], data[index], data[index + 1] >= 0

        // which leads to:

        // data[index - 1].abs() < data[index].abs() &&
        // data_len[index].abs() < data[index + 1].abs()

        // and a maximum can be in the form:

        //
        //
        // -------------------------------------------------------------
        //                          .
        //                        .. ..
        //                       .     .
        //                      .       .
        
        // data[index - 1] < data[index] && data[index] > data[index + 1]
        // with data[index - 1], data[index], data[index + 1] <= 0

        // which leads to:

        // data[index - 1].abs() > data[index].abs() &&
        // data_len[index].abs() > data[index + 1].abs()

        
        // If a minimum or a maximum has been found ...
        if (data[index].abs() > data[index - 1].abs())
            && (data[index].abs() >= data[index + 1].abs())
        {
            // check if the end of the interval where to check for a spike excedes
            // the length of the signal and, eventually, set the interval to end
            // earlier.
            if index + peak_duration > data_length {
                interval = data_length - index - 1;
            } else {
                interval = peak_duration;
            }

            // temporarely set the start of the spike to be at the current index
            peak_start_sample = index;
            peak_start_value = data[index];

            // look for minimum if the start value of the peak is positive
            if peak_start_value > 0f32 {
                peak_end_sample = index + 1;
                peak_end_value = peak_start_value;

                // find the minimum in [index, index+interval]
                in_interval_index = index + 1;
                while in_interval_index < index + interval {
                    if data[in_interval_index] < peak_end_value {
                        peak_end_sample = in_interval_index;
                        peak_end_value = data[in_interval_index];
                    }
                    in_interval_index += 1;
                } // end find minimum

                // find the actual maximum in [index, peak_end_sample]
                in_interval_index = index + 1;
                while in_interval_index < peak_end_sample {
                    if data[in_interval_index] > peak_start_value {
                        peak_start_sample = in_interval_index;
                        peak_start_value = data[in_interval_index];
                    }
                    in_interval_index += 1;
                } // end looking for actual maximum

                // if the minimum has been found at right the boundary of the interval
                // check if the signal is still decreasing and look for the interval in
                // [index + interval, index + interval + OVERLAP] if this value does not
                // overcome the data_length
                if peak_end_sample == index + interval - 1 && index + interval + OVERLAP < data_length {
                    in_interval_index = peak_end_sample + 1;
                    while in_interval_index < index + interval + OVERLAP {
                        if data[in_interval_index] < peak_end_value {
                            peak_end_sample = in_interval_index;
                            peak_end_value = data[in_interval_index];
                        }
                        in_interval_index += 1;
                    }
                }
            }
            // end minimum branch

            else {
                // else look for a maximum
                peak_end_sample = index + 1;
                peak_end_value = peak_start_value;

                // find the maximum in [index, index+interval]
                in_interval_index = index + 1;
                while in_interval_index < index + interval {
                    if data[in_interval_index] > peak_end_value {
                        peak_end_sample = in_interval_index;
                        peak_end_value = data[in_interval_index];
                    }
                    in_interval_index += 1;
                } // end find maximum

                // find the actual minimum in [index, peak_end_sample]
                in_interval_index = index + 1;
                while in_interval_index < peak_end_sample {
                    if data[in_interval_index] < peak_start_value {
                        peak_start_sample = in_interval_index;
                        peak_start_value = data[in_interval_index];
                    }
                    in_interval_index += 1;
                } // end looking for actual minimum

                // if the maximum has been found at the right boundary of the interval
                // check if the signal is still increasing and look for the interval in
                // [index + interval, index + interval + OVERLAP] if this value does not
                // overcome the data_length
                if peak_end_sample == index + interval - 1 && index + interval + OVERLAP < data_length {
                    in_interval_index = peak_end_sample + 1;
                    while in_interval_index < index + interval + OVERLAP {
                        if data[in_interval_index] > peak_end_value {
                            peak_end_sample = in_interval_index;
                            peak_end_value = data[in_interval_index];
                        }
                        in_interval_index += 1;
                    }
                }
            }

            // check if the difference overtakes the threshold
            let difference = peak_start_value - peak_end_value;

            if difference.abs() >= threshold {
                let (last_peak_val, last_peak_time) =
                    if peak_start_value.abs() > peak_end_value.abs() {
                        (peak_start_value, peak_start_sample)
                    } else {
                        (peak_end_value, peak_end_sample)
                    };

                ret_values.push(last_peak_val);
                ret_times.push(last_peak_time);

                // set the new index where to start looking for a peak
                if last_peak_time + refractory_time > peak_end_sample
                    && last_peak_time + refractory_time < data_length
                {
                    index = last_peak_time + refractory_time;
                } else {
                    index = peak_end_sample + 1;
                }

                continue;
            } // end threshold check
        }
        index += 1;
    }
    Ok((ret_times, ret_values))
}

pub fn spike_detection_new_core(
    data: &[f32],
    threshold: f32,
    peak_duration: usize,
    peak_distance: usize,
) -> Result<(Vec<usize>, Vec<f32>), SpikeError> {
    let data_len = data.len();

    if data_len < 2 {
        return Err(SpikeError::SpikeDetectionTooFewSamples);
    }

    // Here it is allocated the storage for the return values of peak times an
    // amplitude. For increasing performance the maximum number of possible peaks
    // that can be found has been reserved. If there is exactly one peak every
    // PEAK_DISTANCE and the minimum distance of a peak is 2 samples the maximum
    // number of peaks will be:
    //
    //
    //      MIN PEAK DURATION                            OTHER PEAK DURATION
    //      --                                           -----------
    //      ^                                            ^
    // ... -------------------------------------------------------------- ... ...
    //     |    v                                      |              v
    //     |   |                                       |    |
    // ... |---------------- | ------------------------| ---------------- ... ...
    //    PEAK  DURATION        PEAK DISTANCE           PEAK DURATION
    //
    //---------------------------------------------------------------------------
    //                           DATA LENGTH
    //

    let mut peak_times = vec![];
    let mut peak_values = vec![];

    let max_peaks = data.len() / (peak_distance + 2);

    peak_times.reserve(max_peaks);
    peak_values.reserve(max_peaks);

    // Now let's scroll the data looking for minumum and maximum
    let mut max_index = 0;
    let mut max_value = f32::MIN;
    let mut min_index = 0;
    let mut min_value = f32::MAX;

    let mut index = 1;
    while index < data.len() {

    }

    Ok((peak_times, peak_values))
}

pub fn spike_detection_new(
    data: &[f32],
    threshold: f32,
    peak_duration: usize,
    peak_distance: usize,
    n_treads: Option<usize>,
) -> Result<(Vec<usize>, Vec<f32>), SpikeError> {
    
    Ok()
}

pub fn compute_peak_train<Channel: ChannelTrait>(
    phase: &mut impl PhaseTrait<Channel>,
    channel: &Channel,
    start: Option<usize>,
    end: Option<usize>,
) -> Result<(), SpikeError> {
    let signal = phase.raw_data(channel, start, end)?;
    let threshold = compute_threshold(&signal[..], phase.sampling_frequency(), 8 as _, 0.0001f32)?;
    let peaks_train = spike_detection(
        &signal[..],
        phase.sampling_frequency(),
        threshold,
        2e-3,
        2e-3,
    )?;
    phase.set_peak_train(channel, peaks_train)?;
    Ok(())
}
