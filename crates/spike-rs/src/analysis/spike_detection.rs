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
                if peak_end_sample == index + interval - 1
                    && index + interval + OVERLAP < data_length
                {
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
                if peak_end_sample == index + interval - 1
                    && index + interval + OVERLAP < data_length
                {
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
) -> Result<(Vec<usize>, Vec<usize>, Vec<f32>, Vec<f32>), SpikeError> {
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

    let max_peaks = data.len() / (peak_duration + peak_distance + 2);
    let window_size = peak_duration / 2;

    let mut max_peak_indices = vec![];
    let mut min_peak_indices = vec![];
    let mut max_peak_values = vec![];
    let mut min_peak_values = vec![];

    min_peak_indices.reserve(max_peaks);
    max_peak_indices.reserve(max_peaks);
    max_peak_values.reserve(max_peaks);
    min_peak_values.reserve(max_peaks);

    let mut a_min_value = f32::MAX;
    let mut a_min_index = 0;
    let mut a_max_value = f32::MIN;
    let mut a_max_index = 0;
    let mut b_min_value = f32::MAX;
    let mut b_min_index = 0;
    let mut b_max_value = f32::MIN;
    let mut b_max_index = 0;
    let mut index = 0;
    let mut a_check_lim = window_size;
    let mut window_scrolled = 0;

    // Now let's scroll the data looking for minumum and maximum. The
    // general idea is to find minimum and maximum in a window that
    // last for half of the `peak_duration` time. This way we should
    // directly obtain the absolute maximum and minimum during the
    // life of peak and scroll the data array just one time. By
    // comparing the mimuma and maxima found in two consecutive
    // windows we should be able to found all peaks without overlaps.

    // fill the a window min and max;
    while index < a_check_lim && index < data.len() {
        if data[index] < a_min_value {
            a_min_value = data[index];
            a_min_index = index;
        }
        if data[index] > a_max_value {
            a_max_value = data[index];
            a_max_index = index;
        }
        index += 1;
    }

    // if there is no place for other checks just check the a_min and a_max
    // for a threshold otherwise go on.
    if index == data.len() {
        if a_max_value - a_min_value >= threshold {
            println!("Inserted peak in A");
            max_peak_indices.push(a_max_index);
            max_peak_values.push(a_max_value);
            min_peak_indices.push(a_min_index);
            min_peak_values.push(a_min_value);
        }
    } else {
        while index < data.len() {
            if data[index] < b_min_value {
                b_min_value = data[index];
                b_min_index = index;
            }
            if data[index] > b_max_value {
                b_max_value = data[index];
                b_max_index = index;
            }
            window_scrolled += 1;
            if window_scrolled == window_size {
                // reset the window scroll. We will move to a new
                // window where to search for minimum and maximum.
                window_scrolled = 0;

                let test_max_value;
                let test_max_index;
                let test_min_value;
                let test_min_index;

                // look for the actual min and the actual max
                if a_max_value > b_max_value {
                    test_max_value = a_max_value;
                    test_max_index = a_max_index;
                } else {
                    test_max_value = b_max_value;
                    test_max_index = b_max_index;
                }

                if a_min_value < b_min_value {
                    test_min_value = a_min_value;
                    test_min_index = a_min_index;
                } else {
                    test_min_value = b_min_value;
                    test_min_index = b_min_index;
                }

                // check for threshold
                if test_max_value - test_min_value >= threshold {
                    max_peak_indices.push(test_max_index);
                    max_peak_values.push(test_max_value);
                    min_peak_indices.push(test_min_index);
                    min_peak_values.push(test_min_value);

                    index = if test_max_index > test_min_index {
                        test_max_index
                    } else {
                        test_min_index
                    } + peak_distance;

                    // fill A values again and check if we are not at
                    // the end of the array
                    a_min_value = f32::MAX;
                    a_max_value = f32::MIN;
                    a_check_lim = index + window_size;
                    while index < a_check_lim && index < data.len() {
                        if data[index] < a_min_value {
                            a_min_value = data[index];
                            a_min_index = index;
                        }
                        if data[index] > a_max_value {
                            a_max_value = data[index];
                            a_max_index = index;
                        }
                        index += 1;
                    }

                    // if there is no place for other checks just check the a_min and a_max
                    // for a threshold otherwise go on.
                    if index == data.len() {
                        if a_max_value - a_min_value >= threshold {
                            println!("Inserted peak in C");
                            max_peak_indices.push(a_max_index);
                            max_peak_values.push(a_max_value);
                            min_peak_indices.push(a_min_index);
                            min_peak_values.push(a_min_value);
                        }
                    }
                    b_min_value = f32::MAX;
                    b_max_value = f32::MIN;

                    // the correct index has been update so continue
                    // to the next loop to avoid increasing it
                    // unnecessary.
                    // continue;
                } else {
                    // we are going to move to the next window, the A
                    // values have already been checked and need the B
                    // values have to be the new A values.
                    a_min_value = b_min_value;
                    a_min_index = b_min_index;
                    a_max_value = b_max_value;
                    a_max_index = b_max_index;
                    b_min_value = f32::MAX;
                    b_max_value = f32::MIN;
                }
            }
            index += 1;
        }
    }

    Ok((
        max_peak_indices,
        min_peak_indices,
        max_peak_values,
        min_peak_values,
    ))
}

pub fn spike_detection_new(
    data: &[f32],
    threshold: f32,
    peak_duration: usize,
    peak_distance: usize,
    n_threads: Option<usize>,
) -> Result<(Vec<usize>, Vec<usize>, Vec<f32>, Vec<f32>), SpikeError> {
    match n_threads {
        Some(_n_threads) => {
            todo!()
        }
        None => spike_detection_new_core(data, threshold, peak_duration, peak_distance),
    }
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
