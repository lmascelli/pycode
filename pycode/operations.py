from typing import List, Optional, Tuple
import numpy as np

from .pycode import (
    PyChannel,
    PyPhase,
    compute_threshold as py_compute_threshold,
    spike_detection as py_spike_detection,
    get_digital_intervals as py_get_digital_intervals,
    subsample_range as py_subsample_range,
    burst_detection as py_burst_detection,
    count_peaks_in_intervals as py_count_peaks_in_intervals,
)


def compute_threshold(
    data: List[float],
    sampling_frequency: float,
    multiplier: float,
    min_threshold: float = 0.0001,
) -> Optional[float]:
    return py_compute_threshold(data, sampling_frequency, multiplier, min_threshold)


def spike_detection(
    data: List[float],
    sampling_frequency: float,
    threshold: float,
    peak_duration: float,
    refractory_time: float,
) -> Optional[Tuple[List[int], List[float]]]:
    return py_spike_detection(
        data, sampling_frequency, threshold, peak_duration, refractory_time
    )


def get_digital_intervals(digital: List[int]) -> List[Tuple[int, int]]:
    return py_get_digital_intervals(digital)

def count_peaks_in_intervals(peak_times: List[int], intervals: List[Tuple[int, int]]) -> List[int]:
    return py_count_peaks_in_intervals(peak_times, intervals)

def subsample_range(
    peaks: List[int], starting_sample: int, bin_size: int, n_bins: int
) -> List[int]:
    return py_subsample_range(peaks, starting_sample, bin_size, n_bins)


def clear_peaks_over_threshold(
    peak_times: List[int], peak_values: List[float], upper_threshold
) -> Tuple[List[int], List[float]]:
    new_peak_times, new_peak_values = [], []
    for i, value in enumerate(peak_values):
        if abs(value) < abs(upper_threshold):
            new_peak_times.append(peak_times[i])
            new_peak_values.append(value)
    return (new_peak_times, new_peak_values)


def psth(phase: PyPhase,
         bin_time_duration: float,
         psth_duration: float,
         excluded_channels: List[PyChannel] = [],
         ) -> np.ndarray:
    """
    Compute the PSTH ociaoooooooo :):):)
    and returns a list with the count of the spikes in each bin averaged by the number
    of channels and the length of the psth.

    @Parameters
    - phase: the Phase of interest
    - bin_time_duration: the duration of the bin IN SECONDS
    - psth_duration: the duration of the whole psth IN SECONDS
    """

    # OPEN THE PYCODE_RS HANDLER FOR THE DATA
    sampling_frequency = phase.sampling_frequency()
    bin_size = int(
        sampling_frequency * bin_time_duration
    )  # this round the size of a bin to the lower integer

    n_bins = int(psth_duration / bin_time_duration)  # number of bin after the stimulus

    channels = phase.channels()  # list of all the available channels

    # get the number of digital channels. if it's different from 1 an error has occurred
    # during the recording phase
    n_digital = phase.n_digitals()
    if n_digital != 1:
        exit(
            f"ERROR: the stimulation phase has {n_digital} digital channels (grazie MultiChannel)"
        )

    res = np.zeros(n_bins)  # variable to accumulate the psth

    # read the digital channel
    digital = phase.digital(0)
    # get the interval timestamps where the stimulation is active
    digital_intervals = get_digital_intervals(digital)
    n_channels = 0

    for interval in digital_intervals:
        for channel in channels:
            if channel not in excluded_channels:
                n_channels += 1
                res = np.add(
                    res,
                    subsample_range(
                        phase.peak_train(channel, None, None)[0],
                        interval[0],
                        bin_size,
                        n_bins,
                    ),
                )

    return res / (n_channels * n_bins)


def burst_detection(
    peak_train: List[int],
    sampling_frequency: float,
    cutoff: float,
) -> None:
    return py_burst_detection(peak_train, sampling_frequency, cutoff)
