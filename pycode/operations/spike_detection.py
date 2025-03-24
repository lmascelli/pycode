from typing import List, Optional, Tuple
import numpy as np
from scipy.signal import butter, filtfilt

from ..pycode import (
    compute_threshold as py_compute_threshold,
    spike_detection as py_spike_detection,
    spike_detection_new as py_spike_detection_new,
)


def compute_threshold(
    data: List[float],
    sampling_frequency: float,
    multiplier: float,
    min_threshold: float = 0.00001,
) -> Optional[float]:
    return py_compute_threshold(data, sampling_frequency, multiplier, min_threshold)


def probe_threshold(
    data: List[float],
    sampling_frequency: float,
    multiplier: float,
    interval_duration: float = 5,
) -> List[Tuple[int, int, float]]:
    """Compute the threshold of the channel every INTERVAL_DURATION seconds and
    return an array of the interval in which the threshold is valid and the
    threshold itself.
    """
    ret = []
    probing_interval = interval_duration * sampling_frequency
    for i in range(int(len(data) / probing_interval)):
        ret.append(
            (
                int(i * interval_duration * sampling_frequency),
                int((i + 1) * interval_duration * sampling_frequency),
                compute_threshold(
                    data[int(i * probing_interval) : int((i + 1) * probing_interval)],
                    sampling_frequency,
                    multiplier,
                ),
            )
        )
    return ret


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


def spike_detection_moving_threshold(
    data: List[float],
    sampling_frequency: float,
    multiplier: float,
    peak_duration: float,
    refractory_time: float,
    probe_interval: float = 5,
    min_threshold: Optional[float] = None,
    max_threshold: Optional[float] = None,
) -> Optional[Tuple[List[int], List[float]]]:
    peak_times = []
    peak_values = []
    thresholds_data = probe_threshold(
        data, sampling_frequency, multiplier, probe_interval
    )
    for i, (start, end, threshold) in enumerate(thresholds_data):
        if min_threshold is not None and threshold < min_threshold:
            print(
                f"WARNING: Found threshold {threshold} under MIN_THRESHOLD in interval ({start}, {end}). It has been replaced with MIN_THRESHOLD {min_threshold}"
            )
            thresholds_data[i][2] = min_threshold
        if max_threshold is not None and threshold > max_threshold:
            print(
                f"WARNING: Found threshold {threshold} above MAX_THRESHOLD in interval ({start}, {end}). It has been replaced with MAX_THRESHOLD {max_threshold}"
            )
            thresholds_data[i] = (start, end, max_threshold)

    for start_interval, end_interval, threshold in thresholds_data:
        t_peak_times, t_peak_values = spike_detection(
            data[start_interval:end_interval],
            sampling_frequency,
            threshold,
            peak_duration,
            refractory_time,
        )
        for p in range(len(t_peak_times)):
            t_peak_times[p] += start_interval
        peak_times += t_peak_times
        peak_values += t_peak_values
    return (peak_times, peak_values)


def spike_detection_new(
    data: List[float],
    threshold: float,
    peak_duration: int,
    refractory_time: int,
) -> Optional[Tuple[List[int], List[float]]]:
    return py_spike_detection_new(data, threshold, peak_duration, refractory_time)


def mega_spike_detection(
    data: List[float],
    sampling_frequency: float,
    peak_duration: int,
    refractory_time: int,
    n_devs: float = 9,
    min_threshold: Optional[float] = None,
    max_threshold: Optional[float] = None,
    probe_threshold_time: Optional[float] = None,
    low_pass_frequency: Optional[float] = None,
    high_pass_frequency: Optional[float] = None,
) -> Optional[Tuple[List[int], List[float], Optional[List[float]]]]:
    ##################################################
    #
    #                FILTERING
    #
    ##################################################

    global filtered
    filtered = True
    if low_pass_frequency is not None and high_pass_frequency is not None:
        print("INFO: Band pass filtering")
        ORDER = 3
        nyquist_frequency = 0.5 * sampling_frequency
        low = low_pass_frequency / nyquist_frequency
        print(low)
        high = high_pass_frequency / nyquist_frequency
        b, a = butter(ORDER, [high, low], btype="band", analog=False, output="ba")
        actual_data = filtfilt(b, a, data)
    elif low_pass_frequency is not None:
        print("INFO: Low pass filtering")
        ORDER = 3
        nyquist_frequency = 0.5 * sampling_frequency
        low = low_pass_frequency / nyquist_frequency
        b, a = butter(ORDER, low, btype="low", analog=False, output="ba")
        actual_data = filtfilt(b, a, data)
    elif high_pass_frequency is not None:
        print("INFO: High pass filtering")
        ORDER = 3
        nyquist_frequency = 0.5 * sampling_frequency
        high = high_pass_frequency / nyquist_frequency
        b, a = butter(ORDER, high, btype="high", analog=False, output="ba")
        actual_data = filtfilt(b, a, data)
    else:
        actual_data = data
        filtered = False

    ##################################################
    #
    #                THRESHOLDING
    #
    ##################################################

    if probe_threshold_time is not None:
        peak_times, peak_values = (
            spike_detection_moving_threshold(
                actual_data,
                sampling_frequency,
                n_devs,
                peak_duration,
                refractory_time,
                probe_threshold_time,
                min_threshold,
                max_threshold,
            )
        )

    else:
        threshold = compute_threshold(
            actual_data, sampling_frequency, n_devs
        )

        peak_times, peak_values = spike_detection(
            actual_data,
            sampling_frequency,
            threshold,
            peak_duration,
            refractory_time,
        )

    return (peak_times, peak_values, actual_data if filtered else None)
