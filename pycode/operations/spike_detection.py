from typing import List, Optional, Tuple
import numpy as np

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
    min_threshold: float = 0.00001,
    max_threshold: float = 1,
    interval_duration: float = 5,
) -> Tuple[List[Tuple[int, int]], List[float]]:
    starting_values = []
    thresholds = []
    probing_interval = interval_duration * sampling_frequency
    for i in range(int(len(data) / probing_interval)):
        starting_values.append(
            (
                int(i * interval_duration * sampling_frequency),
                int((i + 1) * interval_duration * sampling_frequency),
            )
        )
        thresholds.append(
            compute_threshold(
                data[int(i * probing_interval) : int((i + 1) * probing_interval)],
                sampling_frequency,
                multiplier,
                min_threshold,
            )
        )
    return (starting_values, thresholds)


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
) -> Optional[Tuple[List[int], List[float]]]:
    starting_values, thresholds = probe_threshold(data, sampling_frequency, multiplier)
    peak_times, peak_values = [], []
    for i, threshold in enumerate(thresholds):
        t_peak_times, t_peak_values = spike_detection(data[starting_values[i][0]: starting_values[i][1]], sampling_frequency, threshold, peak_duration, refractory_time)
        for p in range(len(t_peak_times)):
            t_peak_times[p] += starting_values[i][0]
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
