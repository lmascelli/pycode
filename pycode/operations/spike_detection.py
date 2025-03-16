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
) -> Optional[Tuple[List[int], List[float]]]:
    peak_times = []
    peak_values = []
    thresholds = probe_threshold(data, sampling_frequency, multiplier, probe_interval)
    for start_interval, end_interval, threshold in thresholds:
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
