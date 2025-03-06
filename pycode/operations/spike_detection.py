from typing import List, Optional, Tuple

from ..pycode import (
    compute_threshold as py_compute_threshold,
    spike_detection as py_spike_detection,
    spike_detection_new as py_spike_detection_new,
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


def spike_detection_new(
    data: List[float],
    threshold: float,
    peak_duration: int,
    refractory_time: int,
) -> Optional[Tuple[List[int], List[float]]]:
    return py_spike_detection_new(data, threshold, peak_duration, refractory_time)
