from typing import List
from ..pycode import (
    burst_detection as py_burst_detection,
)

def burst_detection(
    peak_train: List[int],
    sampling_frequency: float,
    cutoff: float,
) -> None:
    return py_burst_detection(peak_train, sampling_frequency, cutoff)
