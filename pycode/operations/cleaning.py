from typing import List, Tuple

def clear_peaks_over_threshold(
    peak_times: List[int], peak_values: List[float], upper_threshold
) -> Tuple[List[int], List[float]]:
    new_peak_times, new_peak_values = [], []
    for i, value in enumerate(peak_values):
        if abs(value) < abs(upper_threshold):
            new_peak_times.append(peak_times[i])
            new_peak_values.append(value)
    return (new_peak_times, new_peak_values)
