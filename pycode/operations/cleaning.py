from typing import List, Tuple

from ..pycode import find_peaks_around_points as py_find_peaks_around_points


def find_peaks_around_points(
    peak_times: List[int], points: List[int], window_half_size: int
) -> List[int]:
    return py_find_peaks_around_points(peak_times, points, window_half_size)

def remove_indices(original_list, indices_to_remove):
    """
    Removes elements from the original_list at the indices specified in indices_to_remove.

    Args:
        original_list: The list from which elements will be removed.
        indices_to_remove: A list of indices to remove.

    Returns:
        A new list with the specified elements removed.
    """

    # Sort the indices in reverse order to avoid index shifting issues
    indices_to_remove.sort(reverse=True)

    new_list = list(original_list)  # Create a copy to avoid modifying the original list

    for index in indices_to_remove:
        if 0 <= index < len(new_list): # Check if the index is valid
            del new_list[index]
        else:
            print(f"Warning: Index {index} is out of range.") #inform the user about invalid index

    return new_list

def clear_peaks_around_points(
    peak_times: List[int],
    peak_values: List[float],
    points: List[int],
    window_half_size: int,
) -> Tuple[List[int], List[float]]:
    indices = find_peaks_around_points(peak_times, points, window_half_size)
    new_peak_times = remove_indices(peak_times, indices)
    new_peak_values = remove_indices(peak_values, indices)

    return (new_peak_times, new_peak_values)

def clear_peaks_over_threshold(
    peak_times: List[int], peak_values: List[float], upper_threshold
) -> Tuple[List[int], List[float]]:
    new_peak_times, new_peak_values = [], []
    for i, value in enumerate(peak_values):
        if abs(value) < abs(upper_threshold):
            new_peak_times.append(peak_times[i])
            new_peak_values.append(value)
    return (new_peak_times, new_peak_values)
