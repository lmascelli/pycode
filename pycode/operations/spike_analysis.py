from typing import List, Optional, Tuple
import numpy as np

from ..pycode import (
    PyChannel,
    PyPhase,
    subsample_range as py_subsample_range,
    count_peaks_in_intervals as py_count_peaks_in_intervals,
)

from .digital import get_digital_intervals


def count_peaks_in_intervals(
    peak_times: List[int], intervals: List[Tuple[int, int]]
) -> List[int]:
    """
    Count the peaks in a list of intervals. STARTING and ENDING
    value of the intervals must be provided in sample number!!!
    """
    return py_count_peaks_in_intervals(peak_times, intervals)


def subsample_range(
    peaks: List[int], starting_sample: int, bin_size: int, n_bins: int
) -> List[int]:
    """
    Counts the spikes in N_BINS intervals of width BIN_SIZE starting from
    STARTING_SAMPLE.

    @Parameters:
    - peaks: the list of peak times
    - starting_sample: the first sample of the first interval
    - bin_size: the width of the window in which count peaks
    - n_bins: the number of intervals where to count

    @Returns:
    A list with the number of the peaks found in each interval
    """
    return py_subsample_range(peaks, starting_sample, bin_size, n_bins)


def mfr_trend(
    phase: PyPhase, channel: PyChannel, bin_size: int, is_digit: Optional[bool] = None
) -> Tuple[List[int], List[float]]:
    """
    Compute the trend of the MFR (mean firing rate) for a channel in a phase.
    If the phase has a digital signal it uses it to syncronize the bin start.
    This function checks the presence of a digital signal to determine if a
    phase is a stimulation one or not. To manually set that use the IS_DIGIT
    parameter.

    @Parameters:
    - phase
    - channel
    - bin_size: in samples
    - is_digit: if None automatically determine if use the digital or not.

    @Returns:
    Three lists, the first with the starting time of each interval and the
    second with the MFR of each interval and the third is a boolean array
    with a True if the interval is related to a stimulation time, False if
    not.
    """
    if is_digit is None:
        if phase.n_digitals() == 1:
            is_digit = True
        else:
            is_digit = False

    peak_times, _ = phase.peak_train(channel)

    bin_duration = bin_size / phase.sampling_frequency()

    if not is_digit:
        datalen = phase.datalen()
        n_bins = int(datalen / bin_size)
        counts = subsample_range(peak_times, 0, int(bin_size), n_bins)
        return (
            list(map(lambda x: x / bin_duration, counts)),
            [0 + i * bin_size for i in range(n_bins)],
            [False] * n_bins,
        )
    else:
        """
        1. allocate the return lists
        2. find the digital intervals
        3. find the head duration
        4. compute the n_bins for the head duration
        5. subsample the head
        6. find the intervals durations of stimulation and not stimulation
        7. compute the n_bins for those duration
        8. subsample the ranges
        9. compute how much of those bins are under stimulation
        10. concatenate the result in the return list
        11. return the result
        """

        mfr_trend = []
        mfr_interval_starts = []
        mfr_interval_type = []

        digital_intervals = get_digital_intervals(phase.digital(0))
        first_interval_start = digital_intervals[0][0]
        # Head part
        head_duration = first_interval_start - 1
        if head_duration > 0:
            n_bins = int(head_duration / bin_size)
            counts = subsample_range(peak_times, 0, int(bin_size), n_bins)
            mfr_trend += list(map(lambda x: x / bin_duration, counts))
            mfr_interval_starts += [0 + i * bin_size for i in range(n_bins)]
            mfr_interval_type += [False] * n_bins

        for i, (start, end) in enumerate(digital_intervals):
            stim_duration = end - start + 1
            non_stim_duration = (
                digital_intervals[i + 1][0] - end + 1
                if i != len(digital_intervals) - 1
                else phase.datalen() - end + 1
            )

            # Stimulation part
            n_bins = int(stim_duration / bin_size)
            counts = subsample_range(peak_times, start, int(bin_size), n_bins)
            mfr_trend += list(map(lambda x: x / bin_duration, counts))
            mfr_interval_starts += [start + i * bin_size for i in range(n_bins)]
            mfr_interval_type += [True] * int(n_bins)

            # Non stimulation part
            n_bins = int(non_stim_duration / bin_size)
            counts = subsample_range(peak_times, end, int(bin_size), n_bins)
            mfr_trend += list(map(lambda x: x / bin_duration, counts))
            mfr_interval_starts += [start + i * bin_size for i in range(n_bins)]
            mfr_interval_type += [False] * int(n_bins)

        return (mfr_trend, mfr_interval_starts, mfr_interval_type)


def channel_psth(
    phase: PyPhase,
    channel: PyChannel,
    bin_time_duration: float,
    psth_duration: float,
    digital_intervals: List[Tuple[int, int]],
) -> np.ndarray:
    """
    Compute the PSTH of a channel and returns a list with the count of the
    spikes in each bin averaged by the length of the psth.

    @Parameters
    - phase: the Phase of interest
    - channel
    - bin_time_duration: the duration of the bin IN SECONDS
    - psth_duration: the duration of the whole psth IN SECONDS
    - digital_intervals: the list of (start, end) intervals
    """

    # OPEN THE PYCODE_RS HANDLER FOR THE DATA
    sampling_frequency = phase.sampling_frequency()
    bin_size = int(
        sampling_frequency * bin_time_duration
    )  # this round the size of a bin to the lower integer

    n_bins = int(psth_duration / bin_time_duration)  # number of bin after the stimulus

    global res
    res = np.zeros(n_bins)  # variable to accumulate the psth

    peak_times, _ = phase.peak_train(channel, None, None)

    for interval in digital_intervals:
        found_peaks = subsample_range(
                peak_times,
                interval[0],
                bin_size,
                n_bins,
            )
        res = np.add(
            res,
            found_peaks,
        )

    return res  # TODO: average by n_bins     --> Code here: / n_bins


def psth(
    phase: PyPhase,
    bin_time_duration: float,
    psth_duration: float,
    excluded_channels: List[PyChannel] = [],
) -> np.ndarray:
    """
    Compute the PSTH ociaoooooooo :):
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

    return res / (n_channels * len(digital_intervals) * bin_time_duration)
