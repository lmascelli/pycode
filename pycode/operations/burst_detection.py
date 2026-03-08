from typing import List
from ..pycode import (
    burst_detection as py_burst_detection,
)
from pycode.operations.cleaning import clear_peaks_over_threshold
import numpy as np

def burst_detection(
    peak_train: List[int],
    sampling_frequency: float,
    cutoff: float,
) -> None:
    return py_burst_detection(peak_train, sampling_frequency, cutoff)

def networkburst_detection(phase,
                           threshold,
                           cutoff,
                           min_spikes_per_burst,
                           min_bursts,
                           min_mfr, 
                           min_channels,
                           reference = ""):
    """
    Returns a list of tuple containing network burst starts and ends
    
    ----------
    Parameters
    ----------
    phase : pycode.PyPhase

    threshold : float
        Value over which spikes are considered artifacts
        (in Volt).
    cutoff : float
        in seconds
    min_spikes_per_burst : int

    reference : str
        Label of reference electrode (Optional).
    min_mfr : float
        Minimum mean firing rate per second to consider a channel active.
    min_bursts : float
        Minimum number of bursts per minute to consider a channel bursting.
    min_channels : int
        Minimum active channels to consider a network bursting.
        
    -------
    Returns
    -------
    network_bursts : list[tuple[int, int]]

    """
    channels = phase.channel()
    duration = phase.datalen() / phase.sampling_frequency()
    burst_count_per_min = []
    bursts_per_channel = []
    for channel in channels:
            pt, pv = phase.peak_train(channel)
            npt, npv = clear_peaks_over_threshold(pt, pv, threshold)
            sampling_frequency = phase.sampling_frequency()
            mfr = len(npt) / duration
            if min_mfr < mfr:
                burst_data = burst_detection(npt, sampling_frequency, cutoff)
                if burst_data is not None and channel.label() != reference:
                    burst_filtered = (
                        [burst_data[0][i] for i in range(len(burst_data[2])) if burst_data[2][i] >= min_spikes_per_burst],
                        [burst_data[1][i] for i in range(len(burst_data[2])) if burst_data[2][i] >= min_spikes_per_burst],
                        [burst_data[2][i] for i in range(len(burst_data[2])) if burst_data[2][i] >= min_spikes_per_burst]
                        )
                    burst_count = len(burst_filtered)
                else:
                    burst_count = 0
            else:
                burst_count = 0
            burst_count_per_min = burst_count / (duration / 60)
            bursts = []
            if burst_count_per_min >= min_bursts:
                np_burst_filtered = [np.array(burst_filtered[0]), np.array(burst_filtered[1]), np.array(burst_filtered[2])]
                for i in range(len(np_burst_filtered[0])):
                    bursts.append((np_burst_filtered[0][i], np_burst_filtered[1][i]))
            bursts_per_channel.append(bursts)
    
    all_bursts = []
    for ch_idx, bursts in enumerate(bursts_per_channel):
        for b in bursts:
            all_bursts.append((b[0], b[1], ch_idx))
    all_bursts.sort(key=lambda x: x[0])
    network_bursts = []
    current_cluster = []
    current_start = None
    current_end = None
    current_channels = set()
    for start, end, ch in all_bursts:
        if current_cluster == []:
            current_cluster = [(start, end, ch)]
            current_start = start
            current_end = end
            current_channels = {ch}
        else:
            if start <= current_end:
                current_cluster.append((start, end, ch))
                current_end = max(current_end, end)
                current_channels.add(ch)
            else:
                if len(current_channels) >= min_channels:
                    network_bursts.append((current_start, current_end))
                current_cluster = [(start, end, ch)]
                current_start = start
                current_end = end
                current_channels = {ch}
    if len(current_channels) >= min_channels:
        network_bursts.append((current_start, current_end))
    return network_bursts