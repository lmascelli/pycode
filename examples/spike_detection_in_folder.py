import pycode as pc
from pycode.operations import compute_threshold, spike_detection
from os import listdir

BASEPATH = "/home/leonardo/Documents/unige/data/Cardio/11-02-2025/41599"
NDEV = 8
PEAK_DURATION = 3e-3
PEAK_DISTANCE = 100e-3

for file in listdir(BASEPATH):
    if file.endswith(".h5"):
        phase = pc.PyPhase(f"{BASEPATH}/{file}")
        sampling_frequency = phase.sampling_frequency()
        for channel in phase.channels():
            data = phase.raw_data(channel)
            threshold = compute_threshold(data, sampling_frequency, NDEV)
            peak_times, peak_values = spike_detection(
                data, sampling_frequency, threshold, PEAK_DURATION, PEAK_DISTANCE
            )
            phase.set_peak_train(channel, (peak_times, peak_values))
