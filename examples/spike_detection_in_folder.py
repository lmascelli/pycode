import pycode as pc
from pycode.converting_rules import PhaseType
from os import listdir
from pathlib import Path
from scipy.signal import butter, filtfilt


FOLDERPATH = Path("/home/leonardo/Documents/unige/data/12-04-2024/39480_DIV77/raw/")
HIGH_PASS_FREQUENCY = 100

for filename in listdir(FOLDERPATH):
    if filename.endswith(".h5"):
        phase = pc.PyPhase(f"{FOLDERPATH.joinpath(filename)}")
        n_channels = len(phase.channels())
        for i, channel in enumerate(phase.channels()):
            print(f"{i}/{n_channels}")
            print("READING")
            data = phase.raw_data(channel)
            print("FILTERING")
            ORDER = 3
            nyquist_frequency = 0.5 * phase.sampling_frequency()
            high = HIGH_PASS_FREQUENCY / nyquist_frequency
            b, a = butter(ORDER, high, btype="high", analog=False, output="ba")
            actual_data = filtfilt(b, a, data)

            print("PEAK DETECTION")
            threshold = pc.operations.spike_detection.compute_threshold(
                actual_data, phase.sampling_frequency(), 8, 1e-7
            )
            peak_times, peak_values = pc.operations.spike_detection.spike_detection(
                actual_data,
                phase.sampling_frequency(),
                threshold,
                2e-3,
                2e-3,
            )
            print(f"Found {len(peak_times)} spikes for channel {channel.label()}")
            phase.set_peak_train(channel, (peak_times, peak_values))
