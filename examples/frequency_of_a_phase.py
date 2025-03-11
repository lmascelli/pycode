import pycode as pc
import numpy as np

BASEPATH = "/home/leonardo/Documents/unige/data/Cardio/11-02-2025/41599/0005_basale.h5"


def FrequencyOfAPhase(basepath):
    phase = pc.PyPhase(basepath)
    sampling_frequency = phase.sampling_frequency()
    EXCLUDED_CHANNELS = ["31", "15", "47"]

    total_peak = []

    for channel in phase.channels():
        peak_times, peak_values = phase.peak_train(channel)
        global compute
        compute = True
        for excluded in EXCLUDED_CHANNELS:
            if excluded in channel.label():
                compute = False
        if compute:
            diff = np.diff(peak_times)
            total_peak = total_peak + list(diff)
            diff_mean = np.mean(diff)
            diff_std = np.std(diff)
            freq = diff_mean / sampling_frequency
            std = diff_std / sampling_frequency
            print(f"{channel.label()} -> mean: {freq} std: {std}")
        else:
            print(f"EXCLUDED: {channel.label()}")

    diff_mean = np.mean(total_peak)
    diff_std = np.std(total_peak)
    freq = diff_mean / sampling_frequency
    std = diff_std / sampling_frequency
    print(f"MEAN NETWORK FREQUENCY: {freq}, STD: {std}")


if __name__ == "__main__":
    FrequencyOfAPhase(BASEPATH)
