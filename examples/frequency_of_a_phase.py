import pycode as pc
import matplotlib.pyplot as plt
import numpy as np
from scipy.signal import find_peaks

total = False

basepath = "/home/leonardo/Documents/unige/data/Cardio/11-02-2025/41599/0005_basale.h5"
phase = pc.PyPhase(basepath)
sampling_frequency = phase.sampling_frequency()
EXCLUDED_CHANNELS = ["15", ]

if not total:
    channel = next(filter(lambda c: "45" in c.label(), phase.channels()))
    data = phase.raw_data(channel)
    peak_times = find_peaks(data, height = 0.0005)[0]
    peak_values = [data[int(x)] for x in peak_times]
    plt.plot(data)
    plt.scatter(peak_times, peak_values, color="red")
    plt.show()

else:
    total_peak = []

    for channel in phase.channels():
        peak_times, peak_values = phase.peak_train(channel)
        for excluded in EXCLUDED_CHANNELS:
            if excluded not in channel.label():
                total_peak = total_peak + peak_times
                diff = np.diff(peak_times)
                diff_mean = np.mean(diff)
                diff_std = np.std(diff)
                freq = diff_mean/sampling_frequency
                std = diff_std/sampling_frequency
                print(f"{channel.label()} -> mean: {freq} std: {std}")

    diff = np.diff(total_peak)
    diff_mean = np.mean(diff)
    diff_std = np.std(diff)
    freq = diff_mean/sampling_frequency
    std = diff_std/sampling_frequency
    print(freq, std)
