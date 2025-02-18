import pycode as pc
import matplotlib.pyplot as plt

BASEPATH = "/home/leonardo/Documents/unige/data/Cardio/11-02-2025/41599/0003_basale.h5"
NDEV = 8
PEAK_DURATION = 3e-3
PEAK_DISTANCE = 100e-3
CHANNEL = "23"

phase = pc.PyPhase(f"{BASEPATH}")
sampling_frequency = phase.sampling_frequency()
channel = next(filter(lambda c: CHANNEL in c.label(), phase.channels()))
if channel is not None:
    data = phase.raw_data(channel)
    threshold = pc.operations.compute_threshold(data, sampling_frequency, NDEV)
    peak_times, peak_values = pc.operations.spike_detection(
        data, sampling_frequency, threshold, PEAK_DURATION, PEAK_DISTANCE
    )

    plt.plot(data)
    plt.scatter(peak_times, peak_values, color="red")
    plt.show()
