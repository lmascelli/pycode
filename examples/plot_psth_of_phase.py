import pycode as pc
import matplotlib.pyplot as plt
import numpy as np

PHASEFILE = "/home/leonardo/Documents/unige/data/Cardio/11-02-2025/41599/0002_US_50.h5"
BIN_DUR = 0.05  # Seconds
PSTH_DUR = 0.25
TOTAL_DUR = 3  # Seconds
EXCLUDED_CHANNELS_LABELS = ["15", "47"]

phase = pc.PyPhase(PHASEFILE)
excluded_channels = []
psth_x = np.arange(0, TOTAL_DUR, BIN_DUR)
psth = pc.operations.psth(phase, BIN_DUR, TOTAL_DUR)
n_stim = int(PSTH_DUR / BIN_DUR)
x_stim = psth_x[:n_stim]
y_stim = psth[:n_stim]
x_no_stim = psth_x[n_stim:]
y_no_stim = psth[n_stim:]
plt.bar(x_stim, y_stim, width=BIN_DUR / 1.5, color="red")
plt.bar(x_no_stim, y_no_stim, width=BIN_DUR / 1.5, color="blue")
plt.xlabel("Time [s]")
plt.ylabel("")
plt.legend(labels=["US ON", "US OFF"])
plt.show()
