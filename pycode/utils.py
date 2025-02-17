from typing import Optional
from .pycode import PyChannel, PyPhase 
import matplotlib.pyplot as plt
from matplotlib.figure import Figure
import numpy as np

def plot_raw_with_spikes(phase: PyPhase,
                         channel: PyChannel,
                         start: Optional[int] = None,
                         end: Optional[int] = None,
                         ):
    raw_data = phase.raw_data(channel, start, end)
    peaks = phase.peak_train(channel, start, end)

    x_start = 0
    if start is not None:
        x_start = start
    x_end = phase.datalen()
    if end is not None:
        x_end = end

    times = np.arange(x_start, x_end - 1, 1)

    plt.plot(times, raw_data, color = PyCode.get("RAW_DATA_COLOR"))
    plt.stem(peaks[0], peaks[1],
             linefmt=f"{PyCode.get('SPIKES_COLOR_LINE')}",
             markerfmt=f"{PyCode.get('SPIKES_COLOR_HEAD')}")

def rasterplot(phase: PyPhase, ax):
    labels = phase.labels()
    spikes = []
    for label in labels:
        spikes.append(phase.peak_train(label)[0][:])
    ax.eventplot(spikes)
