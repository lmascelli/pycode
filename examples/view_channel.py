import pycode as pc
import matplotlib.pyplot as plt
from matplotlib.patches import Rectangle

BASEPATH = "/home/leonardo/Documents/unige/data/Cardio/11-02-2025/41599/0002_US_50.h5"
WITH_DIGITAL = True
WITH_SPIKES = True
CHANNEL = "23"

phase = pc.PyPhase(BASEPATH)
channel = next(filter(lambda c: CHANNEL in c.label(), phase.channels()))
if channel is not None:
    data = phase.raw_data(channel)
    _, ax = plt.subplots(1, 1)
    ax.plot(data)
    if WITH_SPIKES:
        peak_times, peak_values = phase.peak_train(channel)
        ax.scatter(peak_times, peak_values, color="red")

    if WITH_DIGITAL:
        digital = phase.digital(0)
        intervals = pc.operations.get_digital_intervals(digital)
        ymin, ymax = ax.get_ylim()
        for start, end in intervals:
            ax.add_patch(
                Rectangle(
                    (start, ymin),
                    end - start,
                    ymax - ymin,
                    fill=True,
                    alpha=0.3,
                    linewidth=3,
                    color="brown",
                )
            )

    plt.show()
