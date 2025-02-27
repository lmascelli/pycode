from pathlib import Path
import pycode as pc
from os import listdir
from pprint import pp

BASEDIR = Path("/home/leonardo/Documents/unige/data/Cardio/11-02-2025/41599/")
EXCLUDED_CHANNELS = ["15", "31", "47"]

results = {}

for phase_file in sorted(listdir(BASEDIR)):
    if phase_file.endswith(".h5"):
        phase_file = BASEDIR.joinpath(phase_file)
        phase_results = {}
        phase = pc.PyPhase(f"{phase_file}")
        datalen = phase.datalen() / phase.sampling_frequency()
        with_digital = "US" in str(phase_file)
        global intervals, digital_duration
        intervals = None
        digital_duration = None
        if with_digital:
            intervals = pc.operations.get_digital_intervals(phase.digital(0))
            digital_duration = sum(map(lambda i: i[1]-i[0], intervals)) / phase.sampling_frequency()
        for channel in filter(lambda c: pc.utils.check_excluded_channel(c, EXCLUDED_CHANNELS), phase.channels()):
            channel_results = {}
            peak_times, peak_values = phase.peak_train(channel)
            channel_results["n_spikes"] = len(peak_times)
            channel_results["mfr"] = len(peak_times)/datalen
            if with_digital:
                channel_results["mfr_digital"] = sum(pc.operations.count_peaks_in_intervals(peak_times, intervals)) / digital_duration
            phase_results[f"{channel.label()}"] = channel_results
        results[f"{phase_file.name}"] = phase_results
            
for result in results:
    pp(results[result])
    input(">> ")
