from pathlib import Path
import pycode as pc
from pycode.converting_rules import PhaseType
import numpy as np
from pprint import pp
import csv


def result_to_csv(result_dict, file_name):
    """
    Store the results in a csv file
    """

    headers = list(result_dict.keys())

    row_number = max(len(value) for value in result_dict.values())

    rows = []
    for i in range(row_number):
        row = []
        for value in result_dict.values():
            if i < len(value):
                row.append(value[i])
            else:
                row.append("")
        rows.append(row)

    with open(file_name, "w", newline="") as file_csv:
        writer = csv.writer(file_csv)
        writer.writerow(headers)
        for i in range(row_number):
            writer.writerow(rows[i])


if __name__ == "__main__":
    ##################################################
    #                   PARAMETERS                   #
    ##################################################

    STIMULUS_DURATION = 500e-3  # seconds
    OFF_DURATION = 4  # seconds

    EXPERIMENT_FOLDER = Path(
        "/home/leonardo/Documents/unige/data/Cardio/11-02-2025/41599"
    )
    EXCLUDED_CHANNELS = [
        "15",
        "31",
    ]

    THRESHOLD_PROBE_INTERVAL = 5  # seconds

    ##################################################
    #              DATA INITIALIZATION               #
    ##################################################

    experiment = pc.experiment.Experiment(
        EXPERIMENT_FOLDER, pc.converting_rules.rule_order_type_cond
    )

    # if for some reason the phases have a different number of stimulation pulses
    # the minimum of them should be used as parameter so here i calculate how many
    # intervals each stimulation phase has and take the mimimum of those
    number_of_intervals = None

    for phase in experiment.phases:
        if phase.phase_type is PhaseType.STIM:
            digital = phase.handler.digital(0)
            intervals = pc.operations.digital.get_digital_intervals(digital)
            if number_of_intervals is None or len(intervals) < number_of_intervals:
                number_of_intervals = len(intervals)

    result = {
        "time (s)": [],
        "phase_types": [],
    }

    elapsed_time = 0

    ##################################################
    #                  DATA ITERATION                #
    ##################################################

    for phase in experiment.phases:
        handler = phase.handler
        channels = pc.utils.create_excluded_list(EXCLUDED_CHANNELS, handler.channels())
        sampling_frequency = phase.handler.sampling_frequency()

        # PROBING THE THRESHOLD VALUES
        threshold_probe_interval_in_samples = (
            THRESHOLD_PROBE_INTERVAL * sampling_frequency
        )
        thresholds = {}

        # Here i create the keys in the result return dict for each channel
        for channel in channels:
            if channel.label() not in result.keys():
                result[channel.label()] = []
        match phase.phase_type:
            case PhaseType.BASAL:
                # During the basal phases the signal should be sub sampled by interval with
                # duration of NUMBER_OF_INTERVALS * STIMULUS_DURATION

                sampling_frequency = handler.sampling_frequency()
                phase_duration = (
                    handler.datalen() / sampling_frequency
                )  # find the duration in seconds
                sample_duration = STIMULUS_DURATION * number_of_intervals
                n_intervals = int(phase_duration / (sample_duration))

                sample_duration_in_samples = sample_duration * sampling_frequency

                this_phase_intervals = []

                for i in range(n_intervals):
                    start_time = int(i * sample_duration_in_samples)
                    end_time = int((i + 1) * sample_duration_in_samples)
                    result["time (s)"].append(
                        start_time / sampling_frequency + elapsed_time
                    )
                    result["phase_types"].append(PhaseType.from_int(phase.phase_type))
                    this_phase_intervals.append((start_time, end_time))
                elapsed_time += phase_duration

                for channel in channels:
                    peak_times, _ = handler.peak_train(channel)
                    peak_counts = pc.operations.spike_analysis.count_peaks_in_intervals(
                        peak_times, this_phase_intervals
                    )
                    for peak in peak_counts:
                        result[channel.label()].append(
                            peak / (STIMULUS_DURATION * n_intervals)
                        )

            case PhaseType.STIM:
                # Instead, during the stimulation phases each pulse (on + off of
                # stimulus) must be sampled by interval of STIMULUS_DURATION and the
                # corrispondent samples must be summed togheter so that their total
                # duration still is NUMBER_OF_INTERVALS * STIMULUS_DURATION. It is
                # in pratical a PSTH of the channel

                # get the number of digital channels. if it's different from 1 an error has occurred
                # during the recording phase
                n_digital = handler.n_digitals()
                if n_digital != 1:
                    exit(
                        f"ERROR: the stimulation phase has {n_digital} digital channels (grazie MultiChannel)"
                    )

                # read the digital channel
                digital = handler.digital(0)
                # get the interval timestamps where the stimulation is active
                digital_intervals = pc.operations.digital.get_digital_intervals(digital)

                n_intervals = int(
                    (STIMULUS_DURATION + OFF_DURATION) / STIMULUS_DURATION
                )

                for i in range(n_intervals):
                    result["time (s)"].append(i * STIMULUS_DURATION + elapsed_time)
                    result["phase_types"].append(PhaseType.from_int(phase.phase_type))

                for channel in channels:
                    psth = pc.operations.spike_analysis.channel_psth(
                        handler,
                        channel,
                        STIMULUS_DURATION,
                        STIMULUS_DURATION + OFF_DURATION,
                        digital_intervals,
                    ).tolist()
                    for value in psth:
                        result[channel.label()].append(
                            value / (STIMULUS_DURATION * n_intervals)
                        )

            case PhaseType.UNKNOWN:
                print(f"Unknown type of phase: {phase.filepath}")

    result_to_csv(result, "test.csv")
