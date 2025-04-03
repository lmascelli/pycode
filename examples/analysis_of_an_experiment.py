from pathlib import Path
import pycode as pc
from pycode.converting_rules import PhaseType
import numpy as np
import matplotlib.pyplot as plt
from pprint import pp
import csv


if __name__ == "__main__":
    ##################################################
    #                   PARAMETERS                   #
    ##################################################

    SAVE_CSV = True
    WITH_PEAK_DETECTION = False
    GENERATE_PLOT = True

    STIMULUS_DURATION = 250e-3  # seconds
    OFF_DURATION = 4  # seconds

    EXPERIMENT_FOLDER = Path(
        "/home/leonardo/Documents/unige/data/12-04-2024/39480_DIV77/raw/"
    )
    SAVENAME = "/home/leonardo/Documents/unige/data/12-04-2024/39480_DIV77/39480.csv"

    THRESHOLD_PROBE_INTERVAL = 5  # seconds
    MINIMUM_THRESHOLD = 20e-6  # Volt
    MAXIMUM_THRESHOLD = 350e-6  # Volt
    MINIMUM_MFR = 0.1  # Peak/Seconds
    MAXIMUM_MFR = 40  # Peak/Seconds

    excluded_channels = [
        "E-00155 15",
    ]


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
        "end_time (s)": [],
        "phase_types": [],
    }

    elapsed_time = 0

    ##################################################
    #                  DATA ITERATION                #
    ##################################################
    for i, phase in enumerate(experiment.phases):
        print(f"Phase: {i + 1}/{len(experiment.phases)}  PEAK DETECTION")
        handler = phase.handler
        channels = pc.utils.create_excluded_list(excluded_channels, handler.channels())
        sampling_frequency = handler.sampling_frequency()

        if WITH_PEAK_DETECTION:
            for c_n, channel in enumerate(channels):
                print(f"Channel: {c_n + 1}/{len(channels)}")
                data = handler.raw_data(channel)
                # PROBING THE THRESHOLD VALUES
                thresholds_data = pc.operations.spike_detection.probe_threshold(
                    data,
                    sampling_frequency,
                    9,
                    THRESHOLD_PROBE_INTERVAL,
                )
                for start, end, threshold in thresholds_data:
                    if threshold < MINIMUM_THRESHOLD or threshold > MAXIMUM_THRESHOLD:
                        print(
                            f"Invalid threshold {threshold} found in interval ({start}, {end})"
                        )

                # PEAK DETECTION
                peak_times, peak_values = (
                    pc.operations.spike_detection.spike_detection_moving_threshold(
                        data,
                        sampling_frequency,
                        9,
                        50e-3,
                        50e-3,
                        THRESHOLD_PROBE_INTERVAL,
                    )
                )
                if phase.phase_type is PhaseType.STIM:
                    digital = handler.digital(0)
                    digital_intervals = pc.operations.digital.get_digital_intervals(
                        digital
                    )
                    digital_points = []
                    for interval in digital_intervals:
                        digital_points.append(interval[0])
                        digital_points.append(interval[1])
                    peak_times, peak_values = (
                        pc.operations.cleaning.clear_peaks_around_points(
                            peak_times,
                            peak_values,
                            digital_points,
                            10,  # 10 samples is 1 ms
                        )
                    )
                    peak_times, peak_values = (
                        pc.operations.cleaning.clear_peaks_over_threshold(
                            peak_times,
                            peak_values,
                            MAXIMUM_THRESHOLD,
                        )
                    )
                    handler.set_peak_train(channel, (peak_times, peak_values))
                else:
                    handler.set_peak_train(channel, (peak_times, peak_values))

    # PRELIMINARY DATA ANALYSIS FOR:
    # - THRESHOLD PROBING
    # - EXCLUDING CHANNELS WITH BAD MFR
    for i, phase in enumerate(experiment.phases):
        print(f"Phase: {i + 1}/{len(experiment.phases)}")
        handler = phase.handler
        channels = pc.utils.create_excluded_list(excluded_channels, handler.channels())
        sampling_frequency = handler.sampling_frequency()
        datalen = handler.datalen()

        # LOOK FOR CHANNELS TO EXCLUDE
        phase_duration = (
            handler.datalen() / sampling_frequency
        )  # find the duration in seconds
        for channel in channels:
            mfr = len(handler.peak_train(channel)[0]) / phase_duration
            if mfr < MINIMUM_MFR or mfr > MAXIMUM_MFR:
                print(f"Removed {channel.label()} because it has a MFR of {mfr}")
                excluded_channels.append(channel.label())

    for i, phase in enumerate(experiment.phases):
        print(f"Phase: {i + 1}/{len(experiment.phases)}")
        handler = phase.handler
        sampling_frequency = handler.sampling_frequency()
        channels = pc.utils.create_excluded_list(excluded_channels, handler.channels())
        datalen = handler.datalen()

        # Here i create the keys in the result return dict for each channel
        for channel in channels:
            print(channel.label())
            result[channel.label()] = []

        match phase.phase_type:
            case PhaseType.BASAL:
                # During the basal phases the signal should be sub sampled by interval with
                # duration of NUMBER_OF_INTERVALS * STIMULUS_DURATION

                phase_duration = (
                    datalen / sampling_frequency
                )  # find the duration in seconds

                interval_duration = STIMULUS_DURATION * number_of_intervals
                n_intervals = int(phase_duration / interval_duration)

                interval_duration_in_samples = interval_duration * sampling_frequency

                this_phase_intervals = []

                # here i create an array with the intervals in which compute the MFR
                # of the current phase
                for i in range(n_intervals):
                    start_time = int(i * interval_duration_in_samples)
                    end_time = int((i + 1) * interval_duration_in_samples)
                    if start_time >= datalen or end_time > datalen:
                        print(f"Phase {i} cannot go out of {start_time} or {end_time}")
                        break
                    result["time (s)"].append(
                        start_time / sampling_frequency + elapsed_time
                    )
                    result["end_time (s)"].append(
                        end_time / sampling_frequency + elapsed_time
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
                            peak / (STIMULUS_DURATION * number_of_intervals)
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
                    result["end_time (s)"].append(
                        (i + 1) * STIMULUS_DURATION + elapsed_time
                    )

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
                            value / (STIMULUS_DURATION * len(digital_intervals))
                        )

            case PhaseType.UNKNOWN:
                print(f"Unknown type of phase: {phase.filepath}")

    if GENERATE_PLOT:
        types = result["phase_types"]

        global current_value, current_type, current_plot, current_times
        current_plot = []
        channel_count = 0
        current_value = 0
        current_type = types[0]
        current_time = 0

        for key in result.keys():
            if "time" not in key and "phase_types" not in key:
                channel_count += 1

        for i, t in enumerate(types):
            current_value = 0

            if t is not current_type:
                if current_type == "Basal":
                    times = [
                        t for t in range(current_time, current_time + len(current_plot))
                    ]
                    plt.bar(times, current_plot, color="blue")
                if current_type == "Stimulation":
                    plt.bar(current_time, current_plot[0], color="red")
                    times = [
                        t
                        for t in range(
                            current_time + 1, current_time + len(current_plot)
                        )
                    ]
                    plt.bar(times, current_plot[1:], color="green")
                current_time += len(current_plot)
                current_plot = []

            current_type = types[i]

            pp(f"---------------------------------------------------")
            pp(result.keys())
            for key in result.keys():
                print(len(result[key]))
            pp(f"---------------------------------------------------")

            for key in result.keys():
                if "time" not in key and "phase_types" not in key:
                    print(f"KEY CHECK {key}")
                    current_value += result[key][i]
            current_plot.append(current_value / channel_count)

        if current_type == "Basal":
            times = [t for t in range(current_time, current_time + len(current_plot))]
            plt.bar(times, current_plot, color="blue")
        if current_type == "Stimulation":
            plt.bar(current_time, current_plot[0], color="red")
            times = [
                t for t in range(current_time + 1, current_time + len(current_plot))
            ]
            plt.bar(times, current_plot[1:], color="green")

        plt.ylabel("MFR (Hz)")
        # plt.xlabel("Time (s)")
        plt.show()

    if SAVE_CSV:
        result_to_csv(result, f"{SAVENAME}")
