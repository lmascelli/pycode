from typing import Callable, Optional
from pathlib import Path
import pycode as pc
import matplotlib.pyplot as plt


def do_analisys(
    basefolder: Path,
    converting_rule: Callable[
        pc.converting_rules.ConvertingValues, [str, Optional[str]]
    ],
    stimulus_on_duration: float,
    stimulus_off_duration: float,
    minimum_mfr: float,
    maximum_mfr: float,
    generate_plot: bool = True,
):
    # allocating the variable to store the result during the computation
    start_times = []
    end_times = []
    phase_types = []
    channels_count = {}

    experiment = pc.experiment.Experiment(
        basefolder, pc.converting_rules.rule_order_type_cond
    )

    # if for some reason the phases have a different number of stimulation pulses
    # the minimum of them should be used as parameter so here i calculate how many
    # intervals each stimulation phase has and take the mimimum of those
    print("Looking for the minimum number of intervals in each stimulation phase...")
    number_of_intervals = None

    for phase in experiment.phases:
        if phase.phase_type is pc.converting_rules.PhaseType.STIM:
            digital = phase.handler.digital(0)
            intervals = pc.operations.digital.get_digital_intervals(digital)
            print(f"{phase.filepath}: {len(intervals)} intervals")
            if number_of_intervals is None or len(intervals) < number_of_intervals:
                number_of_intervals = len(intervals)
    print(f"Minimum number of intervals: {number_of_intervals}")
    print(f"------------------------------")

    # traverse all the phases looking for channels with a mfr under or above the
    # given thresholds. those channels will be excluded in all the computations
    print("Looking for channels with not valid MFR")
    excluded_labels = []
    for i, phase in enumerate(experiment.phases):
        print(
            f"{i + 1}/{len(experiment.phases)} Analysing MFR in phase {phase.filepath}"
        )
        handler = phase.handler
        sampling_frequency = handler.sampling_frequency()
        datalen = handler.datalen()

        phase_duration = datalen / sampling_frequency
        for channel in handler.channels():
            mfr = len(handler.peak_train(channel)[0]) / phase_duration
            if mfr < minimum_mfr or mfr > maximum_mfr:
                print(f"Removed {channel.label()} because it has a MFR of {mfr}")
                if channel.label() not in excluded_labels:
                    excluded_labels.append(channel.label())
    print(f"------------------------------")

    # print all the survived channels and add their keys to the result dict
    print("Survived channels:")
    for channel in handler.channels():
        if channel.label() not in excluded_labels:
            print(channel.label(), end=" ")
            channels_count[channel.label()] = []
    print("\n------------------------------")

    # now for each phase evaluate and add the corresponding time
    # stamps and for each of the valid labels count the spikes in each
    # timestamp

    elapsed_time = 0

    print("Populating the data of the peak counts...")
    for i, phase in enumerate(experiment.phases):
        print(
            f"{i + 1}/{len(experiment.phases)} Counting peaks in phase {phase.filepath}"
        )
        handler = phase.handler
        sampling_frequency = handler.sampling_frequency()
        datalen = handler.datalen()

        match phase.phase_type:
            case pc.converting_rules.PhaseType.BASAL:
                # During the basal phases the signal should be sub sampled by interval with
                # duration of NUMBER_OF_INTERVALS * STIMULUS_DURATION

                phase_duration = datalen / sampling_frequency
                interval_duration = (stimulus_on_duration) * number_of_intervals
                n_intervals = int(phase_duration / interval_duration)
                interval_duration_in_samples = interval_duration * sampling_frequency

                # find the boundaries of each interval in the basal phase
                this_phase_intervals = []
                for j in range(n_intervals):
                    start_time = int(j * interval_duration_in_samples)
                    end_time = int((j + 1) * interval_duration_in_samples)
                    if start_time >= datalen or end_time > datalen:
                        print(f"Phase {i} cannot go out of {start_time} or {end_time}")
                        break
                    start_times.append(start_time / sampling_frequency + elapsed_time)
                    end_times.append(end_time / sampling_frequency + elapsed_time)
                    phase_types.append(phase.phase_type)
                    this_phase_intervals.append((start_time, end_time))
                elapsed_time += phase_duration

                for channel in handler.channels():
                    if channel.label() not in excluded_labels:
                        peak_times, _ = handler.peak_train(channel)
                        peak_counts = (
                            pc.operations.spike_analysis.count_peaks_in_intervals(
                                peak_times, this_phase_intervals
                            )
                        )
                        for peak in peak_counts:
                            channels_count[channel.label()].append(
                                peak / ((stimulus_on_duration) * number_of_intervals)
                            )

            case pc.converting_rules.PhaseType.STIM:
                # Instead, during the stimulation phases each pulse (on + off of
                # stimulus) must be sampled by interval of STIMULUS_DURATION and the
                # corrispondent samples must be summed togheter so that their total
                # duration still is NUMBER_OF_INTERVALS * STIMULUS_DURATION. It is
                # in pratical a PSTH of the channel

                n_digital = handler.n_digitals()
                if n_digital != 1:
                    exit(
                        f"ERROR: the stimulation phase has {n_digital} digital channels (grazie MultiChannel)"
                    )

                # read the digital channel
                digital = handler.digital(0)
                # get the interval timestamps where the stimulation is active
                digital_intervals = pc.operations.digital.get_digital_intervals(digital)

                number_of_bins = int(
                    (stimulus_on_duration + stimulus_off_duration)
                    / stimulus_on_duration
                )

                for j in range(number_of_bins):
                    start_times.append(j * stimulus_on_duration + elapsed_time)
                    end_times.append((j + 1) * stimulus_on_duration + elapsed_time)
                    phase_types.append(phase.phase_type)

                for channel in handler.channels():
                    if channel.label() not in excluded_labels:
                        psth = pc.operations.spike_analysis.channel_psth(
                            handler,
                            channel,
                            stimulus_on_duration,
                            stimulus_on_duration + stimulus_off_duration,
                            digital_intervals,
                        ).tolist()
                        for value in psth:
                            channels_count[channel.label()].append(
                                value / (stimulus_on_duration * len(digital_intervals))
                            )
                elapsed_time += handler.datalen() / handler.sampling_frequency()

            case pc.converting_rules.PhaseType.STIM_EL:
                n_events = handler.n_events()
                if n_events != 2:
                    exit(
                        f"ERROR: the electrical stimulation phase has {n_events} events channels (grazie MultiChannel)"
                    )

                start_events = handler.events(0)
                end_events = [event + 250e-3 for event in start_events]
                electrical_intervals = [
                    (start_events[i], end_events[i]) for i in range(len(start_events))
                ]

                number_of_bins = int(
                    (stimulus_on_duration + stimulus_off_duration)
                    / stimulus_on_duration
                )

                for j in range(number_of_bins):
                    start_times.append(j * stimulus_on_duration + elapsed_time)
                    end_times.append((j + 1) * stimulus_on_duration + elapsed_time)
                    phase_types.append(phase.phase_type)

                electrical_intervals = [
                    (
                        int(value[0] * sampling_frequency * 1e-6),
                        0,
                    )
                    for value in electrical_intervals
                ]

                for channel in handler.channels():
                    if channel.label() not in excluded_labels:
                        psth = pc.operations.spike_analysis.channel_psth(
                            handler,
                            channel,
                            stimulus_on_duration,
                            stimulus_on_duration + stimulus_off_duration,
                            electrical_intervals,
                        ).tolist()
                        for value in psth:
                            channels_count[channel.label()].append(
                                value
                                / (stimulus_on_duration * len(electrical_intervals))
                            )
                elapsed_time += handler.datalen() / handler.sampling_frequency()

            case pc.converting_rules.PhaseType.UNKNOWN:
                print(f"Unknown type of phase: {phase.filepath}")

    if generate_plot:
        global current_value, current_type, current_plot, current_times
        current_plot = []
        channel_count = 0
        current_value = 0
        current_type = phase_types[0]
        current_time = 0

        for key in channels_count.keys():
            channel_count += 1

        for i, t in enumerate(phase_types):
            current_value = 0

            if t is not current_type:
                if current_type == pc.converting_rules.PhaseType.BASAL:
                    times = [
                        t for t in range(current_time, current_time + len(current_plot))
                    ]
                    plt.bar(times, current_plot, color="blue")
                if current_type == pc.converting_rules.PhaseType.STIM:
                    plt.bar(current_time, current_plot[0], color="red")
                    times = [
                        t
                        for t in range(
                            current_time + 1, current_time + len(current_plot)
                        )
                    ]
                    plt.bar(times, current_plot[1:], color="green")

                if current_type == pc.converting_rules.PhaseType.STIM_EL:
                    times = [
                        t for t in range(current_time, current_time + len(current_plot))
                    ]
                    plt.bar(times, current_plot, color="yellow")

                current_time += len(current_plot)
                current_plot = []

            current_type = phase_types[i]

            for key in channels_count.keys():
                current_value += channels_count[key][i]
            current_plot.append(current_value / channel_count)

        if current_type == pc.converting_rules.PhaseType.BASAL:
            times = [t for t in range(current_time, current_time + len(current_plot))]
            plt.bar(times, current_plot, color="blue")
        if current_type == pc.converting_rules.PhaseType.STIM:
            plt.bar(current_time, current_plot[0], color="red")
            times = [
                t for t in range(current_time + 1, current_time + len(current_plot))
            ]
            plt.bar(times, current_plot[1:], color="green")
        if current_type == pc.converting_rules.PhaseType.STIM_EL:
            times = [
                t for t in range(current_time, current_time + len(current_plot))
            ]
            plt.bar(times, current_plot, color="yellow")

        plt.ylabel("MFR (Hz)")
        plt.show()


if __name__ == "__main__":
    BASEFOLDER = Path("/home/leonardo/Documents/unige/data/12-04-2024/39480_DIV77/raw/")
    do_analisys(
        basefolder=BASEFOLDER,
        converting_rule=pc.converting_rules.rule_order_type_cond,
        stimulus_on_duration=0.25,
        stimulus_off_duration=4,
        minimum_mfr=0.1,
        maximum_mfr=40,
    )
