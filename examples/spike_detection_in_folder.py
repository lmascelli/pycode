import pycode as pc
from pycode.converting_rules import PhaseType
from os import listdir
from pathlib import Path

EXPERIMENT_FOLDER = Path(
    "/home/leonardo/Documents/unige/data/12-04-2024/39480_DIV77/raw/"
)

NDEV = 8
PEAK_DURATION = 3e-3
PEAK_DISTANCE = 100e-3
THRESHOLD_PROBE_INTERVAL = 5  # seconds
MINIMUM_THRESHOLD = 20e-6  # Volt
MAXIMUM_THRESHOLD = 350e-6  # Volt
MINIMUM_MFR = 0.1  # Peak/Seconds
MAXIMUM_MFR = 40  # Peak/Seconds

EXCLUDED_CHANNELS = [
    "E-00155 15",
]

if __name__ == "__main__":
    experiment = pc.experiment.Experiment(
        EXPERIMENT_FOLDER, pc.converting_rules.rule_order_type_cond
    )

    for i, phase in enumerate(experiment.phases):
        print(f"Phase: {i + 1}/{len(experiment.phases)}  PEAK DETECTION")
        handler = phase.handler
        channels = pc.utils.create_excluded_list(EXCLUDED_CHANNELS, handler.channels())
        sampling_frequency = handler.sampling_frequency()

        for c_n, channel in enumerate(channels):
            print(f"Channel: {c_n + 1}/{len(channels)}")
            data = handler.raw_data(channel)
            # PROBING THE THRESHOLD VALUES
            thresholds_data = pc.operations.spike_detection.probe_threshold(
                data,
                sampling_frequency,
                NDEV,
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
                    NDEV,
                    PEAK_DURATION,
                    PEAK_DISTANCE,
                    THRESHOLD_PROBE_INTERVAL,
                )
            )
            if phase.phase_type is PhaseType.STIM:
                digital = handler.digital(0)
                digital_intervals = pc.operations.digital.get_digital_intervals(digital)
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
