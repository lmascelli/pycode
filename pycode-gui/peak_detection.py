from typing import Any, Dict
from forms.peak_detection_tab import Ui_PeakDetection
from memory import Memory
from pycode.operations.spike_detection import compute_threshold, spike_detection
from pycode.operations.cleaning import clear_peaks_over_threshold

from PySide6 import QtCore as qtc
from PySide6 import QtWidgets as qtw
from PySide6 import QtGui as qtg

from pathlib import Path


class Preset:
    def __init__(self, val: Dict[str, Any]):
        try:
            self.n_std = float(val["n_std"])
            self.peak_lifetime = float(val["peak lifetime"])
            self.peak_distance = float(val["refractary period"])
        except Exception as e:
            print(e)


class PeakDetection(qtw.QWidget, Ui_PeakDetection):
    def __init__(self, phase_id: Path):
        self.phase_id = phase_id
        super().__init__()
        self.setupUi(self)
        self.btn_compute.clicked.connect(self.compute_peak_trains_ptsd)
        self.btn_save.clicked.connect(self.save_peaks)

    def compute_peak_trains_ptsd(self):
        phase_handler = Memory.get_phase_handler(self.phase_id)
        channels = phase_handler.channels()
        sampling_frequency = phase_handler.sampling_frequency()
        global ndevs, peak_duration, peak_distance
        global min_threshold
        ndevs = None
        peak_duration = None
        peak_distance = None
        artifact_threshold = None
        min_threshold = None
        # These are mandatory parameters so they can't fail
        try:
            ndevs = float(self.edt_n_std.text())
            peak_duration = float(self.edt_peak_lifetime.text())
            peak_distance = float(self.edt_refractary.text())
            min_threshold = float(self.edt_min_threshold.text())
        except Exception as e:
            print(e)

        # These is an optional parameters so it can fail
        try:
            artifact_threshold = float(self.edt_artifact.text())
        except Exception as e:
            print(e)

        for i, channel in enumerate(channels):
            self.lbl_output.setText(
                self.lbl_output.toPlainText()
                + "\n"
                + f"{i + 1}/{len(channels)}\ngroup: {channel.group()}\t\t\tlabel: {channel.label()}"
            )
            cursor = qtg.QTextCursor(self.lbl_output.textCursor())
            cursor.movePosition(qtg.QTextCursor.End)
            self.lbl_output.setTextCursor(cursor)
            qtc.QCoreApplication.processEvents()

            data = phase_handler.raw_data(channel)
            threshold = compute_threshold(
                data, sampling_frequency, ndevs, min_threshold
            )
            global peak_times, peak_values
            peak_times, peak_values = spike_detection(
                data, sampling_frequency, threshold, peak_duration, peak_distance
            )
            if artifact_threshold is not None:
                peak_times, peak_values = clear_peaks_over_threshold(
                    peak_times, peak_values, artifact_threshold
                )

            Memory.set_phase_peak_train(
                self.phase_id, channel, (peak_times, peak_values)
            )

    def save_peaks(self):
        phase_handler = Memory.get_phase_handler(self.phase_id)
        for channel in phase_handler.channels():
            phase_handler.set_peak_train(
                channel,
                Memory.get_phase_peak_train(self.phase_id, channel),
            )
