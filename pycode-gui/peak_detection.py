from multiprocessing import Process
from forms.peak_detection_tab import Ui_PeakDetection
import pycode as pc
from pycode.operations import (
    compute_threshold,
    clear_peaks_over_threshold,
    spike_detection,
)

from PySide6 import QtCore as qtc
from PySide6 import QtWidgets as qtw
from PySide6 import QtGui as qtg

from pathlib import Path

PhaseID = Path


class PeakDetection(qtw.QWidget, Ui_PeakDetection):
    def __init__(self, phase_id: PhaseID):
        self.phase_id = phase_id
        super().__init__()
        self.setupUi(self)
        self.btn_compute.clicked.connect(self.compute_peak_trains_ptsd)
        self.btn_save.clicked.connect(self.save_peaks)

    def compute_peak_trains_ptsd(self):
        phase = PyCodeGui.get_phase(self.phase_id)["handler"]
        channels = phase.channels()
        sampling_frequency = phase.sampling_frequency()
        global ndevs, peak_duration, peak_distance
        ndevs = None
        peak_duration = None
        peak_distance = None
        artifact_threshold = None
        try:
            ndevs = float(self.edt_n_std.text())
            peak_duration = float(self.edt_peak_lifetime.text())
            peak_distance = float(self.edt_refractary.text())
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

            data = phase.raw_data(channel)
            threshold = compute_threshold(data, sampling_frequency, ndevs)
            global peak_times, peak_values
            peak_times, peak_values = spike_detection(
                data, sampling_frequency, threshold, peak_duration, peak_distance
            )
            if artifact_threshold is not None:
                peak_times, peak_values = clear_peaks_over_threshold(peak_times, peak_values, artifact_threshold)

            PyCodeGui.get_phase(self.phase_id)["computed_peaks"][
                (channel.group(), channel.label())
            ] = (peak_times, peak_values)

    def save_peaks(self):
        phase = PyCodeGui.get_phase(self.phase_id)["handler"]
        for channel in phase.channels():
            phase.set_peak_train(
                channel,
                PyCodeGui.get_phase(self.phase_id)["computed_peaks"][
                    (channel.group(), channel.label())
                ],
            )
