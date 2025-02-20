from forms.peak_detection_tab import Ui_PeakDetection
import pycode as pc
from pycode.operations import compute_threshold, spike_detection

from PySide6 import QtCore as qtc
from PySide6 import QtWidgets as qtw


class PeakDetection(qtw.QWidget, Ui_PeakDetection):
    def __init__(self, phase: pc.PyPhase):
        self.phase = phase
        super().__init__()
        self.setupUi(self)
        self.btn_compute.clicked.connect(self.compute_peak_trains_ptsd)
        self.btn_save.clicked.connect(self.save_peaks)

    def compute_peak_trains_ptsd(self):
        channels = self.phase.channels()
        sampling_frequency = self.phase.sampling_frequency()
        global ndevs, peak_duration, peak_distance
        ndevs = None
        peak_duration = None
        peak_distance = None
        try:
            ndevs = float(self.edt_n_std.text())
            peak_duration = float(self.edt_peak_lifetime.text())
            peak_distance = float(self.edt_refractary.text())
        except Exception as e:
            print(e)

        for i, channel in enumerate(channels):
            self.lbl_output.setText(
                self.lbl_output.toPlainText()
                + "\n"
                + f"{i}/{len(channels)}\ngroup: {channel.group()}\t\t\tlabel: {channel.label()}"
            )
            data = self.phase.raw_data(channel)
            threshold = compute_threshold(data, sampling_frequency, ndevs)
            self.peak_times, self.peak_values = spike_detection(
                data, sampling_frequency, threshold, peak_duration, peak_distance
            )

    def save_peaks(self):
        for channel in self.phase.channels():
            self.phase.set_peak_train(channel, (self.peak_times, self.peak_values))
