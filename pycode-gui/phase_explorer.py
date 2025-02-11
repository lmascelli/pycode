import sys
import os

from PySide6 import QtCore as qtc
from PySide6 import QtWidgets as qtw

from forms.phase_explorer import Ui_PhaseExplorer

from pycode import PyPhase
from pycode.operations import compute_threshold, spike_detection
from pycode.utils import rasterplot

import matplotlib.pyplot as plt

parent_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
resources_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), "./resources"))
sys.path.append(parent_dir)
sys.path.append(resources_dir)

from channel_viewer import ChannelViewer

class ChannelsModel(qtc.QAbstractTableModel):
    def __init__(self, phase: PyPhase):
        super().__init__()
        self.channels = {}
        for label in phase.labels():
            peak_train = phase.peak_train(label)
            self.channels[label] = len(peak_train[0])

    def columnCount(self, parent):
        return 2 

    def rowCount(self, parent):
        return len(self.channels)

    def data(self, index: qtc.QModelIndex, role=qtc.Qt.ItemDataRole.DisplayRole):
        if role==qtc.Qt.ItemDataRole.DisplayRole:
            label = list(self.channels.keys())[index.row()]
            match index.column():
                case 0:
                    return label
                case 1:
                    return self.channels[label]
        else:
            return None

    def headerData(
        self,
        section: int,
        orientation,
        role=qtc.Qt.ItemDataRole.DisplayRole,
    ):
        if orientation == qtc.Qt.Orientation.Horizontal and role == qtc.Qt.ItemDataRole.DisplayRole:
            match section:
                case 0:
                    return "Channel Label"
                case 1:
                    return "Spike Count"
                case _:
                    return f"{section}"


class PhaseExplorer(qtw.QWidget, Ui_PhaseExplorer):
    def __init__(self, file_name: str):
        super().__init__()
        self.setupUi(self)

        self.phase = PyPhase(file_name)
        self.lbl_name.setText(file_name)
        self.lbl_date.setText(self.phase.date())
        self.lbl_duration.setText(
            f"{self.phase.datalen() / self.phase.sampling_frequency()} seconds"
        )
        self.lbl_sampling_frequency.setText(f"{self.phase.sampling_frequency()}")
        self.channels_model = ChannelsModel(self.phase)
        self.tbl_channels.setModel(self.channels_model)
        self.tbl_channels.doubleClicked.connect(self.open_channel)

        self.btn_rasterplot.clicked.connect(self.rasterplot)
        self.btn_peak_detection.clicked.connect(self.compute_peak_trains)

    def open_channel(self, arg: qtc.QModelIndex):
        label = f"{self.channels_model.itemData(arg.sibling(arg.row(), 0))[0]}"
        self.parent().parent().addTab(ChannelViewer(self.phase, label), f"{label} viewer")

    def rasterplot(self):
        fig = plt.figure()
        ax = fig.subplots(1)
        rasterplot(self.phase, ax)
        plt.show()

    def compute_peak_trains(self):
        labels = self.phase.labels()
        sampling_frequency = self.phase.sampling_frequency()
        for label in labels:
            data = self.phase.raw_data(label)
            threshold = compute_threshold(data, sampling_frequency, 8)
            peak_times, peak_values = spike_detection(data, sampling_frequency, threshold, 2e-3, 2e-3)
            self.phase.set_peak_train(label, (peak_times, peak_values))
