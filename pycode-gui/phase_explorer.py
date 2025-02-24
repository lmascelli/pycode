import sys
import os
from pathlib import Path

from PySide6 import QtCore as qtc
from PySide6 import QtWidgets as qtw

from forms.phase_explorer import Ui_PhaseExplorer

from pycode.utils import rasterplot

import matplotlib.pyplot as plt

parent_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
resources_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), "./resources"))
sys.path.append(parent_dir)
sys.path.append(resources_dir)

from channel_viewer import ChannelViewer  # noqa: E402
from peak_detection import PeakDetection  # noqa: E402


class ChannelsModel(qtc.QAbstractTableModel):
    def __init__(self, phase_id: Path):
        super().__init__()
        self.phase_id = phase_id
        self.channels = {}
        self.computeModel()

    def computeModel(self):
        phase = PyCodeGui.get_phase(self.phase_id)
        self.duration = phase.handler.datalen() / phase.handler.sampling_frequency()
        if phase.peak_train is not None:
            for channel in phase.handler.channels():
                peak_train = phase.peak_train[channel]
                self.channels[channel] = len(peak_train[0])
        else:
            phase.peak_train = {}
            for channel in phase.handler.channels():
                peak_train = phase.handler.peak_train(channel)
                phase.peak_train[channel] = peak_train
                self.channels[channel] = len(peak_train[0])

    def columnCount(self, parent):
        return 4

    def rowCount(self, parent):
        return len(self.channels)

    def data(self, index: qtc.QModelIndex, role=qtc.Qt.ItemDataRole.DisplayRole):
        if role == qtc.Qt.ItemDataRole.DisplayRole:
            channel = list(self.channels.keys())[index.row()]
            match index.column():
                case 0:
                    return channel.group()
                case 1:
                    return channel.label()
                case 2:
                    return self.channels[channel]
                case 3:
                    return self.channels[channel] / self.duration
        else:
            return None

    def headerData(
        self,
        section: int,
        orientation,
        role=qtc.Qt.ItemDataRole.DisplayRole,
    ):
        if (
            orientation == qtc.Qt.Orientation.Horizontal
            and role == qtc.Qt.ItemDataRole.DisplayRole
        ):
            match section:
                case 0:
                    return "Channel Group"
                case 1:
                    return "Channel Label"
                case 2:
                    return "Spike Count"
                case 3:
                    return "MFR"
                case _:
                    return f"{section}"


class PhaseExplorer(qtw.QWidget, Ui_PhaseExplorer):
    def __init__(self, file_path: Path):
        super().__init__()
        self.setupUi(self)

        PyCodeGui.add_phase(file_path)
        self.phase_id = file_path
        phase = PyCodeGui.get_phase(file_path).handler
        self.lbl_name.setText(f"{self.phase_id}")
        self.lbl_date.setText(phase.date())
        self.lbl_duration.setText(
            f"{phase.datalen() / phase.sampling_frequency()} seconds"
        )
        self.lbl_sampling_frequency.setText(f"{phase.sampling_frequency()}")
        self.channels_model = ChannelsModel(file_path)
        self.tbl_channels.setModel(self.channels_model)
        self.tbl_channels.doubleClicked.connect(self.open_channel)

        self.btn_rasterplot.clicked.connect(self.rasterplot)
        self.btn_peak_detection.clicked.connect(self.open_peak_detection)

    def open_channel(self, arg: qtc.QModelIndex):
        channel_label = f"{self.channels_model.itemData(arg.sibling(arg.row(), 1))[0]}"
        channel_group = int(f"{self.channels_model.itemData(arg.sibling(arg.row(), 0))[0]}")
        print(channel_label)
        print(channel_group)
        phase = PyCodeGui.get_phase(self.phase_id).handler
        channel = next(filter(lambda c: c.group() == channel_group and c.label() == channel_label, phase.channels()))
        if channel is not None:
            self.parent().parent().addTab(
                ChannelViewer(self.phase_id, channel),
                f"{channel.group()}-{channel.label()} viewer",
            )

    def rasterplot(self):
        fig = plt.figure()
        ax = fig.subplots(1)
        phase = PyCodeGui.get_phase(self.phase_id).handler
        rasterplot(phase, ax)
        plt.show()

    def open_peak_detection(self):
        self.parent().parent().addTab(PeakDetection(self.phase_id), "Peak Detection")
