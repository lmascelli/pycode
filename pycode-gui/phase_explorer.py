import sys
import os

from PySide6 import QtCore as qtc
from PySide6 import QtWidgets as qtw

from forms.phase_explorer import Ui_PhaseExplorer

from pycode import PyPhase
from pycode.utils import rasterplot

import matplotlib.pyplot as plt

parent_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
resources_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), "./resources"))
sys.path.append(parent_dir)
sys.path.append(resources_dir)

from channel_viewer import ChannelViewer
from peak_detection import PeakDetection

class ChannelsModel(qtc.QAbstractTableModel):
    def __init__(self, phase: PyPhase):
        super().__init__()
        self.channels = {}
        for channel in phase.channels():
            peak_train = phase.peak_train(channel)
            self.channels[channel] = len(peak_train[0])

    def columnCount(self, parent):
        return 3 

    def rowCount(self, parent):
        return len(self.channels)

    def data(self, index: qtc.QModelIndex, role=qtc.Qt.ItemDataRole.DisplayRole):
        if role==qtc.Qt.ItemDataRole.DisplayRole:
            channel = list(self.channels.keys())[index.row()]
            match index.column():
                case 0:
                    return channel.label() 
                case 1:
                    return self.channels[channel]
                case 2:
                    return channel.group() 
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
                case 2:
                    return "Channel Group"
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
        self.btn_peak_detection.clicked.connect(self.open_peak_detection)

    def open_channel(self, arg: qtc.QModelIndex):
        channel_label = f"{self.channels_model.itemData(arg.sibling(arg.row(), 0))[0]}"
        channel_group = f"{self.channels_model.itemData(arg.sibling(arg.row(), 2))[0]}"
        global channel_index
        channel_index = None
        for channel in self.phase.channels():
            if channel.group() == channel_group and channel.label() == channel_label:
                print(f"group: {channel.group()} label: {channel.label()}")
                channel_index = channel
        if channel is not None:
            self.parent().parent().addTab(ChannelViewer(self.phase, channel), f"{channel.group()}-{channel.label()} viewer")

    def rasterplot(self):
        fig = plt.figure()
        ax = fig.subplots(1)
        rasterplot(self.phase, ax)
        plt.show()

    def open_peak_detection(self):
        self.parent().parent().addTab(PeakDetection(self.phase), "Peak Detection")
