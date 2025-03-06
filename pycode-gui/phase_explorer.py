import sys
import os
from pathlib import Path

from PySide6 import QtCore as qtc
from PySide6 import QtWidgets as qtw

from memory import Memory
from forms.phase_explorer import Ui_PhaseExplorer

from pycode.utils import rasterplot

import matplotlib.pyplot as plt

parent_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
resources_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), "./resources"))
sys.path.append(parent_dir)
sys.path.append(resources_dir)

from channel_viewer import ChannelViewer  # noqa: E402
from peak_detection import PeakDetection  # noqa: E402
from psth import Psth  # noqa: E402


class ChannelsModel(qtc.QAbstractTableModel):
    def __init__(self, phase_id: Path):
        super().__init__()
        self.phase_id = phase_id
        self.channels = {}
        self.computeModel()

    def computeModel(self):
        phase_handler = Memory.get_phase_handler(self.phase_id)
        self.duration = phase_handler.datalen() / phase_handler.sampling_frequency()
        if not Memory.peak_train_is_initialized(self.phase_id):
            Memory.peak_train_initialize(self.phase_id)
            for channel in phase_handler.channels():
                peak_train = phase_handler.peak_train(channel)
                Memory.set_phase_peak_train(self.phase_id, channel, peak_train)
                self.channels[channel] = len(peak_train[0])
        else:
            for channel in phase_handler.channels():
                peak_train = phase_handler.peak_train(channel)
                Memory.set_phase_peak_train(self.phase_id, channel, peak_train)
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
                    return "MFR [Hz]"
                case _:
                    return f"{section}"


class PhaseExplorer(qtw.QWidget, Ui_PhaseExplorer):
    def __init__(self, file_path: Path):
        super().__init__()
        self.setupUi(self)

        self.phase_id = file_path
        phase = Memory.get_phase_handler(file_path)
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
        self.btn_psth.clicked.connect(self.psth)
        
    def open_channel(self, arg: qtc.QModelIndex):
        channel_label = f"{self.channels_model.itemData(arg.sibling(arg.row(), 1))[0]}"
        channel_group = int(f"{self.channels_model.itemData(arg.sibling(arg.row(), 0))[0]}")
        phase = Memory.get_phase_handler(self.phase_id)
        channel = next(filter(lambda c: c.group() == channel_group and c.label() == channel_label, phase.channels()))
        if channel is not None:
            channel_viewer = ChannelViewer(self.phase_id, channel)
            Memory.add_tab(
                channel_viewer,
                f"{channel.group()}-{channel.label()} viewer",
                self.phase_id,
            )

    def rasterplot(self):
        fig = plt.figure()
        ax = fig.subplots(1)
        phase = Memory.get_phase_handler(self.phase_id)
        rasterplot(phase, ax, fig)
        plt.show()

    def open_peak_detection(self):
        peak_detection = PeakDetection(self.phase_id)
        Memory.add_tab(peak_detection, "Peak Detection", self.phase_id)

    def psth(self):
        psth_tab = Psth(self.phase_id)
        Memory.add_tab(psth_tab, "Psth", self.phase_id) 
