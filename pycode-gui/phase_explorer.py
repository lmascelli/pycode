# from typing import Optional

import sys
import os

from PySide6 import QtCore as qtc
from PySide6 import QtWidgets as qtw
# from PySide6 import QtGui as qtg

from forms.phase_explorer import Ui_PhaseExplorer

from pycode import PyPhase

parent_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
resources_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), "./resources"))
sys.path.append(parent_dir)
sys.path.append(resources_dir)



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

        phase = PyPhase(file_name)
        self.lbl_name.setText(file_name)
        self.lbl_date.setText(phase.date())
        self.lbl_duration.setText(
            f"{phase.datalen() / phase.sampling_frequency()} seconds"
        )
        self.lbl_sampling_frequency.setText(f"{phase.sampling_frequency()}")
        self.tbl_channels.setModel(ChannelsModel(phase))
