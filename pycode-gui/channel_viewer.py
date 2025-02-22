from typing import List
from forms.channel_viewer import Ui_ChannelViewer
from PySide6 import QtWidgets as qtw
from PySide6 import QtCore as qtc

from pycode import PyChannel, PyPhase

from canvas import MplCanvas

from pathlib import Path
PhaseID = Path

class PlotData:
    def __init__(self):
        pass


class ChannelViewer(qtw.QWidget, Ui_ChannelViewer):
    def __init__(self, phase_id: PhaseID, start_channel: PyChannel):
        phase = PyCodeGui.get_phase(phase_id)["handler"]
        super().__init__()
        self.setupUi(self)
        self.canvas = MplCanvas(1, 1)
        self.grp_plots.layout().addWidget(self.canvas)
        self.data = phase.raw_data(start_channel)
        peak_times, peak_values = phase.peak_train(start_channel)
        self.peak_times = peak_times
        self.peak_values = peak_values
        self.draw_raw_data()

        self.btn_reset.clicked.connect(self.draw_raw_data)

    def draw_raw_data(self, index: int = 0):
        axe = self.canvas.axes[index]
        axe.clear()
        axe.plot(self.data)
        match self.chk_peaks.checkState():
            case qtc.Qt.CheckState.Checked:
                axe.scatter(self.peak_times, self.peak_values, color = "r")
            case _:
                pass
        self.canvas.canvas.draw()
