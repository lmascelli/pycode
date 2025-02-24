from pathlib import Path
from forms.channel_viewer import Ui_ChannelViewer
from PySide6 import QtWidgets as qtw
from PySide6 import QtCore as qtc

from pycode import PyChannel
from canvas import MplCanvas


class PlotData:
    def __init__(self):
        pass


class ChannelViewer(qtw.QWidget, Ui_ChannelViewer):
    def __init__(self, phase_id: Path, start_channel: PyChannel):
        self.phase = PyCodeGui.get_phase(phase_id)
        self.start_channel = start_channel
        super().__init__()
        self.setupUi(self)
        self.canvas = MplCanvas(1, 1)
        self.grp_plots.layout().addWidget(self.canvas)
        self.draw_raw_data()

        self.btn_reset.clicked.connect(self.draw_raw_data)

    def draw_raw_data(self, index: int = 0):
        self.data = self.phase.handler.raw_data(self.start_channel)
        axe = self.canvas.axes[index]
        axe.clear()
        axe.plot(self.data)
        match self.chk_peaks.checkState():
            case qtc.Qt.CheckState.Checked:
                peak_times, peak_values = self.phase.peak_train[self.start_channel]
                axe.scatter(peak_times, peak_values, color = "r")
            case _:
                pass
        self.canvas.canvas.draw()
