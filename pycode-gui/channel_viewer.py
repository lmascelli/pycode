from typing import List
from forms.channel_viewer import Ui_ChannelViewer
from PySide6 import QtWidgets as qtw

from pycode import PyPhase

from canvas import MplCanvas

class PlotData:
    def __init__(self):
        pass


class ChannelViewer(qtw.QWidget, Ui_ChannelViewer):
    def __init__(self, phase: PyPhase, start_label: str):
        super().__init__()
        self.setupUi(self)
        self.canvas = MplCanvas(1, 1)
        self.grp_plots.layout().addWidget(self.canvas)
        self.draw_raw_data(phase.raw_data(start_label))

    def draw_raw_data(self, data: List[float], index: int = 0):
        self.canvas.axes[index].plot(data)
