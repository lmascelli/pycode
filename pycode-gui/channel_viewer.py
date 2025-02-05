from forms.channel_viewer import Ui_ChannelViewer
from PySide6 import QtWidgets as qtw

from pycode import PyPhase

from canvas import MplCanvas

class ChannelViewer(qtw.QWidget, Ui_ChannelViewer):
    def __init__(self, phase: PyPhase, start_label: str):
        super().__init__()
        self.setupUi(self)
        canvas = MplCanvas(1, 1)
        self.grp_plots.layout().addWidget(canvas)
        canvas.axes[0].plot(phase.raw_data(start_label))
