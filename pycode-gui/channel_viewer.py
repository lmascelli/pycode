from pathlib import Path
from forms.channel_viewer import Ui_ChannelViewer
from PySide6 import QtWidgets as qtw
from PySide6 import QtCore as qtc
from matplotlib.patches import Rectangle
import numpy as np

from pycode import PyChannel
from pycode.operations.digital import get_digital_intervals
from canvas import MplCanvas
from memory import Memory


class PlotData:
    def __init__(self):
        pass


class ChannelViewer(qtw.QWidget, Ui_ChannelViewer):
    def __init__(self, phase_id: Path, start_channel: PyChannel):
        self.phase_id = phase_id
        self.start_channel = start_channel
        super().__init__()
        self.setupUi(self)
        self.canvas = MplCanvas(1, 1)
        self.grp_plots.layout().addWidget(self.canvas)
        self.draw_raw_data()

        self.btn_reset.clicked.connect(self.draw_raw_data)

    def draw_raw_data(self, index: int = 0):
        # get drawing contest
        axe = self.canvas.axes[index]
        axe.clear()

        # get data
        phase_handler = Memory.get_phase_handler(self.phase_id)
        data = np.array(phase_handler.raw_data(self.start_channel))
        datalen = phase_handler.datalen()
        sampling_frequency = phase_handler.sampling_frequency()

        # adjust axis scale
        # X axis
        global ascisse, x_scale, y_scale

        x_unit = self.cmb_x_unit.currentText()

        match x_unit:
            case "Sample":
                x_scale = 1
                ascisse = np.linspace(0, datalen, datalen)
            case "Seconds":
                x_scale = 1 / sampling_frequency
                ascisse = np.linspace(0, datalen / sampling_frequency, datalen)
            case "Milliseconds":
                x_scale = 1 / sampling_frequency * 1000
                ascisse = np.linspace(0, datalen / sampling_frequency * 1000, datalen)

        axe.set_xlabel(x_unit)

        y_unit = self.cmb_y_unit.currentText()
        match y_unit:
            case "Volt":
                y_scale = 1
            case "Millivolt":
                y_scale = 1e3
                data *= y_scale
            case "Microvolt":
                y_scale = 1e6
                data *= y_scale

        axe.set_ylabel(y_unit)
        axe.ticklabel_format(useOffset=False)

        # plot data
        axe.plot(ascisse, data)

        match self.chk_peaks.checkState():
            case qtc.Qt.CheckState.Checked:
                peak_times, peak_values = Memory.get_phase_peak_train(
                    self.phase_id, self.start_channel
                )
                peak_times = np.array(peak_times) * x_scale
                peak_values = np.array(peak_values) * y_scale
                axe.scatter(peak_times, peak_values, color="r")
            case _:
                pass
        match self.chk_digital.checkState():
            case qtc.Qt.CheckState.Checked:
                if phase_handler.n_digitals() == 1:
                    digital = phase_handler.digital(0)
                    intervals = get_digital_intervals(digital)
                    ylim = axe.get_ylim()
                    for interval in intervals:
                        rectangle = Rectangle(
                            (interval[0] * x_scale, ylim[0]),
                            (interval[1] - interval[0]) * x_scale,
                            ylim[1] - ylim[0],
                            color="red",
                            alpha=0.1,
                        )
                        axe.add_patch(rectangle)
                pass
            case _:
                pass

        # adjust axis limits

        global xlim_, ylim_
        xlim_ = None
        ylim_ = None

        try:
            xlim_min = float(self.edt_x_min.text())
            xlim_max = float(self.edt_x_max.text())
            xlim_ = [xlim_min, xlim_max]
        except Exception as e:
            print(e)

        try:
            ylim_min = float(self.edt_y_min.text())
            ylim_max = float(self.edt_y_max.text())
            ylim_ = [ylim_min, ylim_max]
        except Exception as e:
            print(e)

        if xlim_ is not None:
            axe.set_xlim(xlim_)

        if ylim_ is not None:
            axe.set_ylim(ylim_)

        self.canvas.canvas.draw()
