from forms.psth import Ui_Psth
from pycode.operations import psth as py_psth
from memory import Memory

from PySide6 import QtWidgets as qtw
import numpy as np
from pathlib import Path
from canvas import MplCanvas


class Psth(qtw.QWidget, Ui_Psth):
    def __init__(self, phase_id: Path):
        self.phase_id = phase_id
        super().__init__()
        self.setupUi(self)
        self.btn_compute.clicked.connect(self.compute_psth)
        self.canvas = MplCanvas(1, 1)
        self.grp_plot.layout().addWidget(self.canvas)

    def compute_psth(self):
        try:
            bin_dur = float(self.edt_bin_duration.text())
            total_dur = float(self.edt_total_duration.text())
            psth_dur = float(self.edt_stim_duration.text())
        except Exception as e:
            print(e)

        phase = Memory.get_phase_handler(self.phase_id)
        psth_x = np.arange(0, total_dur, bin_dur)
        psth = py_psth(phase, bin_dur, total_dur)
        n_stim = int(psth_dur / bin_dur)
        x_stim = psth_x[:n_stim]
        y_stim = psth[:n_stim]
        x_no_stim = psth_x[n_stim:]
        y_no_stim = psth[n_stim:]

        axe = self.canvas.axes[0]
        axe.clear()
        axe.bar(x_stim, y_stim, width=bin_dur / 1.5, color="red")
        axe.bar(x_no_stim, y_no_stim, width=bin_dur / 1.5, color="blue")
        axe.set_xlabel("Time [s]")
        axe.set_ylabel("Spike rate [Hz]")
        axe.legend(labels=["US ON", "US OFF"])
        self.canvas.canvas.draw()
