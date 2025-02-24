from typing import Optional

import sys
import os
from PySide6 import QtWidgets as qtw
from pathlib import Path

parent_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))
resources_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), './resources'))
sys.path.append(parent_dir)
sys.path.append(resources_dir)

from forms.main_window import Ui_MainWindow  # noqa: E402
from phase_explorer import PhaseExplorer  # noqa: E402


class PyCodeMainWindow(qtw.QMainWindow, Ui_MainWindow):
    def __init__(self):
        super().__init__()
        self.setupUi(self)

        self.action_Quit.triggered.connect(self.close)
        self.action_Open.triggered.connect(self.open_phase_test)
        self.tabWidget.tabCloseRequested.connect(self.close_tab)

    def add_tab(self, widget: qtw.QWidget, label: str, index: Optional[int] = None):
        if index is None:
            index = self.tabWidget.count()
        self.tabWidget.insertTab(index, widget, label)

    def close_tab(self, index: int):
        label = self.tabWidget.tabText(index)
        if label == "Welcome":
            self.tabWidget.removeTab(index)
        else:
            self.tabWidget.removeTab(index)

    def open_phase_test(self):
        file_name = Path('/home/leonardo/Documents/unige/data/Cardio/11-02-2025/41599/0002_US_50.h5')
        self.add_tab(PhaseExplorer(f"{file_name}"), file_name.name)

    def open_phase(self):
        file_name = Path(qtw.QFileDialog.getOpenFileName(None, "Select the phase file")[0])
        phase_explorer_name = file_name.name
        phase_explorer = PhaseExplorer(f"{file_name}")
        if phase_explorer is not None:
            self.add_tab(phase_explorer, phase_explorer_name)


if __name__=='__main__':
    import __init__  # noqa: F401
    
    app = qtw.QApplication(sys.argv)
    app.setStyle("windows")

    window = PyCodeMainWindow()
    window.showMaximized()
    window.show()

    sys.exit(app.exec())
