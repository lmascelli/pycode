import sys
import os
from PySide6 import QtWidgets as qtw
from pathlib import Path
from memory import Memory

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
        Memory.register_main_window(self)

        self.action_Quit.triggered.connect(self.close)
        self.action_Open.triggered.connect(self.open_phase_test)
        self.tabWidget.tabCloseRequested.connect(Memory.close_tab)

    def open_phase_test(self):
        file_name = Path('/home/leonardo/Documents/unige/data/Cardio/11-02-2025/41599/0002_US_50.h5')
        Memory.add_phase(file_name)
        phase_explorer = PhaseExplorer(file_name)
        Memory.add_tab(phase_explorer, file_name.name, file_name)

    def open_phase(self):
        file_name = Path(qtw.QFileDialog.getOpenFileName(None, "Select the phase file")[0])
        Memory.add_phase(file_name)
        phase_explorer = PhaseExplorer(file_name)
        if phase_explorer is not None:
            Memory.add_tab(phase_explorer, file_name.name, file_name)


if __name__=='__main__':
    import __init__  # noqa: F401
    
    app = qtw.QApplication(sys.argv)
    # app.setStyle("windows")

    window = PyCodeMainWindow()
    window.showMaximized()
    window.show()

    sys.exit(app.exec())
