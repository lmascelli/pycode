from pathlib import Path
from typing import Callable, List, Optional, Tuple
from pycode import PyChannel, PyPhase
from os import abort
from PySide6 import QtWidgets as qtw


"""
This is a collection of static methods to interact with the
memory of the various elements managed by the GUI.
"""

def _get_pycode_data():
    return PyCodeData  # noqa: F821

class Memory:
    def __init__(self):
        print("Memory class is not meant to be initialized")
        abort()

    def register_main_window(window):
        _get_pycode_data()["MainWindow"] = window

    def get_main_window():
        window = _get_pycode_data()["MainWindow"]
        if window is not None:
            return window
        else:
            print("Trying to retrieve the main window with no window registered")
            abort()

    ##################################################
    # TAB MANAGEMENT
        
    def get_tabs():
        return _get_pycode_data()["Tabs"]

    def add_tab(widget: qtw.QWidget, label: str, phase_id: Path, index: Optional[int] = None,
                close_callback: Optional[Callable] = None):
        window = Memory.get_main_window()
        if index is None:
            index = window.tabWidget.count()
        
        Memory.get_tabs()[index] = (phase_id, close_callback)
        Memory.add_reference(phase_id)
        window.tabWidget.insertTab(index, widget, label)

    def close_tab(index: int):
        window = Memory.get_main_window()
        label = window.tabWidget.tabText(index)
        if label == "Welcome":
            window.tabWidget.removeTab(index)
        else:
            phase_id, close_callback = Memory.get_tabs()[index]
            if close_callback is not None:
                close_callback()
            window.tabWidget.removeTab(index)
            Memory.remove_reference(phase_id)

    ##################################################
    # DATA MANAGEMENT
        
    def add_phase(file_path: Path) -> bool:
        """
        Add a phase to the global space. If the phase isn't already present it
        load it from the filesystem, initialize the peak and burst values and
        returns True, otherwise it leaves it untouched and returns False.
        """
        if file_path not in _get_pycode_data()["PHASES"].keys() or _get_pycode_data()["PHASES"][file_path] is None:
            handler = PyPhase(f"{file_path}")
            _get_pycode_data()["PHASES"][file_path] = {
                "handler": handler,
                "peak_train": None,
                "burst_train": None,
                "references": 0,
            }
            return True
        else:
            return False

    def add_reference(file_path: Path):
        """
        Increment the reference count for the given phase.
        """
        _get_pycode_data()["PHASES"][file_path]["references"] += 1

    def remove_reference(file_path: Path):
        """
        Decrement the reference count for the given phase and delete the phase data
        when there are no more references.
        """
        _get_pycode_data()["PHASES"][file_path]["references"] -= 1
        if _get_pycode_data()["PHASES"][file_path]["references"] == 0:
            print(f"Removed reference {file_path}")
            _get_pycode_data()["PHASES"][file_path] = None

    def get_phase_handler(file_path: Path):
        """
        Returns the raw PyPhase handler to the phase.
        """
        return _get_pycode_data()["PHASES"][file_path]["handler"]

    def peak_train_is_initialized(file_path: Path) -> bool:
        """
        Return if the peak_train data of the phase has been already initialized
        """
        return _get_pycode_data()["PHASES"][file_path]["peak_train"] is not None

    def peak_train_initialize(file_path: Path):
        """
        Return if the peak_train data of the phase has been already initialized
        """
        _get_pycode_data()["PHASES"][file_path]["peak_train"] = {}

    def set_phase_peak_train(
        file_path: Path, channel: PyChannel, peak_train: Tuple[List[int], List[float]]
    ):
        """
        Set the value of the peak train of the channel specified to the
        peak_train value
        """
        _get_pycode_data()["PHASES"][file_path]["peak_train"][channel] = peak_train

    def get_phase_peak_train(file_path: Path, channel: PyChannel):
        """
        Get the value of the peak train of the channel specified
        """
        return _get_pycode_data()["PHASES"][file_path]["peak_train"][channel]
