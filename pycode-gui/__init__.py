import builtins
from pycode import PyPhase
from typing import Any, Dict, Optional
from pathlib import Path

class PyCodeGuiPhase:
    def __init__(self, file_path: Path):
        try:
            self.handler = PyPhase(file_path)
        except Exception as e:
            print(e)
            return None
        self.peak_train = None
        self.burst_train = None

class _PyCodeGui:
    def __init__(self):
        self.variables: Dict[str, Any] = {
            # Add a collection of the handlers for the opened phases The key for
            # each phase is the Path of the hdf5 file containing the data and
            # with Path i mean an instance of the Path class in the pathlib
            # module not just a string with the path of the file
            "PHASES": {},
        }

    def set(self, variable: str, property_value: Any):
        self.variables[variable] = property_value

    def get(self, variable: str) -> Optional[Any]:
        if variable in self.variables:
            return self.variables[variable]
        else:
            return None

    def add_phase(self, file_path: Path):
        handler = PyCodeGuiPhase(file_path)
        if handler is not None:
            self.variables["PHASES"][file_path] = handler
        else:
            print(f"Failed opening the phase at {file_path}")

    def get_phase(self, file_path: Path):
        return self.get("PHASES")[file_path]

setattr(builtins, "PyCodeGui", _PyCodeGui())
