import builtins
from pycode import PyPhase
from typing import Any, Dict, Optional
from pathlib import Path

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

    def add_phase(self, path: Path):
        handler = PyPhase(f"{path}")
        if handler is not None:
            self.variables["PHASES"][path] = {
                "handler": handler,
                "computed_peaks" : {},
            }
        else:
            print(f"Failed opening the phase at {path}")

    def get_phase(self, path: Path):
        return self.get("PHASES")[path]

setattr(builtins, "PyCodeGui", _PyCodeGui())
