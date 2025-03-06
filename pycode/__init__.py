from typing import Any, Dict, Optional
import atexit
import builtins

from .pycode import (
    PyChannel,  # noqa: F401
    PyPhase,  # noqa: F401
    init,  # noqa: F401
    close,  # noqa: F401
    # logspace,  # noqa: F401
    # lowess,  # noqa: F401
)

init()
atexit.register(close)


class _PyCode:
    def __init__(self):
        self.variables: Dict[str, Any] = {}

    def set(self, variable: str, property_value: Any):
        self.variables[variable] = property_value

    def get(self, variable: str) -> Optional[Any]:
        if variable in self.variables:
            return self.variables[variable]
        else:
            return None


setattr(builtins, "PyCode", _PyCode())

from . import operations  # noqa: E402,F401
from . import utils  # noqa: E402,F401
from . import settings  # noqa: E402,F401
