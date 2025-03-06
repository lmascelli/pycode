from typing import List, Tuple
from ..pycode import (
    get_digital_intervals as py_get_digital_intervals,
)

def get_digital_intervals(digital: List[int]) -> List[Tuple[int, int]]:
    return py_get_digital_intervals(digital)
