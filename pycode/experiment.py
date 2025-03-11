"""
Often an experiment consists of a set of different phases (basal or
stimulation at the time) recorded on different files. This module is an attempt
of implementation of a protocol for defining experiment and represent them as
a represent them a succession of phases stored in different files.

Each phase of an experiment must provide those metadata in some way:
- its position in the succession of recordings
- its type (basal or stimulation)
- its duration
- an identifier for the batch used (optional)
- the DIV of the batch (optional)
"""

from typing import Callable, List, Optional
from pycode import PyPhase
from pycode.converting_rules import ConvertingValues, PhaseType
from pathlib import Path
from os import listdir


class Phase:
    def __init__(
        self,
        filepath: Path,
        converting_rule: Optional[Callable[[str], ConvertingValues]] = None,
    ):
        self.filepath = filepath
        self.handler: PyPhase = PyPhase(f"{filepath}")
        self.duration = self.handler.datalen() / self.handler.sampling_frequency()
        self.position = -1
        self.cell_condition = None
        self.phase_type = PhaseType.UNKNOWN
        self.stim_param = None

        if converting_rule is not None:
            try:
                converting_value = converting_rule(f"{filepath.name}")
                self.position = converting_value.i
                self.cell_condition = converting_value.cond
                self.phase_type = converting_value.t
                self.stim_param = converting_value.s_cond

            except Exception as e:
                print(e)

    def __str__(self):
        return f"""
Path: {self.filepath}
Position: {self.position}
Cell condition: {self.cell_condition}
Duration: {self.duration}
Phase type: {PhaseType.from_int(self.phase_type)}
Stimulation parameter: {self.stim_param}
"""


class Experiment:
    def __init__(
        self,
        root_folder: Path,
        converting_rule: [Callable[[str], ConvertingValues]] = None,
    ):
        self.phases: List[Phase] = []

        # cicle for all phases in the folder

        for phase_file in listdir(root_folder):
            if phase_file.endswith(".h5"):
                self.phases.append(
                    Phase(
                        root_folder.joinpath(phase_file),
                        lambda p: converting_rule(p, "41599", "Cardio", "5"),
                    )
                )

        # order phases by position

        self.phases = sorted(self.phases, key=lambda p: p.position)
