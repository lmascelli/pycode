from pathlib import Path
import pycode as pc

STIMULUS_DURATION = 500e-3 # seconds
OFF_DURATION = 4 # seconds

EXPERIMENT_FOLDER = Path("/home/leonardo/Documents/unige/data/Cardio/11-02-2025/41599")

experiment = pc.experiment.Experiment(EXPERIMENT_FOLDER, pc.converting_rules.rule_order_type_cond)

for phase in experiment.phases:
    print(phase)
