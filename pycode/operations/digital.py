from typing import List, Tuple
from ..pycode import (
    get_digital_intervals as py_get_digital_intervals,
)
import numpy as np

def get_digital_intervals(digital: List[int]) -> List[Tuple[int, int]]:
    return py_get_digital_intervals(digital)


def create_digital_input(data_len, 
                         sf, 
                         start_time, 
                         on_time, 
                         off_time ): 
    
    """
    Creates an artificial digital signal based on stimulation period 
    
    ----------
    
    Parameters
    ----------
    data_len : int
        (samples)
    sf : float
        Sampling frequency - in Hz (n. samples/s).
    start_time : float
        Starting time of stimulation (in seconds).
    on_time : float
        Duration of on period of the stimulation (in seconds).
    off_time : float
        Duration of off period of the stimulation (in seconds).
    
    -------
    Returns
    -------
    list[float]

    """
    
    digital_data = np.zeros(shape=(data_len, 1), dtype=np.float32) 
    start_samples = start_time*sf
    start_idx = int(start_samples)
    on_samples = on_time*sf
    on_idx = int(on_samples)
    off_samples = off_time*sf
    off_idx = int(off_samples)
    tot_idx = on_idx + off_idx
    num_stim = int(np.floor((data_len - start_idx)/tot_idx))
    for i in range(num_stim):
        stim_start = int(start_idx + i * tot_idx)
        stim_end = int(stim_start + on_idx)
        digital_data[stim_start:stim_end] = 1
        
    last_stim_sample = int(start_idx + num_stim * tot_idx)
    end_last_stim_sample = int(last_stim_sample + on_idx)
    if last_stim_sample >= data_len:
        return digital_data.T.tolist()[0]
    else: 
        if end_last_stim_sample >= data_len:
            digital_data[last_stim_sample : data_len] = 1
        else: 
            digital_data[last_stim_sample : end_last_stim_sample] = 1
        return digital_data.T.tolist()[0]