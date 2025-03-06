from typing import List, Optional, Iterable, Tuple
from .pycode import PyChannel, PyPhase
from matplotlib.figure import Axes, Figure
import numpy as np


def create_excluded_list(
    excluded_list: Iterable[str],
    channels: Iterable[PyChannel],
) -> List[PyChannel]:
    """
    Create a list of valid channels excluding from CHANNELS those whose label
    is included in the label() method of the original ones. For example, even
    if you pass '35' as element in the EXCLUDED_LIST and a channel has the
    return value of the label method equal to 'E005-35' the channel will be
    excluded. On the other side, passing a value of 'E005' will probably
    exclude all the channels so be careful of how the list of the excluded
    channels is computed.

    @Parameters
    - excluded_list: an iterable of list of string to be matched as excluded
                     channels
    - channels: an iterable of PyChannel from which exclude some channels

    @Returns
    A list of channel without the excluded ones.
    """

    ret = []
    for channel in channels:
        is_excluded = False
        for excluded in excluded_list:
            if excluded in channel.label():
                is_excluded = True
        if not is_excluded:
            ret.append(channel)
    return ret

def create_excluded_list_with_groups(
    excluded_list: Iterable[Tuple[int, Iterable[str]]],
    channels: Iterable[PyChannel],
) -> List[PyChannel]:
    """
    Create a list of valid channels excluding from CHANNELS those whose label
    is included in the label() method and group corresponds to the value of
    group() of the original ones. For example, even if you pass '35' as element
    in the EXCLUDED_LIST and a channel has the return value of the label method
    equal to 'E005-35' the channel will be excluded. On the other side, passing
    a value of 'E005' will probably exclude all the channels so be careful of
    how the list of the excluded channels is computed.

    @Parameters
    - excluded_list: an iterable of tuple containing the group of the channels
                     as first element and a list of string to be matched as
                     excluded channels
    - channels: an iterable of PyChannel from which exclude some channels

    @Returns
    A list of channel without the excluded ones.
    """

    ret = []
    for group, excluded_list_g in excluded_list:
        for channel in channels:
            is_excluded = False
            for excluded in excluded_list_g:
                if excluded in channel.label() and group == channel.group():
                    is_excluded = True
            if not is_excluded and group == channel.group():
                ret.append(channel)
    return ret


def plot_raw_with_spikes(
    phase: PyPhase,
    channel: PyChannel,
    ax: Axes,
    start: Optional[int] = None,
    end: Optional[int] = None,
):
    """
    Utility function for quickly plot a raw data with the relative spikes to
    the given Axes.

    @Parameters:
    - phase: the phase of
    
    """

    raw_data = phase.raw_data(channel, start, end)
    peaks = phase.peak_train(channel, start, end)

    x_start = 0
    if start is not None:
        x_start = start
    x_end = phase.datalen()
    if end is not None:
        x_end = end

    times = np.arange(x_start, x_end - 1, 1)

    ax.plot(times, raw_data, color=PyCode.get("RAW_DATA_COLOR"))
    ax.stem(
        peaks[0],
        peaks[1],
        linefmt=f"{PyCode.get('SPIKES_COLOR_LINE')}",
        markerfmt=f"{PyCode.get('SPIKES_COLOR_HEAD')}",
    )


def rasterplot(phase: PyPhase, ax: Axes, fig: Optional[Figure] = None):
    """
    TODO comment this function
    """

    def on_mouse_move(event):
        print(f"{event.ydata:.2f}")
    
    spikes = []
    for channel in phase.channels():
        spikes.append(phase.peak_train(channel)[0][:])
    ax.eventplot(spikes)

    if fig is not None:
        fig.canvas.mpl_connect('motion_notify_event', on_mouse_move)
