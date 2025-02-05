from matplotlib.figure import Figure
from matplotlib.backends.backend_qt5agg import FigureCanvasQTAgg as FigureCanvas

WIDTH = 5
HEIGHT = 4
DPI = 100

class MplCanvas(FigureCanvas):
    def __init__(self, rows: int, cols: int):
        figure = Figure(figsize=(WIDTH, HEIGHT), dpi=DPI)
        self.axes = []
        for row in range(1, rows + 1):
            for col in range(1, cols + 1):
                self.axes.append(figure.subplots(row, col))
        super().__init__(figure)
