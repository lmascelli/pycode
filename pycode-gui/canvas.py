from PySide6.QtWidgets import QWidget, QVBoxLayout
from matplotlib.figure import Figure
from matplotlib.backends.backend_qt5agg import (
    FigureCanvasQTAgg as FigureCanvas,
    NavigationToolbar2QT
)

WIDTH = 5
HEIGHT = 4
DPI = 100

class MplCanvas(QWidget):
    def __init__(self, rows: int, cols: int):
        super().__init__()
        layout = QVBoxLayout(self)
        self.setLayout(layout)

        figure = Figure(figsize=(WIDTH, HEIGHT), dpi=DPI)
        self.canvas = FigureCanvas(figure)
        navigation_toolbar = NavigationToolbar2QT(self.canvas, self)
        layout.addWidget(navigation_toolbar)
        layout.addWidget(self.canvas)

        self.axes = []

        for row in range(1, rows + 1):
            for col in range(1, cols + 1):
                self.axes.append(figure.subplots(row, col))

    def redraw(self):
        self.canvas.draw()
