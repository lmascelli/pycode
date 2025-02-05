# -*- coding: utf-8 -*-

################################################################################
## Form generated from reading UI file 'main_window.ui'
##
## Created by: Qt User Interface Compiler version 6.8.1
##
## WARNING! All changes made in this file will be lost when recompiling UI file!
################################################################################

from PySide6.QtCore import (QCoreApplication, QDate, QDateTime, QLocale,
    QMetaObject, QObject, QPoint, QRect,
    QSize, QTime, QUrl, Qt)
from PySide6.QtGui import (QAction, QBrush, QColor, QConicalGradient,
    QCursor, QFont, QFontDatabase, QGradient,
    QIcon, QImage, QKeySequence, QLinearGradient,
    QPainter, QPalette, QPixmap, QRadialGradient,
    QTransform)
from PySide6.QtWidgets import (QApplication, QGridLayout, QLabel, QMainWindow,
    QMenu, QMenuBar, QSizePolicy, QSpacerItem,
    QStatusBar, QTabWidget, QWidget)
import icons_rc
import icons_rc
import icons_rc

class Ui_MainWindow(object):
    def setupUi(self, MainWindow):
        if not MainWindow.objectName():
            MainWindow.setObjectName(u"MainWindow")
        MainWindow.resize(800, 665)
        icon = QIcon()
        icon.addFile(u":/Main/Icon.png", QSize(), QIcon.Mode.Normal, QIcon.State.Off)
        MainWindow.setWindowIcon(icon)
        self.action_Open = QAction(MainWindow)
        self.action_Open.setObjectName(u"action_Open")
        self.action_Quit = QAction(MainWindow)
        self.action_Quit.setObjectName(u"action_Quit")
        self.action_Welcome = QAction(MainWindow)
        self.action_Welcome.setObjectName(u"action_Welcome")
        self.centralwidget = QWidget(MainWindow)
        self.centralwidget.setObjectName(u"centralwidget")
        self.gridLayout = QGridLayout(self.centralwidget)
        self.gridLayout.setObjectName(u"gridLayout")
        self.tabWidget = QTabWidget(self.centralwidget)
        self.tabWidget.setObjectName(u"tabWidget")
        self.tabWidget.setTabsClosable(True)
        self.tabWidget.setMovable(True)
        self.welcome_tab = QWidget()
        self.welcome_tab.setObjectName(u"welcome_tab")
        self.gridLayout_2 = QGridLayout(self.welcome_tab)
        self.gridLayout_2.setObjectName(u"gridLayout_2")
        self.label = QLabel(self.welcome_tab)
        self.label.setObjectName(u"label")
        self.label.setTextFormat(Qt.TextFormat.RichText)
        self.label.setAlignment(Qt.AlignmentFlag.AlignLeading|Qt.AlignmentFlag.AlignLeft|Qt.AlignmentFlag.AlignTop)
        self.label.setWordWrap(True)

        self.gridLayout_2.addWidget(self.label, 3, 0, 1, 1)

        self.label_2 = QLabel(self.welcome_tab)
        self.label_2.setObjectName(u"label_2")
        self.label_2.setPixmap(QPixmap(u":/Main/Icon.png"))
        self.label_2.setScaledContents(False)
        self.label_2.setAlignment(Qt.AlignmentFlag.AlignCenter)

        self.gridLayout_2.addWidget(self.label_2, 1, 0, 1, 1)

        self.label_3 = QLabel(self.welcome_tab)
        self.label_3.setObjectName(u"label_3")
        self.label_3.setMinimumSize(QSize(760, 92))
        self.label_3.setMaximumSize(QSize(760, 92))
        self.label_3.setAlignment(Qt.AlignmentFlag.AlignCenter)

        self.gridLayout_2.addWidget(self.label_3, 0, 0, 1, 1, Qt.AlignmentFlag.AlignVCenter)

        self.verticalSpacer = QSpacerItem(20, 40, QSizePolicy.Policy.Minimum, QSizePolicy.Policy.Expanding)

        self.gridLayout_2.addItem(self.verticalSpacer, 2, 0, 1, 1)

        self.tabWidget.addTab(self.welcome_tab, "")

        self.gridLayout.addWidget(self.tabWidget, 0, 0, 1, 1)

        MainWindow.setCentralWidget(self.centralwidget)
        self.menubar = QMenuBar(MainWindow)
        self.menubar.setObjectName(u"menubar")
        self.menubar.setGeometry(QRect(0, 0, 800, 23))
        self.menu_File = QMenu(self.menubar)
        self.menu_File.setObjectName(u"menu_File")
        self.menu = QMenu(self.menubar)
        self.menu.setObjectName(u"menu")
        MainWindow.setMenuBar(self.menubar)
        self.statusbar = QStatusBar(MainWindow)
        self.statusbar.setObjectName(u"statusbar")
        MainWindow.setStatusBar(self.statusbar)

        self.menubar.addAction(self.menu_File.menuAction())
        self.menubar.addAction(self.menu.menuAction())
        self.menu_File.addAction(self.action_Open)
        self.menu_File.addSeparator()
        self.menu_File.addAction(self.action_Quit)
        self.menu.addAction(self.action_Welcome)

        self.retranslateUi(MainWindow)

        QMetaObject.connectSlotsByName(MainWindow)
    # setupUi

    def retranslateUi(self, MainWindow):
        MainWindow.setWindowTitle(QCoreApplication.translate("MainWindow", u"PyCode", None))
        self.action_Open.setText(QCoreApplication.translate("MainWindow", u"&Open", None))
        self.action_Quit.setText(QCoreApplication.translate("MainWindow", u"&Quit", None))
        self.action_Welcome.setText(QCoreApplication.translate("MainWindow", u"&Welcome", None))
        self.label.setText(QCoreApplication.translate("MainWindow", u"<html><head/><body><p>To start viewing some data recorded with Experimenter or MC_Rack open an <span style=\" font-weight:700;\">.h5</span> file converted with <a href=\"https://www.multichannelsystems.com/software/multi-channel-datamanager\"><span style=\" text-decoration: underline; color:#1d99f3;\">MultiChannel Data Manager</span></a></p></body></html>", None))
        self.label_2.setText("")
        self.label_3.setText(QCoreApplication.translate("MainWindow", u"<html><head/><body><p align=\"center\"><span style=\" font-size:24pt; font-weight:700;\">PyCode</span></p><p align=\"center\"><br/></p></body></html>", None))
        self.tabWidget.setTabText(self.tabWidget.indexOf(self.welcome_tab), QCoreApplication.translate("MainWindow", u"Welcome", None))
        self.menu_File.setTitle(QCoreApplication.translate("MainWindow", u"&File", None))
        self.menu.setTitle(QCoreApplication.translate("MainWindow", u"&?", None))
    # retranslateUi

