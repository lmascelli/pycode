# -*- coding: utf-8 -*-

################################################################################
## Form generated from reading UI file 'channel_viewer.ui'
##
## Created by: Qt User Interface Compiler version 6.8.1
##
## WARNING! All changes made in this file will be lost when recompiling UI file!
################################################################################

from PySide6.QtCore import (QCoreApplication, QDate, QDateTime, QLocale,
    QMetaObject, QObject, QPoint, QRect,
    QSize, QTime, QUrl, Qt)
from PySide6.QtGui import (QBrush, QColor, QConicalGradient, QCursor,
    QFont, QFontDatabase, QGradient, QIcon,
    QImage, QKeySequence, QLinearGradient, QPainter,
    QPalette, QPixmap, QRadialGradient, QTransform)
from PySide6.QtWidgets import (QApplication, QGroupBox, QSizePolicy, QVBoxLayout,
    QWidget)

class Ui_ChannelViewer(object):
    def setupUi(self, ChannelViewer):
        if not ChannelViewer.objectName():
            ChannelViewer.setObjectName(u"ChannelViewer")
        ChannelViewer.resize(400, 300)
        self.verticalLayout = QVBoxLayout(ChannelViewer)
        self.verticalLayout.setObjectName(u"verticalLayout")
        self.grp_plots = QGroupBox(ChannelViewer)
        self.grp_plots.setObjectName(u"grp_plots")
        self.verticalLayout_3 = QVBoxLayout(self.grp_plots)
        self.verticalLayout_3.setObjectName(u"verticalLayout_3")

        self.verticalLayout.addWidget(self.grp_plots)

        self.grp_commands = QGroupBox(ChannelViewer)
        self.grp_commands.setObjectName(u"grp_commands")
        self.grp_commands.setMinimumSize(QSize(0, 60))

        self.verticalLayout.addWidget(self.grp_commands)

        self.verticalLayout.setStretch(0, 1)

        self.retranslateUi(ChannelViewer)

        QMetaObject.connectSlotsByName(ChannelViewer)
    # setupUi

    def retranslateUi(self, ChannelViewer):
        ChannelViewer.setWindowTitle(QCoreApplication.translate("ChannelViewer", u"Form", None))
        self.grp_plots.setTitle(QCoreApplication.translate("ChannelViewer", u"Plots", None))
        self.grp_commands.setTitle(QCoreApplication.translate("ChannelViewer", u"Commands", None))
    # retranslateUi

