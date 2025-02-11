# -*- coding: utf-8 -*-

################################################################################
## Form generated from reading UI file 'phase_explorer.ui'
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
from PySide6.QtWidgets import (QApplication, QFormLayout, QGroupBox, QHBoxLayout,
    QHeaderView, QLabel, QPushButton, QSizePolicy,
    QTableView, QVBoxLayout, QWidget)

class Ui_PhaseExplorer(object):
    def setupUi(self, PhaseExplorer):
        if not PhaseExplorer.objectName():
            PhaseExplorer.setObjectName(u"PhaseExplorer")
        PhaseExplorer.resize(697, 533)
        self.horizontalLayout = QHBoxLayout(PhaseExplorer)
        self.horizontalLayout.setObjectName(u"horizontalLayout")
        self.widget = QWidget(PhaseExplorer)
        self.widget.setObjectName(u"widget")
        self.verticalLayout_2 = QVBoxLayout(self.widget)
        self.verticalLayout_2.setObjectName(u"verticalLayout_2")
        self.groupBox = QGroupBox(self.widget)
        self.groupBox.setObjectName(u"groupBox")
        self.formLayout = QFormLayout(self.groupBox)
        self.formLayout.setObjectName(u"formLayout")
        self.label = QLabel(self.groupBox)
        self.label.setObjectName(u"label")

        self.formLayout.setWidget(0, QFormLayout.LabelRole, self.label)

        self.lbl_name = QLabel(self.groupBox)
        self.lbl_name.setObjectName(u"lbl_name")

        self.formLayout.setWidget(0, QFormLayout.FieldRole, self.lbl_name)

        self.label_7 = QLabel(self.groupBox)
        self.label_7.setObjectName(u"label_7")

        self.formLayout.setWidget(1, QFormLayout.LabelRole, self.label_7)

        self.lbl_date = QLabel(self.groupBox)
        self.lbl_date.setObjectName(u"lbl_date")

        self.formLayout.setWidget(1, QFormLayout.FieldRole, self.lbl_date)

        self.label_3 = QLabel(self.groupBox)
        self.label_3.setObjectName(u"label_3")

        self.formLayout.setWidget(2, QFormLayout.LabelRole, self.label_3)

        self.lbl_duration = QLabel(self.groupBox)
        self.lbl_duration.setObjectName(u"lbl_duration")

        self.formLayout.setWidget(2, QFormLayout.FieldRole, self.lbl_duration)

        self.label_5 = QLabel(self.groupBox)
        self.label_5.setObjectName(u"label_5")

        self.formLayout.setWidget(3, QFormLayout.LabelRole, self.label_5)

        self.lbl_sampling_frequency = QLabel(self.groupBox)
        self.lbl_sampling_frequency.setObjectName(u"lbl_sampling_frequency")

        self.formLayout.setWidget(3, QFormLayout.FieldRole, self.lbl_sampling_frequency)


        self.verticalLayout_2.addWidget(self.groupBox)

        self.groupBox_3 = QGroupBox(self.widget)
        self.groupBox_3.setObjectName(u"groupBox_3")
        self.verticalLayout_3 = QVBoxLayout(self.groupBox_3)
        self.verticalLayout_3.setObjectName(u"verticalLayout_3")
        self.btn_peak_detection = QPushButton(self.groupBox_3)
        self.btn_peak_detection.setObjectName(u"btn_peak_detection")

        self.verticalLayout_3.addWidget(self.btn_peak_detection)

        self.btn_burst_detection = QPushButton(self.groupBox_3)
        self.btn_burst_detection.setObjectName(u"btn_burst_detection")

        self.verticalLayout_3.addWidget(self.btn_burst_detection)

        self.btn_rasterplot = QPushButton(self.groupBox_3)
        self.btn_rasterplot.setObjectName(u"btn_rasterplot")

        self.verticalLayout_3.addWidget(self.btn_rasterplot)


        self.verticalLayout_2.addWidget(self.groupBox_3)


        self.horizontalLayout.addWidget(self.widget)

        self.groupBox_2 = QGroupBox(PhaseExplorer)
        self.groupBox_2.setObjectName(u"groupBox_2")
        self.verticalLayout = QVBoxLayout(self.groupBox_2)
        self.verticalLayout.setObjectName(u"verticalLayout")
        self.tbl_channels = QTableView(self.groupBox_2)
        self.tbl_channels.setObjectName(u"tbl_channels")
        self.tbl_channels.setTabKeyNavigation(False)
        self.tbl_channels.setProperty(u"showDropIndicator", False)
        self.tbl_channels.setGridStyle(Qt.PenStyle.SolidLine)
        self.tbl_channels.setCornerButtonEnabled(False)

        self.verticalLayout.addWidget(self.tbl_channels)


        self.horizontalLayout.addWidget(self.groupBox_2)


        self.retranslateUi(PhaseExplorer)

        QMetaObject.connectSlotsByName(PhaseExplorer)
    # setupUi

    def retranslateUi(self, PhaseExplorer):
        PhaseExplorer.setWindowTitle(QCoreApplication.translate("PhaseExplorer", u"PhaseExplorer", None))
        self.groupBox.setTitle(QCoreApplication.translate("PhaseExplorer", u"Info", None))
        self.label.setText(QCoreApplication.translate("PhaseExplorer", u"Name", None))
        self.lbl_name.setText("")
        self.label_7.setText(QCoreApplication.translate("PhaseExplorer", u"Date", None))
        self.lbl_date.setText("")
        self.label_3.setText(QCoreApplication.translate("PhaseExplorer", u"Duration", None))
        self.lbl_duration.setText("")
        self.label_5.setText(QCoreApplication.translate("PhaseExplorer", u"Sampling Frequency", None))
        self.lbl_sampling_frequency.setText("")
        self.groupBox_3.setTitle(QCoreApplication.translate("PhaseExplorer", u"Commands", None))
        self.btn_peak_detection.setText(QCoreApplication.translate("PhaseExplorer", u"Peak detection", None))
        self.btn_burst_detection.setText(QCoreApplication.translate("PhaseExplorer", u"Burst detection", None))
        self.btn_rasterplot.setText(QCoreApplication.translate("PhaseExplorer", u"Rasterplot", None))
        self.groupBox_2.setTitle(QCoreApplication.translate("PhaseExplorer", u"Channels", None))
    # retranslateUi

