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
    QHeaderView, QLabel, QSizePolicy, QTableView,
    QVBoxLayout, QWidget)

class Ui_PhaseExplorer(object):
    def setupUi(self, PhaseExplorer):
        if not PhaseExplorer.objectName():
            PhaseExplorer.setObjectName(u"PhaseExplorer")
        PhaseExplorer.resize(697, 533)
        self.horizontalLayout = QHBoxLayout(PhaseExplorer)
        self.horizontalLayout.setObjectName(u"horizontalLayout")
        self.groupBox = QGroupBox(PhaseExplorer)
        self.groupBox.setObjectName(u"groupBox")
        self.formLayout = QFormLayout(self.groupBox)
        self.formLayout.setObjectName(u"formLayout")
        self.label = QLabel(self.groupBox)
        self.label.setObjectName(u"label")

        self.formLayout.setWidget(0, QFormLayout.LabelRole, self.label)

        self.lbl_name = QLabel(self.groupBox)
        self.lbl_name.setObjectName(u"lbl_name")

        self.formLayout.setWidget(0, QFormLayout.FieldRole, self.lbl_name)

        self.label_3 = QLabel(self.groupBox)
        self.label_3.setObjectName(u"label_3")

        self.formLayout.setWidget(2, QFormLayout.LabelRole, self.label_3)

        self.lbl_duration = QLabel(self.groupBox)
        self.lbl_duration.setObjectName(u"lbl_duration")

        self.formLayout.setWidget(2, QFormLayout.FieldRole, self.lbl_duration)

        self.label_5 = QLabel(self.groupBox)
        self.label_5.setObjectName(u"label_5")

        self.formLayout.setWidget(4, QFormLayout.LabelRole, self.label_5)

        self.lbl_sampling_frequency = QLabel(self.groupBox)
        self.lbl_sampling_frequency.setObjectName(u"lbl_sampling_frequency")

        self.formLayout.setWidget(4, QFormLayout.FieldRole, self.lbl_sampling_frequency)

        self.label_7 = QLabel(self.groupBox)
        self.label_7.setObjectName(u"label_7")

        self.formLayout.setWidget(1, QFormLayout.LabelRole, self.label_7)

        self.lbl_date = QLabel(self.groupBox)
        self.lbl_date.setObjectName(u"lbl_date")

        self.formLayout.setWidget(1, QFormLayout.FieldRole, self.lbl_date)

        self.grp_plots = QGroupBox(self.groupBox)
        self.grp_plots.setObjectName(u"grp_plots")

        self.formLayout.setWidget(3, QFormLayout.FieldRole, self.grp_plots)


        self.horizontalLayout.addWidget(self.groupBox)

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
        self.label_3.setText(QCoreApplication.translate("PhaseExplorer", u"Duration", None))
        self.lbl_duration.setText("")
        self.label_5.setText(QCoreApplication.translate("PhaseExplorer", u"Sampling Frequency", None))
        self.lbl_sampling_frequency.setText("")
        self.label_7.setText(QCoreApplication.translate("PhaseExplorer", u"Date", None))
        self.lbl_date.setText("")
        self.grp_plots.setTitle(QCoreApplication.translate("PhaseExplorer", u"Plots", None))
        self.groupBox_2.setTitle(QCoreApplication.translate("PhaseExplorer", u"Channels", None))
    # retranslateUi

