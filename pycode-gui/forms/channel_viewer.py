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
from PySide6.QtWidgets import (QApplication, QComboBox, QFormLayout, QGridLayout,
    QGroupBox, QLabel, QLineEdit, QPushButton,
    QSizePolicy, QVBoxLayout, QWidget)

class Ui_ChannelViewer(object):
    def setupUi(self, ChannelViewer):
        if not ChannelViewer.objectName():
            ChannelViewer.setObjectName(u"ChannelViewer")
        ChannelViewer.resize(934, 724)
        self.verticalLayout = QVBoxLayout(ChannelViewer)
        self.verticalLayout.setObjectName(u"verticalLayout")
        self.grp_plots = QGroupBox(ChannelViewer)
        self.grp_plots.setObjectName(u"grp_plots")
        self.verticalLayout_3 = QVBoxLayout(self.grp_plots)
        self.verticalLayout_3.setObjectName(u"verticalLayout_3")

        self.verticalLayout.addWidget(self.grp_plots)

        self.groupBox = QGroupBox(ChannelViewer)
        self.groupBox.setObjectName(u"groupBox")
        self.groupBox.setMinimumSize(QSize(0, 100))
        self.gridLayout = QGridLayout(self.groupBox)
        self.gridLayout.setObjectName(u"gridLayout")
        self.groupBox_3 = QGroupBox(self.groupBox)
        self.groupBox_3.setObjectName(u"groupBox_3")
        self.formLayout_2 = QFormLayout(self.groupBox_3)
        self.formLayout_2.setObjectName(u"formLayout_2")
        self.label_6 = QLabel(self.groupBox_3)
        self.label_6.setObjectName(u"label_6")

        self.formLayout_2.setWidget(0, QFormLayout.LabelRole, self.label_6)

        self.label_5 = QLabel(self.groupBox_3)
        self.label_5.setObjectName(u"label_5")

        self.formLayout_2.setWidget(1, QFormLayout.LabelRole, self.label_5)

        self.label_4 = QLabel(self.groupBox_3)
        self.label_4.setObjectName(u"label_4")

        self.formLayout_2.setWidget(2, QFormLayout.LabelRole, self.label_4)

        self.edt_y_min = QLineEdit(self.groupBox_3)
        self.edt_y_min.setObjectName(u"edt_y_min")

        self.formLayout_2.setWidget(1, QFormLayout.FieldRole, self.edt_y_min)

        self.edt_y_max = QLineEdit(self.groupBox_3)
        self.edt_y_max.setObjectName(u"edt_y_max")

        self.formLayout_2.setWidget(2, QFormLayout.FieldRole, self.edt_y_max)

        self.cmb_y_unit = QComboBox(self.groupBox_3)
        self.cmb_y_unit.addItem("")
        self.cmb_y_unit.addItem("")
        self.cmb_y_unit.addItem("")
        self.cmb_y_unit.setObjectName(u"cmb_y_unit")

        self.formLayout_2.setWidget(0, QFormLayout.FieldRole, self.cmb_y_unit)


        self.gridLayout.addWidget(self.groupBox_3, 0, 2, 1, 1)

        self.groupBox_2 = QGroupBox(self.groupBox)
        self.groupBox_2.setObjectName(u"groupBox_2")
        self.formLayout = QFormLayout(self.groupBox_2)
        self.formLayout.setObjectName(u"formLayout")
        self.label = QLabel(self.groupBox_2)
        self.label.setObjectName(u"label")

        self.formLayout.setWidget(0, QFormLayout.LabelRole, self.label)

        self.label_2 = QLabel(self.groupBox_2)
        self.label_2.setObjectName(u"label_2")

        self.formLayout.setWidget(1, QFormLayout.LabelRole, self.label_2)

        self.label_3 = QLabel(self.groupBox_2)
        self.label_3.setObjectName(u"label_3")

        self.formLayout.setWidget(2, QFormLayout.LabelRole, self.label_3)

        self.edt_x_min = QLineEdit(self.groupBox_2)
        self.edt_x_min.setObjectName(u"edt_x_min")

        self.formLayout.setWidget(1, QFormLayout.FieldRole, self.edt_x_min)

        self.edt_x_max = QLineEdit(self.groupBox_2)
        self.edt_x_max.setObjectName(u"edt_x_max")

        self.formLayout.setWidget(2, QFormLayout.FieldRole, self.edt_x_max)

        self.cmb_x_unit = QComboBox(self.groupBox_2)
        self.cmb_x_unit.addItem("")
        self.cmb_x_unit.addItem("")
        self.cmb_x_unit.addItem("")
        self.cmb_x_unit.setObjectName(u"cmb_x_unit")

        self.formLayout.setWidget(0, QFormLayout.FieldRole, self.cmb_x_unit)


        self.gridLayout.addWidget(self.groupBox_2, 0, 1, 1, 1)

        self.widget = QWidget(self.groupBox)
        self.widget.setObjectName(u"widget")
        self.verticalLayout_2 = QVBoxLayout(self.widget)
        self.verticalLayout_2.setObjectName(u"verticalLayout_2")
        self.btn_reset = QPushButton(self.widget)
        self.btn_reset.setObjectName(u"btn_reset")

        self.verticalLayout_2.addWidget(self.btn_reset)

        self.btn_update = QPushButton(self.widget)
        self.btn_update.setObjectName(u"btn_update")

        self.verticalLayout_2.addWidget(self.btn_update)


        self.gridLayout.addWidget(self.widget, 0, 3, 1, 1)


        self.verticalLayout.addWidget(self.groupBox)

        self.verticalLayout.setStretch(0, 1)

        self.retranslateUi(ChannelViewer)

        QMetaObject.connectSlotsByName(ChannelViewer)
    # setupUi

    def retranslateUi(self, ChannelViewer):
        ChannelViewer.setWindowTitle(QCoreApplication.translate("ChannelViewer", u"Form", None))
        self.grp_plots.setTitle(QCoreApplication.translate("ChannelViewer", u"Plots", None))
        self.groupBox.setTitle(QCoreApplication.translate("ChannelViewer", u"Limits", None))
        self.groupBox_3.setTitle(QCoreApplication.translate("ChannelViewer", u"Y Axis", None))
        self.label_6.setText(QCoreApplication.translate("ChannelViewer", u"Unit", None))
        self.label_5.setText(QCoreApplication.translate("ChannelViewer", u"Min", None))
        self.label_4.setText(QCoreApplication.translate("ChannelViewer", u"Max", None))
        self.cmb_y_unit.setItemText(0, QCoreApplication.translate("ChannelViewer", u"Volt", None))
        self.cmb_y_unit.setItemText(1, QCoreApplication.translate("ChannelViewer", u"Millivolt", None))
        self.cmb_y_unit.setItemText(2, QCoreApplication.translate("ChannelViewer", u"Microvolt", None))

        self.groupBox_2.setTitle(QCoreApplication.translate("ChannelViewer", u"X Axis", None))
        self.label.setText(QCoreApplication.translate("ChannelViewer", u"Unit", None))
        self.label_2.setText(QCoreApplication.translate("ChannelViewer", u"Min", None))
        self.label_3.setText(QCoreApplication.translate("ChannelViewer", u"Max", None))
        self.cmb_x_unit.setItemText(0, QCoreApplication.translate("ChannelViewer", u"Sample", None))
        self.cmb_x_unit.setItemText(1, QCoreApplication.translate("ChannelViewer", u"Seconds", None))
        self.cmb_x_unit.setItemText(2, QCoreApplication.translate("ChannelViewer", u"Milliseconds", None))

        self.btn_reset.setText(QCoreApplication.translate("ChannelViewer", u"Reset", None))
        self.btn_update.setText(QCoreApplication.translate("ChannelViewer", u"Update", None))
    # retranslateUi

