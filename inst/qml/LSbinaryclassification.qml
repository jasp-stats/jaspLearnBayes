//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//
import QtQuick 2.8
import QtQuick.Layouts 1.3
import JASP.Controls 1.0
import JASP.Widgets 1.0
import JASP.Theme 1.0
import "../qml/qml_components" as LS

Form {
	LS.LSintrotext {}

	Section
	{
		title: qsTr("Input")
		columns: 1
		RadioButtonGroup
		{
			name: "inputType"; id: inputType; columns: 3; title: qsTr("Input type")
			RadioButton { name: "pointEstimates";		label: qsTr("Point estimates"); checked: true	}
			RadioButton { name: "uncertainEstimates";	label: qsTr("Uncertain estimates")				}
			RadioButton { name: "data";					label: qsTr("Load data and specify threshold")	}
		}

		Group
		{
			visible: inputType.value === "pointEstimates"
			title: qsTr("Estimates")
			FormulaField { name: "sensitivity"; label: qsTr("Sensitivity");	min: 0; max: 1; defaultValue: "0.8" }
			FormulaField { name: "specificity"; label: qsTr("Specificity");	min: 0; max: 1; defaultValue: "0.8"	}
			FormulaField { name: "prevalence";	label: qsTr("Prevalence");	min: 0; max: 1; defaultValue: "0.1"	}
		}

		Group
		{
			visible: inputType.value === "data"
			VariablesForm
			{
				preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
				AvailableVariablesList { name: "allVariablesList" }
				AssignedVariablesList { name: "marker";	title: qsTr("Marker");	suggestedColumns: ["scale"];				singleVariable: true	}
				AssignedVariablesList { name: "labels";	title: qsTr("Positive condition (binary)");	suggestedColumns: ["ordinal", "nominal"];	singleVariable: true	}
			}

			FormulaField { name: "threshold";	label: qsTr("Test threshold"); defaultValue: "0"	}
		}

		Group
		{
			visible: inputType.value === "uncertainEstimates" || inputType.value === "data"
			columns: 2
			title: inputType.value === "data" ? qsTr("Priors") : qsTr("Estimates")
			FormulaField { name: "sensitivityAlpha"; label: qsTr("Sensitivity ~ Beta(α = ");	afterLabel: ", ";		min: 0; defaultValue: "8"	}
			FormulaField { name: "sensitivityBeta";  label: "β = ";								afterLabel: qsTr(")");	min: 0; defaultValue: "2"	}
			FormulaField { name: "specificityAlpha"; label: qsTr("Specificity ~ Beta(α = ");	afterLabel: ", ";		min: 0; defaultValue: "8"	}
			FormulaField { name: "specificityBeta";  label: "β = ";								afterLabel: qsTr(")");	min: 0; defaultValue: "2"	}
			FormulaField { name: "prevalenceAlpha";  label: qsTr("Prevalence ~ Beta(α = ");		afterLabel: ", ";		min: 0; defaultValue: "1"	}
			FormulaField { name: "prevalenceBeta";   label: "β = ";								afterLabel: qsTr(")");	min: 0; defaultValue: "9"	}
		}
	}

	Section
	{
		title: qsTr("Output")

		Group
		{
			title: qsTr("Plots")
			CheckBox { name: "plotPriorPosteriorPositive";			label: qsTr("Probability positive"); checked: true	}
			CheckBox { name: "plotIconPlot";						label: qsTr("Icon plot")							}
			CheckBox { name: "plotROC";								label: qsTr("ROC")									}
			CheckBox { name: "plotVaryingPrevalence";				label: qsTr("PPV and NPV by prevalence")			}
			CheckBox { name: "plotAlluvial";						label: qsTr("Alluvial plot")						}
			CheckBox { name: "plotSignal";							label: qsTr("Signal detection")	; visible: false					}
		}

		Group
		{
			title: qsTr("Tables")
			CheckBox { name: "statistics";  label: qsTr("Statistics") }
			CheckBox
			{
				name: "confusionMatrix"; label: qsTr("Confusion matrix")
				RadioButtonGroup
				{
					name: "confusionMatrixType"
					RadioButton { name: "text";		label: qsTr("Text");	checked: true	}
					RadioButton { name: "number";	label: qsTr("Number")					}
					RadioButton { name: "both";		label: qsTr("Both")						}
				}
				CheckBox { name: "confusionMatrixAddInfo"; label: qsTr("Additional info"); checked: true }
			}
		}
	}

	Section
	{
		title: qsTr("Options")

		CheckBox
		{
			name:					"credibleInterval"
			label:					qsTr("Credible intervals")
			childrenOnSameRow:		true
			checked:				true
			visible:				inputType.value !== "pointEstimates"
			CIField { name: "ciLevel" }
		}

	}
}
