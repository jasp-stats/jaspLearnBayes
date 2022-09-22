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
		FormulaField { name: "prevalence";	label: qsTr("Prevalence");	min: 0.00001; max: 0.99999; defaultValue: "0.1"; fieldWidth: 55	}
		FormulaField { name: "sensitivity"; label: qsTr("Sensitivity");	min: 0.00001; max: 0.99999; defaultValue: "0.8"; fieldWidth: 55 }
		FormulaField { name: "specificity"; label: qsTr("Specificity");	min: 0.00001; max: 0.99999; defaultValue: "0.8"; fieldWidth: 55	}
	}

	Group
	{
		visible: inputType.value === "data"
		VariablesForm
		{
			preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
			AvailableVariablesList { name: "allVariablesList" }
			AssignedVariablesList { name: "marker";	title: qsTr("Marker");	allowedColumns: ["scale"];				singleVariable: true	}
			AssignedVariablesList { name: "labels";	title: qsTr("Positive condition (binary)");	suggestedColumns: ["ordinal", "nominal"];	singleVariable: true	}
		}

		FormulaField { name: "threshold";	label: qsTr("Test threshold"); defaultValue: "0"; fieldWidth: 55	}
	}

	Group
	{
		visible: inputType.value === "uncertainEstimates"
		columns: 2
		title: qsTr("Data")
		IntegerField { name: "truePositive";  label: qsTr("True positive");  min: 0; defaultValue: 0; fieldWidth: 55	}
		IntegerField { name: "falsePositive"; label: qsTr("False positive"); min: 0; defaultValue: 0; fieldWidth: 55	}
		IntegerField { name: "falseNegative"; label: qsTr("False negative"); min: 0; defaultValue: 0; fieldWidth: 55	}
		IntegerField { name: "trueNegative";  label: qsTr("True negative");  min: 0; defaultValue: 0; fieldWidth: 55	}
	}

	Group
	{
		visible: inputType.value === "uncertainEstimates" || inputType.value === "data"
		columns: 3
		title: qsTr("Priors")
		Text{ text: qsTr("Prevalence") }
		FormulaField { name: "priorPrevalenceAlpha";  label: qsTr("~ Beta(α = ");		afterLabel: ",";		min: 0; defaultValue: "1"	; fieldWidth: 55}
		FormulaField { name: "priorPrevalenceBeta";   label: "β = ";								afterLabel: qsTr(")");	min: 0; defaultValue: "9"; fieldWidth: 55	}

		Text{ text: qsTr("Sensitivity") }
		FormulaField { name: "priorSensitivityAlpha"; label: qsTr("~ Beta(α = ");	afterLabel: ",";		min: 0; defaultValue: "8"; fieldWidth: 55	}
		FormulaField { name: "priorSensitivityBeta";  label: "β = ";								afterLabel: qsTr(")");	min: 0; defaultValue: "2"; fieldWidth: 55	}

		Text{ text: qsTr("Specificity") }
		FormulaField { name: "priorSpecificityAlpha"; label: qsTr("~ Beta(α = ");	afterLabel: ",";		min: 0; defaultValue: "8"; fieldWidth: 55	}
		FormulaField { name: "priorSpecificityBeta";  label: "β = ";								afterLabel: qsTr(")");	min: 0; defaultValue: "2"; fieldWidth: 55	}
	}

	Group
	{
		title: qsTr("Tables")
		CheckBox { name: "statistics";  label: qsTr("Statistics");	checked: true }
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
			CheckBox { name: "confusionMatrixAdditionalInfo"; label: qsTr("Additional info"); checked: true }
		}
		CheckBox { name: "priorPosterior"; label: qsTr("Priors and posteriors"); visible: inputType.value === "uncertainEstimates" || inputType.value === "data" }
	}

	Section
	{
		title: qsTr("Plots")

		Group
		{
			CheckBox
			{
				name: "probabilityPositivePlot";	label: qsTr("Probability positive")
				CheckBox { name: "probabilityPositivePlotEntireDistribution"; label: qsTr("Show entire distribution"); visible: inputType.value === "uncertainEstimates" || inputType.value === "data"}
			}
			CheckBox { name: "iconPlot";						label: qsTr("Icon plot")							}
			CheckBox
			{
				name: "rocPlot"; label: qsTr("ROC")
				CheckBox
				{
					name: "rocPlotPosteriorRealizations"; label: qsTr("Add realizations from the posterior"); visible: inputType.value === "uncertainEstimates" || inputType.value === "data"; childrenOnSameRow: true;
					IntegerField{ name: "plotRocLinesNr"; min: 0; defaultValue: 100; max: 1000 }
				}
			}
			CheckBox { name: "testCharacteristicsPlot";				label: qsTr("Test characteristics by threshold")	}
			CheckBox { name: "predictiveValuesByPrevalence";		label: qsTr("PPV and NPV by prevalence")			}
			CheckBox { name: "alluvialPlot";						label: qsTr("Alluvial plot")						}
			CheckBox { name: "signalDetectionPlot";					label: qsTr("Signal detection"); 					}
			CheckBox
			{
				name: "estimatesPlot"; label: qsTr("Estimates"); columns: 2
				CheckBox { name: "estimatesPlotPrevalence";					label: qsTr("Prevalence");					checked: true; Layout.columnSpan: 2	}
				CheckBox { name: "estimatesPlotSensitivity";				label: qsTr("Sensitivity");					checked: true						}
				CheckBox { name: "estimatesPlotSpecificity";				label: qsTr("Specificity");					checked: true						}
				CheckBox { name: "estimatesPlotTruePositiveRate";			label: qsTr("True positive rate")												}
				CheckBox { name: "estimatesPlotFalsePositiveRate";			label: qsTr("False positive rate")												}
				CheckBox { name: "estimatesPlotTrueNegativeRate";			label: qsTr("True negative rate")												}
				CheckBox { name: "estimatesPlotFalseNegativeRate";			label: qsTr("False negative rate")												}
				CheckBox { name: "estimatesPlotPositivePredictiveValue";	label: qsTr("Positive predictive value")										}
				CheckBox { name: "estimatesPlotNegativePredictiveValue";	label: qsTr("Negative predictive value")										}
				CheckBox { name: "estimatesPlotFalseDiscoveryRate";			label: qsTr("False discovery rate")												}
				CheckBox { name: "estimatesPlotFalseOmissionRate";			label: qsTr("False omission rate")												}
				CheckBox { name: "estimatesPlotFalsePositiveRate";			label: qsTr("False positive rate")												}
				CheckBox { name: "estimatesPlotFalseNegativeRate";			label: qsTr("False negative rate")												}
				CheckBox { name: "estimatesPlotAccuracy";					label: qsTr("Accuracy"); Layout.columnSpan: 2 }
				RadioButtonGroup
				{
					name: "plotEstimatesType"; title: qsTr("Plot type")
					visible: inputType.value === "uncertainEstimates" || inputType.value === "data"
					RadioButton { name: "interval"; label: qsTr("Interval") }
					RadioButton { name: "halfEye";  label: qsTr("Half-eye") }
				}
			}
		}

	}

	Section
	{
		title: qsTr("Options")
		LS.LSintrotext {Layout.columnSpan: 2}

		Group
		{
			visible: inputType.value !== "pointEstimates"
			columns: 1
			CheckBox
			{
				name:					"ci"
				label:					qsTr("Credible intervals")
				childrenOnSameRow:		true
				checked:				true
				CIField { name: "ciLevel" }
			}

			IntegerField
			{
				name: "samples"
				label: qsTr("Number of posterior samples")
				defaultValue: 10000
				min: 1000
				fieldWidth: 50
			}
		}

	}
}
