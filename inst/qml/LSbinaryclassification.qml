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
		FormulaField { name: "prevalence";	label: qsTr("Prevalence");	min: 0.00001; max: 0.99999; defaultValue: "0.1"	}
		FormulaField { name: "sensitivity"; label: qsTr("Sensitivity");	min: 0.00001; max: 0.99999; defaultValue: "0.8" }
		FormulaField { name: "specificity"; label: qsTr("Specificity");	min: 0.00001; max: 0.99999; defaultValue: "0.8"	}
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
		visible: inputType.value === "uncertainEstimates"
		columns: 2
		title: qsTr("Data")
		IntegerField { name: "truePositive";  label: qsTr("True positive");  min: 0; defaultValue: 0	}
		IntegerField { name: "falsePositive"; label: qsTr("False positive"); min: 0; defaultValue: 0	}
		IntegerField { name: "falseNegative"; label: qsTr("False negative"); min: 0; defaultValue: 0	}
		IntegerField { name: "trueNegative";  label: qsTr("True negative");  min: 0; defaultValue: 0	}
	}

	Group
	{
		visible: inputType.value === "uncertainEstimates" || inputType.value === "data"
		columns: 2
		title: qsTr("Priors")
		FormulaField { name: "prevalenceAlpha";  label: qsTr("Prevalence ~ Beta(α = ");		afterLabel: ", ";		min: 0; defaultValue: "1"	}
		FormulaField { name: "prevalenceBeta";   label: "β = ";								afterLabel: qsTr(")");	min: 0; defaultValue: "9"	}
		FormulaField { name: "sensitivityAlpha"; label: qsTr("Sensitivity ~ Beta(α = ");	afterLabel: ", ";		min: 0; defaultValue: "8"	}
		FormulaField { name: "sensitivityBeta";  label: "β = ";								afterLabel: qsTr(")");	min: 0; defaultValue: "2"	}
		FormulaField { name: "specificityAlpha"; label: qsTr("Specificity ~ Beta(α = ");	afterLabel: ", ";		min: 0; defaultValue: "8"	}
		FormulaField { name: "specificityBeta";  label: "β = ";								afterLabel: qsTr(")");	min: 0; defaultValue: "2"	}
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
			CheckBox { name: "confusionMatrixAddInfo"; label: qsTr("Additional info"); checked: true }
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
				name: "plotPriorPosteriorPositive";	label: qsTr("Probability positive")
				CheckBox { name: "plotPriorPosteriorPositiveDistribution"; label: qsTr("Show entire distribution"); visible: inputType.value === "uncertainEstimates" || inputType.value === "data"}
			}
			CheckBox { name: "plotIconPlot";						label: qsTr("Icon plot")							}
			CheckBox
			{
				name: "plotROC"; label: qsTr("ROC")
				CheckBox
				{
					name: "plotRocLines"; label: qsTr("Add realizations from the posterior"); visible: inputType.value === "uncertainEstimates" || inputType.value === "data"; childrenOnSameRow: true;
					IntegerField{ name: "plotRocLinesNr"; min: 0; defaultValue: 100; max: 1000 }
				}
			}
			CheckBox { name: "plotTestCharacteristics";				label: qsTr("Test characteristics by threshold")	}
			CheckBox { name: "plotVaryingPrevalence";				label: qsTr("PPV and NPV by prevalence")			}
			CheckBox { name: "plotAlluvial";						label: qsTr("Alluvial plot")						}
			CheckBox { name: "plotSignal";							label: qsTr("Signal detection"); 					}
			CheckBox
			{
				name: "plotEstimates"; label: qsTr("Estimates"); columns: 2
				CheckBox { name: "plotPrevalence";  label: qsTr("Prevalence");  checked: true; Layout.columnSpan: 2 }
				CheckBox { name: "plotSensitivity"; label: qsTr("Sensitivity"); checked: true }
				CheckBox { name: "plotSpecificity"; label: qsTr("Specificity"); checked: true }
				CheckBox { name: "plotTruePositive"; label: qsTr("True positive rate") }
				CheckBox { name: "plotFalsePositive"; label: qsTr("False positive rate") }
				CheckBox { name: "plotTrueNegative"; label: qsTr("True negative rate") }
				CheckBox { name: "plotFalseNegative"; label: qsTr("False negative rate") }
				CheckBox { name: "plotPPV"; label: qsTr("Positive predictive value") }
				CheckBox { name: "plotNPV"; label: qsTr("Negative predictive value") }
				CheckBox { name: "plotFDR"; label: qsTr("False discovery rate") }
				CheckBox { name: "plotFOR"; label: qsTr("False omission rate") }
				CheckBox { name: "plotFPF"; label: qsTr("False positive rate") }
				CheckBox { name: "plotFNF"; label: qsTr("False negative rate") }
				CheckBox { name: "plotAccuracy"; label: qsTr("Accuracy"); Layout.columnSpan: 2 }
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
				name:					"credibleInterval"
				label:					qsTr("Credible intervals")
				childrenOnSameRow:		true
				checked:				true
				CIField { name: "ciLevel" }
			}

			IntegerField
			{
				name: "numberOfSamples"
				label: qsTr("Number of posterior samples")
				defaultValue: 10000
				min: 1000
				fieldWidth: 50
			}
		}

	}
}
