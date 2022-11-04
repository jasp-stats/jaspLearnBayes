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
import QtQuick			2.8
import QtQuick.Layouts	1.3
import JASP.Controls	1.0
import JASP.Widgets		1.0
import JASP				1.0

Section
{
	expanded: false
	title: qsTr("Prior and Posterior")
	columns: 2

	property string analysisType:				"binomial"
	property alias plotsPosteriorObserved:		plotsPosteriorObserved.label
	property alias plotsBothSampleProportion:	plotsBothSampleProportion.label


	CheckBox
	{
		name:		"priorDistributionPlot"
		label:		qsTr("Prior distribution")
		checked:	false

		RadioButtonGroup
		{
			name:	"priorDistributionPlotType"

			RadioButton
			{
				value:		"conditional"
				label:		qsTr("Conditional")
				checked:	true

				CheckBox
				{
					label:				qsTr("Point estimate")
					name:				"priorDistributionPlotConditionalPointEstimate"
					childrenOnSameRow:	true

					DropDown
					{
						name:	"priorDistributionPlotConditionalPointEstimateType"
						label:	""
						values:	["mean", "median", "mode"]
					}
				}

				CheckBox
				{
					name:				"priorDistributionPlotConditionalCi"
					label:				qsTr("CI")
					id:					plotsPriorCI
					childrenOnSameRow:	true

					DropDown
					{
						name:	"priorDistributionPlotConditionalCiType"
						label:	""
						values:	["central", "HPD", "custom"]
						id:		plotsPriorTypeCI
					}
				}

				Group
				{
					columns:	2

					CIField
					{
						visible:		plotsPriorTypeCI.currentText == "central" | plotsPriorTypeCI.currentText == "HPD"
						enabled:		plotsPriorCI.checked
						name:			"priorDistributionPlotConditionalCiMass"
						label:			qsTr("Mass")
						fieldWidth:		50
						defaultValue:	95
						min:			0
						max:			100
						inclusive:		JASP.MaxOnly
					}

					DoubleField
					{
						visible:		plotsPriorTypeCI.currentText == "custom"
						enabled:		plotsPriorCI.checked
						name:			"priorDistributionPlotConditionalCiLower"
						label:			qsTr("Lower")
						id:				plotsPriorLower
						fieldWidth:		50
						defaultValue:	analysisType === "binomial" ? 0.25 : -1
						min:			analysisType === "binomial" ? 0    : -9999999999
						max:			plotsPriorUpper.value
						inclusive:		JASP.MinMax
					}

					DoubleField
					{
						visible:		plotsPriorTypeCI.currentText == "custom"
						enabled:		plotsPriorCI.checked
						name:			"priorDistributionPlotConditionalCiUpper"
						label:			qsTr("Upper")
						id:				plotsPriorUpper
						fieldWidth:		50
						defaultValue:	analysisType === "binomial" ? 0.75 : 1
						min:			plotsPriorLower.value
						max:			analysisType === "binomial" ? 1    : 9999999999
						inclusive:		JASP.MinMax
					}
				}
			}

			RadioButton
			{
				value:	"joint"
				label:	qsTr("Joint")

				RadioButtonGroup
				{
					name:	"priorDistributionPlotJointType"

					RadioButton
					{
						value:		"overlying"
						label:		qsTr("Overlying")
						checked:	true
					}

					RadioButton
					{
						value:		"stacked"
						label:		qsTr("Stacked")
					}

				}

			}

			RadioButton
			{
				value:	"marginal";
				label:	qsTr("Marginal")

				CheckBox
				{
					label:				qsTr("Point estimate")
					name:				"priorDistributionPlotMarginalPointEstimate"
					childrenOnSameRow:	true

					DropDown
					{
						name:	"priorDistributionPlotMarginalPointEstimateType"
						label:	""
						values:	["mean", "median", "mode"]
					}
				}

				CheckBox
				{
					name:				"priorDistributionPlotMarginalCi"
					label:				qsTr("CI")
					id:					plotsPriorMarginalCI
					childrenOnSameRow:	true

					DropDown
					{
						name:		"priorDistributionPlotMarginalCiType"
						label:		""
						values:		["central", "HPD", "custom"]
						id:			plotsPriorMarginalType
					}
				}

				Group
				{
					columns:	2

					CIField{
						visible:		plotsPriorMarginalType.currentText == "central" | plotsPriorMarginalType.currentText == "HPD"
						enabled:		plotsPriorMarginalCI.checked
						name:			"priorDistributionPlotMarginalCiMass"
						label:			qsTr("Mass")
						fieldWidth:		50
						defaultValue:	95
						min:			0
						max:			100
						inclusive:		JASP.MaxOnly
					}

					DoubleField
					{
						visible:		plotsPriorMarginalType.currentText == "custom"
						enabled:		plotsPriorMarginalCI.checked
						name:			"priorDistributionPlotMarginalCiLower"
						label:			qsTr("Lower")
						id:				plotsPriorMarginalLower
						fieldWidth:		50
						defaultValue:	analysisType === "binomial" ? 0.25 : -1
						min:			analysisType === "binomial" ? 0    : -9999999999
						max:			plotsPriorMarginalUpper.value
						inclusive:		JASP.MinMax
					}

					DoubleField
					{
						visible:		plotsPriorMarginalType.currentText == "custom"
						enabled:		plotsPriorMarginalCI.checked
						name:			"priorDistributionPlotMarginalCiUpper"
						label:			qsTr("Upper")
						id:				plotsPriorMarginalUpper
						fieldWidth:		50
						defaultValue:	analysisType === "binomial" ? 0.75 : 1
						min:			plotsPriorMarginalLower.value
						max:			analysisType === "binomial" ? 1    : 9999999999
						inclusive:		JASP.MinMax
					}
				}

			}
		}
	}


	CheckBox
	{
		name:		"posteriorDistributionPlot"
		label:		qsTr("Posterior distribution")
		checked:	false

		RadioButtonGroup
		{
			name:	"posteriorDistributionPlotType"

			RadioButton
			{
				checked:	true
				value:		"conditional"
				label:		qsTr("Conditional")

				CheckBox
				{
					label:				qsTr("Point estimate")
					name:				"posteriorDistributionPlotConditionalPointEstimate"
					childrenOnSameRow:	true

					DropDown
					{
						name:	"posteriorDistributionPlotConditionalPointEstimateType"
						label:	""
						values:	["mean", "median", "mode"]
					}
				}

				CheckBox
				{
					name:				"posteriorDistributionPlotConditionalCi"
					label:				qsTr("CI")
					id:					posteriorDistributionPlotConditionalCi
					childrenOnSameRow:	true

					DropDown
					{
						name:	"posteriorDistributionPlotConditionalCiType"
						label:	""
						values:	["central", "HPD", "custom","support"]
						id:		posteriorDistributionPlotConditionalCiType
					}
				}

				Group
				{
					columns:	2

					CIField
					{
						visible:		posteriorDistributionPlotConditionalCiType.currentText == "central" | posteriorDistributionPlotConditionalCiType.currentText == "HPD"
						enabled:		posteriorDistributionPlotConditionalCi.checked
						name:			"posteriorDistributionPlotConditionalCiMass"
						label:			qsTr("Mass")
						fieldWidth:		50
						defaultValue:	95
						min:			0
						max:			100
						inclusive:		JASP.MaxOnly
					}

					DoubleField
					{
						visible:		posteriorDistributionPlotConditionalCiType.currentText == "custom"
						enabled:		posteriorDistributionPlotConditionalCi.checked
						name:			"posteriorDistributionPlotConditionalCiLower"
						label:			qsTr("Lower")
						id:				posteriorDistributionPlotConditionalCiLower
						fieldWidth:		50
						defaultValue:	analysisType === "binomial" ? 0.25 : -1
						min:			analysisType === "binomial" ? 0    : -9999999999
						max:			"posteriorDistributionPlotConditionalCiUpper".value
						inclusive:		JASP.MinMax
					}

					DoubleField
					{
						visible:		posteriorDistributionPlotConditionalCiType.currentText == "custom"
						enabled:		posteriorDistributionPlotConditionalCi.checked
						name:			"posteriorDistributionPlotConditionalCiUpper"
						label:			qsTr("Upper")
						id:				posteriorDistributionPlotConditionalCiUpper
						fieldWidth:		50
						defaultValue:	analysisType === "binomial" ? 0.75 : 1
						min:			posteriorDistributionPlotConditionalCiLower.value
						max:			analysisType === "binomial" ? 1    : 9999999999
						inclusive:		JASP.MinMax
					}

					FormulaField
					{
						visible:		posteriorDistributionPlotConditionalCiType.currentText == "support"
						enabled:		posteriorDistributionPlotConditionalCi.checked
						name:			"posteriorDistributionPlotConditionalCiBf"
						label:			qsTr("BF")
						fieldWidth:		50
						defaultValue:	"1"
						min:			0
						inclusive:		JASP.None
					}
				}

			}

			RadioButton
			{
				value:	"joint"
				label:	qsTr("Joint")

				RadioButtonGroup
				{
					name:	"posteriorDistributionPlotJointType"

					RadioButton
					{
						value:		"overlying"
						label:		qsTr("Overlying")
						checked:	true
					}

					RadioButton
					{
						value:		"stacked"
						label:		qsTr("Stacked")
					}

				}

			}

			RadioButton
			{
				value:	"marginal";
				label:	qsTr("Marginal")

				CheckBox
				{
					label:				qsTr("Point estimate")
					name:				"posteriorDistributionPlotMarginalPointEstimate"
					childrenOnSameRow:	true

					DropDown
					{
						name:		"posteriorDistributionPlotMarginalPointEstimateType"
						label:		""
						values:		["mean", "median", "mode"]
					}
				}

				CheckBox
				{
					name:				"posteriorDistributionPlotMarginalPointEstimateType"
					label:				qsTr("CI")
					id:					plotsPosteriorMarginalCI
					childrenOnSameRow:	true

					DropDown
					{
						name:		"posteriorDistributionPlotMarginalCiType"
						label:		""
						values:		["central", "HPD", "custom","support"]
						id:			plotsPosteriorMarginalType
					}
				}

				Group
				{
					columns:	2

					CIField
					{
						visible:		plotsPosteriorMarginalType.currentText == "central" | plotsPosteriorMarginalType.currentText == "HPD"
						enabled:		plotsPosteriorMarginalCI.checked
						name:			"posteriorDistributionPlotMarginalCiMass"
						label:			qsTr("Mass")
						fieldWidth:		50
						defaultValue:	95
						min:			0
						max:			100
						inclusive:		JASP.MaxOnly
					}

					DoubleField
					{
						visible:		plotsPosteriorMarginalType.currentText == "custom"
						enabled:		plotsPosteriorMarginalCI.checked
						name:			"posteriorDistributionPlotMarginalCiLower"
						label:			qsTr("Lower")
						id:				plotsPosteriorMarginalLower
						fieldWidth:		50
						defaultValue:	analysisType === "binomial" ? 0.25 : -1
						min:			analysisType === "binomial" ? 0    : -9999999999
						max:			plotsPosteriorMarginalUpper.value; inclusive: JASP.MinMax
					}

					DoubleField
					{
						visible:		plotsPosteriorMarginalType.currentText == "custom"
						enabled:		plotsPosteriorMarginalCI.checked
						name:			"posteriorDistributionPlotMarginalCiUpper"
						label:			qsTr("Upper")
						id:				plotsPosteriorMarginalUpper
						fieldWidth:		50
						defaultValue:	analysisType === "binomial" ? 0.75 : 1
						min:			plotsPosteriorMarginalLower.value
						max:			analysisType === "binomial" ? 1    : 9999999999
						inclusive:		JASP.MinMax
					}

					FormulaField
					{
						visible:		plotsPosteriorMarginalType.currentText == "support"
						enabled:		plotsPosteriorMarginalCI.checked
						name:			"posteriorDistributionPlotMarginalCiBf"
						label:			qsTr("BF")
						fieldWidth:		50
						defaultValue:	"1"
						min:			0
						inclusive:		JASP.None
					}
				}


			}

		}

		CheckBox
		{
			name:		"posteriorDistributionPlotObservedProportion"
			id:			plotsPosteriorObserved
			label:		qsTr("Observed proportion")
			checked:	false
		}
	}


	CheckBox
	{
		name:		"priorAndPosteriorDistributionPlot"
		label:		qsTr("Prior and posterior distribution")
		checked:	false

		RadioButtonGroup
		{
			name:	"priorAndPosteriorDistributionPlotType"

			RadioButton
			{
				checked:	true
				value:		"conditional"
				label:		qsTr("Conditional")
			}

			RadioButton
			{
				value:		"joint";
				label:		qsTr("Joint")
			}

			RadioButton
			{
				value:		"marginal";
				label:		qsTr("Marginal")
			}

		}

		CheckBox
		{
			name:		"priorAndPosteriorDistributionPlotObservedProportion"
			id:			plotsBothSampleProportion
			label:		qsTr("Observed proportion")
			checked:	false
		}
	}

}
