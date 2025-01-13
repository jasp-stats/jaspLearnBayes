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
import QtQuick
import QtQuick.Layouts
import JASP.Controls
import JASP

Section
{
	expanded:	false
	title:		qsTr("Prior and Posterior Distributions")
	columns:	2

	property string analysisType:							"binomial"
	property alias	plotsPosteriorIndividualProportion:		plotsPosteriorIndividualProportion.label

	DropDown
	{
		Layout.columnSpan:	2
		name:				"priorAndPosteriorPointEstimate"
		label:				qsTr("Point estimate")
		values:				["mean", "median", "mode"]
	}


	CheckBox
	{
		name:		"priorDistributionPlot"
		label:		qsTr("Prior distribution")
		checked:	false

		RadioButtonGroup
		{
  		title:		qsTr("Type")
			name:	"priorDistributionPlotType"

			RadioButton
			{
				value:		"overlying"
				label:		qsTr("All")
				checked:	true
			}

			RadioButton
			{
				value:		"stacked"
				label:		qsTr("Stacked")
			}

			RadioButton
			{
				value:		"individual"
				label:		qsTr("Individual")

				CheckBox
				{
					label:				qsTr("Point estimate")
					name:				"priorDistributionPlotIndividualPointEstimate"
					childrenOnSameRow:	true

					DropDown
					{
						name:		"priorDistributionPlotIndividualPointEstimateType"
						label:		""
						values:		["mean", "median", "mode"]
					}
				}

				CheckBox
				{
					name:				"priorDistributionPlotIndividualCi"
					label:				qsTr("CI")
					id: 				plotsPriorIndividualCI
					childrenOnSameRow:	true

					DropDown
					{
						name:		"priorDistributionPlotIndividualCiType"
						label:		""
						values:		["central", "HPD", "custom"]
						id:			plotsPriorIndividualType
					}
				}

				Group
				{
					columns: 2
					CIField
					{
						visible:		plotsPriorIndividualType.currentText == "central" | plotsPriorIndividualType.currentText == "HPD"
						enabled:		plotsPriorIndividualCI.checked
						name:			"priorDistributionPlotIndividualCiMass"
						label:			qsTr("Mass")
						fieldWidth:		50
						defaultValue:	95
						min:			1
						max:			100
						inclusive:		JASP.MinMax
					}

					DoubleField
					{
						visible:		plotsPriorIndividualType.currentText == "custom"
						enabled:		plotsPriorIndividualCI.checked
						name:			"priorDistributionPlotIndividualCiLower"
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
						visible:		plotsPriorIndividualType.currentText == "custom"
						enabled:		plotsPriorIndividualCI.checked
						name:			"priorDistributionPlotIndividualCiUpper"
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
		}
	}

	CheckBox
	{
		name:		"posteriorDistributionPlot"
		label:		qsTr("Posterior distribution")
		checked:	false

		RadioButtonGroup
		{
  		title:		qsTr("Type")
			name: 	"posteriorDistributionPlotType"

			RadioButton
			{
				value:		"overlying"
				label:		qsTr("All")
				checked:	true
			}

			RadioButton
			{
				value:		"stacked"
				label:		qsTr("Stacked")
			}

			RadioButton
			{
				value:	"individual"
				label:	qsTr("Individual")

				CheckBox
				{
					label:				qsTr("Point estimate")
					name:				"posteriorDistributionPlotIndividualPointEstimate"
					childrenOnSameRow:	true

					DropDown
					{
						name:	"posteriorDistributionPlotIndividualPointEstimateType"
						label:	""
						values:	["mean", "median", "mode"]
					}
				}

				CheckBox
				{
					name:				"posteriorDistributionPlotIndividualCi"
					label:				qsTr("CI")
					id:					plotsPosteriorIndividualCI
					childrenOnSameRow:	true

					DropDown
					{
						name:		"posteriorDistributionPlotIndividualCiType"
						label:		""
						values:		["central", "HPD", "custom", "support"]
						id:			plotsPosteriorIndividualType
					}
				}

				Group
				{
					columns: 2

					CIField
					{
						visible:		plotsPosteriorIndividualType.currentText == "central" | plotsPosteriorIndividualType.currentText == "HPD"
						enabled:		plotsPosteriorIndividualCI.checked
						name:			"posteriorDistributionPlotIndividualCiMass"
						label:			qsTr("Mass")
						fieldWidth:		50
						defaultValue: 	95
						min:			1
						max:			100
						inclusive:		JASP.MinMax
					}

					DoubleField
					{
						visible:		plotsPosteriorIndividualType.currentText == "custom"
						enabled:		plotsPosteriorIndividualCI.checked
						name:			"posteriorDistributionPlotIndividualCiLower"
						label:			qsTr("Lower")
						id:				plotsPosteriorLower
						fieldWidth:		50
						defaultValue:	analysisType === "binomial" ? 0.25 : -1
						min:			analysisType === "binomial" ? 0    : -9999999999
						max:			plotsPosteriorUpper.value
						inclusive:		JASP.MinMax
					}

					DoubleField
					{
						visible:		plotsPosteriorIndividualType.currentText == "custom"
						enabled:		plotsPosteriorIndividualCI.checked
						name:			"posteriorDistributionPlotIndividualCiUpper"
						label:			qsTr("Upper")
						id:				plotsPosteriorUpper
						fieldWidth:		50
						defaultValue:	analysisType === "binomial" ? 0.75 : 1
						min:			plotsPosteriorLower.value
						max:			analysisType === "binomial" ? 1    : 9999999999
						inclusive:		JASP.MinMax
					}

					FormulaField
					{
						visible:		plotsPosteriorIndividualType.currentText == "support"
						enabled:		plotsPosteriorIndividualCI.checked
						name:			"posteriorDistributionPlotIndividualCiBf"
						label:			qsTr("BF")
						fieldWidth:		50
						defaultValue:	"1"
						min:			0
						inclusive:		JASP.None
					}
				}

				CheckBox
				{
					name:		"posteriorDistributionPloPriorDistribution"
					label:		qsTr("Prior distribution")
					checked:	false
				}

				CheckBox
				{
					name:		"posteriorDistributionPlotObservedProportion"
					label:		qsTr("Observed proportion")
					id:			plotsPosteriorIndividualProportion
					checked:	false
				}

			}

		}
	}

}
