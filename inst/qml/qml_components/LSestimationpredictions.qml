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
	title: qsTr("Posterior Prediction")

	property string analysisType:			"binomial"
	property alias 	predictionPlotProp:		predictionPlotProp.label

	Group
	{
		IntegerField
		{
			name:			"posteriorPredictionNumberOfFutureTrials"
			label:			qsTr("Number of future trials")
			id:				predictionN
			min:			1
			defaultValue:	10
		}

		CheckBox
		{
			name:		"posteriorPredictionSummaryTable"
			label:		qsTr("Summary table")

			DropDown
			{
				label:		qsTr("Point estimate")
				name:		"posteriorPredictionSummaryTablePointEstimate"
				values:		["mean", "median", "mode"]
			}
		}

		Group
		{
			title:	qsTr("Plots")

			CheckBox
			{
				label:	qsTr("Distribution plot")
				name:	"posteriorPredictionDistributionPlot"

				RadioButtonGroup
				{
				  title:		qsTr("Type")
					name:	"posteriorPredictionDistributionPlotType"

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
							name:				"posteriorPredictionDistributionPlotIndividualPointEstimate"
							childrenOnSameRow:	true

							DropDown
							{
								name:		"posteriorPredictionDistributionPlotIndividualPointEstimateType"
								label:		""
								values:		["mean", "median", "mode"]
							}
						}

						CheckBox
						{
							name:				"posteriorPredictionDistributionPlotIndividualCi"
							label:				qsTr("CI")
							id:					plotsPredictionCI
							childrenOnSameRow:	true

							DropDown
							{
								name:			"posteriorPredictionDistributionPlotIndividualCiType"
								label:			""
								values:			["central", "HPD", "custom"]
								id:				plotsPredictionType
							}
						}

						Group
						{
							columns: 2

							CIField
							{
								visible:		plotsPredictionType.currentText == "central" | plotsPredictionType.currentText == "HPD"
								enabled:		plotsPredictionCI.checked
								name:			"posteriorPredictionDistributionPlotIndividualCiMass"
								label:			qsTr("Mass")
								fieldWidth:		50
								defaultValue:	95
								min:			1
								max:			100
								inclusive:		JASP.MinMax
							}

							DoubleField
							{
								visible:		plotsPredictionType.currentText == "custom"
								enabled:		plotsPredictionCI.checked
								name:			"posteriorPredictionDistributionPlotIndividualCiLower"
								label:			qsTr("Lower")
								id:				plotsPredictionLower
								fieldWidth:		50
								defaultValue:	analysisType === "binomial" ? 0 : -1
								min:			analysisType === "binomial" ? 0 : -9999999999
								max:			plotsPredictionUpper.value
								inclusive:		JASP.MinMax
							}

							DoubleField
							{
								visible:		plotsPredictionType.currentText == "custom"
								enabled:		plotsPredictionCI.checked
								name:			"posteriorPredictionDistributionPlotIndividualCiUpper"
								label:			qsTr("Upper")
								id:				plotsPredictionUpper
								fieldWidth:		50
								defaultValue:	1
								min:			plotsPredictionLower.value
								max:			predictionPlotProp.checked ? 1	: predictionN.value
								inclusive:		JASP.MinMax
							}

						}
					}

					CheckBox
					{
						name:	"posteriorPredictionDistributionPlotAsSampleProportion"
						id:		predictionPlotProp
						label:	qsTr("As sample proportion")
					}

					CheckBox
					{
						name:	"posteriorPredictionDistributionPlotPredictionsTable"
						label:	qsTr("Predictions table")
					}

				}
			}
		}
	}
}
