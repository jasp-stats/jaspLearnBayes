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
	expanded:	false
	title:		qsTr("Posterior Prediction")

	property string analysisType:			"binomial"
	property alias predictionPlotProp: predictionPlotProp.label

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

			CheckBox
			{
				name:		"posteriorPredictionDistributionPlot"
				label:		qsTr("Predictive distribution plot")
				checked:	false

				RadioButtonGroup
				{
				  title:		qsTr("Type")
					name:	"posteriorPredictionDistributionPlotType"

					RadioButton
					{
						value:		"conditional"
						label:		qsTr("Conditional")
						checked:	true

						CheckBox
						{
							label:				qsTr("Point estimate")
							name:				"posteriorPredictionDistributionPlotConditionalPointEstimate"
							childrenOnSameRow:	true

							DropDown
							{
								name:		"posteriorPredictionDistributionPlotConditionalPointEstimateType"
								label:		""
								values:		["mean", "median", "mode"]
							}
						}

						CheckBox
						{
							name:				"posteriorPredictionDistributionPlotConditionalCi"
							label:				qsTr("CI")
							id:					plotsPredictionPostCI
							childrenOnSameRow:	true

							DropDown
							{
								name:		"posteriorPredictionDistributionPlotConditionalCiType"
								label:		""
								values:		["central", "HPD", "custom"]
								id:			plotsPredictionPostTypeCI
							}
						}

						Group
						{
							columns:	2

							CIField
							{
								visible:		plotsPredictionPostTypeCI.currentText == "central" | plotsPredictionPostTypeCI.currentText == "HPD"
								enabled:		plotsPredictionPostCI.checked
								name:			"posteriorPredictionDistributionPlotConditionalCiMass"
								label:			qsTr("mass")
								fieldWidth:		50
								defaultValue:	95
								min:			1
								max:			100
								inclusive:		JASP.MinMax
							}

							IntegerField
							{
								visible:		plotsPredictionPostTypeCI.currentText == "custom"
								enabled:		plotsPredictionPostCI.checked
								name:			"posteriorPredictionDistributionPlotConditionalCiLower"
								label:			qsTr("lower")
								id:				plotsPredictionPostLower
								fieldWidth:		50
								defaultValue:	0
								min:			0
								max:			plotsPredictionPostUpper.value
								inclusive:		JASP.MinMax
							}

							IntegerField
							{
								visible:		plotsPredictionPostTypeCI.currentText == "custom"
								enabled:		plotsPredictionPostCI.checked
								name:			"posteriorPredictionDistributionPlotConditionalCiUpper"
								label:			qsTr("upper")
								id:				plotsPredictionPostUpper
								fieldWidth:		50
								defaultValue:	1
								min:			plotsPredictionPostLower.value
								max:			predictionN.value
								inclusive:		JASP.MinMax
							}

						}
					}

					RadioButton
					{
						value:		"joint"
						label:		qsTr("Joint")

						RadioButtonGroup
						{
						  title:		qsTr("Type")
							name:	"posteriorPredictionDistributionPlotJoinType"

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
							name:				"posteriorPredictionDistributionPlotMarginalPointEstimate"
							childrenOnSameRow:	true

							DropDown
							{
								name:		"posteriorPredictionDistributionPlotMarginalPointEstimateType"
								label:		""
								values:		["mean", "median", "mode"]
							}
						}


						CheckBox
						{
							name:					"posteriorPredictionDistributionPlotMarginalCi"
							label:					qsTr("CI")
							id:						plotsPredictionPostMarginalCI
							childrenOnSameRow:		true

							DropDown
							{
								name:		"posteriorPredictionDistributionPlotMarginalCiType"
								label:		""
								values:		["central", "HPD", "custom"]
								id:			plotsPredictionPostMarginalTypeCI
							}
						}

						Group
						{
							columns:	2

							CIField
							{
								visible:			plotsPredictionPostMarginalTypeCI.currentText == "central" | plotsPredictionPostMarginalTypeCI.currentText == "HPD"
								enabled:			plotsPredictionPostMarginalCI.checked
								name:				"posteriorPredictionDistributionPlotMarginalCiMass"
								label:				qsTr("Mass")
								fieldWidth:			50
								defaultValue:		95
								min:				1
								max:				100
								inclusive:			JASP.MinMax
							}

							DoubleField
							{
								id:					plotsPredictionPostMarginalLower
								visible:			plotsPredictionPostMarginalTypeCI.currentText == "custom"
								enabled:			plotsPredictionPostMarginalCI.checked
								name:				"posteriorPredictionDistributionPlotMarginalCiLower"
								label:				qsTr("Lower")
								fieldWidth:			50
								defaultValue:		analysisType === "binomial" ? 0 : -1
								min:				analysisType === "binomial" ? 0 : -9999999999
								max:				plotsPredictionPostMarginalUpper.value
								inclusive:			JASP.MinMax
							}

							DoubleField
							{
								visible:			plotsPredictionPostMarginalTypeCI.currentText == "custom"
								enabled:			plotsPredictionPostMarginalCI.checked
								name:				"posteriorPredictionDistributionPlotMarginalCiUpper"
								label:				qsTr("Upper")
								id:					plotsPredictionPostMarginalUpper
								fieldWidth:			50
								defaultValue:		analysisType === "binomial" ? 1 : 1
								min:				plotsPredictionPostMarginalLower.value
								max:				predictionN.value
								inclusive:			JASP.MinMax
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
