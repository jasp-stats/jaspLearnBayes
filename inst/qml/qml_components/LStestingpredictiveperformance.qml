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
	title: qsTr("Prior Predictive Performance")
	columns: 2

	property string analysisType:				"binomial"
	property alias plotsPredictionsObserved:	plotsPredictionsObserved.label
	property alias bfTypevsName: 				bfTypevsName.source


	CheckBox
	{
		name:		"priorPredictivePerformanceDistributionPlot"
		label:		qsTr("Distribution plot")
		checked:	false

		RadioButtonGroup
		{
			title:		qsTr("Type")
			name:		"priorPredictivePerformanceDistributionPlotType"

			RadioButton
			{
				value:		"conditional"
				label:		qsTr("Conditional")
				checked:	true

				CheckBox
				{
					label:				qsTr("Point estimate")
					name:				"priorPredictivePerformanceDistributionPlotConditionalPointEstimate"
					childrenOnSameRow:	true

					DropDown
					{
						name:		"priorPredictivePerformanceDistributionPlotConditionalPointEstimateType"
						label:		""
						values:		["mean", "median", "mode"]
					}
				}

				CheckBox
				{
					name:				"priorPredictivePerformanceDistributionPlotConditionalCi"
					label:				qsTr("CI")
					id:					plotsPredictionCI
					childrenOnSameRow:	true

					DropDown
					{
						name:		"priorPredictivePerformanceDistributionPlotConditionalCiType"
						label:		""
						values:		["central", "HPD", "custom"]
						id:			plotsPredictionTypeCI
					}
				}

				Group
				{
					columns:	2

					CIField
					{
						visible:		plotsPredictionTypeCI.currentText == "central" | plotsPredictionTypeCI.currentText == "HPD"
						enabled:		plotsPredictionCI.checked
						name:			"priorPredictivePerformanceDistributionPlotConditionalCiMass"
						label:			qsTr("Mass")
						fieldWidth:		50
						defaultValue:	95
						min:			1
						max:			100
						inclusive:		JASP.MinMax
					}

					IntegerField
					{
						visible:		plotsPredictionTypeCI.currentText == "custom"
						enabled:		plotsPredictionCI.checked
						name:			"priorPredictivePerformanceDistributionPlotConditionalCiLower"
						label:			qsTr("Lower")
						id:				plotsPredictionLower
						fieldWidth:		50
						defaultValue:	analysisType === "binomial" ? 0		: -1
						min:			analysisType === "binomial" ? 0		: -9999999999
						max:			plotsPredictionUpper.value; inclusive: JASP.MinMax
					}

					IntegerField
					{
						visible:		plotsPredictionTypeCI.currentText == "custom"
						enabled:		plotsPredictionCI.checked
						name:			"priorPredictivePerformanceDistributionPlotConditionalCiUpper"
						label:			qsTr("Upper")
						id:				plotsPredictionUpper
						fieldWidth:		50
						defaultValue:	analysisType === "binomial" ? 1		: 1
						min:			plotsPredictionLower.value
						//max:			analysisType === "binomial" ? 1		: 9999999999
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
				  title:		qsTr("Type")
					name:	"priorPredictivePerformanceDistributionPlotJoinType"

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
					name:				"priorPredictivePerformanceDistributionPlotMarginalPointEstimate"
					childrenOnSameRow:	true

					DropDown
					{
						name:		"priorPredictivePerformanceDistributionPlotMarginalPointEstimateType"
						label:		""
						values:		["mean", "median", "mode"]
					}
				}

				CheckBox
				{
					name:				"priorPredictivePerformanceDistributionPlotMarginalCi"
					label:				qsTr("CI")
					id:					plotsPredictionMarginalCI
					childrenOnSameRow:	true

					DropDown
					{
						name:		"priorPredictivePerformanceDistributionPlotMarginalCiType"
						label:		""
						values:		["central", "HPD", "custom"]
						id:			plotsPredictionMarginalTypeCI
					}
				}

				Group
				{
					columns:	2

					CIField
					{
						visible:		plotsPredictionMarginalTypeCI.currentText == "central" | plotsPredictionMarginalTypeCI.currentText == "HPD"
						enabled:		plotsPredictionMarginalCI.checked
						name:			"priorPredictivePerformanceDistributionPlotMarginalCiMass"
						label:			qsTr("Mass")
						fieldWidth:		50
						defaultValue:	95
						min:			1
						max:			100
						inclusive:		JASP.MinMax
					}

					IntegerField
					{
						visible:		plotsPredictionMarginalTypeCI.currentText == "custom"
						enabled:		plotsPredictionMarginalCI.checked
						name:			"priorPredictivePerformanceDistributionPlotMarginalCiLower"
						label:			qsTr("Lower")
						id:				plotsPredictionMarginalLower
						fieldWidth:		50
						defaultValue:	analysisType === "binomial" ? 0		:  -1
						min:			analysisType === "binomial" ? 0		: -9999999999
						max:			plotsPredictionMarginalUpper.value;
						inclusive:		JASP.MinMax
					}

					IntegerField
					{
						visible:		plotsPredictionMarginalTypeCI.currentText == "custom"
						enabled:		plotsPredictionMarginalCI.checked
						name:			"priorPredictivePerformanceDistributionPlotMarginalCiUpper"
						label:			qsTr("Upper")
						id:				plotsPredictionMarginalUpper
						fieldWidth:		50
						defaultValue:	analysisType === "binomial" ? 1		: 1
						min:			plotsPredictionMarginalLower.value
						//max:			analysisType === "binomial" ? 1    : 9999999999
						inclusive:		JASP.MinOnly
					}
				}
			}

		}

		CheckBox
		{
			name:		"priorPredictivePerformanceDistributionPlotObservedNumberOfSuccessess"
			id:			plotsPredictionsObserved
			label:		qsTr("Observed number of successes")
			checked:	false
		}

		CheckBox
		{
			name:	"priorPredictivePerformanceDistributionPlotPredictionsTable"
			label:	qsTr("Predictions table")
		}
	}


	Group
	{


		CheckBox
		{
			name:		"priorPredictivePerformanceAccuracyPlot"
			label:		qsTr("Predictive accuracy plot")
			checked:	false

			RadioButtonGroup
			{
			  title:		qsTr("Type")
				name: "priorPredictivePerformanceAccuracyPlotType"
				RadioButton { value: "conditional"; label: qsTr("Conditional"); checked: true}
				RadioButton { value: "joint";		label: qsTr("Joint")}
				RadioButton { value: "marginal"; 	label: qsTr("Normalized")}

			}
		}


		Group
		{
			columns:			2
			title:	qsTr("Bayes Factor")

			RadioButtonGroup
			{
			  title:		qsTr("Comparison")
				name:	"priorPredictivePerformanceBfComparison"

				RadioButton
				{
					name:				"vs"
					label:				qsTr("vs.")
					childrenOnSameRow:	true
					checked:			true

					DropDown
					{
						name:				"priorPredictivePerformanceBfVsHypothesis"
						id:					bfTypevsName
						indexDefaultValue:	0
					}
				}

				RadioButton
				{
					value:	"inclusion"
					label:	qsTr("vs. rest")
				}

				RadioButton
				{
					value:	"best"
					label:	qsTr("vs. best")
				}


			}

			RadioButtonGroup
			{
			  title:		qsTr("Type")
				name:	"priorPredictivePerformanceBfType"

				RadioButton { label: qsTr("BF\u2081\u2080")			; name: "BF10"; checked: true}
				RadioButton { label: qsTr("BF\u2080\u2081")			; name: "BF01"}
				RadioButton { label: qsTr("log(BF\u2081\u2080)")	; name: "LogBF10"}
			}
		}
	}

}
