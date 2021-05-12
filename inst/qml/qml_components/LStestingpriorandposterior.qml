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
	title: qsTr("Prior and Posterior Distributions")
	columns: 2

	property string analysisType:				"binomial"
	property alias plotsPosteriorObserved:		plotsPosteriorObserved.label
	property alias plotsBothSampleProportion:	plotsBothSampleProportion.label


	CheckBox
	{
		name:		"plotsPrior"
		label:		qsTr("Prior distribution")
		checked:	false

		RadioButtonGroup
		{
			name:	"plotsPriorType"

			RadioButton
			{
				value:		"conditional"
				label:		qsTr("Conditional")
				checked:	true

				CheckBox
				{
					label:				qsTr("Point estimate")
					name:				"plotsPriorEstimate"
					childrenOnSameRow:	true

					DropDown
					{
						name:	"plotsPriorEstimateType"
						label:	""
						values:	["mean", "median", "mode"]
					}
				}

				CheckBox
				{
					name:				"plotsPriorCI"
					label:				qsTr("CI")
					id:					plotsPriorCI
					childrenOnSameRow:	true

					DropDown
					{
						name:	"plotsPriorTypeCI"
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
						name:			"plotsPriorCoverage"
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
						name:			"plotsPriorLower"
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
						name:			"plotsPriorUpper"
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
					name:	"plotsPriorJointType"

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
					name:				"plotsPriorMarginalEstimate"
					childrenOnSameRow:	true

					DropDown
					{
						name:	"plotsPriorMarginalEstimateType"
						label:	""
						values:	["mean", "median", "mode"]
					}
				}

				CheckBox
				{
					name:				"plotsPriorMarginalCI"
					label:				qsTr("CI")
					id:					plotsPriorMarginalCI
					childrenOnSameRow:	true

					DropDown
					{
						name:		"plotsPriorMarginalType"
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
						name:			"plotsPriorMarginalCoverage"
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
						name:			"plotsPriorMarginalLower"
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
						name:			"plotsPriorMarginalUpper"
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
		name:		"plotsPosterior"
		label:		qsTr("Posterior distribution")
		checked:	false

		RadioButtonGroup
		{
			name:	"plotsPosteriorType"

			RadioButton
			{
				checked:	true
				value:		"conditional"
				label:		qsTr("Conditional")

				CheckBox
				{
					label:				qsTr("Point estimate")
					name:				"plotsPosteriorEstimate"
					childrenOnSameRow:	true

					DropDown
					{
						name:	"plotsPosteriorEstimateType"
						label:	""
						values:	["mean", "median", "mode"]
					}
				}

				CheckBox
				{
					name:				"plotsPosteriorCI"
					label:				qsTr("CI")
					id:					plotsPosteriorCI
					childrenOnSameRow:	true

					DropDown
					{
						name:	"plotsPosteriorTypeCI"
						label:	""
						values:	["central", "HPD", "custom","support"]
						id:		plotsPosteriorTypeCI
					}
				}

				Group
				{
					columns:	2

					CIField
					{
						visible:		plotsPosteriorTypeCI.currentText == "central" | plotsPosteriorTypeCI.currentText == "HPD"
						enabled:		plotsPosteriorCI.checked
						name:			"plotsPosteriorCoverage"
						label:			qsTr("Mass")
						fieldWidth:		50
						defaultValue:	95
						min:			0
						max:			100
						inclusive:		JASP.MaxOnly
					}

					DoubleField
					{
						visible:		plotsPosteriorTypeCI.currentText == "custom"
						enabled:		plotsPosteriorCI.checked
						name:			"plotsPosteriorLower"
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
						visible:		plotsPosteriorTypeCI.currentText == "custom"
						enabled:		plotsPosteriorCI.checked
						name:			"plotsPosteriorUpper"
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
						visible:		plotsPosteriorTypeCI.currentText == "support"
						enabled:		plotsPosteriorCI.checked
						name:			"plotsPosteriorBF"
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
					name:	"plotsPosteriorJointType"

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
					name:				"plotsPosteriorMarginalEstimate"
					childrenOnSameRow:	true

					DropDown
					{
						name:		"plotsPosteriorMarginalEstimateType"
						label:		""
						values:		["mean", "median", "mode"]
					}
				}

				CheckBox
				{
					name:				"plotsPosteriorMarginalCI"
					label:				qsTr("CI")
					id:					plotsPosteriorMarginalCI
					childrenOnSameRow:	true

					DropDown
					{
						name:		"plotsPosteriorMarginalType"
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
						name:			"plotsPosteriorMarginalCoverage"
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
						name:			"plotsPosteriorMarginalLower"
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
						name:			"plotsPosteriorMarginalUpper"
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
						name:			"plotsPosteriorMarginalBF"
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
			name:		"plotsPosteriorObserved"
			id:			plotsPosteriorObserved
			label:		qsTr("Observed proportion")
			checked:	false
		}
	}


	CheckBox
	{
		name:		"plotsBoth"
		label:		qsTr("Prior and posterior distribution")
		checked:	false

		RadioButtonGroup
		{
			name:	"plotsBothType"

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
			name:		"plotsBothSampleProportion"
			id:			plotsBothSampleProportion
			label:		qsTr("Observed proportion")
			checked:	false
		}
	}

}
