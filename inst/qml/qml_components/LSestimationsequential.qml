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
	title:		qsTr("Sequential Analysis")

	property string analysisType:	"binomial"
	property alias plotsIterativeOverlying:		plotsIterativeOverlying
	property alias plotsIterativeInterval:		plotsIterativeInterval
	property alias plotsIterativeStacked:		plotsIterativeStacked
	property alias doIterative:					doIterative

	CheckBox
	{
		name:		"plotsIterativeOverlying"
		id:			plotsIterativeOverlying
		label:		qsTr("Point estimate")

		DropDown
		{
			name:		"plotsIterativeEstimateType"
			values:		["mean", "median", "mode"]
		}

		CheckBox
		{
			name:				"plotsIterativeOverlyingCI"
			label:				qsTr("CI")
			id:					plotsIterativeOverlyingCI
			childrenOnSameRow:	true

			DropDown
			{
				name:			"plotsIterativeOverlyingType"
				label:			""
				values:			["central", "HPD", "support"]
				id:				plotsIterativeOverlyingType
			}
		}

		CIField
		{
			visible:			plotsIterativeOverlyingType.currentText == "central" |	plotsIterativeOverlyingType.currentText == "HPD"
			enabled:			plotsIterativeOverlyingCI.checked
			name:				"plotsIterativeCoverage"
			label:				qsTr("Mass")
			fieldWidth:			50
			defaultValue:		95
			min:				0
			max:				100
			inclusive:			JASP.MaxOnly
		}

		FormulaField
		{
			visible:			plotsIterativeOverlyingType.currentText == "support"
			enabled:			plotsIterativeOverlyingCI.checked
			name:				"plotsIterativeBF"
			label:				qsTr("BF")
			fieldWidth:			50
			defaultValue:		"1"
			min:				0
			inclusive:			JASP.None
		}

		CheckBox
		{
			name: 	"plotsIterativeUpdatingTable"
			label:	qsTr("Updating table")
		}

	}

	CheckBox
	{
		name:		"plotsIterativeInterval"
		id:			plotsIterativeInterval
		label:		qsTr("Interval")
		checked:	false

		RadioButtonGroup
		{
			name:		"plotsIterativeIntervalType"
			id:			plotsIterativeIntervalType

			Group
			{
				columns: 2
				DoubleField
				{
					enabled:			plotsIterativeInterval.checked
					name:				"plotsIterativeIntervalLower"
					label:				qsTr("Lower")
					id:					plotsIterativeIntervalLower
					fieldWidth:			50
					defaultValue:		analysisType === "binomial" ? 0.25 : -1
					min:				analysisType === "binomial" ? 0    : -9999999999
					max:				plotsIterativeIntervalUpper.value
					inclusive:			JASP.MinOnly
				}

				DoubleField
				{
					enabled:			plotsIterativeInterval.checked
					name:				"plotsIterativeIntervalUpper"
					label:				qsTr("Upper")
					id:					plotsIterativeIntervalUpper
					fieldWidth:			50
					defaultValue:		analysisType === "binomial" ? 0.75 : 1
					min:				plotsIterativeIntervalLower.value
					max:				analysisType === "binomial" ? 1    : 9999999999
					inclusive: 			JASP.MaxOnly
				}
			}

			RadioButton
			{
				value:	"overlying"
				label:	qsTr("All")
			}

			RadioButton
			{
				value:	"stacked"
				label:	qsTr("Stacked")
			}

			CheckBox
			{
				name: 	"plotsIterativeIntervalUpdatingTable"
				label:	qsTr("Updating table")
			}
		}
	}

	CheckBox
	{
		name:	"plotsIterativeStacked"
		id:		plotsIterativeStacked
		label:	qsTr("Stacked distributions")
	}
	
	CheckBox
	{
		Layout.columnSpan: 2
		name:		"doIterative"
		id:			doIterative
		label:		qsTr("Posterior updating table")
		checked:	false
	}
}
