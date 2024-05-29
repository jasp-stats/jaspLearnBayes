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
	expanded:	true
	title:		qsTr("Data")
	columns:	1

	property alias dataInputType: dataInputType

	RadioButtonGroup
	{
		columns:	3
		name:		"dataInputType"
		title:		qsTr("Input Type")
		id:			dataInputType
		defaultValue: dataSetInfo.dataAvailable ? "variable" : "counts"


		RadioButton
		{
			value:		"variable"
			label:		qsTr("Select variable")
			id:			dataInputTypeC
			enabled:	dataSetInfo.dataAvailable
		}

		RadioButton
		{
			value:		"counts"
			label:		qsTr("Specify counts")
			id:			dataInputTypeA
		}

		RadioButton
		{
			value:		"sequence"
			label:		qsTr("Enter sequence")
			id:			dataInputTypeB
		}

	}

	Group
	{
		title: qsTr("Count Data")
		visible: dataInputTypeA.checked

		IntegerField
		{
			name:			"dataCountsSuccesses"
			label:			qsTr("Successes")
			defaultValue: 	0
			id: 			nSuccesses
		}

		IntegerField
		{
			name:			"dataCountsFailures"
			label:			qsTr("Failures")
			defaultValue:	0
			id:				nFailures
		}
	}

	TextArea
	{
		title:		qsTr("Comma-separated Sequence of Observations")
		visible:	dataInputTypeB.checked
		height:		100
		name:		"dataSequenceSequenceOfObservations"
		textType:	JASP.TextTypeSource
		separators:	[",",";","\n"]
	}

	Group
	{
		visible:	dataInputTypeB.checked

		VariablesForm
		{
			preferredHeight:	200

			AvailableVariablesList
			{
				name:	"dataSequenceLevels"
				title:	qsTr("Levels")
				source:	"dataSequenceSequenceOfObservations"
			}

			AssignedVariablesList
			{
				name:	"dataSequenceSuccesses"
				title:	qsTr("Successes")
			}

			AssignedVariablesList
			{
				name:	"dataSequenceFailures"
				title:	qsTr("Failures")
			}
		}
	}

	Group
	{
		visible: dataInputTypeC.checked

		VariablesForm
		{
			preferredHeight:	150

			AvailableVariablesList
			{
				name:	"dataVariableAvailable"
				title:	qsTr("Available")
			}

			AssignedVariablesList
			{
				name:				"dataVariableSelected"
				title:				qsTr("Selected")
				singleVariable:		true
				allowedColumns:		["ordinal", "nominal"]

				onCountChanged:
				{
					while (dataVariableSuccesses.count > 0)
						dataVariableSuccesses.itemDoubleClicked(0)
					while (dataVariableFailures.count > 0)
						dataVariableFailures.itemDoubleClicked(0)
				}
			}
		}

		VariablesForm
		{
			preferredHeight: 200

			AvailableVariablesList
			{
				name:	"dataVariableLevels"
				title:	qsTr("Levels")
				source:	[{name: "dataVariableSelected", use: "levels"}]
			}

			AssignedVariablesList
			{
				id:		dataVariableSuccesses
				name:	"dataVariableSuccesses"
				title:	qsTr("Successes")
			}

			AssignedVariablesList
			{
				id:		dataVariableFailures
				name:	"dataVariableFailures"
				title:	qsTr("Failures")
			}
		}
	}

	CheckBox
	{
		visible:	dataInputTypeB.checked || dataInputTypeC.checked
		name:		"dataSummary"
		label:		qsTr("Data summary")
		checked:	true
	}

}
