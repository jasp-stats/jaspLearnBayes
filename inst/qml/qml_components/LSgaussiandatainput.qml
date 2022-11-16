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
		id:			dataInputType
		title:		qsTr("Input type")

		RadioButton
		{
			value:		"variable"
			label:		qsTr("Select variable")
			id:			dataInputTypeC
			checked: 	mainWindow.dataAvailable
			enabled:	mainWindow.dataAvailable
		}

		RadioButton
		{
			value:		"counts"
			label:		qsTr("Specify data summary")
			id:			dataInputTypeA
			checked:	!mainWindow.dataAvailable
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
		title: qsTr("Data summary")
		visible: dataInputTypeA.checked

		DoubleField
		{
			name:			"dataCountsMean"
			label:			qsTr("Mean")
			defaultValue: 	0
		}

		DoubleField
		{
			name:			"dataCountsSd"
			label:			qsTr("SD")
			defaultValue:	1
			min:			0
			inclusive:		JASP.None
		}

		DoubleField
		{
			name:			"dataCountsN"
			label:			qsTr("Observations")
			defaultValue:	0
			min:			0
			inclusive:		JASP.MinMax
		}
	}


	TextArea
	{
		title:		qsTr("Comma-separated sequence of observations")
		visible:	dataInputTypeB.checked
		height:		100
		name:		"dataSequenceSequenceOfObservations"
		textType:	JASP.TextTypeSource
		separators:	[",",";","\n"]
	}

	DoubleField
	{
		name:			"dataSequenceSequenceSd"
		visible:		dataInputTypeB.checked
		label:			qsTr("SD")
		defaultValue:	1
		min:			0
		inclusive:		JASP.None
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
				allowedColumns:		["scale"]
			}

			DoubleField
			{
				name:			"dataVariableSd"
				label:			qsTr("SD")
				defaultValue:	1
				min:			0
				inclusive:		JASP.None
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
