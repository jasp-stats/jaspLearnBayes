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
import "../qml/qml_components" as LS

Form {
	id: form

	columns: 2

	LS.LSintrotext{}

	ColorPalette{}

	LS.LSbinomialdatainput
	{
		id:	binomialDataInput
	}

	Section
	{
		expanded: true
		title: qsTr("Model")

		ColumnLayout
		{
			spacing:				0
			Layout.preferredWidth:	parent.width

			RowLayout
			{
				Label { text: qsTr("Model");			Layout.leftMargin: 5 * preferencesModel.uiScale; Layout.preferredWidth: 100 * preferencesModel.uiScale}
				Label { text: qsTr("Distribution");		Layout.preferredWidth: 97 * preferencesModel.uiScale}
				Label { text: qsTr("Parameter (θ)"); 	Layout.preferredWidth: 128 * preferencesModel.uiScale}
				Label { text: qsTr("Truncation"); }
			}

			ComponentsList
			{
				name:					"models"
				defaultValues: 			[]
				rowComponent: 			RowLayout
				{
					Row
					{
						spacing:				4 * preferencesModel.uiScale
						Layout.preferredWidth:	100 * preferencesModel.uiScale
						TextField
						{
							label: 				""
							name: 				"name"
							startValue:			qsTr("Model ") + (rowIndex + 1)
							fieldWidth:			100 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder:			true
						}
					}
					Row
					{
						spacing: 4 * preferencesModel.uiScale
						Layout.preferredWidth: 90 * preferencesModel.uiScale
						DropDown
						{
							id: typeItem
							name: "type"
							useExternalBorder: true
							values:
							[
								{ label: qsTr("Beta"),		value: "beta"},
								{ label: qsTr("Spike"),		value: "spike"}
							]
						}
					}
					Row
					{
						spacing:				4 * preferencesModel.uiScale
						FormulaField
						{
							label:				qsTr("α")
							name:				"betaPriorAlpha"
							visible:			typeItem.currentValue === "beta"
							defaultValue:		"1"
							min:				0
							inclusive:			JASP.None
							fieldWidth:			55 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder:			true
						}
						FormulaField
						{
							label:				qsTr("β")
							name:				"betaPriorBeta"
							visible:			typeItem.currentValue === "beta"
							defaultValue:		"1"
							min:				0
							inclusive:			JASP.None
							fieldWidth:			55 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder:			true
						}
						FormulaField
						{
							label:				qsTr("θ₀")
							name:				"spikePoint"
							visible:			typeItem.currentValue === "spike"
							defaultValue:		"0.5"
							min:				0
							max:				1
							inclusive:			JASP.MinMax
							fieldWidth:			55 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder:			true
						}
					}
					Row
					{
						spacing:				4 * preferencesModel.uiScale
						FormulaField
						{
							label:				qsTr("Lower")
							name:				"truncationLower"
							id:					truncationLower
							visible:			typeItem.currentValue === "beta"
							defaultValue:		"0"
							min:				0
							max:				truncationUpper.value
							inclusive:			JASP.MinOnly
							fieldWidth:			55 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder:			true
						}
						FormulaField
						{
							label:				qsTr("Upper")
							name:				"truncationUpper"
							id:					truncationUpper
							visible:			typeItem.currentValue === "beta"
							defaultValue:		"1"
							min:				truncationLower.value
							max:				1
							inclusive:			JASP.MaxOnly
							fieldWidth:			55 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder:			true
						}
					}
				}
			}
		}
	}

	LS.LSestimationpriorandposterior{}

	LS.LSestimationsequential
	{
		enabled: binomialDataInput.dataInputType.value !== "counts"
		onEnabledChanged:
		{
			if (!enabled) {
				expanded = false
				plotsIterativeOverlying.checked = false
				plotsIterativeInterval.checked = false
				plotsIterativeStacked.checked = false
				doIterative.checked = false
			}
		}
	}

	LS.LSestimationpredictions{}
}
