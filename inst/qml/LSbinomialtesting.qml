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
		title: qsTr("Hypothesis")


		ColumnLayout
		{
			spacing:				0
			Layout.preferredWidth:	parent.width

			RowLayout
			{
				Label { text: qsTr("Hypothesis");			Layout.leftMargin: 5 * preferencesModel.uiScale; Layout.preferredWidth: 83 * preferencesModel.uiScale}
				Label { text: qsTr("Prior prob.");			Layout.preferredWidth: 78 * preferencesModel.uiScale	}
				Label { text: qsTr("Distribution");			Layout.preferredWidth: 82 * preferencesModel.uiScale	}
				Label { text: qsTr("Parameter (θ)"); 		Layout.preferredWidth: 122 * preferencesModel.uiScale	}
				Label { text: qsTr("Truncation"); }
			}
			ComponentsList
			{
				name:					"models"
				defaultValues: 			[]
				rowComponent: 			RowLayout
				{
					spacing: 10 * preferencesModel.uiScale
					Row
					{
						Layout.preferredWidth:	90 * preferencesModel.uiScale
						TextField
						{
							label: 				""
							name: 				"name"
							startValue:			qsTr("Hypothesis ") + (rowIndex + 1)
							fieldWidth:			90 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder:			true
						}
					}
					Row
					{
						Layout.preferredWidth:	80 * preferencesModel.uiScale
						FormulaField
						{
							label: 				qsTr("P(H)")
							name: 				"priorWeight"
							value:				"1"
							min: 				0
							inclusive: 			JASP.None
							fieldWidth:			40 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder:			true
						}
					}
					Row
					{
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
							value:				"1"
							min:				0
							inclusive:			JASP.None
							fieldWidth:			50 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder:			true
						}
						FormulaField
						{
							label:				qsTr("β")
							name:				"betaPriorBeta"
							visible:			typeItem.currentValue === "beta"
							value:				"1"
							min:				0
							inclusive:			JASP.None
							fieldWidth:			50 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder:			true
						}
						FormulaField
						{
							label:				qsTr("θ₀")
							name:				"spikePoint"
							visible:			typeItem.currentValue === "spike"
							value:				"0.5"
							min:				0
							max:				1
							inclusive:			JASP.MinMax
							fieldWidth:			50 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder:			true
						}
					}
					Row
					{
						spacing:				4 * preferencesModel.uiScale
						FormulaField
						{
							label:				qsTr("Lo")
							name:				"truncationLower"
							id:					truncationLower
							visible:			typeItem.currentValue === "beta"
							value:				"0"
							min:				0
							max:				truncationUpper.value
							inclusive:			JASP.MinOnly
							fieldWidth:			50 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder:			true
						}
						FormulaField
						{
							label:				qsTr("Up")
							name:				"truncationUpper"
							id:					truncationUpper
							visible:			typeItem.currentValue === "beta"
							value:				"1"
							min:				truncationLower.value
							max:				1
							inclusive:			JASP.MaxOnly
							fieldWidth:			50 * preferencesModel.uiScale
							useExternalBorder:	false
							showBorder:			true
						}
					}
				}
			}
		}

	}

	LS.LStestingpriorandposterior{}

	LS.LStestingpredictiveperformance
	{
		bfTypevsName:				"models.name"
	}

	LS.LStestingsequential
	{
		enabled:					binomialDataInput.dataInputType.value !== "counts"
		bfTypevsNameSequential:		"models.name"
		onEnabledChanged:
		{
			if (!enabled) {
				expanded = false
				plotsIterative.checked = false
			}
		}
	}

	LS.LStestingpredictions{}

}
