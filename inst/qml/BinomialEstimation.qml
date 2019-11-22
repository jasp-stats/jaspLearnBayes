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
import QtQuick 2.8
import QtQuick.Layouts 1.3
import JASP.Controls 1.0
import JASP.Widgets 1.0
import JASP.Theme 1.0

Form {
	id: form

	Section
	{
		expanded: true
		title: "Data"

		Group{
		
		RadioButtonGroup
		{
		name: "dataType"
		title: qsTr("Input type")
			RadioButton { value: "dataVariable"; 	label: qsTr("Select variable");		id: dataTypeC; 	checked: true}
			RadioButton { value: "dataCounts"; 		label: qsTr("Specify counts"); 		id: dataTypeA 	}
			RadioButton { value: "dataSequence"; 	label: qsTr("Enter sequence"); 		id: dataTypeB	}

		}

		Group
		{
		title: qsTr("Enter count data")
		visible: dataTypeA.checked
		IntegerField { name: "nSuccesses";	label: qsTr("Successes");		defaultValue: 0 }
		IntegerField { name: "nFailures";	label: qsTr("Failures     ");	defaultValue: 0 } 
		// this is definitelly the wrong way how to allign the boxes
		}

		Group
		{
		title: qsTr("Enter comma separated sequence of observation")
		visible: dataTypeB.checked
		TextField { 
			name: "data_sequence"; 
			label: "";
			placeholderText: qsTr("Enter the sequence of successes or failures (ie. '1,0,1,1,0,1')")
			fieldWidth: 400 
			}
		
		Group{
			title: qsTr("Encoding of")
			TextField {
				name: "key_success_Seq"; 
				label: qsTr("Successes");
				fieldWidth: 75
			}
			TextField {
				name: "key_failure_Seq"; 
				label: qsTr("Failures     ");
				fieldWidth: 75 
			}
			}
		}

		Group
		{
		title: qsTr("Select variable containing the experiment")
		visible: dataTypeC.checked
		VariablesForm {
			height: 100
			AvailableVariablesList{ name: "allVariables" }
			AssignedVariablesList{	name: "selectedVariable";
									label: qsTr("Selected variable");
									suggestedColumns: ["nominal"];
									singleVariable: true }
			}
		
		Group{
			title: qsTr("Encoding of")
			TextField {
				name: "key_success_Var"; 
				label: qsTr("Successes");
				fieldWidth: 75
			}
			TextField {
				name: "key_failure_Var"; 
				label: qsTr("Failures     ");
				fieldWidth: 75 
			}
			}
		}

		Group
		{
		visible: dataTypeB.checked || dataTypeC.checked
			Group
			{
			CheckBox { name: "dataSummary"; label: qsTr("Data summary"); checked: true }
			}
		}
		
		}
	}

	Section
	{
		expanded: true
		title: "Model"

		InputListView
		{
			height: 200
			title				: qsTr("Name")
			name				: "priors"
			optionKey			: "name"
			placeHolder			: qsTr("New model")
			rowComponentsTitles: [qsTr("Parameter (θ)")]

			rowComponents:
			[
				Component
				{
					DropDown
					{
						name: "type"
						useExternalBorder: true
						values: ["beta", "point"]
						Layout.rightMargin: 50
					}
				},
				Component
				{
					DoubleField
					{
						label: "α"
						name: "parAlpha"
						visible: fromRowComponents["type"].currentText === "beta"
						defaultValue: 1
						min: 0
						inclusive: "no"
					}
				},
				Component
				{
					DoubleField
					{
						label: "β"
						name: "parBeta"
						visible: fromRowComponents["type"].currentText === "beta"
						defaultValue: 1
						min: 0
						inclusive: "no"
					}
				},
				Component
				{
					DoubleField
					{
						label: "θ"
						name: "parPoint"
						visible: fromRowComponents["type"].currentText === "point"
						Layout.rightMargin: width
						defaultValue: 0.5
						min: 0
						max: 1
					}
				}
			]
		}
	}

	Section
	{
		expanded: true
		title: "Output"
		Layout.columnSpan: 1

		Group
		{
			CheckBox
			{
				visible: dataTypeB.checked || dataTypeC.checked
				name: "doIterative"
				label: qsTr("Sequential analysis")
				checked: false
			}

			Group
			{
				title: "Plots"
				CheckBox
				{
					name: "plotsPrior"; label: qsTr("Prior distribution"); checked: false	;
					RadioButtonGroup
					{
						name: "plotsPriorType"
						RadioButton { value: "overlying"; 	label: qsTr("Overlying"); checked: true}
						RadioButton { value: "stacked"; 	label: qsTr("Stacked")	}
//						RadioButton {
//							value: "individual"
//							label: qsTr("Individual")
//							RadioButtonGroup
//							{
//								name: "plotsPriorIndividualType"
//								RadioButton { value: "central"; label: qsTr("Central");
//									DoubleField{ name: "priorCentralCoverage"; label: qsTr("Coverage");fieldWidth: 45}}
//								RadioButton { value: "HDP"; 	label: qsTr("HDP")}
//								RadioButton { value: "user"; 	label: qsTr("Custom")}
//							}
//						}
					}
				}

				CheckBox
				{
					name: "plotsPosterior"; label: qsTr("Posterior distribution"); checked: false
					RadioButtonGroup
					{
						name: "plotsPosteriorType"
						RadioButton { value: "overlying"; 	label: qsTr("Overlying"); checked: true}
						RadioButton { value: "stacked"; 	label: qsTr("Stacked")	}
					}
				}

				CheckBox { 	name: "plotsBoth"; 		label: qsTr("Prior and Posterior distribution");	checked: false}
				CheckBox
				{
					name: "plotsIterative";	label: qsTr("Sequential analysis"); checked: false;
					visible: dataTypeB.checked || dataTypeC.checked
					RadioButtonGroup
					{
						name: "plotsIterativeType"
						RadioButton { value: "overlying"; 	label: qsTr("Overlying")	}
						RadioButton { value: "stacked"; 	label: qsTr("Stacked")	}
					}
				}
			}
		}
	}

	Section
	{
		expanded: true
		title: "Prediction"
		Layout.columnSpan: 1
	}
	
}