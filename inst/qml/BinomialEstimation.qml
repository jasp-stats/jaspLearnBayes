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

	Group
	{

		RadioButtonGroup
		{
		name: "dataType"
		title: qsTr("Enter data")
			RadioButton { value: "dataCounts"; 		label: qsTr("Counts"); 			id: dataTypeA;	checked: true 	}
			RadioButton { value: "dataSequence"; 	label: qsTr("Sequence"); 		id: dataTypeB					}
			RadioButton { value: "dataVariable"; 	label: qsTr("Select variable");	id: dataTypeC					}
		}

		Group
		{
		title: qsTr("Enter count data")
		visible: dataTypeA.checked
		IntegerField { name: "nSuccesses";	label: qsTr("Number of successes");	defaultValue: 0 }
		IntegerField { name: "nFailures";	label: qsTr("Number of failures");	defaultValue: 0 }
		}

		Group
		{
		title: qsTr("Enter the sequence of observation")
		visible: dataTypeB.checked
		TextField { 
			name: "data_sequence"; 
			label: "";
			placeholderText: qsTr("Enter the sequence of successes or failures (ie. '101101')")
			fieldWidth: 400 
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
				name: "key_success"; 
				label: qsTr("Successes");
				fieldWidth: 75
			}
			TextField {
				name: "key_failure"; 
				label: qsTr("Failures");
				fieldWidth: 75 
			}
			}
		}
		

		Group
		{
		visible: dataTypeB.checked || dataTypeC.checked
			Group
			{
			CheckBox { name: "dataSummary"; label: qsTr("Show data summary");			checked: true }
			CheckBox { name: "doIterative"; label: qsTr("Show iterative computation");	checked: false }
			}
		}
	}

	Section
	{
		expanded: true
		title: "Hypotheses"

		InputListView
		{
			height: 200
			title				: qsTr("Distributions")
			name				: "priors"
			optionKey			: "name"
			placeHolder			: qsTr("New Hypothesis")
			rowComponentsTitles: [qsTr("Parameters")]

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
						label: "alpha"
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
						label: "beta"
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
						label: "theta"
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
		title: "Plots"
		Layout.columnSpan: 2
		Group
		{
			CheckBox
			{
				name: "plotsPrior"; label: qsTr("Prior"); checked: false	;
				RadioButtonGroup
				{
					name: "plotsPriorType"
					RadioButton { value: "overlying"; 	label: qsTr("Overlying densities")	}
					RadioButton { value: "stacked"; 	label: qsTr("Stacked densities")	}
				}
			}

			CheckBox
			{
				name: "plotsPosterior"; label: qsTr("Posterior"); checked: false
				RadioButtonGroup
				{
					name: "plotsPosteriorType"
					RadioButton { value: "overlying"; 	label: qsTr("Overlying densities")	}
					RadioButton { value: "stacked"; 	label: qsTr("Stacked densities")	}
				}
			}

			CheckBox { 	name: "plotsBoth"; 		label: qsTr("Prior and Posterior");	checked: false}
			CheckBox { 	name: "plotsIterative";	label: qsTr("Iterative");			checked: false;
						visible: dataTypeB.checked || dataTypeC.checked}
		}
	}



	Section
	{
		expanded: true
		title: "Prediction"
		
		Group
		{
			title: qsTr("Show prediction after")
			IntegerField { name: "predictionSuccess";	label: qsTr("Number of successes");	defaultValue: 1  }
			IntegerField { name: "predictionFailure";	label: qsTr("Number of failures");	defaultValue: 1  }
		}
	}
    

}
