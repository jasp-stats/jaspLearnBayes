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
				
				DropDown
				{
					name: "colorPalette"
					label: qsTr("Color palette")
					indexDefaultValue: 0
					values:
						[
						{ label: qsTr("Colorblind"),		value: "colorblind"		},
						{ label: qsTr("Colorblind Alt."),	value: "colorblind3"	},
						{ label: qsTr("Viridis"),			value: "viridis"		},
						{ label: qsTr("ggplot2"),			value: "ggplot2"		},
						{ label: qsTr("Gray"),				value: "gray"			}
						]
				}
				
				
				CheckBox
				{
					name: "plotsPrior"; label: qsTr("Prior distribution"); checked: false	;
					RadioButtonGroup
					{
						name: "plotsPriorType"
						RadioButton { value: "overlying"; 	label: qsTr("Overlying"); checked: true}
						RadioButton { value: "stacked"; 	label: qsTr("Stacked")	}
						RadioButton {
							value: "individual"
							label: qsTr("Individual")
							
							Group
							{
								columns: 5
								
								CheckBox
								{
									name: "plotsPriorIndividualCI"
									label: qsTr("CI")
									id: plotsPriorIndividualCI
								}
								
								DropDown
								{
									visible: plotsPriorIndividualCI.checked
									name: "plotsPriorIndividualType"
									label: ""
									values: ["Central", "HPD", "Custom"]
									id: plotsPriorIndividualType
								}
								
								DoubleField{
									visible: (plotsPriorIndividualType.currentText == "Central" |
											  plotsPriorIndividualType.currentText == "HPD") &
											 plotsPriorIndividualCI.checked
									name: "plotsPriorCoverage"
									label: qsTr("coverage")
									fieldWidth: 40
									defaultValue: 95; min: 0; max: 100; inclusive: "no"
								}
								
								DoubleField{
									visible: plotsPriorIndividualType.currentText == "Custom" &
											 plotsPriorIndividualCI.checked
									name: "plotsPriorLower"
									label: qsTr("lower")
									id: plotsPriorLower
									fieldWidth: 40
									defaultValue: 0.25; min: 0; max: plotsPriorUpper.value; inclusive: "no"
								}
								
								DoubleField{
									visible: plotsPriorIndividualType.currentText == "Custom" &
											 plotsPriorIndividualCI.checked
									name: "plotsPriorUpper"
									label: qsTr("upper")
									id: plotsPriorUpper
									fieldWidth: 40
									defaultValue: 0.75; min: plotsPriorLower.value; max: 1; inclusive: "no"
								}
							
							}
							
						}

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
						RadioButton {
							value: "individual"
							label: qsTr("Individual")
							
							Group
							{
								columns: 5
								
								CheckBox
								{
									name: "plotsPosteriorIndividualCI"
									label: qsTr("CI")
									id: plotsPosteriorIndividualCI
								}
								
								DropDown
								{
									visible: plotsPosteriorIndividualCI.checked
									name: "plotsPosteriorIndividualType"
									label: ""
									values: ["Central", "HPD", "Custom"]
									id: plotsPosteriorIndividualType
								}
								
								DoubleField{
									visible: (plotsPosteriorIndividualType.currentText == "Central" |
											  plotsPosteriorIndividualType.currentText == "HPD") &
											 plotsPosteriorIndividualCI.checked
									name: "plotsPosteriorCoverage"
									label: qsTr("coverage")
									fieldWidth: 40
									defaultValue: 95; min: 0; max: 100; inclusive: "no"
								}
								
								DoubleField{
									visible: plotsPosteriorIndividualType.currentText == "Custom" &
											 plotsPosteriorIndividualCI.checked
									name: "plotsPosteriorLower"
									label: qsTr("lower")
									id: plotsPosteriorLower
									fieldWidth: 40
									defaultValue: 0.25; min: 0; max: plotsPosteriorUpper.value; inclusive: "no"
								}
								
								DoubleField{
									visible: plotsPosteriorIndividualType.currentText == "Custom" &
											 plotsPosteriorIndividualCI.checked
									name: "plotsPosteriorUpper"
									label: qsTr("upper")
									id: plotsPosteriorUpper
									fieldWidth: 40
									defaultValue: 0.75; min: plotsPosteriorLower.value; max: 1; inclusive: "no"
								}
							
							}
							
						}
					}
				}

				CheckBox
				{
					name: "plotsBoth"
					label: qsTr("Prior and Posterior distribution")
					checked: false
					CheckBox{name: "plotsBothSampleProportion"; label: qsTr("Sample proportion"); checked: false}
				}

				CheckBox
				{
					name: "plotsIterative";	label: qsTr("Sequential analysis"); checked: false;
					visible: dataTypeB.checked || dataTypeC.checked
					RadioButtonGroup
					{
						name: "plotsIterativeType"
						RadioButton
						{
							value: "overlying"
							label: qsTr("Overlying")
							
							RadioButtonGroup
							{
								name: "plotsIterativeCenter"
								RadioButton{value: "mean"; label: qsTr("Mean")}
								RadioButton{value: "median"; label: qsTr("Median")}
							}
							
							Group
							{
								columns: 3
								
								CheckBox
								{
									name: "plotsIterativeIndividualCI"
									label: qsTr("CI")
									id: plotsIterativeIndividualCI
								}
								
								DropDown
								{
									visible: plotsIterativeIndividualCI.checked
									name: "plotsIterativeIndividualType"
									label: ""
									values: ["Central", "HPD"]
									id: plotsIterativeIndividualType
								}
								
								DoubleField{
									visible: (plotsIterativeIndividualType.currentText == "Central" |
											  plotsIterativeIndividualType.currentText == "HPD") &
											 plotsIterativeIndividualCI.checked
									name: "plotsIterativeCoverage"
									label: qsTr("coverage")
									fieldWidth: 40
									defaultValue: 95; min: 0; max: 100; inclusive: "no"
								}
							
							}
						
						}
						RadioButton { value: "stacked"; 	label: qsTr("Stacked")		}
					}
				}
			}
		}
	}


// EXPLAIN THIS TO ME
	Section
	{
		expanded: false
		title: "WTF"
		Layout.columnSpan: 1
	}

	
	Section
	{
		expanded: true
		title: "Prediction"
		Layout.columnSpan: 1
		
		Group
		{
			IntegerField
			{
				name: "predictionN"
				label: qsTr("Future observations")
				id: predictionN
				min: 1
				defaultValue: 1
			}
			
			CheckBox
			{
				name: "predictionTable"
				label: qsTr("Summary")
			}
			
			CheckBox
			{
				name: "predictionPlot"
				label: qsTr("Plot")
				
				Group
				{
					columns: 5
					
					CheckBox
					{
						name: "plotsPredictionCI"
						label: qsTr("CI")
						id: plotsPredictionCI
					}
					
					DropDown
					{
						visible: plotsPredictionCI.checked
						name: "plotsPredictionType"
						label: ""
						values: ["Central", "HPD", "Custom"]
						id: plotsPredictionType
					}
				
					DoubleField{
						visible: (plotsPredictionType.currentText == "Central" |
								  plotsPredictionType.currentText == "HPD") &
								 plotsPredictionCI.checked
						name: "plotsPredictionCoverage"
						label: qsTr("coverage")
						fieldWidth: 40
						defaultValue: 95; min: 0; max: 100; inclusive: "no"
					}
					
					IntegerField{
						visible: plotsPredictionType.currentText == "Custom" &
								 plotsPredictionCI.checked
						name: "plotsPredictionLower"
						label: qsTr("lower")
						id: plotsPredictionLower
						fieldWidth: 40
						defaultValue: 0; min: 0; max: plotsPredictionUpper.value; inclusive: "yes"
					}
					
					IntegerField{
						visible: plotsPredictionType.currentText == "Custom" &
								 plotsPredictionCI.checked
						name: "plotsPredictionUpper"
						label: qsTr("upper")
						id: plotsPredictionUpper
						fieldWidth: 40
						defaultValue: 1
						min: plotsPredictionLower.value; max: predictionN.value; inclusive: "yes"
					}
				
				}
			
			}
		}
	}
}