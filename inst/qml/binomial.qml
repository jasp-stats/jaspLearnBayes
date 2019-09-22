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
		Layout.columnSpan: 2

		RadioButtonGroup
		{
		name: "procedureType"
		title: qsTr("Select a computational procedure")
			RadioButton { value: "procedureAll"; 		label: qsTr("All in one"); 	id: procedureA;	checked: true 	}
			RadioButton { value: "procedureIterative"; 	label: qsTr("Iterative"); 	id: procedureB					}
		}		
		
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
        IntegerField { name: "nSuccesses";	label: qsTr("Number of successes");	defaultValue: 1  }
        IntegerField { name: "nFailures";	label: qsTr("Number of failures");	defaultValue: 1  }
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
			AvailableVariablesList { name: "allVariables" }
			//allowedColumns: ["nominal"]
			AssignedVariablesList  { name: "selectedVariable"; label: qsTr("Selected variable"); singleVariable: true }
			}
		}
		
		Group
		{
		title: qsTr("Show data summary")
		visible: dataTypeB.checked || dataTypeC.checked
			CheckBox { name: "dataSummary"; label: qsTr("Show data summary"); checked: false }
		}
		
	}
  
	Section
	{
		expanded: true
		title: "Prior"
		
		// table with prior
		//
		//
		
	}
	
	Section
	{
		
		expanded: true
		title: "Plots"
		Layout.columnSpan: 2
		Group{
		
			RadioButtonGroup
			{
			name: "plotsPlottingType"
			title: qsTr("Plotting type")
				RadioButton { value: "plotsFinal"; 		label: qsTr("Final"); 											id: plotsTypeA;	checked: true 	}
				RadioButton { value: "plotsOverTime"; 	label: qsTr("Over time (does not allow stacked densities)"); 	id: plotsTypeB					}
			}
			
			CheckBox { name: "plotsSeparate"; label: qsTr("Separate"); checked: false }	
			
			CheckBox 
			{ 
								name: "plotsCombined"; 			label: qsTr("Combined"); checked: false;
				Group
				{
					CheckBox { 	name: "plotsCombinedPrior"; 	label: qsTr("Prior");		checked: true	}
					CheckBox { 	name: "plotsCombinedPosterior"; label: qsTr("Posterior");	checked: true	}
					RadioButtonGroup{
					name: "plotsCombinedType"
					title: qsTr("Type")
						RadioButton { value: "plotsCombinedOverlying"; 	label: qsTr("Overlying densities")	}
						Group
						{
						visible: plotsTypeA.checked
						RadioButton { value: "plotsCombinedStacked"; 	label: qsTr("Stacked densities")	}
						}
					}		
				}
			}
		}
		
	}
	
	Section
	{
		expanded: true
		title: "Prediction"
		
		Group
		{
			title: qsTr("Probability of observing")
			IntegerField { name: "predictionSuccess";	label: qsTr("Number of successes");	defaultValue: 1  }
			IntegerField { name: "predictionFailure";	label: qsTr("Number of failures");	defaultValue: 1  }
		}
	}
    

}
