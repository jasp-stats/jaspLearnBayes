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
import JASP.Theme		1.0

Form
{
	columns: 1

	IntegerField 
	{ 
		name: "length"; 
		label: qsTr("Proportion of needle length to interline distance:")
		defaultValue: 80
		afterLabel: qsTr("%")
		min: 1
		max: 100
	}


	IntegerField   
	{ 
		name:			"n"
		id: n
		label:			qsTr("Number of tosses:")
		fieldWidth:		50
		defaultValue:	100
		min: 0  
	}

	Group
	{
  		title: qsTr("Prior for the proportion of crosses")

		IntegerField   
		{ 
			name:			"a"
			label:			qsTr("Beta prior: parameter a")
			fieldWidth:		50
			defaultValue:	1
			min: 0  
		}

		IntegerField   
		{ 
			name:			"b"
			label:			qsTr("Beta prior: parameter b")
			fieldWidth:		50
			defaultValue:	1
			min: 0  
		}

	}

	CIField 
	{ 
		name: "CI"; 
		label: qsTr("Credible interval")
		defaultValue: 95 
	}

	Group
	{
  		title: qsTr("Plot")

		CheckBox 
		{ 
			name: "showNeedlePlot";
			label: qsTr("Needle plot"); 
			checked: true 



	
			CheckBox 
			{ 
			name: "color";
			label: qsTr("Color crossing needles"); 
			checked: false 
			}
		}



	

		CheckBox 
		{ 
			name: "showPropDistPlot";
			label: qsTr("Prior and posterior for the proportion of crosses");

			CheckBox 
			{ 
				name: "CIPropDistPlot"; 
				label: qsTr("Credible interval")
				checked: false
			}
			CheckBox 
			{ 
				name: "legendPropDistPlot"; 
				label: qsTr("Legend")
				checked: false
			}
 
		}

        CheckBox
		{ 
			name: "showPiDistPlot";
			label: qsTr("Implied prior and posterior for " + "\u03c0"); 
			checked: true
 
			CheckBox 
			{ 
				name: "CIPiDistPlot"; 
				label: qsTr("Credible interval")
				checked: false
			}

			CheckBox 
			{ 
				name: "legendPiDistPlot"; 
				label: qsTr("Legend")
				checked: false
			}
            Group
            {
                id: options
                property bool negativeValues	: false
                property double	min		: negativeValues ? -Infinity : 2
                property double	max		: 4

                CheckBox
                {
                    name: "highlight";
                    label: qsTr("Highlight interval")
                    checked: false
                }


                Group
                {
                    columns: 2

                    DoubleField
                    {
                        name: "min";
                        label: qsTr("from");
                        min: options.min;
                        max: parseFloat(minmaxMax.value);
                        defaultValue: 3;
                        id: minmaxMin;
                        enabled: minmax.checked
                        Layout.leftMargin: jaspTheme.columnGroupSpacing
                    }

                    DoubleField
                    {
                        name: "max";
                        label: qsTr("to");
                        min: parseFloat(minmaxMin.value);
                        max: options.max;
                        defaultValue: 3.2;
                        id: minmaxMax;
                        enabled: minmax.checked;
                        Layout.leftMargin: jaspTheme.columnGroupSpacing

                    }

                }


            }


		}


	
	}

	

}



