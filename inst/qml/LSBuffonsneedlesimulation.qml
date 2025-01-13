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

Form
{
	columns: 1

	IntegerField
	{
		name: "lengthToDistanceProportion";
		label: qsTr("Proportion of needle length to interline distance:")
		defaultValue: 80
		afterLabel: qsTr("%")
		min: 1
		max: 100
	}


	IntegerField
	{
		name:			"numberOfThrows"
		id: n
		label:			qsTr("Number of tosses:")
		fieldWidth:		50
		defaultValue:	100
		min: 1
	}

	Group
	{
  		title: qsTr("Prior for the proportion of crosses")

		IntegerField
		{
			name:			"priorAlpha"
			label:			qsTr("Beta prior: parameter a")
			fieldWidth:		50
			defaultValue:	1
			min: 0
		}

		IntegerField
		{
			name:			"priorBeta"
			label:			qsTr("Beta prior: parameter b")
			fieldWidth:		50
			defaultValue:	1
			min: 0
		}

	}

	CIField
	{
		name: "ciLevel";
		label: qsTr("Credible interval")
		defaultValue: 95
	}

	Group
	{
  		title: qsTr("Plot")

		CheckBox
		{
			name: "needlePlot";
			label: qsTr("Needle plot");
			checked: true




			CheckBox
			{
			name: "needlePlotCrossingNeedlesColored";
			label: qsTr("Color crossing needles");
			checked: false
			}
		}





		CheckBox
		{
			name: "priorPosteriorProportion";
			label: qsTr("Prior and posterior for the proportion of crosses");

			CheckBox
			{
				name: "priorPosteriorProportionCi";
				label: qsTr("Credible interval")
				checked: false
			}
			CheckBox
			{
				name: "priorPosteriorProportionLegend";
				label: qsTr("Legend")
				checked: false
			}

		}

        CheckBox
		{
			name: "priorPosteriorPi";
			label: qsTr("Implied prior and posterior for " + "\u03c0");
			checked: true

			CheckBox
			{
				name: "priorPosteriorPiCi";
				label: qsTr("Credible interval")
				checked: false
			}

			CheckBox
			{
				name: "priorPosteriorPiLegend";
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
                    columns: 2

                    DoubleField
                    {
                        name: "min";
                        label: qsTr("from");
                        min: options.min;
                        max: parseFloat(minmaxMax.value);
                        defaultValue: 3;
                        id: minmaxMin;
                    }

                    DoubleField
                    {
                        name: "max";
                        label: qsTr("to");
                        min: parseFloat(minmaxMin.value);
                        max: options.max;
                        defaultValue: 3.2;
                        id: minmaxMax;
                    }

                }


            }


		}



	}



}



