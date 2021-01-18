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



	Slider
	{
  	name: "length"
  	label: qsTr("Proportion of needle length to inter-line distance in %")
  	value: 80
	vertical: false
	max: 100
	decimals: 0
	}


	IntegerField   
	{ 
		name:			"n"
		label:			qsTr("Observations:")
		fieldWidth:		50
		defaultValue:	100
		min: 0  
	}


	IntegerField   
	{ 
		name:			"a"
		label:			qsTr("Prior parameter a: ")
		fieldWidth:		50
		defaultValue:	1
		min: 0  
	}

	IntegerField   
	{ 
		name:			"b"
		label:			qsTr("Prior parameter b:")
		fieldWidth:		50
		defaultValue:	1
		min: 0  
	}

}



