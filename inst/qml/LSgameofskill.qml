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


Form {
	columns: 1

	SimpleTableView
	{
		name:				"players"
		cornerText:			"Player"
		buttonAddText:		"Add player"
		buttonDeleteText:	"Delete player"
		values:				["Prior skill parameter", "Points gained"]
		columnName:			""
        initialColumnCount: 2
        buttonsInRow:       true
        function getColHeaderText(defaultName, colIndex) { return String.fromCharCode(65 + colIndex); }
		function getDefaultValue(columnIndex, rowIndex)	{ return rowIndex === 0 ? 1 : 0; }
	}

	IntegerField  
	{ 
		name: "pointsToWin"; 
		label: qsTr("Points needed to win the game"); 
		fieldWidth: 50
		defaultValue: 2 
	}

	IntegerField   
	{ 
		name: "numberOfSimulatedGames"; 
		label: qsTr("Number of simulated games"); 
		fieldWidth: 50
		defaultValue: 500
		min: 100
	}

	CheckBox 
	{ 
		name: "ci"; 
		label: qsTr("95% HPD"); 
		checked: true 
	}

}
