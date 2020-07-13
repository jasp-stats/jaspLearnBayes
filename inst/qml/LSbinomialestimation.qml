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
import "../qml" as LS

Form {
	id: form

	LS.LSdatainput{}

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
			rowComponentsTitles: [qsTr("Parameter (θ)                        ")]

			rowComponents:
			[
				Component
				{
					DropDown
					{
						name: "type"
						useExternalBorder: true
						values: ["beta", "spike"]
						Layout.rightMargin: 50
					}
				},
				Component
				{
					FormulaField
					{
						label: "α"
						name: "parAlpha"
						visible: fromRowComponents["type"].currentText === "beta"
						value: "1"
						min: 0
						inclusive: JASP.None
						fieldWidth: 70
						useExternalBorder: false
						showBorder: true
					}
				},
				Component
				{
					FormulaField
					{
						label: "β"
						name: "parBeta"
						visible: fromRowComponents["type"].currentText === "beta"
						value: "1"
						min: 0
						inclusive: JASP.None
						fieldWidth: 70
						useExternalBorder: false
						showBorder: true
					}
				},
				Component
				{
					FormulaField
					{
						label: "θ"
						name: "parPoint"
						visible: fromRowComponents["type"].currentText === "spike"
						Layout.rightMargin: width
						value: "0.5"
						min: 0
						max: 1
						inclusive: JASP.None
						fieldWidth: 70
						useExternalBorder: false
						showBorder: true
					}
				}
			]
		}
	}

	Section
	{
		expanded: true
		title: "Inference"
		columns: 2

		Group
		{
			Layout.columnSpan: 2

			Group
			{

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
			}
		}

		CheckBox
		{
			name: "plotsPrior"; label: qsTr("Prior distribution"); checked: false	;
			RadioButtonGroup
			{
				name: "plotsPriorType"
				RadioButton { value: "overlying"; 	label: qsTr("All"); checked: true}
				RadioButton { value: "stacked"; 	label: qsTr("Stacked")	}
				RadioButton {
					value: "individual"
					label: qsTr("Individual")

					CheckBox
					{
						name: "plotsPriorIndividualCI"
						label: qsTr("CI")
						id: plotsPriorIndividualCI
						childrenOnSameRow: true

						DropDown
						{
							name: "plotsPriorIndividualType"
							label: ""
							values: ["central", "HPD", "custom"]
							id: plotsPriorIndividualType
						}
					}

					Group
					{
						columns: 2
						CIField{
							visible: plotsPriorIndividualType.currentText == "central" |
									 plotsPriorIndividualType.currentText == "HPD"
							enabled: plotsPriorIndividualCI.checked
							name: "plotsPriorCoverage"
							label: qsTr("probability")
							fieldWidth: 40
							defaultValue: 95; min: 0; max: 100; inclusive: JASP.MaxOnly
						}

						DoubleField{
							visible: plotsPriorIndividualType.currentText == "custom"
							enabled: plotsPriorIndividualCI.checked
							name: "plotsPriorLower"
							label: qsTr("lower")
							id: plotsPriorLower
							fieldWidth: 50
							defaultValue: 0.25; min: 0; max: plotsPriorUpper.value; inclusive: JASP.MinMax
						}

						DoubleField{
							visible: plotsPriorIndividualType.currentText == "custom"
							enabled: plotsPriorIndividualCI.checked
							name: "plotsPriorUpper"
							label: qsTr("upper")
							id: plotsPriorUpper
							fieldWidth: 50
							defaultValue: 0.75; min: plotsPriorLower.value; max: 1; inclusive: JASP.MinMax
						}
					}
				}
			}
		}

		CheckBox
		{
			name: "plotsPosterior"; label: qsTr("Posterior distribution"); checked: false	;
			RadioButtonGroup
			{
				name: "plotsPosteriorType"
				RadioButton { value: "overlying"; 	label: qsTr("All"); checked: true}
				RadioButton { value: "stacked"; 	label: qsTr("Stacked")	}
				RadioButton {
					value: "individual"
					label: qsTr("Individual")

					CheckBox
					{
						name: "plotsPosteriorIndividualCI"
						label: qsTr("CI")
						id: plotsPosteriorIndividualCI
						childrenOnSameRow: true

						DropDown
						{
							visible: plotsPosteriorIndividualCI.checked
							name: "plotsPosteriorIndividualType"
							label: ""
							values: ["central", "HPD", "custom", "support"]
							id: plotsPosteriorIndividualType
						}
					}

					Group
					{
						columns: 2
						CIField{
							visible: plotsPosteriorIndividualType.currentText == "central" |
									 plotsPosteriorIndividualType.currentText == "HPD"
							enabled: plotsPosteriorIndividualCI.checked
							name: "plotsPosteriorCoverage"
							label: qsTr("probability")
							fieldWidth: 40
							defaultValue: 95; min: 0; max: 100; inclusive: JASP.MaxOnly
						}

						DoubleField{
							visible: plotsPosteriorIndividualType.currentText == "custom"
							enabled: plotsPosteriorIndividualCI.checked
							name: "plotsPosteriorLower"
							label: qsTr("lower")
							id: plotsPosteriorLower
							fieldWidth: 50
							defaultValue: 0.25; min: 0; max: plotsPosteriorUpper.value; inclusive: JASP.MinMax
						}

						DoubleField{
							visible: plotsPosteriorIndividualType.currentText == "custom"
							enabled: plotsPosteriorIndividualCI.checked
							name: "plotsPosteriorUpper"
							label: qsTr("upper")
							id: plotsPosteriorUpper
							fieldWidth: 50
							defaultValue: 0.75; min: plotsPosteriorLower.value; max: 1; inclusive: JASP.MinMax
						}

						DoubleField{
							visible: plotsPosteriorIndividualType.currentText == "support"
							enabled: plotsPosteriorIndividualCI.checked
							name: "plotsPosteriorBF"
							label: qsTr("BF")
							fieldWidth: 50
							defaultValue: 1; min: 0; inclusive: JASP.None
						}
					}

				}

			}
		}

		CheckBox
		{
			Layout.columnSpan: 2
			name: "plotsBoth"
			label: qsTr("Prior and posterior distribution")
			checked: false

			CheckBox{name: "plotsBothSampleProportion"; label: qsTr("Sample proportion"); checked: false}
		}

		Group
		{
			title: "Sequential analysis"
			visible: dataTypeB.checked || dataTypeC.checked

			CheckBox
			{
				visible: dataTypeB.checked || dataTypeC.checked
				name: "plotsIterative"
				label: qsTr("Point estimate")
				checked: false

				RadioButtonGroup
				{
					name: "plotsIterativeType"
					RadioButton
					{
						value: "overlying"
						label: qsTr("All")

						RadioButtonGroup
						{
							name: "plotsIterativeCenter"
							RadioButton{value: "mean"; label: qsTr("Mean")}
							RadioButton{value: "median"; label: qsTr("Median")}
						}

						Group
						{

							CheckBox
							{
								name: "plotsIterativeIndividualCI"
								label: qsTr("CI")
								id: plotsIterativeIndividualCI
								childrenOnSameRow: true

								DropDown
								{
									name: "plotsIterativeIndividualType"
									label: ""
									values: ["central", "HPD", "support"]
									id: plotsIterativeIndividualType
								}
							}

							CIField
							{
								visible: plotsIterativeIndividualType.currentText == "central" |
										 plotsIterativeIndividualType.currentText == "HPD"
								enabled: plotsIterativeIndividualCI.checked
								name: "plotsIterativeCoverage"
								label: qsTr("probability")
								fieldWidth: 40
								defaultValue: 95; min: 0; max: 100; inclusive: JASP.MaxOnly
							}

							DoubleField{
								visible: plotsIterativeIndividualType.currentText == "support"
								enabled: plotsIterativeIndividualCI.checked
								name: "plotsIterativeBF"
								label: qsTr("BF")
								fieldWidth: 50
								defaultValue: 1; min: 0; inclusive: JASP.None
							}

						}

					}

					RadioButton { value: "stacked"; label: qsTr("Stacked")}

					CheckBox
					{
						name:  "plotsIterativeUpdatingTable"
						label: qsTr("Updating table")
					}
				}

			}

		}

		Group
		{
			title: " "
			visible: dataTypeB.checked || dataTypeC.checked
		
			CheckBox
			{
				name: "plotsIterativeInterval"
				label: qsTr("Interval")
				checked: false

				RadioButtonGroup
				{
					name: "plotsIterativeIntervalType"
					id: "plotsIterativeIntervalType"

					Group
						{
						columns: 2
						DoubleField{
							enabled: plotsIterativeIntervalType.checked
							name: "plotsIterativeIntervalLower"
							label: qsTr("lower")
							id: plotsIterativeIntervalLower
							fieldWidth: 50
							defaultValue: 0.25; min: 0; max: plotsIterativeIntervalUpper.value; inclusive: JASP.MinOnly
						}

						DoubleField{
							enabled: plotsIterativeIntervalType.checked
							name: "plotsIterativeIntervalUpper"
							label: qsTr("upper")
							id: plotsIterativeIntervalUpper
							fieldWidth: 50
							defaultValue: 0.75; min: plotsIterativeIntervalLower.value; max: 1; inclusive: JASP.MaxOnly
						}
					}

					RadioButton	{ value: "overlying"; label: qsTr("All")}
					RadioButton { value: "stacked"; label: qsTr("Stacked")}
					CheckBox	{
						name:  "plotsIterativeIntervalUpdatingTable"
						label: qsTr("Updating table")
					}
				}
			}
		}

		CheckBox
		{
			visible: dataTypeB.checked || dataTypeC.checked
			Layout.columnSpan: 2
			name: "doIterative"
			label: qsTr("Posterior updating table")
			checked: false
		}

	}

	Section
	{
		expanded: true
		title: "Posterior prediction"

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


			Group
			{
				title: qsTr("Plots")

				DropDown
				{
					name: "colorPalettePrediction"
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
					label: qsTr("Posterior predictive distribution")
					name: "plotsPredictions"

					RadioButtonGroup
					{
						name: "predictionPlotType"
						RadioButton { value: "overlying"; 	label: qsTr("All"); checked: true}
						RadioButton { value: "stacked"; 	label: qsTr("Stacked")}
						RadioButton
						{
							value: "individual"
							label: qsTr("Individual")

							CheckBox
							{
								name: "plotsPredictionCI"
								label: qsTr("CI")
								id: plotsPredictionCI
								childrenOnSameRow: true

								DropDown
								{
									visible: plotsPredictionCI.checked
									name: "plotsPredictionType"
									label: ""
									values: ["central", "HPD", "custom"]
									id: plotsPredictionType
								}
							}	

							Group
							{
								columns: 2

								CIField{
									visible: plotsPredictionType.currentText == "central" |
											 plotsPredictionType.currentText == "HPD"
									enabled: plotsPredictionCI.checked
									name: "plotsPredictionCoverage"
									label: qsTr("probability")
									fieldWidth: 40
									defaultValue: 95; min: 0; max: 100; inclusive: JASP.MaxOnly
								}

								IntegerField{
									visible: plotsPredictionType.currentText == "custom"
									enabled: plotsPredictionCI.checked
									name: "plotsPredictionLower"
									label: qsTr("lower")
									id: plotsPredictionLower
									fieldWidth: 50
									defaultValue: 0; min: 0; max: plotsPredictionUpper.value; inclusive: JASP.MinMax
								}

								IntegerField{
									visible: plotsPredictionType.currentText == "custom"
									enabled: plotsPredictionCI.checked
									name: "plotsPredictionUpper"
									label: qsTr("upper")
									id: plotsPredictionUpper
									fieldWidth: 50
									defaultValue: 1
									min: plotsPredictionLower.value; max: predictionN.value; inclusive: JASP.MinMax
								}

							}
						}
						CheckBox{ name: "predictionPlotProp"; label: qsTr("Show sample proportions")}
					}
				}
			}
		}
	}
}
