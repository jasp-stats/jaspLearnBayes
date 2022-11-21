import QtQuick		2.12
import JASP.Module	1.0

Upgrades
{

	Upgrade
	{
		functionName:	"LSbinaryclassification"
		fromVersion:	"0.16.4"
		toVersion:		"0.17.0"

		ChangeRename {	from: "prevalenceAlpha";	to: "priorPrevalenceAlpha"	}
		ChangeRename {	from: "prevalenceBeta";		to: "priorPrevalenceBeta"	}
		ChangeRename {	from: "sensitivityAlpha";	to: "priorSensitivityAlpha"	}
		ChangeRename {	from: "sensitivityBeta";	to: "priorSensitivityBeta"	}
		ChangeRename {	from: "specificityAlpha";	to: "priorSpecificityAlpha"	}
		ChangeRename {	from: "specificityBeta";	to: "priorSpecificityBeta"	}

		ChangeRename {	from: "confusionMatrixAddInfo";	to: "confusionMatrixAdditionalInfo"	}

		ChangeRename {	from: "plotPriorPosteriorPositive";				to: "probabilityPositivePlot" }
		ChangeRename {	from: "plotPriorPosteriorPositiveDistribution";	to: "probabilityPositivePlotEntireDistribution" }

		ChangeRename {	from: "plotIconPlot";	to: "iconPlot"	}

		ChangeRename {	from: "plotROC";		to: "rocPlot"								}
		ChangeRename {	from: "plotRocLines";	to: "rocPlotPosteriorRealizations"			}
		ChangeRename {	from: "plotRocLinesNr"; to: "rocPlotPosteriorRealizationsNumber"	}

		ChangeRename {	from: "plotTestCharacteristics";	to: "testCharacteristicsPlot"		}
		ChangeRename {	from: "plotVaryingPrevalence";		to: "predictiveValuesByPrevalence"	}
		ChangeRename {	from: "plotAlluvial";				to: "alluvialPlot"					}
		ChangeRename {	from: "plotSignal";					to: "signalDetectionPlot"			}

		ChangeRename {	from: "plotEstimates";				to: "estimatesPlot"								}
		ChangeRename {	from: "plotPrevalence";				to: "estimatesPlotPrevalence"					}
		ChangeRename {	from: "plotSensitivity";			to: "estimatesPlotSensitivity"					}
		ChangeRename {	from: "plotSpecificity";			to: "estimatesPlotSpecificity"					}
		ChangeRename {	from: "plotTruePositive";			to: "estimatesPlotTruePositive"					}
		ChangeRename {	from: "plotFalsePositive";			to: "estimatesPlotFalsePositive"				}
		ChangeRename {	from: "plotTrueNegative";			to: "estimatesPlotTrueNegative"					}
		ChangeRename {	from: "plotFalseNegative";			to: "estimatesPlotFalseNegative"				}
		ChangeRename {	from: "plotPPV";					to: "estimatesPlotPositivePredictiveValue"		}
		ChangeRename {	from: "plotNPV";					to: "estimatesPlotNegativePredictiveValue"		}
		ChangeRename {	from: "plotFDR";					to: "estimatesPlotFalseDiscoveryRate"			}
		ChangeRename {	from: "plotFOR";					to: "estimatesPlotFalseOmissionRate"			}
		ChangeRename {	from: "plotFPF";					to: "estimatesPlotFalsePositiveRate"			}
		ChangeRename {	from: "plotFNF";					to: "estimatesPlotFalseNegativeRate"			}
		ChangeRename {	from: "plotAccuracy";				to: "estimatesPlotAccuracy"						}

		ChangeRename {	from: "credibleInterval";	to: "ci"		}
		ChangeRename {	from: "numberOfSamples";	to: "samples"	}

		// qml_components/LSintrotext.qml
		ChangeRename { from: "introText";	to: "introductoryText" }
	}


	Upgrade
	{
		functionName: 	"LSbinomialestimation"
		fromVersion:	"0.16.4"
		toVersion:		"0.17.0"

		// qml_components/LSintrotext.qml
		ChangeRename { from: "introText";	to: "introductoryText"}

		// qml_components/LSbinomialdatainput.qml
		ChangeRename { from: "dataType";	to: "dataInputType"}
		ChangeJS
		{
			name:		"dataInputType"
			jsFunction:	function(options)
			{
				switch(options["dataInputType"])
				{
					case "dataVariable":	return "variable";
					case "dataCounts":		return "counts";
					case "dataSequence":	return "sequence"
				}
			}
		}
		ChangeRename { from: "selectedVariable";		to: "dataVariableSelected"}
		ChangeRename { from: "keySuccessVar";			to: "dataVariableSuccesses"}
		ChangeRename { from: "keyFailureVar";			to: "dataVariableFailures"}
		ChangeRename { from: "nSuccesses";				to: "dataCountsSuccesses"}
		ChangeRename { from: "nFailures";				to: "dataCountsFailures"}
		ChangeRename { from: "dataSequenceInput";		to: "dataSequenceSequenceOfObservations"}
		ChangeRename { from: "keySuccessSeq";			to: "dataSequenceSuccesses"}
		ChangeRename { from: "keyFailureSeq";			to: "dataSequenceFailures"}

		// models
		ChangeRename {	from: "priors";					to: "models"	}
		ChangeJS
		{
			name:		"models"
			jsFunction:	function(options)
			{
				let newModels = options["models"].map(model => {
					let newModel 			= {};
 					newModel["name"] 			= model["name"];
					newModel["type"] 			= model["type"];
					newModel["betaPriorAlpha"] 	= model["parAlpha"];
					newModel["betaPriorBeta"] 	= model["parBeta"];
					newModel["spikePoint"] 		= model["parPoint"];
					newModel["value"]           = model["value"];

					return newModel;
				})

				return newModels;
			}
		}


		// qml_components/LSestimationpredictions.qml
		ChangeRename { from: "predictionN";					to: "posteriorPredictionNumberOfFutureTrials"}
		ChangeRename { from: "predictionTable";				to: "posteriorPredictionSummaryTable"}
		ChangeRename { from: "predictionTableEstimate";		to: "posteriorPredictionSummaryTablePointEstimate"}
		ChangeRename { from: "plotsPredictions";			to: "posteriorPredictionDistributionPlot"}
		ChangeRename { from: "predictionPlotType";			to: "posteriorPredictionDistributionPlotType"}
		ChangeRename { from: "plotsPredictionEstimate";		to: "posteriorPredictionDistributionPlotIndividualPointEstimate"}
		ChangeRename { from: "plotsPredictionEstimateType";	to: "posteriorPredictionDistributionPlotIndividualPointEstimateType"}
		ChangeRename { from: "plotsPredictionCI";			to: "posteriorPredictionDistributionPlotIndividualCi"}
		ChangeRename { from: "plotsPredictionType";			to: "posteriorPredictionDistributionPlotIndividualCiType"}
		ChangeRename { from: "plotsPredictionCoverage";		to: "posteriorPredictionDistributionPlotIndividualCiMass"}
		ChangeRename { from: "plotsPredictionLower";		to: "posteriorPredictionDistributionPlotIndividualCiLower"}
		ChangeRename { from: "plotsPredictionUpper";		to: "posteriorPredictionDistributionPlotIndividualCiUpper"}
		ChangeRename { from: "predictionPlotProp";			to: "posteriorPredictionDistributionPlotAsSampleProportion"}
		ChangeRename { from: "predictionPlotTable";			to: "posteriorPredictionDistributionPlotPredictionsTable"}

		// qml_components/LSestimationpriorandposterior.qml
		ChangeRename { from: "pointEstimate";							to: "priorAndPosteriorPointEstimate"}
		ChangeRename { from: "plotsPrior";								to: "priorDistributionPlot"}
		ChangeRename { from: "plotsPriorType";							to: "priorDistributionPlotType"}
		ChangeRename { from: "plotsPriorIndividualEstimate";			to: "priorDistributionPlotIndividualPointEstimate"}
		ChangeRename { from: "plotsPriorIndividualEstimateType";		to: "priorDistributionPlotIndividualPointEstimateType"}
		ChangeRename { from: "plotsPriorIndividualCI";					to: "priorDistributionPlotIndividualCi"}
		ChangeRename { from: "plotsPriorIndividualType";				to: "priorDistributionPlotIndividualCiType"}
		ChangeRename { from: "plotsPriorCoverage";						to: "priorDistributionPlotIndividualCiMass"}
		ChangeRename { from: "plotsPriorLower";							to: "priorDistributionPlotIndividualCiLower"}
		ChangeRename { from: "plotsPriorUpper";							to: "priorDistributionPlotIndividualCiUpper"}
		ChangeRename { from: "plotsPosterior";							to: "posteriorDistributionPlot"}
		ChangeRename { from: "plotsPosteriorType";						to: "posteriorDistributionPlotType"}
		ChangeRename { from: "plotsPosteriorIndividualEstimate";		to: "posteriorDistributionPlotIndividualPointEstimate"}
		ChangeRename { from: "plotsPosteriorIndividualEstimateType";	to: "posteriorDistributionPlotIndividualPointEstimateType"}
		ChangeRename { from: "plotsPosteriorIndividualCI";				to: "posteriorDistributionPlotIndividualCi"}
		ChangeRename { from: "plotsPosteriorIndividualType";			to: "posteriorDistributionPlotIndividualCiType"}
		ChangeRename { from: "plotsPosteriorCoverage";					to: "posteriorDistributionPlotIndividualCiMass"}
		ChangeRename { from: "plotsPosteriorLower";						to: "posteriorDistributionPlotIndividualCiLower"}
		ChangeRename { from: "plotsPosteriorUpper";						to: "posteriorDistributionPlotIndividualCiUpper"}
		ChangeRename { from: "plotsPosteriorBF";						to: "posteriorDistributionPlotIndividualCiBf"}
		ChangeRename { from: "plotsPosteriorIndividualPrior";			to: "posteriorDistributionPloPriorDistribution"}
		ChangeRename { from: "plotsPosteriorIndividualProportion";		to: "posteriorDistributionPlotObservedProportion"}

		// qml_components/LSestimationsequential.qml
		ChangeRename { from: "plotsIterativeOverlying";					to: "sequentialAnalysisPointEstimatePlot"}
		ChangeRename { from: "plotsIterativeEstimateType";				to: "sequentialAnalysisPointEstimatePlotType"}
		ChangeRename { from: "plotsIterativeOverlyingCI";				to: "sequentialAnalysisPointEstimatePlotCi"}
		ChangeRename { from: "plotsIterativeOverlyingType";				to: "sequentialAnalysisPointEstimatePlotCiType"}
		ChangeRename { from: "plotsIterativeCoverage";					to: "sequentialAnalysisPointEstimatePlotCiMass"}
		ChangeRename { from: "plotsIterativeBF";						to: "sequentialAnalysisPointEstimatePlotCiBf"}
		ChangeRename { from: "plotsIterativeUpdatingTable";				to: "sequentialAnalysisPointEstimatePlotUpdatingTable"}
		ChangeRename { from: "plotsIterativeInterval";					to: "sequentialAnalysisIntervalEstimatePlot"}
		ChangeRename { from: "plotsIterativeIntervalType";				to: "sequentialAnalysisIntervalEstimatePlotType"}
		ChangeRename { from: "plotsIterativeIntervalLower";				to: "sequentialAnalysisIntervalEstimatePlotLower"}
		ChangeRename { from: "plotsIterativeIntervalUpper";				to: "sequentialAnalysisIntervalEstimatePlotUpper"}
		ChangeRename { from: "plotsIterativeIntervalUpdatingTable";		to: "sequentialAnalysisIntervalEstimatePlotUpdatingTable"}
		ChangeRename { from: "plotsIterativeStacked";					to: "sequentialAnalysisStackedDistributionsPlot"}
		ChangeRename { from: "doIterative";								to: "sequentialAnalysisPosteriorUpdatingTable"}

	}

	Upgrade
	{
		functionName: 	"LSbinomialtesting"
		fromVersion:	"0.16.4"
		toVersion:		"0.17.0"

		// qml_components/LSintrotext.qml
		ChangeRename { from: "introText";	to: "introductoryText"}

		// qml_components/LSbinomialdatainput.qml
		ChangeRename { from: "dataType";	to: "dataInputType"}
		ChangeJS
		{
			name:		"dataInputType"
			jsFunction:	function(options)
			{
				switch(options["dataInputType"])
				{
					case "dataVariable":	return "variable";
					case "dataCounts":		return "counts";
					case "dataSequence":	return "sequence"
				}
			}
		}
		ChangeRename { from: "selectedVariable";		to: "dataVariableSelected"}
		ChangeRename { from: "keySuccessVar";			to: "dataVariableSuccesses"}
		ChangeRename { from: "keyFailureVar";			to: "dataVariableFailures"}
		ChangeRename { from: "nSuccesses";				to: "dataCountsSuccesses"}
		ChangeRename { from: "nFailures";				to: "dataCountsFailures"}
		ChangeRename { from: "dataSequenceInput";		to: "dataSequenceSequenceOfObservations"}
		ChangeRename { from: "keySuccessSeq";			to: "dataSequenceSuccesses"}
		ChangeRename { from: "keyFailureSeq";			to: "dataSequenceFailures"}

		// models
		ChangeRename {	from: "priors";					to: "models"	}
		ChangeJS
		{
			name:		"models"
			jsFunction:	function(options)
			{
				let newModels = options["models"].map(model => {
					let newModel 				= {};
					newModel["name"] 			= model["name"];
					newModel["priorWeight"]		= model["PH"];
					newModel["type"] 			= model["type"];
					newModel["betaPriorAlpha"] 	= model["parAlpha"];
					newModel["betaPriorBeta"] 	= model["parBeta"];
					newModel["spikePoint"] 		= model["parPoint"];
					newModel["value"]           = model["value"];

					return newModel;
				})
				return newModels;
			}
		}

		// qml_components/LStestingpredictions.qml
		ChangeRename { from: "predictionN";								to: "posteriorPredictionNumberOfFutureTrials"}
		ChangeRename { from: "predictionTable";							to: "posteriorPredictionSummaryTable"}
		ChangeRename { from: "predictionTableEstimate";					to: "posteriorPredictionSummaryTablePointEstimate"}
		ChangeRename { from: "plotsPredictionsPost";					to: "posteriorPredictionDistributionPlot"}
		ChangeRename { from: "plotsPredictionPostType";					to: "posteriorPredictionDistributionPlotType"}
		ChangeRename { from: "plotsPredictionPostEstimate";				to: "posteriorPredictionDistributionPlotConditionalPointEstimate"}
		ChangeRename { from: "plotsPredictionPostEstimateType";			to: "posteriorPredictionDistributionPlotConditionalPointEstimateType"}
		ChangeRename { from: "plotsPredictionPostCI";					to: "posteriorPredictionDistributionPlotConditionalCi"}
		ChangeRename { from: "plotsPredictionPostTypeCI";				to: "posteriorPredictionDistributionPlotConditionalCiType"}
		ChangeRename { from: "plotsPredictionPostCoverage";				to: "posteriorPredictionDistributionPlotConditionalCiMass"}
		ChangeRename { from: "plotsPredictionPostLower";				to: "posteriorPredictionDistributionPlotConditionalCiLower"}
		ChangeRename { from: "plotsPredictionPostUpper";				to: "posteriorPredictionDistributionPlotConditionalCiUpper"}
		ChangeRename { from: "plotsPredictionPostJointType";			to: "posteriorPredictionDistributionPlotJoinType"}
		ChangeRename { from: "plotsPredictionPostMarginalEstimate";		to: "posteriorPredictionDistributionPlotMarginalPointEstimate"}
		ChangeRename { from: "plotsPredictionPostMarginalEstimateType";	to: "posteriorPredictionDistributionPlotMarginalPointEstimateType"}
		ChangeRename { from: "plotsPredictionPostMarginalCI";			to: "posteriorPredictionDistributionPlotMarginalCi"}
		ChangeRename { from: "plotsPredictionPostMarginalTypeCI";		to: "posteriorPredictionDistributionPlotMarginalCiType"}
		ChangeRename { from: "plotsPredictionPostMarginalCoverage";		to: "posteriorPredictionDistributionPlotMarginalCiMass"}
		ChangeRename { from: "plotsPredictionPostMarginalLower";		to: "posteriorPredictionDistributionPlotMarginalCiLower"}
		ChangeRename { from: "plotsPredictionPostMarginalUpper";		to: "posteriorPredictionDistributionPlotMarginalCiUpper"}
		ChangeRename { from: "predictionPostPlotProp";					to: "posteriorPredictionDistributionPlotAsSampleProportion"}
		ChangeRename { from: "predictionPostPlotTable";					to: "posteriorPredictionDistributionPlotPredictionsTable"}

		// qml_components/LStestingpredictiveperformance.qml
		ChangeRename { from: "plotsPredictions";					to: "priorPredictivePerformanceDistributionPlot"}
		ChangeRename { from: "plotsPredictionType";					to: "priorPredictivePerformanceDistributionPlotType"}
		ChangeRename { from: "plotsPredictionEstimate";				to: "priorPredictivePerformanceDistributionPlotConditionalPointEstimate"}
		ChangeRename { from: "plotsPredictionEstimateType";			to: "priorPredictivePerformanceDistributionPlotConditionalPointEstimateType"}
		ChangeRename { from: "plotsPredictionCI";					to: "priorPredictivePerformanceDistributionPlotConditionalCi"}
		ChangeRename { from: "plotsPredictionTypeCI";				to: "priorPredictivePerformanceDistributionPlotConditionalCiType"}
		ChangeRename { from: "plotsPredictionCoverage";				to: "priorPredictivePerformanceDistributionPlotConditionalCiMass"}
		ChangeRename { from: "plotsPredictionLower";				to: "priorPredictivePerformanceDistributionPlotConditionalCiLower"}
		ChangeRename { from: "plotsPredictionUpper";				to: "priorPredictivePerformanceDistributionPlotConditionalCiUpper"}
		ChangeRename { from: "plotsPredictionJointType";			to: "priorPredictivePerformanceDistributionPlotJoinType"}
		ChangeRename { from: "plotsPredictionMarginalEstimate";		to: "priorPredictivePerformanceDistributionPlotMarginalPointEstimate"}
		ChangeRename { from: "plotsPredictionMarginalEstimateType";	to: "priorPredictivePerformanceDistributionPlotMarginalPointEstimateType"}
		ChangeRename { from: "plotsPredictionMarginalCI";			to: "priorPredictivePerformanceDistributionPlotMarginalCi"}
		ChangeRename { from: "plotsPredictionMarginalTypeCI";		to: "priorPredictivePerformanceDistributionPlotMarginalCiType"}
		ChangeRename { from: "plotsPredictionMarginalCoverage";		to: "priorPredictivePerformanceDistributionPlotMarginalCiMass"}
		ChangeRename { from: "plotsPredictionMarginalLower";		to: "priorPredictivePerformanceDistributionPlotMarginalCiLower"}
		ChangeRename { from: "plotsPredictionMarginalUpper";		to: "priorPredictivePerformanceDistributionPlotMarginalCiUpper"}
		ChangeRename { from: "plotsPredictionsObserved";			to: "priorPredictivePerformanceDistributionPlotObservedNumberOfSuccessess"}
		ChangeRename { from: "predictionPlotTable";					to: "priorPredictivePerformanceDistributionPlotPredictionsTable"}
		ChangeRename { from: "plotsPredictiveAccuracy";				to: "priorPredictivePerformanceAccuracyPlot"}
		ChangeRename { from: "plotsPredictiveAccuracyType";			to: "priorPredictivePerformanceAccuracyPlotType"}
		ChangeRename { from: "bfType";								to: "priorPredictivePerformanceBfComparison"}
		ChangeRename { from: "bfTypevsName";						to: "priorPredictivePerformanceBfVsHypothesis"}
		ChangeRename { from: "bayesFactorType";						to: "priorPredictivePerformanceBfType"}

		// qml_components/LStestingpriorandposterior.qml
		ChangeRename { from: "plotsPrior";							to: "priorDistributionPlot"}
		ChangeRename { from: "plotsPriorType";						to: "priorDistributionPlotType"}
		ChangeRename { from: "plotsPriorEstimate";					to: "priorDistributionPlotConditionalPointEstimate"}
		ChangeRename { from: "plotsPriorEstimateType";				to: "priorDistributionPlotConditionalPointEstimateType"}
		ChangeRename { from: "plotsPriorCI";						to: "priorDistributionPlotConditionalCi"}
		ChangeRename { from: "plotsPriorTypeCI";					to: "priorDistributionPlotConditionalCiType"}
		ChangeRename { from: "plotsPriorCoverage";					to: "priorDistributionPlotConditionalCiMass"}
		ChangeRename { from: "plotsPriorLower";						to: "priorDistributionPlotConditionalCiLower"}
		ChangeRename { from: "plotsPriorUpper";						to: "priorDistributionPlotConditionalCiUpper"}
		ChangeRename { from: "plotsPriorJointType";					to: "priorDistributionPlotJointType"}
		ChangeRename { from: "plotsPriorMarginalEstimate";			to: "priorDistributionPlotMarginalPointEstimate"}
		ChangeRename { from: "plotsPriorMarginalEstimateType";		to: "priorDistributionPlotMarginalPointEstimateType"}
		ChangeRename { from: "plotsPriorMarginalCI";				to: "priorDistributionPlotMarginalCi"}
		ChangeRename { from: "plotsPriorMarginalType";				to: "priorDistributionPlotMarginalCiType"}
		ChangeRename { from: "plotsPriorMarginalCoverage";			to: "priorDistributionPlotMarginalCiMass"}
		ChangeRename { from: "plotsPriorMarginalLower";				to: "priorDistributionPlotMarginalCiLower"}
		ChangeRename { from: "plotsPriorMarginalUpper";				to: "priorDistributionPlotMarginalCiUpper"}
		ChangeRename { from: "plotsPosterior";						to: "posteriorDistributionPlot"}
		ChangeRename { from: "plotsPosteriorType";					to: "posteriorDistributionPlotType"}
		ChangeRename { from: "plotsPosteriorEstimate";				to: "posteriorDistributionPlotConditionalPointEstimate"}
		ChangeRename { from: "plotsPosteriorEstimateType";			to: "posteriorDistributionPlotConditionalPointEstimateType"}
		ChangeRename { from: "plotsPosteriorCI";					to: "posteriorDistributionPlotConditionalCi"}
		ChangeRename { from: "plotsPosteriorTypeCI";				to: "posteriorDistributionPlotConditionalCiType"}
		ChangeRename { from: "plotsPosteriorCoverage";				to: "posteriorDistributionPlotConditionalCiMass"}
		ChangeRename { from: "plotsPosteriorLower";					to: "posteriorDistributionPlotConditionalCiLower"}
		ChangeRename { from: "plotsPosteriorUpper";					to: "posteriorDistributionPlotConditionalCiUpper"}
		ChangeRename { from: "plotsPosteriorBF";					to: "posteriorDistributionPlotConditionalCiBf"}
		ChangeRename { from: "plotsPosteriorJointType";				to: "posteriorDistributionPlotJointType"}
		ChangeRename { from: "plotsPosteriorMarginalEstimate";		to: "posteriorDistributionPlotMarginalPointEstimate"}
		ChangeRename { from: "plotsPosteriorMarginalEstimateType";	to: "posteriorDistributionPlotMarginalPointEstimateType"}
		ChangeRename { from: "plotsPosteriorMarginalCI";			to: "posteriorDistributionPlotMarginalCi"}
		ChangeRename { from: "plotsPosteriorMarginalType";			to: "posteriorDistributionPlotMarginalCiType"}
		ChangeRename { from: "plotsPosteriorMarginalCoverage";		to: "posteriorDistributionPlotMarginalCiMass"}
		ChangeRename { from: "plotsPosteriorMarginalLower";			to: "posteriorDistributionPlotMarginalCiLower"}
		ChangeRename { from: "plotsPosteriorMarginalUpper";			to: "posteriorDistributionPlotMarginalCiUpper"}
		ChangeRename { from: "plotsPosteriorMarginalBF";			to: "posteriorDistributionPlotMarginalCiBf"}
		ChangeRename { from: "plotsPosteriorObserved";				to: "posteriorDistributionPlotObservedProportion"}
		ChangeRename { from: "plotsBoth";							to: "priorAndPosteriorDistributionPlot"}
		ChangeRename { from: "plotsBothType";						to: "priorAndPosteriorDistributionPlotType"}
		ChangeRename { from: "plotsBothSampleProportion";			to: "priorAndPosteriorDistributionPlotObservedProportion"}

		// qml_components/LStestingsequential.qml
		ChangeRename { from: "plotsIterative";							to: "sequentialAnalysisPredictivePerformancePlot"}
		ChangeRename { from: "plotsIterativeType";						to: "sequentialAnalysisPredictivePerformancePlotType"}
		ChangeRename { from: "bfTypeSequential";						to: "sequentialAnalysisPredictivePerformancePlotBfComparison"}
		ChangeRename { from: "bfTypevsNameSequential";					to: "sequentialAnalysisPredictivePerformancePlotBfVsHypothesis"}
		ChangeRename { from: "bayesFactorTypeSequential";				to: "sequentialAnalysisPredictivePerformancePlotBfType"}
		ChangeRename { from: "plotsIterativeUpdatingTable";				to: "sequentialAnalysisPredictivePerformancePlotUpdatingTable"}
	}


	Upgrade
	{
		functionName:		"LSBuffonsneedlemanipulation"
		fromVersion:		"0.16.4"
		toVersion:			"0.17.0"

		ChangeRename {	from: "length";		to: "lengthToDistanceProportion"	}
		ChangeRename {	from: "n";			to: "numberOfThrows"				}
		ChangeRename {	from: "k";			to: "numberOfCrosses"				}
		ChangeRename {	from: "a";			to: "priorAlpha"					}
		ChangeRename {	from: "b";			to: "priorBeta"						}
		ChangeRename {	from: "CI";			to: "ciLevel"						}

		ChangeRename {	from: "showPropDistPlot";		to: "priorPosteriorProportion"			}
		ChangeRename {	from: "legendPropDistPlot";		to: "priorPosteriorProportionLegend"	}

		ChangeRename {	from: "showPiDistPlot";			to: "priorPosteriorPi"			}
		ChangeRename {	from: "CIArrow";				to: "priorPosteriorPiCi"		}
		ChangeRename {	from: "legendPiDistPlot";		to: "priorPosteriorPiLegend"	}
	}

	Upgrade
	{
		functionName:		"LSBuffonsneedlesimulation"
		fromVersion:		"0.16.4"
		toVersion:			"0.17.0"

		ChangeRename {	from: "length";		to: "lengthToDistanceProportion"	}
		ChangeRename {	from: "n";			to: "numberOfThrows"				}
		ChangeRename {	from: "a";			to: "priorAlpha"					}
		ChangeRename {	from: "b";			to: "priorBeta"						}
		ChangeRename {	from: "CI";			to: "ciLevel"						}

		ChangeRename {	from: "showNeedlePlot";		to: "needlePlot"			}
		ChangeRename {	from: "color";				to: "needlePlotCrossingNeedlesColored"			}

		ChangeRename {	from: "showPropDistPlot";		to: "priorPosteriorProportion"			}
		ChangeRename {	from: "legendPropDistPlot";		to: "priorPosteriorProportionLegend"	}

		ChangeRename {	from: "showPiDistPlot";			to: "priorPosteriorPi"			}
		ChangeRename {	from: "CIArrow";				to: "priorPosteriorPiCi"		}
		ChangeRename {	from: "legendPiDistPlot";		to: "priorPosteriorPiLegend"	}
	}

	Upgrade
	{
		functionName:		"LSgameofchance"
		fromVersion:		"0.16.4"
		toVersion:			"0.17.0"

		ChangeRename {	from: "winPoints";	to: "pointsToWin"				}
		ChangeRename {	from: "nSims";		to: "numberOfSimulatedGames"	}
		ChangeRename {	from: "CI";			to: "ci"						}
	}

	Upgrade
	{
		functionName:		"LSgameofskill"
		fromVersion:		"0.16.4"
		toVersion:			"0.17.0"

		ChangeRename {	from: "winPoints";	to: "pointsToWin"				}
		ChangeRename {	from: "nSims";		to: "numberOfSimulatedGames"	}
		ChangeRename {	from: "CI";			to: "ci"						}
	}
}
