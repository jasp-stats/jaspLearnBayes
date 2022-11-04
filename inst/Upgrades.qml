import QtQuick		2.12
import JASP.Module	1.0

Upgrades
{

	Upgrade
	{
		functionName: 	"MixedModelsCommon"
		fromVersion:	"0.17.0"
		toVersion:		"0.18.0"
		
		# qml_components/LSbinomialdatainput.qml
		ChangeRename { from: "dataType";	to: "dataInputType"}
		ChangeJS
		{
			name:		"dataType"
			jsFunction:	function(options)
			{
				switch(options["dataType"])
				{
					case "dataVariable":	return "variable";
					case "dataCounts":		return "counts";
					case "dataSequence":	return "sequence"
				}
			}
		}
		ChangeRename { from: "selectedVariable";		to: "dataVariableSelected"}
		ChangeRename { from: "allVariables";			to: "dataVariableAvailable"}
		ChangeRename { from: "dataVariableSD";			to: "dataVariableSd"}
		ChangeRename { from: "selectedVariable";		to: "dataVariableSelected"}
		ChangeRename { from: "levelsVar";				to: "dataVariableLevels"}
		ChangeRename { from: "keySuccessVar";			to: "dataVariableSuccesses"}
		ChangeRename { from: "keyFailureVar";			to: "dataVariableFailures"}
		ChangeRename { from: "dataCountsMean";			to: "dataCountsMean"}
		ChangeRename { from: "dataCountsSD";			to: "dataCountsSD"}
		ChangeRename { from: "dataCountsN";				to: "dataCountsN"}
		ChangeRename { from: "nSuccesses";				to: "dataCountsSuccesses"}
		ChangeRename { from: "nFailures";				to: "dataCountsFailures"}
		ChangeRename { from: "dataSequenceSD";			to: "dataSequenceSequenceSd"}
		ChangeRename { from: "dataSequenceInput";		to: "dataSequenceSequenceOfObservations"}
		ChangeRename { from: "levelsSeq";				to: "dataSequenceLevels"}
		ChangeRename { from: "keySuccessSeq";			to: "dataSequenceSuccesses"}
		ChangeRename { from: "keyFailureSeq";			to: "dataSequenceFailures"}

		# qml_components/LSestimationpredictions.qml
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
		ChangeRename { from: "predictionPlotProp";			to: "posteriorPredictionDistributionPlotSampleProportions"}
		ChangeRename { from: "predictionPlotTable";			to: "posteriorPredictionDistributionPlotPredictionsTable"}

		# qml_components/LSestimationpriorandposterior.qml
		ChangeRename { from: "pointEstimate";						to: "priorAndPosteriorPointEstimate"}
		ChangeRename { from: "plotsPrior";							to: "priorDistributionPlot"}
		ChangeRename { from: "plotsPriorType";						to: "priorDistributionPlotType"}
		ChangeRename { from: "plotsPriorIndividualEstimate";		to: "priorDistributionPlotIndividualPointEstimate"}
		ChangeRename { from: "plotsPriorIndividualEstimateType";	to: "priorDistributionPlotIndividualPointEstimateType"}
		ChangeRename { from: "plotsPriorIndividualCI";				to: "priorDistributionPlotIndividualCi"}
		ChangeRename { from: "plotsPriorIndividualType";			to: "priorDistributionPlotIndividualCiType"}
		ChangeRename { from: "plotsPriorCoverage";					to: "priorDistributionPlotIndividualCiMass"}
		ChangeRename { from: "plotsPriorLower";						to: "priorDistributionPlotIndividualCiLower"}
		ChangeRename { from: "plotsPriorUpper";						to: "priorDistributionPlotIndividualCiUpper"}
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
		ChangeRename { from: "plotsPosteriorIndividualPrior";			to: "posteriorDistributionPlotAddPriorDistribution"}
		ChangeRename { from: "plotsPosteriorIndividualProportion";		to: "posteriorDistributionPlotAddObservedProportion"}

		# qml_components/LSestimationsequential.qml
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

}
