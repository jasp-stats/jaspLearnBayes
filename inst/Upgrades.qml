import QtQuick		2.12
import JASP.Module	1.0

Upgrades
{

	Upgrade
	{
		functionName: 	"MixedModelsCommon"
		fromVersion:	"0.17.0"
		toVersion:		"0.18.0"

		# qml_components/LSintrotext.qml
		ChangeRename { from: "introText";	to: "introductoryText"}

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
		ChangeRename { from: "selectedVariable";		to: "dataVariableSelected"}
		ChangeRename { from: "levelsVar";				to: "dataVariableLevels"}
		ChangeRename { from: "keySuccessVar";			to: "dataVariableSuccesses"}
		ChangeRename { from: "keyFailureVar";			to: "dataVariableFailures"}
		ChangeRename { from: "nSuccesses";				to: "dataCountsSuccesses"}
		ChangeRename { from: "nFailures";				to: "dataCountsFailures"}
		ChangeRename { from: "dataSequenceInput";		to: "dataSequenceSequenceOfObservations"}
		ChangeRename { from: "levelsSeq";				to: "dataSequenceLevels"}
		ChangeRename { from: "keySuccessSeq";			to: "dataSequenceSuccesses"}
		ChangeRename { from: "keyFailureSeq";			to: "dataSequenceFailures"}

		# qml_components/LSgaussiandatainput.qml
		ChangeRename { from: "dataCountsMean";			to: "dataCountsMean"}
		ChangeRename { from: "dataCountsSD";			to: "dataCountsSD"}
		ChangeRename { from: "dataCountsN";				to: "dataCountsN"}
		ChangeRename { from: "dataVariableSD";			to: "dataVariableSd"}
		ChangeRename { from: "dataSequenceSD";			to: "dataSequenceSequenceSd"}

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

		# qml_components/LStestingpredictions.qml
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
		ChangeRename { from: "predictionPostPlotProp";					to: "posteriorPredictionDistributionPlotSampleProportion"}
		ChangeRename { from: "predictionPostPlotTable";					to: "posteriorPredictionDistributionPlotPredictionsTable"}

		# qml_components/LStestingpredictiveperformance.qml
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
		ChangeRename { from: "plotsPredictionsObserved";			to: "priorPredictivePerformanceDistributionPlotObservedNumberOfSuccess"}
		ChangeRename { from: "predictionPlotTable";					to: "priorPredictivePerformanceDistributionPlotPredictionsTable"}
		ChangeRename { from: "plotsPredictiveAccuracy";				to: "priorPredictivePerformanceAccuracyPlot"}
		ChangeRename { from: "plotsPredictiveAccuracyType";			to: "priorPredictivePerformanceAccuracyPlotType"}
		ChangeRename { from: "bfType";								to: "priorPredictivePerformanceBfType"}
		ChangeRename { from: "bfTypevsName";						to: "priorPredictivePerformanceBfVsHypothesis"}
		ChangeRename { from: "bayesFactorType";						to: "priorPredictivePerformanceBfType"}

		# qml_components/LStestingpriorandposterior.qml
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
		ChangeRename { from: "plotsPosterior";							to: "posteriorDistributionPlot"}
		ChangeRename { from: "plotsPosteriorType";						to: "posteriorDistributionPlotType"}
		ChangeRename { from: "plotsPosteriorEstimate";					to: "posteriorDistributionPlotConditionalPointEstimate"}
		ChangeRename { from: "plotsPosteriorEstimateType";				to: "posteriorDistributionPlotConditionalPointEstimateType"}
		ChangeRename { from: "plotsPosteriorCI";						to: "posteriorDistributionPlotConditionalCi"}
		ChangeRename { from: "plotsPosteriorTypeCI";					to: "posteriorDistributionPlotConditionalCiType"}
		ChangeRename { from: "plotsPosteriorCoverage";					to: "posteriorDistributionPlotConditionalCiMass"}
		ChangeRename { from: "plotsPosteriorLower";						to: "posteriorDistributionPlotConditionalCiLower"}
		ChangeRename { from: "plotsPosteriorUpper";						to: "posteriorDistributionPlotConditionalCiUpper"}
		ChangeRename { from: "plotsPosteriorBF";						to: "posteriorDistributionPlotConditionalCiBf"}
		ChangeRename { from: "plotsPosteriorJointType";					to: "posteriorDistributionPlotJointType"}
		ChangeRename { from: "plotsPosteriorMarginalEstimate";			to: "posteriorDistributionPlotMarginalPointEstimate"}
		ChangeRename { from: "plotsPosteriorMarginalEstimateType";		to: "posteriorDistributionPlotMarginalPointEstimateType"}
		ChangeRename { from: "plotsPosteriorMarginalCI";				to: "posteriorDistributionPlotMarginalCi"}
		ChangeRename { from: "plotsPosteriorMarginalType";				to: "posteriorDistributionPlotMarginalCiType"}
		ChangeRename { from: "plotsPosteriorMarginalCoverage";			to: "posteriorDistributionPlotMarginalCiMass"}
		ChangeRename { from: "plotsPosteriorMarginalLower";				to: "posteriorDistributionPlotMarginalCiLower"}
		ChangeRename { from: "plotsPosteriorMarginalUpper";				to: "posteriorDistributionPlotMarginalCiUpper"}
		ChangeRename { from: "plotsPosteriorMarginalBF";				to: "posteriorDistributionPlotMarginalCiBf"}
		ChangeRename { from: "plotsPosteriorObserved";				to: "posteriorDistributionPlotObservedProportion"}
		ChangeRename { from: "plotsBoth";							to: "priorAndPosteriorDistributionPlot"}
		ChangeRename { from: "plotsBothType";						to: "priorAndPosteriorDistributionPlotType"}
		ChangeRename { from: "plotsBothSampleProportion";			to: "priorAndPosteriorDistributionPlotObservedProportion"}


	}

}
