#
# Copyright (C) 2013-2025 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

# This is a generated file. Don't change it!

#' Binomial Testing
#'
LSbinomialtesting <- function(
          data = NULL,
          version = "1",
          colorPalette = "colorblind",
          dataCountsFailures = 0,
          dataCountsSuccesses = 0,
          dataInputType = "counts",
          dataSequenceFailures = list(types = list(), value = list()),
          dataSequenceSequenceOfObservations = "",
          dataSequenceSuccesses = list(types = list(), value = list()),
          dataSummary = TRUE,
          dataVariableFailures = list(types = list(), value = list()),
          dataVariableSelected = list(types = list(), value = ""),
          dataVariableSuccesses = list(types = list(), value = list()),
          introductoryText = FALSE,
          models = list(),
          plotHeight = 320,
          plotWidth = 480,
          posteriorDistributionPlot = FALSE,
          posteriorDistributionPlotConditionalCi = FALSE,
          posteriorDistributionPlotConditionalCiBf = 1,
          posteriorDistributionPlotConditionalCiLower = 0.25,
          posteriorDistributionPlotConditionalCiMass = 0.95,
          posteriorDistributionPlotConditionalCiType = "central",
          posteriorDistributionPlotConditionalCiUpper = 0.75,
          posteriorDistributionPlotConditionalPointEstimate = FALSE,
          posteriorDistributionPlotConditionalPointEstimateType = "mean",
          posteriorDistributionPlotJointType = "overlying",
          posteriorDistributionPlotMarginalCi = FALSE,
          posteriorDistributionPlotMarginalCiBf = 1,
          posteriorDistributionPlotMarginalCiLower = 0.25,
          posteriorDistributionPlotMarginalCiMass = 0.95,
          posteriorDistributionPlotMarginalCiType = "central",
          posteriorDistributionPlotMarginalCiUpper = 0.75,
          posteriorDistributionPlotMarginalPointEstimate = FALSE,
          posteriorDistributionPlotMarginalPointEstimateType = "mean",
          posteriorDistributionPlotObservedProportion = FALSE,
          posteriorDistributionPlotType = "conditional",
          posteriorPredictionDistributionPlot = FALSE,
          posteriorPredictionDistributionPlotAsSampleProportion = FALSE,
          posteriorPredictionDistributionPlotConditionalCi = FALSE,
          posteriorPredictionDistributionPlotConditionalCiLower = 0,
          posteriorPredictionDistributionPlotConditionalCiMass = 0.95,
          posteriorPredictionDistributionPlotConditionalCiType = "central",
          posteriorPredictionDistributionPlotConditionalCiUpper = 1,
          posteriorPredictionDistributionPlotConditionalPointEstimate = FALSE,
          posteriorPredictionDistributionPlotConditionalPointEstimateType = "mean",
          posteriorPredictionDistributionPlotJoinType = "overlying",
          posteriorPredictionDistributionPlotMarginalCi = FALSE,
          posteriorPredictionDistributionPlotMarginalCiLower = 0,
          posteriorPredictionDistributionPlotMarginalCiMass = 0.95,
          posteriorPredictionDistributionPlotMarginalCiType = "central",
          posteriorPredictionDistributionPlotMarginalCiUpper = 1,
          posteriorPredictionDistributionPlotMarginalPointEstimate = FALSE,
          posteriorPredictionDistributionPlotMarginalPointEstimateType = "mean",
          posteriorPredictionDistributionPlotPredictionsTable = FALSE,
          posteriorPredictionDistributionPlotType = "conditional",
          posteriorPredictionNumberOfFutureTrials = 10,
          posteriorPredictionSummaryTable = FALSE,
          posteriorPredictionSummaryTablePointEstimate = "mean",
          priorAndPosteriorDistributionPlot = FALSE,
          priorAndPosteriorDistributionPlotObservedProportion = FALSE,
          priorAndPosteriorDistributionPlotType = "conditional",
          priorDistributionPlot = FALSE,
          priorDistributionPlotConditionalCi = FALSE,
          priorDistributionPlotConditionalCiLower = 0.25,
          priorDistributionPlotConditionalCiMass = 0.95,
          priorDistributionPlotConditionalCiType = "central",
          priorDistributionPlotConditionalCiUpper = 0.75,
          priorDistributionPlotConditionalPointEstimate = FALSE,
          priorDistributionPlotConditionalPointEstimateType = "mean",
          priorDistributionPlotJointType = "overlying",
          priorDistributionPlotMarginalCi = FALSE,
          priorDistributionPlotMarginalCiLower = 0.25,
          priorDistributionPlotMarginalCiMass = 0.95,
          priorDistributionPlotMarginalCiType = "central",
          priorDistributionPlotMarginalCiUpper = 0.75,
          priorDistributionPlotMarginalPointEstimate = FALSE,
          priorDistributionPlotMarginalPointEstimateType = "mean",
          priorDistributionPlotType = "conditional",
          priorPredictivePerformanceAccuracyPlot = FALSE,
          priorPredictivePerformanceAccuracyPlotType = "conditional",
          priorPredictivePerformanceBfComparison = "vs",
          priorPredictivePerformanceBfType = "BF10",
          priorPredictivePerformanceBfVsHypothesis = "",
          priorPredictivePerformanceDistributionPlot = FALSE,
          priorPredictivePerformanceDistributionPlotConditionalCi = FALSE,
          priorPredictivePerformanceDistributionPlotConditionalCiLower = 0,
          priorPredictivePerformanceDistributionPlotConditionalCiMass = 0.95,
          priorPredictivePerformanceDistributionPlotConditionalCiType = "central",
          priorPredictivePerformanceDistributionPlotConditionalCiUpper = 1,
          priorPredictivePerformanceDistributionPlotConditionalPointEstimate = FALSE,
          priorPredictivePerformanceDistributionPlotConditionalPointEstimateType = "mean",
          priorPredictivePerformanceDistributionPlotJoinType = "overlying",
          priorPredictivePerformanceDistributionPlotMarginalCi = FALSE,
          priorPredictivePerformanceDistributionPlotMarginalCiLower = 0,
          priorPredictivePerformanceDistributionPlotMarginalCiMass = 0.95,
          priorPredictivePerformanceDistributionPlotMarginalCiType = "central",
          priorPredictivePerformanceDistributionPlotMarginalCiUpper = 1,
          priorPredictivePerformanceDistributionPlotMarginalPointEstimate = FALSE,
          priorPredictivePerformanceDistributionPlotMarginalPointEstimateType = "mean",
          priorPredictivePerformanceDistributionPlotObservedNumberOfSuccessess = FALSE,
          priorPredictivePerformanceDistributionPlotPredictionsTable = FALSE,
          priorPredictivePerformanceDistributionPlotType = "conditional",
          sequentialAnalysisPredictivePerformancePlot = FALSE,
          sequentialAnalysisPredictivePerformancePlotBfComparison = "inclusion",
          sequentialAnalysisPredictivePerformancePlotBfType = "BF10",
          sequentialAnalysisPredictivePerformancePlotBfVsHypothesis = "",
          sequentialAnalysisPredictivePerformancePlotType = "conditional",
          sequentialAnalysisPredictivePerformancePlotUpdatingTable = FALSE) {

   defaultArgCalls <- formals(jaspLearnBayes::LSbinomialtesting)
   defaultArgs <- lapply(defaultArgCalls, eval)
   options <- as.list(match.call())[-1L]
   options <- lapply(options, eval)
   defaults <- setdiff(names(defaultArgs), names(options))
   options[defaults] <- defaultArgs[defaults]
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("colorPalette", "dataSequenceFailures", "dataSequenceSequenceOfObservations", "dataSequenceSuccesses", "dataVariableFailures", "dataVariableSelected", "dataVariableSuccesses", "models", "posteriorDistributionPlotConditionalCiType", "posteriorDistributionPlotConditionalPointEstimateType", "posteriorDistributionPlotMarginalCiType", "posteriorDistributionPlotMarginalPointEstimateType", "posteriorPredictionDistributionPlotConditionalCiType", "posteriorPredictionDistributionPlotConditionalPointEstimateType", "posteriorPredictionDistributionPlotMarginalCiType", "posteriorPredictionDistributionPlotMarginalPointEstimateType", "posteriorPredictionSummaryTablePointEstimate", "priorDistributionPlotConditionalCiType", "priorDistributionPlotConditionalPointEstimateType", "priorDistributionPlotMarginalCiType", "priorDistributionPlotMarginalPointEstimateType", "priorPredictivePerformanceBfVsHypothesis", "priorPredictivePerformanceDistributionPlotConditionalCiType", "priorPredictivePerformanceDistributionPlotConditionalPointEstimateType", "priorPredictivePerformanceDistributionPlotMarginalCiType", "priorPredictivePerformanceDistributionPlotMarginalPointEstimateType", "sequentialAnalysisPredictivePerformancePlotBfVsHypothesis")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspLearnBayes", "LSbinomialtesting", "LSbinomialtesting.qml", options, version, TRUE))
}