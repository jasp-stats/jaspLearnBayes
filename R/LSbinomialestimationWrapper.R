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

#' Binomial Estimation
#'
LSbinomialestimation <- function(
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
          posteriorDistributionPloPriorDistribution = FALSE,
          posteriorDistributionPlot = FALSE,
          posteriorDistributionPlotIndividualCi = FALSE,
          posteriorDistributionPlotIndividualCiBf = 1,
          posteriorDistributionPlotIndividualCiLower = 0.25,
          posteriorDistributionPlotIndividualCiMass = 0.95,
          posteriorDistributionPlotIndividualCiType = "central",
          posteriorDistributionPlotIndividualCiUpper = 0.75,
          posteriorDistributionPlotIndividualPointEstimate = FALSE,
          posteriorDistributionPlotIndividualPointEstimateType = "mean",
          posteriorDistributionPlotObservedProportion = FALSE,
          posteriorDistributionPlotType = "overlying",
          posteriorPredictionDistributionPlot = FALSE,
          posteriorPredictionDistributionPlotAsSampleProportion = FALSE,
          posteriorPredictionDistributionPlotIndividualCi = FALSE,
          posteriorPredictionDistributionPlotIndividualCiLower = 0,
          posteriorPredictionDistributionPlotIndividualCiMass = 0.95,
          posteriorPredictionDistributionPlotIndividualCiType = "central",
          posteriorPredictionDistributionPlotIndividualCiUpper = 1,
          posteriorPredictionDistributionPlotIndividualPointEstimate = FALSE,
          posteriorPredictionDistributionPlotIndividualPointEstimateType = "mean",
          posteriorPredictionDistributionPlotPredictionsTable = FALSE,
          posteriorPredictionDistributionPlotType = "overlying",
          posteriorPredictionNumberOfFutureTrials = 10,
          posteriorPredictionSummaryTable = FALSE,
          posteriorPredictionSummaryTablePointEstimate = "mean",
          priorAndPosteriorPointEstimate = "mean",
          priorDistributionPlot = FALSE,
          priorDistributionPlotIndividualCi = FALSE,
          priorDistributionPlotIndividualCiLower = 0.25,
          priorDistributionPlotIndividualCiMass = 0.95,
          priorDistributionPlotIndividualCiType = "central",
          priorDistributionPlotIndividualCiUpper = 0.75,
          priorDistributionPlotIndividualPointEstimate = FALSE,
          priorDistributionPlotIndividualPointEstimateType = "mean",
          priorDistributionPlotType = "overlying",
          sequentialAnalysisIntervalEstimatePlot = FALSE,
          sequentialAnalysisIntervalEstimatePlotLower = 0.25,
          sequentialAnalysisIntervalEstimatePlotType = "overlying",
          sequentialAnalysisIntervalEstimatePlotUpdatingTable = FALSE,
          sequentialAnalysisIntervalEstimatePlotUpper = 0.75,
          sequentialAnalysisPointEstimatePlot = FALSE,
          sequentialAnalysisPointEstimatePlotCi = FALSE,
          sequentialAnalysisPointEstimatePlotCiBf = 1,
          sequentialAnalysisPointEstimatePlotCiMass = 0.95,
          sequentialAnalysisPointEstimatePlotCiType = "central",
          sequentialAnalysisPointEstimatePlotType = "mean",
          sequentialAnalysisPointEstimatePlotUpdatingTable = FALSE,
          sequentialAnalysisPosteriorUpdatingTable = FALSE,
          sequentialAnalysisStackedDistributionsPlot = FALSE) {

   defaultArgCalls <- formals(jaspLearnBayes::LSbinomialestimation)
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

   optionsWithFormula <- c("colorPalette", "dataSequenceFailures", "dataSequenceSequenceOfObservations", "dataSequenceSuccesses", "dataVariableFailures", "dataVariableSelected", "dataVariableSuccesses", "models", "posteriorDistributionPlotIndividualCiType", "posteriorDistributionPlotIndividualPointEstimateType", "posteriorPredictionDistributionPlotIndividualCiType", "posteriorPredictionDistributionPlotIndividualPointEstimateType", "posteriorPredictionSummaryTablePointEstimate", "priorAndPosteriorPointEstimate", "priorDistributionPlotIndividualCiType", "priorDistributionPlotIndividualPointEstimateType", "sequentialAnalysisPointEstimatePlotCiType", "sequentialAnalysisPointEstimatePlotType")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspLearnBayes", "LSbinomialestimation", "LSbinomialestimation.qml", options, version, TRUE))
}