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

#' Binary Classification
#'
LSbinaryclassification <- function(
          data = NULL,
          version = "0.97.1",
          alluvialPlot = FALSE,
          areaPlot = FALSE,
          burnin = 500,
          chains = 4,
          ci = TRUE,
          ciLevel = 0.95,
          colorFalseNegative = "red",
          colorFalsePositive = "darkorange",
          colorTrueNegative = "steelblue",
          colorTruePositive = "darkgreen",
          computeResults = TRUE,
          confusionMatrix = FALSE,
          confusionMatrixAdditionalInfo = TRUE,
          confusionMatrixType = "text",
          estimatesPlot = FALSE,
          estimatesPlotAccuracy = FALSE,
          estimatesPlotFalseDiscoveryRate = FALSE,
          estimatesPlotFalseNegative = FALSE,
          estimatesPlotFalseNegativeRate = FALSE,
          estimatesPlotFalseOmissionRate = FALSE,
          estimatesPlotFalsePositive = FALSE,
          estimatesPlotFalsePositiveRate = FALSE,
          estimatesPlotNegativePredictiveValue = FALSE,
          estimatesPlotPositivePredictiveValue = FALSE,
          estimatesPlotPrevalence = TRUE,
          estimatesPlotSensitivity = TRUE,
          estimatesPlotSpecificity = TRUE,
          estimatesPlotTrueNegative = FALSE,
          estimatesPlotTruePositive = FALSE,
          falseNegative = 0,
          falsePositive = 0,
          iconPlot = FALSE,
          inputType = "pointEstimates",
          introductoryText = FALSE,
          labels = list(types = list(), value = ""),
          marker = list(types = list(), value = ""),
          negativeTests = 0,
          orderConstraint = TRUE,
          plotEstimatesType = "halfEye",
          plotHeight = 320,
          plotWidth = 480,
          positiveTests = 0,
          ppvNpvPlot = FALSE,
          prPlot = FALSE,
          prPlotPosteriorRealizations = FALSE,
          prPlotPosteriorRealizationsNumber = 100,
          predictiveValuesByPrevalence = FALSE,
          prevalence = 0.1,
          priorPosterior = FALSE,
          priorPrevalenceAlpha = 1,
          priorPrevalenceBeta = 9,
          priorSensitivityAlpha = 8,
          priorSensitivityBeta = 2,
          priorSpecificityAlpha = 8,
          priorSpecificityBeta = 2,
          probabilityPositivePlot = FALSE,
          probabilityPositivePlotEntireDistribution = FALSE,
          rocPlot = FALSE,
          rocPlotPosteriorRealizations = FALSE,
          rocPlotPosteriorRealizationsNumber = 100,
          samples = 10000,
          seed = 1,
          sensitivity = 0.8,
          setSeed = FALSE,
          signalDetectionPlot = FALSE,
          specificity = 0.8,
          statistics = TRUE,
          statisticsAdditional = FALSE,
          testCharacteristicsPlot = FALSE,
          thinning = 1,
          threshold = 0,
          tocPlot = FALSE,
          trueNegative = 0,
          truePositive = 0,
          updatePrevalence = TRUE,
          varyingThresholdBurnin = 500,
          varyingThresholdChains = 2,
          varyingThresholdSamples = 1000,
          varyingThresholdThinning = 1) {

   defaultArgCalls <- formals(jaspLearnBayes::LSbinaryclassification)
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

   optionsWithFormula <- c("labels", "marker")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspLearnBayes", "LSbinaryclassification", "LSbinaryclassification.qml", options, version, TRUE))
}