#
# Copyright (C) 2019 University of Amsterdam
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

LSbinaryclassification <- function(jaspResults, dataset, options, state = NULL) {
  options <- .parseAndStoreFormulaOptions(jaspResults, options,
                                          levels(interaction(c("sensitivity", "specificity", "prevalence"), c("", "Alpha", "Beta"), sep = "")))

  results <- .bcComputeResults(jaspResults, dataset, options)
  .bcPlotPriorPosteriorPositive(results, jaspResults, dataset, options)
  .bcPlotROC                   (results, jaspResults, dataset, options)
}

.bcComputeResults <- function(jaspResults, dataset, options) {
  if(!is.null(jaspResults[["results"]])) return(jaspResults[["results"]]$object)

  results <- switch(options[["inputType"]],
                    pointEstimates     = .bcComputeResultsPointEstimates(options),
                    list(sensitivity = NA, specificity = NA))

  jaspResults[["results"]] <-
    createJaspState(object       = results,
                    dependencies = c("inputType",
                                     levels(interaction(c("sensitivity", "specificity", "prevalence"), c("", "Alpha", "Beta"), sep = ""))
                                     )
                    )

  return(results)
}

.bcComputeResultsPointEstimates <- function(options) {
  prevalence              <- options[["prevalence"]]
  sensitivity             <- options[["sensitivity"]]
  specificity             <- options[["specificity"]]
  truePositive            <- prevalence*sensitivity
  falsePositive           <- (1-prevalence)*(1-specificity)
  trueNegative            <- (1-prevalence)*specificity
  falseNegative           <- prevalence*(1-sensitivity)
  positivePredictiveValue <- truePositive / (truePositive + falsePositive)
  negativePredictiveValue <- trueNegative / (trueNegative + falseNegative)
  falseDiscoveryRate      <- 1-positivePredictiveValue
  falseOmissionRate       <- 1-negativePredictiveValue
  accuracy                <- truePositive + trueNegative

  results <- list(
    prevalence              = prevalence,
    sensitivity             = sensitivity,
    specificity             = specificity,
    truePositive            = truePositive,
    falsePositive           = falsePositive,
    trueNegative            = trueNegative,
    falseNegative           = falseNegative,
    positivePredictiveValue = positivePredictiveValue,
    negativePredictiveValue = negativePredictiveValue,
    falseDiscoveryRate      = falseDiscoveryRate,
    falseOmissionRate       = falseOmissionRate,
    accuracy                = accuracy
  )

  class(results) <- "bcPointEstimates"
  return(results)
}


.bcPlotPriorPosteriorPositive <- function(results, jaspResults, dataset, options) {
  UseMethod(".bcPlotPriorPosteriorPositive")
}


.bcPlotPriorPosteriorPositive.bcPointEstimates <- function(results, jaspResults, dataset, options) {
  if( isFALSE(options[["plotPriorPosteriorPositive"]])     ) return()
  if(!is.null(jaspResults[["plotPriorPosteriorPositive"]]) ) return()

  data <- expand.grid(test   = gettext(c("Not tested", "Tested")),
                      result = gettext(c("Negative", "Positive")))
  data$probPositive <- numeric(length = nrow(data))

  for(i in 1:nrow(data)) {
    if(data$test[i] == gettext("Not tested")) {
      data$probPositive[i] <- results[["prevalence"]]
    } else if(data$result[i] == gettext("Negative")) {
      data$probPositive[i] <- results[["falseOmissionRate"]]
    } else {
      data$probPositive[i] <- results[["positivePredictiveValue"]]
    }
  }

  plot <- ggplot2::ggplot(data    = data,
                          mapping = ggplot2::aes(x=result, y=probPositive, fill=test)
                          ) +
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(), col = "black") +
    ggplot2::xlab(gettext("Test result")) +
    ggplot2::ylab(gettext("P(positive)")) +
    ggplot2::scale_fill_discrete(name = NULL)

  plot <- jaspGraphs::themeJasp(plot, legend.position = "right")

  jaspResults[["plotPriorPosteriorPositive"]] <-
    createJaspPlot(title        = gettext("Probability positive"),
                   plot         = plot,
                   dependencies = c("prevalence", "sensitivity", "specificity"),
                   position     = 2, width = 400
                  )

  return()
}

.bcPlotROC <- function(results, jaspResults, dataset, options) {
  UseMethod(".bcPlotROC")
}

.bcPlotROC.bcPointEstimates <- function(results, jaspResults, dataset, options) {
  if( isFALSE(options[["plotROC"]])     ) return()
  if(!is.null(jaspResults[["plotROC"]]) ) return()

  threshold    <- qnorm(results[["specificity"]])
  meanPositive <- qnorm(results[["sensitivity"]], mean = threshold)

  falsePositiveFraction <- seq(0, 1, by = 0.01)
  varyingThreshold <- qnorm(falsePositiveFraction, lower.tail = FALSE)
  data <- data.frame(
    falsePositiveFraction = falsePositiveFraction,
    truePositiveFraction  = pnorm(varyingThreshold, meanPositive, lower.tail = FALSE)
  )

  pointData <- data.frame(
    falsePositiveFraction = 1-results[["specificity"]],
    truePositiveFraction  = results[["sensitivity"]]
  )
  plot <- ggplot2::ggplot(data = data,
                          mapping = ggplot2::aes(x = falsePositiveFraction,
                                                 y =  truePositiveFraction)
                          ) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2) +
    ggplot2::geom_line(size = 2) +
    jaspGraphs::geom_point(data = pointData, size = 5) +
    ggplot2::xlab(gettext("False positive fraction (1-Specificity)")) +
    ggplot2::ylab(gettext("True positive fraction (Sensitivity)"))

  plot <- jaspGraphs::themeJasp(plot)

  jaspResults[["plotROC"]] <-
    createJaspPlot(title = gettext("Receiving Operating Characteristic Curve"),
                   plot  = plot,
                   dependencies = c("prevalence", "sensitivity", "specificity"),
                   position = 4, aspectRatio = 1, width = 400)
}
