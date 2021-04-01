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
  .bcTableStatistics           (results, jaspResults, dataset, options)
  .bcTableConfusion            (results, jaspResults, dataset, options)
  .bcPlots                     (results, jaspResults, dataset, options)
  # .bcPlotPriorPosteriorPositive(results, jaspResults, dataset, options)
  # .bcPlotIconPlot              (results, jaspResults, dataset, options)
  # .bcPlotROC                   (results, jaspResults, dataset, options)
  # .bcPlotVaryingPrevalence     (results, jaspResults, dataset, options)
  # .bcPlotAlluvial              (results, jaspResults, dataset, options)
}

.bcComputeResults <- function(jaspResults, dataset, options) {
  if(!is.null(jaspResults[["results"]])) return(jaspResults[["results"]]$object)

  results <- switch(options[["inputType"]],
                    pointEstimates     = .bcComputeResultsPointEstimates(options),
                    uncertainEstimates = .bcComputeResultsUncertainEstimates(options),
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
  results <- .bcStatistics(
    prevalence  = options[["prevalence"]],
    sensitivity = options[["sensitivity"]],
    specificity = options[["specificity"]]
    )

  class(results) <- "bcPointEstimates"
  return(results)
}

summary.bcPointEstimates <- function(results, ...) {
  output <- data.frame(
    statistic      = .bcTexts("statistic")[names(results)],
    estimate       = unlist(results),
    notation       = .bcTexts("notation")[names(results)],
    interpretation = .bcTexts("interpretation")[names(results)]
    )

  return(output)
}

.bcComputeResultsUncertainEstimates <- function(options) {
  prevalence  <- rbeta(n=5e3, options[["prevalenceAlpha"]],  options[["prevalenceBeta"]])
  sensitivity <- rbeta(n=5e3, options[["sensitivityAlpha"]], options[["sensitivityBeta"]])
  specificity <- rbeta(n=5e3, options[["specificityAlpha"]], options[["specificityBeta"]])
  results <- .bcStatistics(prevalence = prevalence, sensitivity = sensitivity, specificity = specificity)

  class(results) <- "bcUncertainEstimates"
  return(results)
}

summary.bcUncertainEstimates <- function(results, ciLevel = 0.95) {
  alpha <- 1-ciLevel

  output <- data.frame(
    statistic      = .bcTexts("statistic")[names(results)],
    estimate       = vapply(results, mean, numeric(1)),
    lowerCI        = vapply(results, quantile, numeric(1), prob=alpha/2),
    upperCI        = vapply(results, quantile, numeric(1), prob=1-alpha/2),
    notation       = .bcTexts("notation")[names(results)],
    interpretation = .bcTexts("interpretation")[names(results)]
    )

  return(output)
}

.bcStatistics <- function(prevalence, sensitivity, specificity) {
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

  return(results)
}

.bcTableStatistics <- function(results, jaspResults, dataset, options) {
  UseMethod(".bcTableStatistics")
}

.bcTableStatistics.default <- function(results, jaspResults, dataset, options) {
  if( isFALSE(options[["statistics"]])     ) return()
  if(!is.null(jaspResults[["statistics"]]) ) return()

  table <- createJaspTable(title = gettext("Statistics"), position = 1)
  table$dependOn(optionsFromObject = jaspResults[["results"]], options = c("statistics", "introText"))
  table$showSpecifiedColumnsOnly <- TRUE

  table$addColumnInfo(name = "statistic",  title = "")
  table$addColumnInfo(name = "estimate", title = gettext("Value"), type = "number")


  if(!inherits(results, "bcPointEstimates")) {
    table$addColumnInfo(name = "lowerCI", title = gettext("Lower"), overtitle = gettext("Credible interval"), type = "number")
    table$addColumnInfo(name = "upperCI", title = gettext("Upper"), overtitle = gettext("Credible interval"), type = "number")
  }

  if(options[["introText"]]) {
    table$addColumnInfo(name = "notation", title = gettext("Notation"))
    table$addColumnInfo(name = "interpretation", title = gettext("Interpretation"))
  }

  table$setData(summary(results))
  jaspResults[["statistics"]] <- table
}

.bcTableConfusion <- function(results, jaspResults, dataset, options) {
  UseMethod(".bcTableConfusion")
}

.bcTableConfusion.bcPointEstimates <- function(results, jaspResults, dataset, options) {
  if( isFALSE(options[["confusionMatrix"]])     ) return()
  if(!is.null(jaspResults[["confusionMatrix"]]) ) return()

  table <- createJaspTable(title = gettext("Confusion matrix"), position = 2)
  table$dependOn(c("prevalence", "sensitivity", "specificity", "confusionMatrix", "confusionMatrixType", "confusionMatrixAddInfo"))
  table$showSpecifiedColumnsOnly <- TRUE
  table$transpose <- TRUE


  table$addColumnInfo(name = "head", title = gettext(""))

  if(options[["confusionMatrixType"]] == "text") {
    table$addColumnInfo(name = "pos",  title = gettext("Positive test"))
  } else if(options[["confusionMatrixType"]] == "number") {
    table$addColumnInfo(name = "pos_value",  title = gettext("Positive test"), type = "number")
  } else {
    table$addColumnInfo(name = "pos",  title = gettext("Outcome"), overtitle = gettext("Positive test"))
    table$addColumnInfo(name = "pos_value", title = gettext("Value"), overtitle = gettext("Positive test"), type = "number")
  }

  if(options[["confusionMatrixType"]] == "text") {
    table$addColumnInfo(name = "neg",  title = gettext("Negative test"))
  } else if(options[["confusionMatrixType"]] == "number") {
    table$addColumnInfo(name = "neg_value",  title = gettext("Negative test"), type = "number")
  } else {
    table$addColumnInfo(name = "neg",  title = gettext("Outcome"), overtitle = gettext("Negative test"))
    table$addColumnInfo(name = "neg_value", title = gettext("Value"), overtitle = gettext("Negative test"), type = "number")
  }

  if(isTRUE(options[["confusionMatrixAddInfo"]])) {
    if(options[["confusionMatrixType"]] == "text") {
      table$addColumnInfo(name = "add1", title = gettext("Positive/Total"))
    } else if(options[["confusionMatrixType"]] == "number") {
      table$addColumnInfo(name = "add1_value", title = gettext("Positive/Total"), type = "number")
    } else {
      table$addColumnInfo(name = "add1", title = gettext("Outcome"), overtitle = gettext("Positive/Total"))
      table$addColumnInfo(name = "add1_value", title = gettext("Value"), overtitle = gettext("Positive/Total"), type = "number")
    }


    if(options[["confusionMatrixType"]] == "text") {
      table$addColumnInfo(name = "add2", title = gettext("Negative/Total"))
    } else if(options[["confusionMatrixType"]] == "number") {
      table$addColumnInfo(name = "add2_value", title = gettext("Negative/Total"), type = "number")
    } else {
      table$addColumnInfo(name = "add2", title = gettext("Outcome"), overtitle = gettext("Negative/Total"))
      table$addColumnInfo(name = "add2_value", title = gettext("Value"), overtitle = gettext("Negative/Total"), type = "number")
    }

    if(options[["confusionMatrixType"]] == "text") {
      table$addColumnInfo(name = "tot", title = gettext("Total"))
    } else if(options[["confusionMatrixType"]] == "number") {
      table$addColumnInfo(name = "tot_value", title = gettext("Total"), type = "number")
    } else {
      table$addColumnInfo(name = "tot", title = gettext("Outcome"), overtitle = gettext("Total"))
      table$addColumnInfo(name = "tot_value", title = gettext("Value"), overtitle = gettext("Total"), type = "number")
    }
  }


  df <- data.frame(
    head = gettext(c("Positive condition", "Negative condition", "True/Total")),
    pos  = gettext(c("True positive", "False positive", "Positive predictive value")),
    pos_value = unlist(results[c("truePositive", "falsePositive", "positivePredictiveValue")]),
    neg  = gettext(c("False negative", "True negative", "Negative predictive value")),
    neg_value = unlist(results[c("falseNegative", "trueNegative", "negativePredictiveValue")]),
    add1 = gettext(c("Sensitivity", "False positive fraction", "Accuracy")),
    add1_value = c(results[["sensitivity"]], 1-results[["sensitivity"]], results[["accuracy"]]),
    add2 = gettext(c("False negative fraction", "Specificity", "")),
    add2_value = c(1-results[["specificity"]], results[["specificity"]], NA),
    tot = gettext(c("Prevalence", "Rareness", "")),
    tot_value = c(results[["prevalence"]], 1-results[["prevalence"]], NA)
  )

  if(isFALSE(options[["confusionMatrixAddInfo"]]))
    df <- df[1:2,]



  table$setData(df)

  jaspResults[["confusionMatrix"]] <- table
}

.bcPlots <- function(results, jaspResults, dataset, options) {

  if(is.null(jaspResults[["plots"]])) {
    plotsContainer <- createJaspContainer(title = gettext("Plots"))
    plotsContainer$dependOn(optionsFromObject = jaspResults[["results"]])
    plotsContainer$position <- 2
    jaspResults[["plots"]] <- plotsContainer
  } else {
    plotsContainer <- jaspResults[["plots"]]
  }

  .bcPlotPriorPosteriorPositive(results, plotsContainer, dataset, options)
  .bcPlotIconPlot              (results, plotsContainer, dataset, options)
  .bcPlotROC                   (results, plotsContainer, dataset, options)
  .bcPlotVaryingPrevalence     (results, plotsContainer, dataset, options)
  .bcPlotAlluvial              (results, plotsContainer, dataset, options)
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
                   dependencies = "plotPriorPosteriorPositive",
                   position     = 3,
                   width        = 400
                  )

  return()
}

.bcPlotIconPlot <- function(results, jaspResults, dataset, options) {
  UseMethod(".bcPlotIconPlot")
}

.bcPlotIconPlot.bcPointEstimates <- function(results, jaspResults, dataset, options) {
  if( isFALSE(options[["plotIconPlot"]])     ) return()
  if(!is.null(jaspResults[["plotIconPlot"]]) ) return()

  data <- expand.grid(cond = gettext(c("Positive", "Negative")),
                      test = gettext(c("Positive", "Negative")),
                      KEEP.OUT.ATTRS = FALSE)
  data$out <- factor(
    gettext(c("True positive", "False positive", "False negative", "True negative")),
    levels = gettext(c("True positive", "False positive", "False negative", "True negative"))
  )
  data$prop <- unlist(results[c("truePositive", "falsePositive", "falseNegative", "trueNegative")])

  # if(any(data$prop < 1e-3)) {
  #   npoints <- 1e4
  #   xside <-  yside <- 1e2
  # } else

  if(any(data$prop < 1e-2)) {
    npoints <- 1e4
    xside   <- 10
    yside   <- 1e3
  } else {
    npoints <- 1e2
    xside <- yside <- 10
  }

  data$n <- round(npoints*data$prop, digits = 0)

  data <- data.frame(
    outcome = rep(data$out, data$n),
    x = rep(c(1:xside,xside:1), times = yside/2),
    y = rep(seq_len(yside), each = xside)
  )
  plot <- ggplot2::ggplot(data=data, mapping = ggplot2::aes(x=x,y=y,fill=outcome))

  if(npoints <= 100) {
    plot <- plot + ggplot2::geom_tile(color = "black")
  } else {
    plot <- plot + ggplot2::geom_tile()
  }

  plot <- plot +
    ggplot2::coord_fixed(ratio = xside/yside) +
    ggplot2::ylab(NULL) + ggplot2::xlab(NULL)


  plot <- jaspGraphs::themeJasp(plot, xAxis = FALSE, yAxis = FALSE, legend.position = "right")

  jaspResults[["plotIconPlot"]] <-
    createJaspPlot(title        = gettext("Icon plot"),
                   plot         = plot,
                   dependencies = "plotIconPlot",
                   position     = 3,
                   aspectRatio  = 1,
                   width        = 400
    )
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
    createJaspPlot(title        = gettext("Receiving Operating Characteristic Curve"),
                   plot         = plot,
                   dependencies = "plotROC",
                   position     = 4,
                   aspectRatio  = 1,
                   width        = 400
                   )
}

.bcPlotVaryingPrevalence <- function(results, jaspResults, dataset, options) {
  UseMethod(".bcPlotVaryingPrevalence")
}

.bcPlotVaryingPrevalence.bcPointEstimates <- function(results, jaspResults, dataset, options) {
  if( isFALSE(options[["plotVaryingPrevalence"]])     ) return()
  if(!is.null(jaspResults[["plotVaryingPrevalence"]]) ) return()

  data <- .bcComputeResultsPointEstimates(modifyList(results, list(prevalence = seq(0, 1, by=0.01))))
  data <- data.frame(prevalence = data[["prevalence"]],
                     positivePredictiveValue = data[["positivePredictiveValue"]],
                     negativePredictiveValue = data[["negativePredictiveValue"]]
                     )

  pointData <- data.frame(
    x = rep(results[["prevalence"]], 2),
    y = c(results[["positivePredictiveValue"]], results[["negativePredictiveValue"]])
  )
  plot <- ggplot2::ggplot(data = data) +
    ggplot2::geom_vline(xintercept = results[["prevalence"]], linetype = 2) +
    ggplot2::geom_line(mapping = ggplot2::aes(x = prevalence, y = positivePredictiveValue), size = 2, color = "steelblue") +
    ggplot2::geom_line(mapping = ggplot2::aes(x = prevalence, y = negativePredictiveValue), size = 2, color = "firebrick") +
    jaspGraphs::geom_point(data = pointData, mapping = ggplot2::aes(x = x, y = y), size = 5)

  plot <- jaspGraphs::themeJasp(plot)

  jaspResults[["plotVaryingPrevalence"]] <-
    createJaspPlot(title        = gettext("PPV and NPV by prevalence"),
                   plot         = plot,
                   dependencies = "plotVaryingPrevalence",
                   position     = 6,
                   width        = 400
                   )
}

.bcPlotAlluvial <- function(results, jaspResults, dataset, options) {
  UseMethod(".bcPlotAlluvial")
}

.bcPlotAlluvial.bcPointEstimates <- function(results, jaspResults, dataset, options) {
  if( isFALSE(options[["plotAlluvial"]])     ) return()
  if(!is.null(jaspResults[["plotAlluvial"]]) ) return()

  data <- expand.grid(cond = gettext(c("Positive", "Negative")),
                      test = gettext(c("Positive", "Negative")),
                      KEEP.OUT.ATTRS = FALSE)
  data$out <- factor(
    gettext(c("True positive", "False positive", "False negative", "True negative")),
    levels = gettext(c("True positive", "False positive", "False negative", "True negative"))
  )
  data$prop <- unlist(results[c("truePositive", "falsePositive", "falseNegative", "trueNegative")])

  plot <- ggplot2::ggplot(data = data,
                          mapping = ggplot2::aes(y = prop, axis1 = cond, axis2 = test)) +
    ggalluvial::geom_alluvium(mapping = ggplot2::aes(fill = out)) +
    ggalluvial::geom_stratum(width = 0.5) +
    ggalluvial::stat_stratum(geom = "text", ggplot2::aes(label = ggplot2::after_stat(stratum)), size = 6) +
    ggplot2::scale_x_discrete(limits = c("cond", "test"),
                              labels = gettext(c("Condition", "Test"))) +
    ggplot2::scale_fill_manual(name = "", values = c("darkgreen", "darkorange", "red", "steelblue")) +
    ggplot2::ylab(gettext("Proportion in population"))

  plot <- jaspGraphs::themeJasp(plot, legend.position = "right")

  jaspResults[["plotAlluvial"]] <-
    createJaspPlot(title        = gettext("Alluvial plot"),
                   plot         = plot,
                   dependencies = "plotAlluvial",
                   position     = 8,
                   width        = 600, height = 500
    )
}


.bcTexts <- function(what = c("statistic", "interpretation", "notation")) {
  what <- match.arg(what)
  out <- switch(what,
    statistic = c(
      prevalence              = gettext("Prevalence"),
      sensitivity             = gettext("Sensitivity"),
      specificity             = gettext("Specificity"),
      truePositive            = gettext("True positive"),
      falsePositive           = gettext("False positive"),
      trueNegative            = gettext("True negative"),
      falseNegative           = gettext("False negative"),
      positivePredictiveValue = gettext("Positive predictive value"),
      negativePredictiveValue = gettext("Negative predictive value"),
      falseDiscoveryRate      = gettext("False discovery rate"),
      falseOmissionRate       = gettext("False omission rate"),
      accuracy                = gettext("Accuracy")
    ),
    interpretation = c(
      prevalence              = gettext("Proportion of a population affected by the condition."),
      sensitivity             = gettext("(True positive fraction) Proportion of those who are affected by the condition and are correctly tested positive."),
      specificity             = gettext("(True negative fraction) Proportion of those who are not affected by the condition and are correctly tested negative."),
      truePositive            = gettext("Proportion of a population affected by a condition and correctly tested positive."),
      falsePositive           = gettext("Proportion of a population not affected by a condition and incorrectly tested negative."),
      trueNegative            = gettext("Proportion of a population affected by a condition and incorrectly tested negative."),
      falseNegative           = gettext("Proportion of a population not affected by a condition and correctly tested negative."),
      positivePredictiveValue = gettext("Proportion of those who tested positive and are affected by the condition."),
      negativePredictiveValue = gettext("Proportion of those who tested negative and are not affected by the condition."),
      falseDiscoveryRate      = gettext("Proportion of false positives in the pool of those that test positive."),
      falseOmissionRate       = gettext("Proportion of false negatives in the pool of those that test negative."),
      accuracy                = gettext("Proportion of the population that is true positive or true negative.")
   ),
   notation = c(
     prevalence              = gettext("P(Condition = positive)"),
     sensitivity             = gettext("P(Test = positive | Condition = positive)"),
     specificity             = gettext("P(Test = negative | Condition = negative)"),
     truePositive            = gettextf("P(Condition = positive %s Test = positive)", "\u2227"),
     falsePositive           = gettextf("P(Condition = negative %s Test = positive)", "\u2227"),
     trueNegative            = gettextf("P(Condition = negative %s Test = negative)", "\u2227"),
     falseNegative           = gettextf("P(Condition = positive %s Test = negative)", "\u2227"),
     positivePredictiveValue = gettext("P(Condition = positive | Test = positive)"),
     negativePredictiveValue = gettext("P(Condition = negative | Test = negative)"),
     falseDiscoveryRate      = gettext("P(Condition = negative | Test = positive)"),
     falseOmissionRate       = gettext("P(Condition = positive | Test = negative)"),
     accuracy                = gettextf("P(Condition = positive %1$s Test = positive %2$s Condition = negative %1$s Test = negative)", "\u2227", "\u2228")
   )
  )

  return(out)
}
