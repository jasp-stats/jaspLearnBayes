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
  options <- .bcParseOptions(jaspResults, options)
  ready   <- .bcIsReady     (options)
  dataset <- .bcReadData    (jaspResults, dataset, options, ready)

  results <- .bcComputeResults(jaspResults, dataset, options, ready)

  .bcTables(results, jaspResults, dataset, options, ready)
  .bcPlots (results, jaspResults, dataset, options, ready)
}

.bcParseOptions <- function(jaspResults, options) {
  options <- .parseAndStoreFormulaOptions(jaspResults, options,
                                          c(levels(interaction(c("sensitivity", "specificity", "prevalence"), c("", "Alpha", "Beta"), sep = "")),
                                            "threshold")
                                          )

  if(options[["inputType"]] == "pointEstimates") options[["credibleInterval"]] <- FALSE

  return(options)
}

.bcIsReady <- function(options) {
  if(options[["inputType"]] != "data") return(TRUE)

  if(options[["marker"]] != "" && options[["labels"]] != "") {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

.bcReadData <- function(jaspResults, dataset, options, ready) {
  if(options[["inputType"]] != "data" || !ready) return(NULL)

  if(is.null(dataset)) {
    dataset <- .readDataSetToEnd(columns = c(options[["marker"]], options[["labels"]]))
    dataset <- dataset[complete.cases(dataset),]

    labels <- dataset[[options[["labels"]]]]
    levels <- levels(labels)
    if(length(levels) != 2) .quitAnalysis(gettext("The 'labels' variable must have two levels!"))

    dataset <- data.frame(
      marker    = dataset[[options[["marker"]]]],
      condition = labels == levels[2], # second level is "positive" condition
      test      = dataset[[options[["marker"]]]] >= options[["threshold"]]
    )
  }

  # TODO: add check whether marker of condition == TRUE is larger than marker of condition == FALSE

  return(dataset)
}

# Results computation and reshaping ----
.bcComputeResults <- function(jaspResults, dataset, options, ready) {
  if(!is.null(jaspResults[["results"]])) return(jaspResults[["results"]]$object)

  results <- switch(options[["inputType"]],
                    pointEstimates     = .bcComputeResultsPointEstimates(options),
                    uncertainEstimates = .bcComputeResultsUncertainEstimates(options),
                    data               = .bcComputeResultsFromData(options, dataset, ready)
                    )

  jaspResults[["results"]] <-
    createJaspState(object       = results,
                    dependencies = c("inputType", "marker", "labels", "threshold",
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
    lowerCI        = NA,
    upperCI        = NA,
    notation       = .bcTexts("notation")[names(results)],
    interpretation = .bcTexts("interpretation")[names(results)],
    stringsAsFactors = FALSE
    )

  return(output)
}

.bcComputeResultsUncertainEstimates <- function(options) {
  prevalence <- sensitivity <- specificity <- numeric(1e4)
  startProgressbar(expectedTicks = 1e4, label = gettext("Computing samples"))
  for(i in seq_len(1e4)) {
    prevalence[i]  <- rbeta(n=1L, options[["prevalenceAlpha"]],  options[["prevalenceBeta"]])
    invalid <- TRUE
    while(invalid) {
      sensitivity[i] <- rbeta(n=1L, options[["sensitivityAlpha"]], options[["sensitivityBeta"]])
      specificity[i] <- rbeta(n=1L, options[["specificityAlpha"]], options[["specificityBeta"]])
      invalid <- (1-specificity[i]) > sensitivity[i]
    }

    progressbarTick()
  }
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
    interpretation = .bcTexts("interpretation")[names(results)],
    stringsAsFactors = FALSE
    )

  return(output)
}

.bcComputeResultsFromData <- function(options, dataset, ready) {
  if(!ready) return(.bcComputeResultsUncertainEstimates(options))

  res <- list(
    prevalenceAlpha  = options[["prevalenceAlpha"]]  + sum( dataset[["condition"]]),
    prevalenceBeta   = options[["prevalenceBeta"]]   + sum(!dataset[["condition"]]),
    sensitivityAlpha = options[["sensitivityAlpha"]] + sum( dataset[["condition"]] &  dataset[["test"]]),
    sensitivityBeta  = options[["sensitivityBeta"]]  + sum( dataset[["condition"]] & !dataset[["test"]]),
    specificityAlpha = options[["specificityAlpha"]] + sum(!dataset[["condition"]] & !dataset[["test"]]),
    specificityBeta  = options[["specificityBeta"]]  + sum(!dataset[["condition"]] &  dataset[["test"]])
  )

  results <- .bcComputeResultsUncertainEstimates(res)

  class(results) <- c("bcUncertainEstimates", "bcData")
  return(results)
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
  falsePositiveFraction   <- 1-sensitivity
  falseNegativeFraction   <- 1-specificity
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
    falsePositiveFraction   = falsePositiveFraction,
    falseNegativeFraction   = falseNegativeFraction,
    accuracy                = accuracy
  )

  return(results)
}

# Output tables ----
.bcTables <- function(results, jaspResults, dataset, options, ready) {

  if(is.null(jaspResults[["plots"]])) {
    tablesContainer <- createJaspContainer(title = gettext("Tables"))
    tablesContainer$dependOn(optionsFromObject = jaspResults[["results"]], options = c("credibleInterval", "ciLevel"))
    tablesContainer$position <- 1
    jaspResults[["tables"]] <- tablesContainer
  } else {
    tablesContainer <- jaspResults[["tables"]]
  }

  .bcTableStatistics(results, tablesContainer, dataset, options, ready)
  .bcTableConfusion (results, tablesContainer, dataset, options, ready)
}

.bcTableStatistics <- function(results, jaspResults, dataset, options, ready) {
  if( isFALSE(options[["statistics"]])     ) return()
  if(!is.null(jaspResults[["statistics"]]) ) return()

  table <- createJaspTable(title = gettext("Statistics"), position = 1)
  table$dependOn(options = c("statistics", "introText"))
  table$showSpecifiedColumnsOnly <- TRUE

  table$addColumnInfo(name = "statistic",  title = "")
  table$addColumnInfo(name = "estimate", title = gettext("Estimate"), type = "number")


  if(options[["credibleInterval"]]) {
    table$addFootnote(message = .bcTexts("footnote")[["credibleInterval"]])
    ciLevelPercent <- gettextf("%s%% Credible Interval", 100*options[["ciLevel"]])
    table$addColumnInfo(name = "lowerCI", title = gettext("Lower"), overtitle = ciLevelPercent, type = "number")
    table$addColumnInfo(name = "upperCI", title = gettext("Upper"), overtitle = ciLevelPercent, type = "number")
  }

  if(options[["introText"]]) {
    table$addColumnInfo(name = "notation", title = gettext("Notation"))
    table$addColumnInfo(name = "interpretation", title = gettext("Interpretation"))
  }

  if(ready) table$setData(summary(results, ciLevel = options[["ciLevel"]]))

  jaspResults[["statistics"]] <- table
}

.bcTableConfusion <- function(results, jaspResults, dataset, options, ready) {
  if( isFALSE(options[["confusionMatrix"]])     ) return()
  if(!is.null(jaspResults[["confusionMatrix"]]) ) return()

  table <- createJaspTable(title = gettext("Confusion matrix"), position = 2)
  table$dependOn(options = c("confusionMatrix", "confusionMatrixType", "confusionMatrixAddInfo"))
  table$showSpecifiedColumnsOnly <- TRUE
  table$transpose <- TRUE

  ciText <- gettextf("%s%% CI", 100*options[["ciLevel"]])

  table$addColumnInfo(name = "head", title = gettext(""))

  if(options[["confusionMatrixType"]] == "text") {
    table$addColumnInfo(name = "pos",  title = gettext("Positive test"))
  } else if(options[["confusionMatrixType"]] == "number") {
    table$addColumnInfo(name = "pos_value",  title = gettext("Positive test"), type = "number")
  } else {
    table$addColumnInfo(name = "pos",  title = gettext("Outcome"), overtitle = gettext("Positive test"))
    table$addColumnInfo(name = "pos_value", title = gettext("Value"), overtitle = gettext("Positive test"), type = "number")
  }

  if(options[["credibleInterval"]] && options[["confusionMatrixType"]] != "text") {
    table$addFootnote(message = .bcTexts("footnote")[["credibleInterval"]])
    table$addColumnInfo(name = "pos_lower", title = gettextf("Lower %s", ciText), type = "number")
    table$addColumnInfo(name = "pos_upper", title = gettextf("Upper %s", ciText), type = "number")
  }

  if(options[["confusionMatrixType"]] == "text") {
    table$addColumnInfo(name = "neg",  title = gettext("Negative test"))
  } else if(options[["confusionMatrixType"]] == "number") {
    table$addColumnInfo(name = "neg_value",  title = gettext("Negative test"), type = "number")
  } else {
    table$addColumnInfo(name = "neg",  title = gettext("Outcome"), overtitle = gettext("Negative test"))
    table$addColumnInfo(name = "neg_value", title = gettext("Value"), overtitle = gettext("Negative test"), type = "number")
  }

  if(options[["credibleInterval"]] && options[["confusionMatrixType"]] != "text") {
    table$addColumnInfo(name = "neg_lower", title = gettextf("Lower %s", ciText), type = "number")
    table$addColumnInfo(name = "neg_upper", title = gettextf("Upper %s", ciText), type = "number")
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

    if(options[["credibleInterval"]] && options[["confusionMatrixType"]] != "text") {
      table$addColumnInfo(name = "add1_lower", title = gettextf("Lower %s", ciText), type = "number")
      table$addColumnInfo(name = "add1_upper", title = gettextf("Upper %s", ciText), type = "number")
    }


    if(options[["confusionMatrixType"]] == "text") {
      table$addColumnInfo(name = "add2", title = gettext("Negative/Total"))
    } else if(options[["confusionMatrixType"]] == "number") {
      table$addColumnInfo(name = "add2_value", title = gettext("Negative/Total"), type = "number")
    } else {
      table$addColumnInfo(name = "add2", title = gettext("Outcome"), overtitle = gettext("Negative/Total"))
      table$addColumnInfo(name = "add2_value", title = gettext("Value"), overtitle = gettext("Negative/Total"), type = "number")
    }

    if(options[["credibleInterval"]] && options[["confusionMatrixType"]] != "text") {
      table$addColumnInfo(name = "add2_lower", title = gettextf("Lower %s", ciText), type = "number")
      table$addColumnInfo(name = "add2_upper", title = gettextf("Upper %s", ciText), type = "number")
    }

    if(options[["confusionMatrixType"]] == "text") {
      table$addColumnInfo(name = "tot", title = gettext("Total"))
    } else if(options[["confusionMatrixType"]] == "number") {
      table$addColumnInfo(name = "tot_value", title = gettext("Total"), type = "number")
    } else {
      table$addColumnInfo(name = "tot", title = gettext("Outcome"), overtitle = gettext("Total"))
      table$addColumnInfo(name = "tot_value", title = gettext("Value"), overtitle = gettext("Total"), type = "number")
    }

    if(options[["credibleInterval"]] && options[["confusionMatrixType"]] != "text") {
      table$addColumnInfo(name = "tot_lower", title = gettextf("Lower %s", ciText), type = "number")
      table$addColumnInfo(name = "tot_upper", title = gettextf("Upper %s", ciText), type = "number")
    }
  }

  if(ready) .bcFillTableConfusion(results, table, options)

  jaspResults[["confusionMatrix"]] <- table
}

.bcFillTableConfusion <- function(results, table, options) {
  tab <- summary(results)

  df <- data.frame(
    head       = gettext(c("Positive condition", "Negative condition", "True/Total")),

    pos        = tab[c("truePositive", "falsePositive", "positivePredictiveValue"), "statistic", drop=TRUE],
    pos_value  = tab[c("truePositive", "falsePositive", "positivePredictiveValue"), "estimate",  drop=TRUE],
    pos_lower  = tab[c("truePositive", "falsePositive", "positivePredictiveValue"), "lowerCI",   drop=TRUE],
    pos_upper  = tab[c("truePositive", "falsePositive", "positivePredictiveValue"), "upperCI",   drop=TRUE],

    neg        = tab[c("falseNegative", "trueNegative", "negativePredictiveValue"), "statistic", drop=TRUE],
    neg_value  = tab[c("falseNegative", "trueNegative", "negativePredictiveValue"), "estimate",  drop=TRUE],
    neg_lower  = tab[c("falseNegative", "trueNegative", "negativePredictiveValue"), "lowerCI",   drop=TRUE],
    neg_upper  = tab[c("falseNegative", "trueNegative", "negativePredictiveValue"), "upperCI",   drop=TRUE],

    add1       = tab[c("sensitivity", "falsePositiveFraction", "accuracy"), "statistic", drop=TRUE],
    add1_value = tab[c("sensitivity", "falsePositiveFraction", "accuracy"), "estimate",  drop=TRUE],
    add1_lower = tab[c("sensitivity", "falsePositiveFraction", "accuracy"), "lowerCI",   drop=TRUE],
    add1_upper = tab[c("sensitivity", "falsePositiveFraction", "accuracy"), "upperCI",   drop=TRUE],

    add2       = c(tab[c("falseNegativeFraction", "specificity"), "statistic", drop=TRUE], ""),
    add2_value = c(tab[c("falseNegativeFraction", "specificity"), "estimate",  drop=TRUE], NA),
    add2_lower = c(tab[c("falseNegativeFraction", "specificity"), "lowerCI",   drop=TRUE], NA),
    add2_upper = c(tab[c("falseNegativeFraction", "specificity"), "upperCI",   drop=TRUE], NA),

    tot        = c(gettext(c("Prevalence", "Rareness")), ""),
    tot_value  = c(tab["prevalence", "estimate", drop=TRUE], 1-tab["prevalence", "estimate", drop=TRUE], NA),
    tot_lower  = c(tab["prevalence", "lowerCI",  drop=TRUE], 1-tab["prevalence", "lowerCI",  drop=TRUE], NA),
    tot_upper  = c(tab["prevalence", "upperCI",  drop=TRUE], 1-tab["prevalence", "upperCI",  drop=TRUE], NA)
  )

  if(isFALSE(options[["confusionMatrixAddInfo"]]))
    df <- df[1:2,]

  table$setData(df)
}

# Output plots ----
.bcPlots <- function(results, jaspResults, dataset, options, ready) {

  if(is.null(jaspResults[["plots"]])) {
    plotsContainer <- createJaspContainer(title = gettext("Plots"))
    plotsContainer$dependOn(optionsFromObject = jaspResults[["results"]])
    plotsContainer$position <- 2
    jaspResults[["plots"]] <- plotsContainer
  } else {
    plotsContainer <- jaspResults[["plots"]]
  }

  .bcPlotPriorPosteriorPositive(results, plotsContainer, dataset, options, ready, position = 1)
  .bcPlotIconPlot              (results, plotsContainer, dataset, options, ready, position = 2)
  .bcPlotROC                   (results, plotsContainer, dataset, options, ready, position = 3)
  .bcPlotVaryingPrevalence     (results, plotsContainer, dataset, options, ready, position = 4)
  if(!inherits(results, "bcPointEstimates")) return()
  .bcPlotAlluvial              (results, plotsContainer, dataset, options, ready, position = 5)
  .bcPlotSignal                (results, plotsContainer, dataset, options, ready, position = 6)
}

## Prior posterior plot ----
.bcPlotPriorPosteriorPositive <- function(results, plotsContainer, dataset, options, ready, position) {
  if( isFALSE(options[["plotPriorPosteriorPositive"]])     ) return()
  if(!is.null(plotsContainer[["plotPriorPosteriorPositive"]]) ) return()

  plotsContainer[["plotPriorPosteriorPositive"]] <-
    createJaspPlot(title        = gettext("Probability positive"),
                   dependencies = c("plotPriorPosteriorPositive", "credibleInterval", "ciLevel"),
                   position     = position,
                   width        = 500
    )

  if(ready) plotsContainer[["plotPriorPosteriorPositive"]]$plotObject <-
    .bcFillPlotPriorPosteriorPositive(results, dataset, options)
}

.bcFillPlotPriorPosteriorPositive <- function(results, dataset, options) {
  UseMethod(".bcFillPlotPriorPosteriorPositive")
}

.bcFillPlotPriorPosteriorPositive.bcPointEstimates <- function(results, dataset, options) {

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

  return(plot)
}

.bcFillPlotPriorPosteriorPositive.bcUncertainEstimates <- function(results, dataset, options) {
  data <- expand.grid(test   = gettext(c("Not tested", "Tested")),
                      result = gettext(c("Negative", "Positive")))
  data$mean <- data$lower <- data$upper <- numeric(length(nrow(data)))
  alpha <- 1-options[["ciLevel"]]

  for(i in 1:nrow(data)) {
    if(data$test[i] == gettext("Not tested")) {
      data$mean [i] <- mean    (results[["prevalence"]])
      data$lower[i] <- quantile(results[["prevalence"]], p =   alpha/2)
      data$upper[i] <- quantile(results[["prevalence"]], p = 1-alpha/2)
    } else if(data$result[i] == gettext("Negative")) {
      data$mean [i] <- mean    (results[["falseOmissionRate"]])
      data$lower[i] <- quantile(results[["falseOmissionRate"]], p =   alpha/2)
      data$upper[i] <- quantile(results[["falseOmissionRate"]], p = 1-alpha/2)
    } else {
      data$mean [i] <- mean    (results[["positivePredictiveValue"]])
      data$lower[i] <- quantile(results[["positivePredictiveValue"]], p =   alpha/2)
      data$upper[i] <- quantile(results[["positivePredictiveValue"]], p = 1-alpha/2)
    }
  }

  plot <- ggplot2::ggplot(data    = data,
                          mapping = ggplot2::aes(x=result, y=mean, fill=test, ymin=lower, ymax=upper)) +
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(width=0.7), col = "black", width=0.7) +
    ggplot2::xlab(gettext("Test result")) +
    ggplot2::ylab(gettext("P(positive)")) +
    ggplot2::scale_fill_discrete(name = NULL)

  if(options[["credibleInterval"]]) {
    plot <- plot +
      ggplot2::geom_errorbar(position = ggplot2::position_dodge(width=0.7), size = 1, width = 0.5)
  }


  plot <- jaspGraphs::themeJasp(plot, legend.position = "right")

  return(plot)
}

## Icon plot ----
.bcPlotIconPlot <- function(results, plotsContainer, dataset, options, ready, position) {
  if( isFALSE(options[["plotIconPlot"]])     ) return()
  if(!is.null(plotsContainer[["plotIconPlot"]]) ) return()

  plotsContainer[["plotIconPlot"]] <-
    createJaspPlot(title        = gettext("Icon plot"),
                   dependencies = "plotIconPlot",
                   position     = position,
                   aspectRatio  = 1,
                   width        = 400
    )

  if(ready) plotsContainer[["plotIconPlot"]]$plotObject <-
    .bcFillPlotIconPlot(results, dataset, options)

}

.bcFillPlotIconPlot <- function(results, dataset, options) {
  UseMethod(".bcFillPlotIconPlot")
}

.bcFillPlotIconPlot.bcPointEstimates <- function(results, dataset, options) {

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

  return(plot)
}

.bcFillPlotIconPlot.bcUncertainEstimates <- function(results, dataset, options) {
  plot <- ggplot2::ggplot()

  plot <- jaspGraphs::themeJasp(plot, xAxis = FALSE, yAxis = FALSE, legend.position = "right")

  return(plot)
}

## ROC curve plot ----
.bcPlotROC <- function(results, plotsContainer, dataset, options, ready, position) {
  if( isFALSE(options[["plotROC"]])     ) return()
  if(!is.null(plotsContainer[["plotROC"]]) ) return()

  plotsContainer[["plotROC"]] <-
    createJaspPlot(
      title        = gettext("Receiving Operating Characteristic Curve"),
      dependencies = c("plotROC", "credibleInterval", "ciLevel"),
      position     = position,
      aspectRatio  = 1,
      width        = 400
      )

  if(ready) plotsContainer[["plotROC"]]$plotObject <-
    .bcFillPlotROC(results, dataset, options)
}

.bcFillPlotROC <- function(results, dataset, options) {
  UseMethod(".bcFillPlotROC")
}

.bcFillPlotROC.bcPointEstimates <- function(results, dataset, options) {

  threshold    <- qnorm(results[["specificity"]])
  meanPositive <- qnorm(results[["sensitivity"]], mean = threshold)

  falsePositiveFraction <- seq(0, 1, by = 0.01)
  varyingThreshold <- qnorm(falsePositiveFraction, lower.tail = FALSE)
  data <- data.frame(
    falsePositiveFraction = falsePositiveFraction,
    truePositiveFraction  = pnorm(varyingThreshold, meanPositive, lower.tail = FALSE)
  )

  pointData <- data.frame(
    falsePositiveFraction = results[["falsePositiveFraction"]],
    truePositiveFraction  = results[["sensitivity"]]
  )
  plot <- ggplot2::ggplot(data    = data,
                          mapping = ggplot2::aes(x = falsePositiveFraction,
                                                 y = truePositiveFraction)
                          ) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2) +
    ggplot2::geom_line(size = 2) +
    jaspGraphs::geom_point(data = pointData, size = 5) +
    ggplot2::xlab(gettext("False positive fraction (1-Specificity)")) +
    ggplot2::ylab(gettext("True positive fraction (Sensitivity)"))

  plot <- jaspGraphs::themeJasp(plot)

  return(plot)
}

.bcFillPlotROC.bcUncertainEstimates <- function(results, dataset, options) {
  alpha <- 1-options[["ciLevel"]]

  threshold <- qnorm(results[["specificity"]])
  meanPositive <- qnorm(results[["sensitivity"]], mean = threshold)

  falsePositiveFraction <- seq(0, 1, by = 0.01)
  varyingThreshold <- qnorm(falsePositiveFraction, lower.tail = FALSE)
  data <- data.frame(falsePositiveFraction = falsePositiveFraction,
                     mean = NA, lower = NA, upper = NA)

  for(i in seq_along(falsePositiveFraction)) {
    truePositiveFraction <- pnorm(varyingThreshold[i], meanPositive, lower.tail = FALSE)

    data[i, "mean"]  <- median    (truePositiveFraction)
    data[i, "lower"] <- quantile(truePositiveFraction, p =   alpha/2)
    data[i, "upper"] <- quantile(truePositiveFraction, p = 1-alpha/2)
  }

  plot <- ggplot2::ggplot(data    = data,
                          mapping = ggplot2::aes(x    = falsePositiveFraction,
                                                 y    = mean,
                                                 ymin = lower,
                                                 ymax = upper)
                          ) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2)

  if(options[["credibleInterval"]])
    plot <- plot + ggplot2::geom_ribbon(alpha = 0.5)

  plot <- plot +
    ggplot2::geom_line(size = 2) +
    ggplot2::xlab(gettext("False positive fraction (1-Specificity)")) +
    ggplot2::ylab(gettext("True positive fraction (Sensitivity)"))


  plot <- jaspGraphs::themeJasp(plot)

  return(plot)
}

## Varying prevalence plot ----
.bcPlotVaryingPrevalence <- function(results, plotsContainer, dataset, options, ready, position) {
  if( isFALSE(options[["plotVaryingPrevalence"]])     ) return()
  if(!is.null(plotsContainer[["plotVaryingPrevalence"]]) ) return()

  plotsContainer[["plotVaryingPrevalence"]] <-
    createJaspPlot(title        = gettext("PPV and NPV by prevalence"),
                   dependencies = c("plotVaryingPrevalence", "credibleInterval", "ciLevel"),
                   position     = 6,
                   width        = 500
    )

  if(ready) plotsContainer[["plotVaryingPrevalence"]]$plotObject <-
    .bcFillPlotVaryingPrevalence(results, dataset, options)
}

.bcFillPlotVaryingPrevalence <- function(results, dataset, options) {
  UseMethod(".bcFillPlotVaryingPrevalence")
}

.bcFillPlotVaryingPrevalence.bcPointEstimates <- function(results, dataset, options) {

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
    ggplot2::geom_line(mapping = ggplot2::aes(x = prevalence, y = positivePredictiveValue, color = gettext("Positive")), size = 2) +
    ggplot2::geom_line(mapping = ggplot2::aes(x = prevalence, y = negativePredictiveValue, color = gettext("Negative")), size = 2) +
    jaspGraphs::geom_point(data = pointData, mapping = ggplot2::aes(x = x, y = y), size = 5) +
    ggplot2::xlab(gettext("Prevalence")) +
    ggplot2::ylab(gettext("Predictive Value")) +
    ggplot2::scale_color_manual(
      name   = gettext("Predictive Value"),
      values = c("steelblue", "firebrick")
      )

  plot <- jaspGraphs::themeJasp(plot, legend.position = "right")

  return(plot)
}

.bcFillPlotVaryingPrevalence.bcUncertainEstimates <- function(results, dataset, options) {
  alpha <- 1-options[["ciLevel"]]
  sensitivity <- results[["sensitivity"]]
  specificity <- results[["specificity"]]

  prevalence <- seq(0, 1, by = 0.01)
  data <- data.frame(prevalence = prevalence,
                     meanPPV = NA, lowerPPV = NA, upperPPV = NA,
                     meanNPV = NA, lowerNPV = NA, upperNPV = NA)

  for(i in seq_along(prevalence)) {
    truePositive            <- prevalence[i]*sensitivity
    falsePositive           <- (1-prevalence[i])*(1-specificity)
    trueNegative            <- (1-prevalence[i])*specificity
    falseNegative           <- prevalence[i]*(1-sensitivity)
    positivePredictiveValue <- truePositive / (truePositive + falsePositive)
    negativePredictiveValue <- trueNegative / (trueNegative + falseNegative)

    data[i, "meanPPV"]  <- mean    (positivePredictiveValue)
    data[i, "lowerPPV"] <- quantile(positivePredictiveValue, p =   alpha/2)
    data[i, "upperPPV"] <- quantile(positivePredictiveValue, p = 1-alpha/2)

    data[i, "meanNPV"]  <- mean    (negativePredictiveValue)
    data[i, "lowerNPV"] <- quantile(negativePredictiveValue, p =   alpha/2)
    data[i, "upperNPV"] <- quantile(negativePredictiveValue, p = 1-alpha/2)
  }

  plot <- ggplot2::ggplot(data = data)

  if(options[["credibleInterval"]]) {
    plot <- plot +
      ggplot2::geom_ribbon(mapping = ggplot2::aes(x = prevalence, ymin = lowerPPV, ymax = upperPPV, fill = gettext("Positive")), alpha = 0.5) +
      ggplot2::geom_ribbon(mapping = ggplot2::aes(x = prevalence, ymin = lowerNPV, ymax = upperNPV, fill = gettext("Negative")), alpha = 0.5)
  }


  plot <- plot +
    ggplot2::geom_line(mapping = ggplot2::aes(x = prevalence, y = meanPPV, color = gettext("Positive")), size = 2) +
    ggplot2::geom_line(mapping = ggplot2::aes(x = prevalence, y = meanNPV, color = gettext("Negative")), size = 2) +
    ggplot2::xlab(gettext("Prevalence")) +
    ggplot2::ylab(gettext("Predictive Value")) +
    ggplot2::scale_color_manual(
      name   = gettext("Predictive Value"),
      values = c("steelblue", "firebrick")
    ) +
    ggplot2::scale_fill_manual(
      name   = gettext("Predictive Value"),
      values = c("steelblue", "firebrick")
    )

  plot <- jaspGraphs::themeJasp(plot, legend.position = "right")

  return(plot)
}

## Alluvial plot ----
.bcPlotAlluvial <- function(results, plotsContainer, dataset, options, ready, position) {
  if( isFALSE(options[["plotAlluvial"]])     ) return()
  if(!is.null(plotsContainer[["plotAlluvial"]]) ) return()

  plotsContainer[["plotAlluvial"]] <-
    createJaspPlot(title        = gettext("Alluvial plot"),
                   dependencies = "plotAlluvial",
                   position     = position,
                   width        = 600,
                   height       = 500
    )

  if(ready) plotsContainer[["plotAlluvial"]]$plotObject <-
    .bcFillPlotAlluvial(results, dataset, options)
}

.bcFillPlotAlluvial <- function(results, dataset, options) {
  UseMethod(".bcFillPlotAlluvial")
}

.bcFillPlotAlluvial.bcPointEstimates <- function(results, dataset, options) {

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

  return(plot)
}

## Signal detection plot ----
.bcPlotSignal <- function(results, plotsContainer, dataset, options, ready, position) {
  if( isFALSE(options[["plotSignal"]])     ) return()
  if(!is.null(plotsContainer[["plotSignal"]]) ) return()

  plotsContainer[["plotSignal"]] <-
    createJaspPlot(title        = gettext("Signal detection"),
                   dependencies = "plotSignal",
                   position     = position,
                   width        = 600, height = 500
    )

  if(ready) plotsContainer[["plotSignal"]]$plotObject <-
    .bcFillPlotSignal(results, dataset, options)

}

.bcFillPlotSignal <- function(results, dataset, options) {
  UseMethod(".bcFillPlotSignal")
}

.bcFillPlotSignal.bcPointEstimates <- function(results, dataset, options) {

  threshold    <- qnorm(results[["specificity"]])
  meanPositive <- qnorm(results[["sensitivity"]], mean = threshold)

  lowerLimitX <- min(qnorm(0.01, c(0, meanPositive)))
  upperLimitX <- max(qnorm(0.99, c(0, meanPositive)))

  plot <- ggplot2::ggplot() +
    ggplot2::stat_function(fun = .bcwdnorm, args = list(mean = 0, sd = 1, w = 1-options[["prevalence"]]), xlim = c(lowerLimitX, threshold), geom = "area", mapping = ggplot2::aes(fill = "steelblue"), alpha = 0.7)  +
    ggplot2::stat_function(fun = .bcwdnorm, args = list(mean = 0, sd = 1, w = 1-options[["prevalence"]]), xlim = c(threshold, upperLimitX), geom = "area", mapping = ggplot2::aes(fill = "darkorange"), alpha = 0.7) +
    ggplot2::stat_function(fun = .bcwdnorm, args = list(mean = meanPositive, sd = 1, w = options[["prevalence"]]), xlim = c(lowerLimitX, threshold), geom = "area", mapping = ggplot2::aes(fill = "red"), alpha = 0.7) +
    ggplot2::stat_function(fun = .bcwdnorm, args = list(mean = meanPositive, sd = 1, w = options[["prevalence"]]), xlim = c(threshold, upperLimitX), geom = "area", mapping = ggplot2::aes(fill = "darkgreen"), alpha = 0.7) +
    ggplot2::stat_function(fun = .bcwdnorm, args = list(mean = 0, sd = 1, w = 1-options[["prevalence"]]), size = 1) +
    ggplot2::stat_function(fun = .bcwdnorm, args = list(mean = meanPositive, sd = 1, w = options[["prevalence"]]), size = 1) +
    ggplot2::geom_vline(xintercept = threshold, linetype = 2, size = 1.5) +
    ggplot2::scale_x_continuous(breaks = jaspGraphs::getPrettyAxisBreaks(c(lowerLimitX, upperLimitX)),
                                limits = c(lowerLimitX, upperLimitX)) +
    ggplot2::scale_fill_manual(name = "", values = c("darkgreen", "darkorange", "red", "steelblue"), labels = gettext(c("True positive", "False positive", "False negative", "True negative"))) +
    ggplot2::xlab(gettext("Marker")) +
    ggplot2::ylab(gettext("Density"))

  plot <- jaspGraphs::themeJasp(plot, legend.position = "right")

  return(plot)
}

# Helpers ----
.bcTexts <- function(what = c("statistic", "interpretation", "notation", "footnote")) {
  what <- match.arg(what)
  out <- switch(what,
    statistic = c(
      prevalence              = gettext("Prevalence"),
      sensitivity             = gettext("Sensitivity"),
      specificity             = gettext("Specificity"),
      truePositive            = gettext("True positive rate"),
      falsePositive           = gettext("False positive rate"),
      trueNegative            = gettext("True negative rate"),
      falseNegative           = gettext("False negative rate"),
      positivePredictiveValue = gettext("Positive predictive value"),
      negativePredictiveValue = gettext("Negative predictive value"),
      falseDiscoveryRate      = gettext("False discovery rate"),
      falseOmissionRate       = gettext("False omission rate"),
      falsePositiveFraction   = gettext("False positive fraction"),
      falseNegativeFraction   = gettext("False negative fraction"),
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
      falsePositiveFraction   = gettext("Complement proportion to sensitivity."),
      falseNegativeFraction   = gettext("Complement proportion to specificity."),
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
     falsePositiveFraction   = gettext("P(Test = positive | Condition = negative)"),
     falseNegativeFraction   = gettext("P(Test = negative | Condition = positive)"),
     accuracy                = gettextf("P(Condition = positive %1$s Test = positive %2$s Condition = negative %1$s Test = negative)", "\u2227", "\u2228")
   ),
   footnote = c(
     credibleInterval = gettext("Central credible intervals are based on 10,000 samples.")
   )
  )

  return(out)
}

.bcwdnorm <- function(x, mean = 0, sd = 1, w = 1) {
  w * dnorm(x, mean = mean, sd = sd)
}
