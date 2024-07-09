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

LSbinomialtesting   <- function(jaspResults, dataset, options, state = NULL) {

  options <- .parseAndStoreFormulaOptions(jaspResults, options, c("posteriorDistributionPlotMarginalCiBf", "posteriorDistributionPlotConditionalCiBf"))

  # a vector of two, first for data, second for hypotheses
  ready <- .readyBinomialLS(options)

  # introductory text
  if (options[["introductoryText"]])
    .introductoryTextLS(jaspResults, options, "binTest")

  # evaluate the expressions in models
  if (ready["models"])
    options[["models"]] <- .evaluatePriors(options[["models"]], "binTest")
  # scale the prior probabilities
  if (ready["models"])
    options[["models"]] <- .scalePriors(options[["models"]])

  # load, check, transform and process data
  if (ready["data"])
    data <- .readDataBinomialLS(dataset, options)

  # data summary table ifrequested (but not ifthe data counts were added directly)
  .summaryBinomialLS(jaspResults, data, options, "binTest")

  ### inference
  # summary table
  .testsBinomialLS(jaspResults, data, ready, options)

  # prior parameter
  if (options[["priorDistributionPlot"]]) {
    if (options[["priorDistributionPlotType"]] != "conditional")
      .plotsSimpleBinomial2LS(jaspResults, data, ready, options, type = "Prior")
    if (options[["priorDistributionPlotType"]] == "conditional")
      .plotsIndividualBinomial2LS(jaspResults, data, ready, options, type = "Prior")
  }

  # prior predictive
  if (options[["priorPredictivePerformanceDistributionPlot"]]) {
    if (options[["priorPredictivePerformanceDistributionPlotType"]] != "conditional")
      .plotsPredictionsBinomial2LS(jaspResults, data, ready, options, type = "Prior")
    if (options[["priorPredictivePerformanceDistributionPlotType"]] == "conditional")
      .plotsPredictionsIndividualBinomial2LS(jaspResults, data, ready, options, type = "Prior")
    if (options[["priorPredictivePerformanceDistributionPlotPredictionsTable"]])
      .tablePredictions2LS(jaspResults, data, ready, options, type = "Prior")
  }

  # predictive accuracy
  if (options[["priorPredictivePerformanceAccuracyPlot"]])
    .plotsPredAccuracyBinomial2LS(jaspResults, data, ready, options)

  # posterior parameter
  if (options[["posteriorDistributionPlot"]]) {
    if (options[["posteriorDistributionPlotType"]] != "conditional")
      .plotsSimpleBinomial2LS(jaspResults, data, ready, options, type = "Posterior")
    if (options[["posteriorDistributionPlotType"]] == "conditional")
      .plotsIndividualBinomial2LS(jaspResults, data, ready, options, type = "Posterior")
  }

  # prior and posterior
  if (options[["priorAndPosteriorDistributionPlot"]]) {
    if (options[["priorAndPosteriorDistributionPlotType"]] != "conditional")
      .plotsBothBinomialLS2(jaspResults, data, ready, options)
    if (options[["priorAndPosteriorDistributionPlotType"]] == "conditional")
      .plotsBothIndividualBinomial2LS(jaspResults, data, ready, options)
  }


  ### sequential analysis
  if (options[["sequentialAnalysisPredictivePerformancePlot"]])
    .plotsIterativeOverlyingBinomial2LS(jaspResults, data, ready, options)
  if (options[["sequentialAnalysisPredictivePerformancePlot"]] && options[["sequentialAnalysisPredictivePerformancePlotUpdatingTable"]])
    .tableIterativeBinomial2LS(jaspResults, data, ready, options)


  ### posterior predictive
  if (options[["posteriorPredictionSummaryTable"]])
    .tablePredictionsBinomialLS2(jaspResults, data, ready, options)
  if (options[["posteriorPredictionDistributionPlot"]]) {
    if (options[["posteriorPredictionDistributionPlotType"]] != "conditional")
      .plotsPredictionsBinomial2LS(jaspResults, data, ready, options, type = "Posterior")
    if (options[["posteriorPredictionDistributionPlotType"]] == "conditional")
      .plotsPredictionsIndividualBinomial2LS(jaspResults, data, ready, options, type = "Posterior")
    if (options[["posteriorPredictionDistributionPlotPredictionsTable"]])
      .tablePredictions2LS(jaspResults, data, ready, options, type = "Posterior")
  }

  return()
}

.testsBinomialLS              <- function(jaspResults, data, ready, options) {

  if (is.null(jaspResults[["testsContainer"]])) {
    testsContainer <- createJaspContainer("Hypotheses")
    testsContainer$position <- 2
    jaspResults[["testsContainer"]] <- testsContainer
  } else
    testsContainer <- jaspResults[["testsContainer"]]


  if (options[["introductoryText"]] && is.null(testsContainer[['introText']])) {

    introText <- createJaspHtml()
    introText$dependOn("introductoryText")
    introText$position <- 1

    introText[['text']] <- .explanatoryTextLS("tests", options, "binTest")

    testsContainer[['introText']] <- introText
  }


  if (is.null(testsContainer[['testsTable']])) {

    testsTable <- createJaspTable(title = gettext("Testing Summary"))

    testsTable$position <- 2
    testsTable$dependOn(c(.dataDependenciesBinomialLS, "priorPredictivePerformanceBfComparison", "priorPredictivePerformanceBfVsHypothesis", "priorPredictivePerformanceBfType"))

    bfTypeName <- switch(
      options[["priorPredictivePerformanceBfType"]],
      "BF10"    = gettextf("BF%s",     "\u2081\u2080"),
      "BF01"    = gettextf("BF%s",     "\u2080\u2081"),
      "LogBF10" = gettextf("log(BF%s)","\u2081\u2080")
    )

    testsTable$addColumnInfo(name = "hypothesis",   title = gettext("Hypothesis"),          type = "string")
    testsTable$addColumnInfo(name = "prior",        title = gettext("P(H)"),                type = "number")
    testsTable$addColumnInfo(name = "logLik",       title = gettext("log(likelihood)"),     type = "number")
    testsTable$addColumnInfo(name = "posterior",    title = gettext("P(H|data)"),           type = "number")
    testsTable$addColumnInfo(name = "bf",           title = bfTypeName,                    type = "number")

    testsTable$setExpectedSize(length(options[["models"]]))

    testsContainer[["testsTable"]] <- testsTable

    if (ready["data"] && !ready["models"])
      return()
    else if (!ready["data"]) {

      if ((options[["dataInputType"]] == "variable" && options[["dataVariableSelected"]]  != "") ||
          (options[["dataInputType"]] == "sequence" && options[["dataSequenceSequenceOfObservations"]] != ""))
        testsTable$addFootnote(gettext("Please specify successes and failures."))

      return()
    } else if (ready["models"]) {

      tempResults  <- .testBinomialLS(data, options[["models"]])
      marglikIssue <- FALSE
      tableRows    <- list()

      for (i in 1:length(options[["models"]])) {

        tempRow <- list(
          hypothesis  = options[["models"]][[i]]$name,
          prior       = tempResults$prior[i],
          logLik      = tempResults$logLik[i],
          posterior   = tempResults$posterior[i])

        if (options[["priorPredictivePerformanceBfComparison"]] == "inclusion")
          tempBF <- (tempResults$posterior[i] / (1-tempResults$posterior[i])) / (tempResults$prior[i] / (1-tempResults$prior[i]))
        else if (options[["priorPredictivePerformanceBfComparison"]] == "best")
          tempBF <- exp(tempResults$logLik[i]) / exp(tempResults$logLik[which.max(tempResults$logLik)])
        else if (options[["priorPredictivePerformanceBfComparison"]] == "vs")
          if (options[["models"]][[i]][["name"]] == options[["priorPredictivePerformanceBfVsHypothesis"]])
            tempBF <- NA
          else
            tempBF <- exp(tempResults$logLik[i]) / exp(tempResults$logLik[sapply(options[["models"]], function(p)p$name) == options[["priorPredictivePerformanceBfVsHypothesis"]]])

        tempRow$bf <- switch(
          options[["priorPredictivePerformanceBfType"]],
          "BF10"    = tempBF,
          "BF01"    = 1/tempBF,
          "LogBF10" = log(tempBF)
        )

        if (is.na(tempRow$logLik)) {
          marglikIssue <- TRUE
          testsTable$addFootnote(
            gettextf(
              "Summary of %1$s could not be computed. The most likely reason is the lack of numerical precision due to the truncation-data conflict (i.e., majority of the posterior distribution lies outside of the truncation range).",
              options[["models"]][[i]]$name),
            symbol = gettext("Warning:"))
        }

        tableRows[[i]] <- tempRow
      }

      tableRows <- do.call(rbind.data.frame, tableRows)
      if (marglikIssue) {
        tableRows$bf <- NA
        tableRows$posterior <- NA
        testsTable$addFootnote(
          gettext("Bayes factors and posterior probabilities could not be computed since the marginal likelihoods are not available for some models."),
          symbol = gettext("Warning:"))
      }
      testsTable$setData(tableRows)

      # add footnote clarifying what dataset was used
      testsTable$addFootnote(gettextf("These results are based on %1$i success(es) and %2$i failure(s)", data[["nSuccesses"]], data[["nFailures"]]))
    }

  }

  return()
}
.plotsSimpleBinomial2LS       <- function(jaspResults, data, ready, options, type = c("Prior", "Posterior")) {

  containerPlots <- .containerPlots2LS(jaspResults, options, "binTest", type)

  if (is.null(containerPlots[[paste0("plots",type)]])) {

    plotsSimple <- createJaspPlot(
      width  = if (options[[ifelse (type == "Prior", "priorDistributionPlotType", "posteriorDistributionPlotType")]] == "joint" &&
                   options[[ifelse (type == "Prior", "priorDistributionPlotJointType", "posteriorDistributionPlotJointType")]] != "stacked")
        700 else 530,
      height = 400)

    plotsSimple$position <- 2
    if (type == "Prior") {
      dependencies <- c("priorDistributionPlotJointType", "priorDistributionPlotMarginalPointEstimate", "priorDistributionPlotMarginalPointEstimateType",
                        "priorDistributionPlotMarginalCi", "priorDistributionPlotMarginalCiType",
                        "priorDistributionPlotMarginalCiMass", "priorDistributionPlotMarginalCiLower", "priorDistributionPlotMarginalCiUpper")
    } else if (type == "Posterior") {
      dependencies <- c("posteriorDistributionPlotJointType", "posteriorDistributionPlotMarginalPointEstimate", "posteriorDistributionPlotMarginalPointEstimateType",
                        "posteriorDistributionPlotMarginalCi", "posteriorDistributionPlotMarginalCiType",
                        "posteriorDistributionPlotMarginalCiMass", "posteriorDistributionPlotMarginalCiLower", "posteriorDistributionPlotMarginalCiUpper",
                        "posteriorDistributionPlotObservedProportion", "posteriorDistributionPlotMarginalCiBf")
    }
    plotsSimple$dependOn(c(dependencies, .dataDependenciesBinomialLS, "colorPalette", "scaleSpikes"))

    containerPlots[[paste0("plots",type)]] <- plotsSimple

    if ((type == "Posterior" && !ready["data"]) || !ready["models"])
      return()

    if (type == "Prior")
      data <- list(
        nSuccesses = 0,
        nFailures  = 0
      )

    allLines    <- c()
    allArrows   <- c()
    legend       <- NULL
    tempResults <- .testBinomialLS(data, options[["models"]])

    if (anyNA(tempResults$posterior)) {
      plotsSimple$setError(gettext("The plot could not be created because the posterior model probabilities are not defined for all models."))
      return()
    }

    for (i in 1:length(options[["models"]])) {

      if (options[["models"]][[i]]$type == "spike") {

        dfArrowPP       <- .dataArrowBinomialLS(options[["models"]][[i]])
        dfArrowPP$yEnd  <- exp(log(dfArrowPP$yEnd)+log(tempResults[i, tolower(type)]))
        dfArrowPP$g     <- options[["models"]][[i]]$name

        allArrows      <- c(allArrows, list(dfArrowPP))
        legend          <- rbind(legend, c(options[["models"]][[i]]$type, options[["models"]][[i]]$name))

      } else if (options[["models"]][[i]]$type == "beta") {

        dfLinesPP   <- .dataLinesBinomialLS(data, options[["models"]][[i]])
        dfLinesPP   <- dfLinesPP[dfLinesPP$g == type,]
        dfLinesPP$y <- exp(log(dfLinesPP$y)+log(tempResults[i, tolower(type)]))
        dfLinesPP$g <- options[["models"]][[i]]$name

        allLines    <- c(allLines, list(dfLinesPP))
        legend      <- rbind(legend, c(options[["models"]][[i]]$type, options[["models"]][[i]]$name))

      }
    }

    if (type == "Posterior" && options[["posteriorDistributionPlotObservedProportion"]]) {
      dfPoints <- data.frame(
        x = data[["nSuccesses"]]/(data[["nSuccesses"]] + data[["nFailures"]]),
        y = 0,
        g = "Observed"
      )
    } else
      dfPoints <- NULL

    xName  <- bquote(.(gettext("Population proportion"))~theta)

    if (options[[ifelse (type == "Prior", "priorDistributionPlotType", "posteriorDistributionPlotType")]] == "joint") {

      if (options[[ifelse (type == "Prior", "priorDistributionPlotJointType", "posteriorDistributionPlotJointType")]] == "overlying")
        p <- .plotOverlyingLS(allLines, allArrows, dfPoints, xName = xName, palette = options[["colorPalette"]])
      else if (options[[ifelse (type == "Prior", "priorDistributionPlotJointType", "posteriorDistributionPlotJointType")]] == "stacked")
        p <- .plotStackedLS(allLines, allArrows, legend, dfPoints, xName = xName)

    } else if (options[[ifelse (type == "Prior", "priorDistributionPlotType", "posteriorDistributionPlotType")]] == "marginal") {

      allLinesNew <- c()
      allSpikes   <- list()
      if (length(allLines) > 0) {

        for (i in 1:length(allLines)) {

          if (i == 1)
            allLinesNew[[1]] <- allLines[[i]]
          else
            allLinesNew[[1]]$y <- allLinesNew[[1]]$y + allLines[[i]]$y

        }
        allLinesNew[[1]]$g <- "__marginal"
      }

      if (length(allArrows) > 0) {
        for (i in 1:length(allArrows)) {
          allArrows[[i]]$g <- "__marginal"
        }
      }

      if (type == "Prior") {
        for (i in 1:length(options[["models"]])) {
          if (options[["models"]][[i]]$type == "spike") {
            allSpikes <- c(
              allSpikes,
              list(data.frame(y = options[["models"]][[i]]$priorWeight, x = options[["models"]][[i]]$spikePoint, g = "__marginal"))
            )
          }
        }
      } else {
        tempResults <- .testBinomialLS(data, options[["models"]])
        for (i in 1:length(options[["models"]])) {
          if (options[["models"]][[i]]$type == "spike") {
            allSpikes <- c(
              allSpikes,
              list(data.frame(y = tempResults$posterior[i], x = options[["models"]][[i]]$spikePoint, g = "__marginal"))
            )
          }
        }

      }

      if (options[[ifelse (type == "Prior", "priorDistributionPlotMarginalCi", "posteriorDistributionPlotMarginalCi")]]) {

        if (options[[ifelse (type == "Prior", "priorDistributionPlotMarginalCiType", "posteriorDistributionPlotMarginalCiType")]] == "central") {

          dfCI <- .marginalCentralBinomialLS(allLinesNew[[1]], allSpikes,
                                             options[[ifelse (type == "Prior", "priorDistributionPlotMarginalCiMass", "posteriorDistributionPlotMarginalCiMass")]])

        } else if (options[[ifelse (type == "Prior", "priorDistributionPlotMarginalCiType", "posteriorDistributionPlotMarginalCiType")]] == "HPD") {

          dfCI <- .marginalHPDBinomialLS(allLinesNew[[1]], allSpikes,
                                         options[[ifelse (type == "Prior", "priorDistributionPlotMarginalCiMass", "posteriorDistributionPlotMarginalCiMass")]])

        } else if (options[[ifelse (type == "Prior", "priorDistributionPlotMarginalCiType", "posteriorDistributionPlotMarginalCiType")]] == "custom") {

          dfCI <- .marginalCustomBinomialLS(allLinesNew[[1]], allSpikes,
                                            lCI = options[[ifelse (type == "Prior", "priorDistributionPlotMarginalCiLower", "posteriorDistributionPlotMarginalCiLower")]],
                                            uCI = options[[ifelse (type == "Prior", "priorDistributionPlotMarginalCiUpper", "posteriorDistributionPlotMarginalCiUpper")]])

        } else if (options[[ifelse (type == "Prior", "priorDistributionPlotMarginalCiType", "posteriorDistributionPlotMarginalCiType")]] == "support") {

          dfCI <- .marginalSupportBinomialLS(data, options[["models"]], allLinesNew[[1]], allSpikes, options[["posteriorDistributionPlotMarginalCiBf"]])

        }

      } else
        dfCI <- NULL

      if (options[[ifelse (type == "Prior", "priorDistributionPlotMarginalPointEstimate", "posteriorDistributionPlotMarginalPointEstimate")]]) {

        dfPointEstimate <- .dataPointMarginalBinomial(if (type == "Prior") NULL else data, options, allLinesNew[[1]], allSpikes, N = NULL,
                                                      type = "parameter", type2 = type,
                                                      estimate = options[[ifelse (type == "Prior", "priorDistributionPlotMarginalPointEstimateType", "posteriorDistributionPlotMarginalPointEstimateType")]])
      } else
        dfPointEstimate <- NULL

      p <- .plotOverlyingLS(allLinesNew, allArrows, dfPoints, dfPointEstimate, CI = dfCI, xName = xName, noLegend = T)

    }

    plotsSimple$plotObject <- p
  }

  return()
}
.plotsIndividualBinomial2LS   <- function(jaspResults, data, ready, options, type = c("Prior", "Posterior")) {

  containerPlots <- .containerPlots2LS(jaspResults, options, "binTest", type)

  if (is.null(containerPlots[[paste0("plots",type)]])) {

    plotsIndividual <- createJaspContainer()

    plotsIndividual$position <- 2
    if (type == "Prior") {
        dependencies <- c("priorDistributionPlotConditionalPointEstimate", "priorDistributionPlotConditionalPointEstimateType",
                          "priorDistributionPlotConditionalCi", "priorDistributionPlotConditionalCiType", "priorDistributionPlotConditionalCiMass", "priorDistributionPlotConditionalCiLower", "priorDistributionPlotConditionalCiUpper")
      } else if (type == "Posterior") {
        dependencies <- c("posteriorDistributionPlotConditionalPointEstimate", "posteriorDistributionPlotConditionalPointEstimateType",
                          "posteriorDistributionPlotConditionalCi", "posteriorDistributionPlotConditionalCiType", "posteriorDistributionPlotConditionalCiMass", "posteriorDistributionPlotConditionalCiLower", "posteriorDistributionPlotConditionalCiUpper",
                          "posteriorDistributionPlotObservedProportion", "posteriorDistributionPlotConditionalCiBf")
      }
    plotsIndividual$dependOn(c(dependencies, .dataDependenciesBinomialLS, "scaleSpikes"))

    containerPlots[[paste0("plots",type)]] <- plotsIndividual


    if ((type == "Posterior" && !ready["data"]) || !ready["models"]) {

      plotsIndividual[[""]] <- createJaspPlot(title = "", width = 530, height = 400, aspectRatio = 0.7)
      return()

    } else if ((type == "Posterior" && !ready["data"]) || (type == "Posterior" && !ready["models"])) {

      for (i in 1:length(options[["models"]])) {
        plotsIndividual[[options[["models"]][[i]]$name]] <- createJaspPlot(title = options[["models"]][[i]]$name, width = 530, height = 400, aspectRatio = 0.7)
      }
      return()

    } else {

      if (type == "Prior") {
        tempData <- list(
          nSuccesses = 0,
          nFailures  = 0
        )
      } else
        tempData <- data

      tempResults <- .testBinomialLS(tempData, options[["models"]])

      if (type == "Posterior" && options[["posteriorDistributionPlotObservedProportion"]]) {
        dfPoints <- data.frame(
          x = tempData[["nSuccesses"]]/(tempData[["nSuccesses"]] + tempData[["nFailures"]]),
          y = 0,
          g = "Observed"
        )
      } else
        dfPoints <- NULL

      for (i in 1:length(options[["models"]])) {

        tempPlot <- createJaspPlot(title = options[["models"]][[i]]$name, width = 530, height = 400, aspectRatio = 0.7)

        plotsIndividual[[options[["models"]][[i]]$name]] <- tempPlot

        xName  <- bquote(.(gettext("Population proportion"))~theta)

        dfArrowPP   <- NULL
        dfLinesPP   <- NULL
        dfCI        <- NULL
        dfCILinesPP <- NULL

        if (options[[ifelse (type == "Prior", "priorDistributionPlotConditionalCi", "posteriorDistributionPlotConditionalCi")]]) {

          if (options[[ifelse (type == "Prior", "priorDistributionPlotConditionalCiType", "posteriorDistributionPlotConditionalCiType")]] == "central")
            dfCI <- .dataCentralBinomialLS(tempData, options[["models"]][[i]], options[[ifelse (type == "Prior", "priorDistributionPlotConditionalCiMass", "posteriorDistributionPlotConditionalCiMass")]], type = "parameter")
          else if (options[[ifelse (type == "Prior", "priorDistributionPlotConditionalCiType", "posteriorDistributionPlotConditionalCiType")]] == "HPD")
            dfCI <- .dataHPDBinomialLS(tempData, options[["models"]][[i]], options[[ifelse (type == "Prior", "priorDistributionPlotConditionalCiMass", "posteriorDistributionPlotConditionalCiMass")]], type = "parameter")
          else if (options[[ifelse (type == "Prior", "priorDistributionPlotConditionalCiType", "posteriorDistributionPlotConditionalCiType")]] == "custom")
            dfCI <- .dataCustomBinomialLS(tempData, options[["models"]][[i]], options[[ifelse (type == "Prior", "priorDistributionPlotConditionalCiLower", "posteriorDistributionPlotConditionalCiLower")]],
                                          options[[ifelse (type == "Prior", "priorDistributionPlotConditionalCiUpper", "posteriorDistributionPlotConditionalCiUpper")]], type = "parameter")
          else if (options[[ifelse (type == "Prior", "priorDistributionPlotConditionalCiType", "posteriorDistributionPlotConditionalCiType")]] == "support")
            dfCI <- .dataSupportBinomialLS(tempData, options[["models"]][[i]], options[["posteriorDistributionPlotConditionalCiBf"]])

          if (anyNA(dfCI)) {
            tempPlot$setError(gettextf("Plot could not be produced due to lacking numerical precision for %1$s.", options[["models"]][[i]]$name))
            next
          }

        }

        if (options[["models"]][[i]]$type == "spike")
          dfArrowPP  <- .dataArrowBinomialLS(options[["models"]][[i]])
        else if (options[["models"]][[i]]$type == "beta") {

          dfLinesPP   <- .dataLinesBinomialLS(tempData, options[["models"]][[i]])
          dfLinesPP   <- dfLinesPP[dfLinesPP$g == type,]
          dfLinesPP$y <- dfLinesPP$y

          if (anyNA(dfLinesPP)) {
            tempPlot$setError(gettextf("Plot could not be produced due to lacking numerical precision for %1$s.", options[["models"]][[i]]$name))
            next
          }

          if (!is.null(dfCI)) {
            for (r in 1:nrow(dfCI)) {
              tempCILinesPP   <- dfLinesPP[dfLinesPP$x >= dfCI$xStart[r] & dfLinesPP$x <= dfCI$xEnd[r],]
              tempCILinesPP$g <- paste(c(as.character(dfCI$g), r), collapse = "")
              tempCILinesPP   <- rbind.data.frame(
                data.frame(x = dfCI$xStart[r], y = 0, g = tempCILinesPP$g[1]),
                tempCILinesPP,
                data.frame(x = dfCI$xEnd[r], y = 0, g = tempCILinesPP$g[1])
              )
              dfCILinesPP <- rbind.data.frame(dfCILinesPP, tempCILinesPP)
            }
          }

        }

        if (options[[ifelse (type == "Prior", "priorDistributionPlotConditionalPointEstimate", "posteriorDistributionPlotConditionalPointEstimate")]]) {
          dfPointEstimate <- .estimateDataPointBinomial(tempData, options[["models"]][[i]], N = NULL, type = "parameter",
                                                        estimate = options[[ifelse (type == "Prior", "priorDistributionPlotConditionalPointEstimateType", "posteriorDistributionPlotConditionalPointEstimateType")]])
        } else
          dfPointEstimate <- NULL

        p <- .plotIndividualLS(dfLinesPP, dfArrowPP, dfPointEstimate, dfCI, dfCILinesPP, dfPoints, c(0,1), xName, nRound = 3)
        tempPlot$plotObject <- p
      }

    }
  }

  return()
}
.plotsPredictionsBinomial2LS  <- function(jaspResults, data, ready, options, type = c("Prior", "Posterior")) {

  containerPlots <- .containerPrediction2PlotsLS(jaspResults, options, "binTest", type)

  if (is.null(containerPlots[[paste0("priorPredictivePerformanceDistributionPlot",type)]])) {

    plotsPredictions <- createJaspPlot(
      width  = if (options[[ifelse (type == "Prior", "priorPredictivePerformanceDistributionPlotType",  "posteriorPredictionDistributionPlotType")]] == "joint" &&
                   options[[ifelse (type == "Prior", "priorPredictivePerformanceDistributionPlotJoinType", "posteriorPredictionDistributionPlotJoinType")]] != "stacked")
        700 else 530,
      height = 400)

    plotsPredictions$position <- 2
    if (type == "Prior") {
        dependencies <- c("priorPredictivePerformanceDistributionPlotMarginalPointEstimate", "priorPredictivePerformanceDistributionPlotMarginalPointEstimateType", "priorPredictivePerformanceDistributionPlotMarginalCi",
          "priorPredictivePerformanceDistributionPlotMarginalCiType", "priorPredictivePerformanceDistributionPlotMarginalCiMass", "priorPredictivePerformanceDistributionPlotMarginalCiLower",
          "priorPredictivePerformanceDistributionPlotMarginalCiUpper", "priorPredictivePerformanceDistributionPlotJoinType", "priorPredictivePerformanceDistributionPlotObservedNumberOfSuccessess")
      } else if (type == "Posterior") {
        dependencies <- c("posteriorPredictionDistributionPlotMarginalPointEstimate", "posteriorPredictionDistributionPlotMarginalPointEstimateType", "posteriorPredictionDistributionPlotMarginalCi",
          "posteriorPredictionDistributionPlotMarginalCiType", "posteriorPredictionDistributionPlotMarginalCiMass", "posteriorPredictionDistributionPlotMarginalCiLower",
          "posteriorPredictionDistributionPlotMarginalCiUpper", "posteriorPredictionDistributionPlotJoinType", "posteriorPredictionDistributionPlotAsSampleProportion")
      }
    plotsPredictions$dependOn(c(dependencies, .dataDependenciesBinomialLS, "colorPalette"))

    containerPlots[[paste0("priorPredictivePerformanceDistributionPlot",type)]] <- plotsPredictions


    if (!all(ready) || (data[["nSuccesses"]] == 0 && data[["nFailures"]] == 0))
      return()
    else {

      if (type == "Prior") {
        predictionN  <- data[["nSuccesses"]] + data[["nFailures"]]
        tempResults <- .testBinomialLS(data, options[["models"]])
        tempData    <- data.frame(
          nSuccesses = 0,
          nFailures  = 0
        )
      } else if (type == "Posterior") {
        predictionN  <- options[["posteriorPredictionNumberOfFutureTrials"]]
        tempResults <- .testBinomialLS(data, options[["models"]])
        tempData    <- data

        if (anyNA(tempResults$posterior)) {
          plotsPredictions$setError(gettext("The plot could not be created because the posterior model probabilities are not defined for all models."))
          return()
        }
      }

      if (type == "Posterior" && options[["posteriorPredictionDistributionPlotAsSampleProportion"]]) {
        xName  <- gettext("Predicted sample proportions")
        yName  <- gettext("Probability")
        xRange <- c(-.5/predictionN, 1 + .5/predictionN)
        proportions <- options[["posteriorPredictionDistributionPlotAsSampleProportion"]]
        nRound <- 3
      } else {
        xName  <- gettext("Predicted number of successes")
        yName  <- gettext("Probability")
        xRange <- c(-.5, predictionN + .5)
        nRound <- 0
        proportions <- FALSE
      }


      allLines  <- c()
      legend     <- NULL

      for (i in 1:length(options[["models"]])) {

        dfHist   <- .dataHistBinomialLS2(tempData, options[["models"]][[i]], predictionN)
        dfHist$g <- options[["models"]][[i]]$name
        dfHist$y <- dfHist$y*tempResults[i,ifelse (type == "Prior","prior","posterior")]

        if (type == "Posterior" && options[["posteriorPredictionDistributionPlotAsSampleProportion"]])
          dfHist$x <- dfHist$x/predictionN

        # it's not beta, but I'm lazzy to rewrite a function I wanna use
        legend   <- rbind(legend, c("beta", options[["models"]][[i]]$name))
        allLines <- c(allLines, list(dfHist))
      }

      if (type == "Prior") {
        if (options[["priorPredictivePerformanceDistributionPlotObservedNumberOfSuccessess"]])
          dfPoint <- data.frame(x = data[["nSuccesses"]], y = 0)
        else
          dfPoint <- NULL
      } else
        dfPoint <- NULL

      if (options[[ifelse (type == "Prior","priorPredictivePerformanceDistributionPlotType", "posteriorPredictionDistributionPlotType")]] == "joint") {

        if (options[[ifelse (type == "Prior", "priorPredictivePerformanceDistributionPlotJoinType", "posteriorPredictionDistributionPlotJoinType")]] == "overlying") {
          p <- .plotOverlyingLS(allLines, NULL, dfPoints = dfPoint, xName = xName, yName = yName, xRange = xRange,
                                palette = options[["colorPalette"]], nRound = nRound,
                                discrete = TRUE, proportions = proportions)
        } else if (options[[ifelse (type == "Prior", "priorPredictivePerformanceDistributionPlotJoinType", "posteriorPredictionDistributionPlotJoinType")]] == "stacked") {
          p <- .plotStackedLS(allLines, NULL, legend, dfPoints = dfPoint, xName = xName, xRange = xRange,
                              proportions = proportions, discrete = TRUE)
        }



      } else if (options[[ifelse (type == "Prior","priorPredictivePerformanceDistributionPlotType", "posteriorPredictionDistributionPlotType")]] == "marginal") {

        if (length(allLines) > 0) {

          for (i in 1:length(allLines)) {

            if (i == 1)
              allLinesNew <- allLines[[i]]
            else
              allLinesNew$y <- allLinesNew$y + allLines[[i]]$y

          }
          allLinesNew$g <- "__marginal"
        }

        allLinesNew   <- allLinesNew[seq(1,nrow(allLinesNew),2),]
        if (type == "Posterior" && options[["posteriorPredictionDistributionPlotAsSampleProportion"]])
          allLinesNew$x <- allLinesNew$x + .5/predictionN
        else
          allLinesNew$x <- allLinesNew$x + .5

        if (type == "Prior") {
          if (options[["priorPredictivePerformanceDistributionPlotObservedNumberOfSuccessess"]])
            xBlacked <- data[["nSuccesses"]]
          else
            xBlacked <- NULL
        } else
          xBlacked <- NULL

        if (options[[ifelse (type == "Prior", "priorPredictivePerformanceDistributionPlotMarginalCi", "posteriorPredictionDistributionPlotMarginalCi")]]) {

          if (options[[ifelse (type == "Prior", "priorPredictivePerformanceDistributionPlotMarginalCiType", "posteriorPredictionDistributionPlotMarginalCiType")]] == "central") {

            dfCI <- .marginalCentralBinomialLS(allLinesNew, NULL, options[["priorPredictivePerformanceDistributionPlotMarginalCiMass"]], 0, predictionN, TRUE)

          } else if (options[[ifelse (type == "Prior", "priorPredictivePerformanceDistributionPlotMarginalCiType", "posteriorPredictionDistributionPlotMarginalCiType")]] == "HPD") {

            dfCI <- .marginalHPDBinomialLS(allLinesNew, list(),
                                           options[[ifelse (type == "Prior", "priorPredictivePerformanceDistributionPlotMarginalCiMass", "posteriorPredictionDistributionPlotMarginalCiMass")]],
                                           0, predictionN, TRUE)

          } else if (options[[ifelse (type == "Prior", "priorPredictivePerformanceDistributionPlotMarginalCiType", "posteriorPredictionDistributionPlotMarginalCiType")]] == "custom") {

            dfCI <- .marginalCustomBinomialLS(allLinesNew, list(),
                                              lCI = options[[ifelse (type == "Prior", "priorPredictivePerformanceDistributionPlotMarginalCiLower", "posteriorPredictionDistributionPlotMarginalCiLower")]],
                                              uCI = options[[ifelse (type == "Prior", "priorPredictivePerformanceDistributionPlotMarginalCiUpper", "posteriorPredictionDistributionPlotMarginalCiUpper")]],
                                              TRUE)

            if (options[[ifelse (type == "Prior", "priorPredictivePerformanceDistributionPlotMarginalCiUpper", "posteriorPredictionDistributionPlotMarginalCiUpper")]]
                > predictionN) {

              plotsPredictions$setError("The upper CI limit is higher than the number of future
                                       observations. Please change the value of the upper CI limit
                                       in the settings panel.")

              return()
            }
            if (options[[ifelse (type == "Prior", "priorPredictivePerformanceDistributionPlotMarginalCiLower", "posteriorPredictionDistributionPlotMarginalCiLower")]]
                > predictionN) {

              plotsPredictions$setError("The lower CI limit is higher than the number of future
                                       observations. Please change the value of the lower CI limit
                                       in the settings panel.")

              return()
            }
            if (options[[ifelse (type == "Prior", "priorPredictivePerformanceDistributionPlotMarginalCiLower", "posteriorPredictionDistributionPlotMarginalCiLower")]] >
                options[[ifelse (type == "Prior", "priorPredictivePerformanceDistributionPlotMarginalCiUpper", "posteriorPredictionDistributionPlotMarginalCiUpper")]]) {

              plotsPredictions$setError("The lower CI limit is higher than the upper CI limit.
                                       Please change the value of the CI limits
                                       in the settings panel.")

              return()
            }

          }
        } else
          dfCI <- NULL

        if (type == "Posterior" && options[["posteriorPredictionDistributionPlotAsSampleProportion"]])
          xRange <- c(-.5/predictionN, 1 + .5/predictionN)
        else
          xRange <- c(0, predictionN)

        if (options[[ifelse (type == "Prior", "priorPredictivePerformanceDistributionPlotMarginalPointEstimate", "posteriorPredictionDistributionPlotMarginalPointEstimate")]]) {

          dfPointEstimate <- .dataPointMarginalBinomial(if (type == "Prior") NULL else data, options, allLinesNew, NULL, N = predictionN,
                                                        type = "prediction", type2 = type,
                                                        estimate = options[[ifelse (type == "Prior", "priorPredictivePerformanceDistributionPlotMarginalPointEstimateType", "posteriorPredictionDistributionPlotMarginalPointEstimateType")]],
                                                        prop = if (type == "Posterior") options[["posteriorPredictionDistributionPlotAsSampleProportion"]] else FALSE)
        } else
          dfPointEstimate <- NULL

        p <- .plotPredictionLS(allLinesNew, dfPointEstimate, dfCI, xRange = xRange, xName = xName, yName = yName, nRound = nRound, xBlacked = xBlacked,
                               proportions = proportions, predictionN = predictionN)

      } else
        p <- .plotStackedLS(allLines, NULL, legend, dfPoints = dfPoint, xName = xName, xRange = xRange, proportions = proportions)

      plotsPredictions$plotObject <- p
    }
  }

  return()
}
.plotsPredictionsIndividualBinomial2LS  <- function(jaspResults, data, ready, options, type = c("Prior", "Posterior")) {

  containerPlots <- .containerPrediction2PlotsLS(jaspResults, options, "binTest", type)

  if (is.null(containerPlots[[paste0("priorPredictivePerformanceDistributionPlot",type)]])) {

    plotsPredictionsIndividual <- createJaspContainer()

    plotsPredictionsIndividual$position <- 2
    if (type == "Prior") {
        dependencies <- c("priorPredictivePerformanceDistributionPlotConditionalPointEstimate", "priorPredictivePerformanceDistributionPlotConditionalPointEstimateType", "priorPredictivePerformanceDistributionPlotConditionalCi",
          "priorPredictivePerformanceDistributionPlotConditionalCiType", "priorPredictivePerformanceDistributionPlotConditionalCiMass", "priorPredictivePerformanceDistributionPlotConditionalCiLower",
          "priorPredictivePerformanceDistributionPlotConditionalCiUpper", "priorPredictivePerformanceDistributionPlotObservedNumberOfSuccessess")
      } else if (type == "Posterior") {
        dependencies <- c("posteriorPredictionDistributionPlotConditionalPointEstimate", "posteriorPredictionDistributionPlotConditionalPointEstimateType", "posteriorPredictionDistributionPlotConditionalCi",
          "posteriorPredictionDistributionPlotConditionalCiType", "posteriorPredictionDistributionPlotConditionalCiMass", "posteriorPredictionDistributionPlotConditionalCiLower",
          "posteriorPredictionDistributionPlotConditionalCiUpper", "posteriorPredictionDistributionPlotAsSampleProportion")
      }
    plotsPredictionsIndividual$dependOn(c(dependencies, .dataDependenciesBinomialLS, "colorPalette"))


    containerPlots[[paste0("priorPredictivePerformanceDistributionPlot",type)]] <- plotsPredictionsIndividual


    if (all(!ready) || (ready["data"] && !ready["models"])) {

      plotsPredictionsIndividual[[""]] <- createJaspPlot(title = "", width = 530, height = 400, aspectRatio = 0.7)
      return()

    } else if ((!ready["data"] && ready["models"]) || (data[["nSuccesses"]] == 0 & data[["nFailures"]] == 0)) {

      for (i in 1:length(options[["models"]])) {
        plotsPredictionsIndividual[[options[["models"]][[i]]$name]] <- createJaspPlot(title = options[["models"]][[i]]$name,
                                                                                      width = 530, height = 400, aspectRatio = 0.7)
      }
      return()

    } else {

      if (type == "Prior") {
        predictionN  <- data[["nSuccesses"]] + data[["nFailures"]]
        tempResults  <- .testBinomialLS(data, options[["models"]])
        tempData     <- data.frame(
          nSuccesses = 0,
          nFailures  = 0
        )
      } else if (type == "Posterior") {
        predictionN  <- options[["posteriorPredictionNumberOfFutureTrials"]]
        tempResults  <- .testBinomialLS(data, options[["models"]])
        tempData     <- data
      }

      for (i in 1:length(options[["models"]])) {

        tempPlot <- createJaspPlot(title = options[["models"]][[i]]$name, width = 530, height = 400, aspectRatio = 0.7)

        plotsPredictionsIndividual[[options[["models"]][[i]]$name]] <- tempPlot

        if (type == "Posterior" && options[["posteriorPredictionDistributionPlotAsSampleProportion"]]) {
          xName  <- gettext("Predicted sample proportions")
          yName  <- gettext("Density")
          xRange <- c(-.5/predictionN, 1 + .5/predictionN)
          proportions <- options[["posteriorPredictionDistributionPlotAsSampleProportion"]]
        } else {
          xName  <- gettext("Predicted number of successes")
          yName  <- gettext("Probability")
          xRange <- c(0, predictionN)
          proportions <- FALSE
        }


        dfCI   <- NULL
        dfHist <- NULL

        if (options[[ifelse (type == "Prior","priorPredictivePerformanceDistributionPlotConditionalCi","posteriorPredictionDistributionPlotConditionalCi")]]) {

          if (options[[ifelse (type == "Prior","priorPredictivePerformanceDistributionPlotConditionalCiType","posteriorPredictionDistributionPlotConditionalCiType")]] == "central") {

            dfCI <- .dataCentralBinomialLS(tempData, options[["models"]][[i]],
                                           options[[ifelse (type == "Prior","priorPredictivePerformanceDistributionPlotConditionalCiMass","posteriorPredictionDistributionPlotConditionalCiMass")]],
                                           n = predictionN,type = "prediction")

          } else if (options[[ifelse (type == "Prior","priorPredictivePerformanceDistributionPlotConditionalCiType","posteriorPredictionDistributionPlotConditionalCiType")]] == "HPD") {

            dfCI <- .dataHPDBinomialLS(tempData, options[["models"]][[i]],
                                       options[[ifelse (type == "Prior","priorPredictivePerformanceDistributionPlotConditionalCiMass","posteriorPredictionDistributionPlotConditionalCiMass")]],
                                       n = predictionN, type = "prediction")

          } else if (options[[ifelse (type == "Prior","priorPredictivePerformanceDistributionPlotConditionalCiType","posteriorPredictionDistributionPlotConditionalCiType")]] == "custom") {

            if (type == "Posterior" && options[["posteriorPredictionDistributionPlotAsSampleProportion"]]) {
              options[[ifelse (type == "Prior","priorPredictivePerformanceDistributionPlotConditionalCiLower","posteriorPredictionDistributionPlotConditionalCiLower")]] <-
                predictionN * options[[ifelse (type == "Prior","priorPredictivePerformanceDistributionPlotConditionalCiLower","posteriorPredictionDistributionPlotConditionalCiLower")]]
              options[[ifelse (type == "Prior","priorPredictivePerformanceDistributionPlotConditionalCiUpper","posteriorPredictionDistributionPlotConditionalCiUpper")]] <-
                predictionN * options[[ifelse (type == "Prior","priorPredictivePerformanceDistributionPlotConditionalCiUpper","posteriorPredictionDistributionPlotConditionalCiUpper")]]
            }
            dfCI <- .dataCustomBinomialLS(tempData, options[["models"]][[i]],
                                          options[[ifelse (type == "Prior","priorPredictivePerformanceDistributionPlotConditionalCiLower","posteriorPredictionDistributionPlotConditionalCiLower")]],
                                          options[[ifelse (type == "Prior","priorPredictivePerformanceDistributionPlotConditionalCiUpper","posteriorPredictionDistributionPlotConditionalCiUpper")]],
                                          n = predictionN, type = "prediction")

            if (options[[ifelse (type == "Prior","priorPredictivePerformanceDistributionPlotConditionalCiUpper","posteriorPredictionDistributionPlotConditionalCiUpper")]] > predictionN) {

              plotsPredictionsIndividual[[options[["models"]][[i]]$name]]$setError(gettext(
                "The upper CI limit is higher than the number of future observations. Please, change the value of the upper CI limit in the settings panel."))

              return()
            }
            if (options[[ifelse (type == "Prior","priorPredictivePerformanceDistributionPlotConditionalCiLower","posteriorPredictionDistributionPlotConditionalCiLower")]]  > predictionN) {

              plotsPredictionsIndividual[[options[["models"]][[i]]$name]]$setError(gettext(
                "The lower CI limit is higher than the number of future observations. Please, change the value of the lower CI limit in the settings panel."))

              return()
            }
            if (options[[ifelse (type == "Prior","priorPredictivePerformanceDistributionPlotConditionalCiLower","posteriorPredictionDistributionPlotConditionalCiLower")]]
                > options[[ifelse (type == "Prior","priorPredictivePerformanceDistributionPlotConditionalCiUpper","posteriorPredictionDistributionPlotConditionalCiUpper")]]) {

              plotsPredictionsIndividual[[options[["models"]][[i]]$name]]$setError(gettext(
                "The lower CI limit is higher than the upper CI limit. Please, change the value of the CI limits in the settings panel."))

              return()
            }

          }
        }

        dfHist  <- .dataHistBinomialLS(tempData, options[["models"]][[i]], predictionN)

        if (anyNA(dfCI) || anyNA(dfHist)) {
          tempPlot$setError(gettextf("Plot could not be produced due to lacking numerical precision for %1$s.", options[["models"]][[i]]$name))
          next
        }

        if (type == "Prior") {
          if (options[["priorPredictivePerformanceDistributionPlotObservedNumberOfSuccessess"]])
            xBlacked <- data[["nSuccesses"]]
          else
            xBlacked <- NULL
        } else
          xBlacked <- NULL

        if (type == "Posterior" && options[["posteriorPredictionDistributionPlotAsSampleProportion"]]) {
          dfHist$x <- dfHist$x/predictionN
          if (options[["posteriorPredictionDistributionPlotConditionalCi"]]) {
            dfCI$xStart <- dfCI$xStart/predictionN
            dfCI$xEnd   <- dfCI$xEnd  /predictionN
          }
          nRound <- 3
        } else
          nRound <- 0

        if (options[[ifelse (type == "Prior", "priorPredictivePerformanceDistributionPlotConditionalPointEstimate", "posteriorPredictionDistributionPlotConditionalPointEstimate")]]) {
          dfPointEstimate <- .estimateDataPointBinomial(tempData, options[["models"]][[i]], N = predictionN, type = "prediction",
                                                        estimate = options[[ifelse (type == "Prior", "priorPredictivePerformanceDistributionPlotConditionalPointEstimateType", "posteriorPredictionDistributionPlotConditionalPointEstimateType")]],
                                                        prop = ifelse (type == "Prior", FALSE, options[["posteriorPredictionDistributionPlotAsSampleProportion"]])
          )
        } else
          dfPointEstimate <- NULL

        p <- .plotPredictionLS(dfHist, dfPointEstimate, dfCI, xRange, xName, yName, nRound = nRound, xBlacked = xBlacked,
                               proportions = proportions, predictionN = predictionN)
        tempPlot$plotObject <- p
      }
    }
  }

  return()
}
.tablePredictions2LS                    <- function(jaspResults, data, ready, options, type = c("Prior", "Posterior")) {

  containerPlots <- .containerPrediction2PlotsLS(jaspResults, options, "binTest", type)

  if (is.null(containerPlots[["tablePredictions"]])) {

    tablePredictions <- createJaspTable()

    tablePredictions$position <- 3
    tablePredictions$dependOn(c(
      .dataDependenciesBinomialLS,
      ifelse (type == "Prior", "priorPredictivePerformanceDistributionPlotPredictionsTable", "posteriorPredictionDistributionPlotPredictionsTable"),
      if (type == "Posterior") "posteriorPredictionDistributionPlotAsSampleProportion"
    ))
    containerPlots[["tablePredictions"]] <- tablePredictions


    if (type == "Prior") {
      tempData <- list(
        nSuccesses = 0,
        nFailures  = 0
      )
      tempN <- data$nSuccesses + data$nFailures
    } else if (type == "Posterior") {
      tempData <- data
      tempN <- options[["posteriorPredictionNumberOfFutureTrials"]]
    }


    if (type == "Posterior" && options[["posteriorPredictionDistributionPlotAsSampleProportion"]]) {
      tablePredictions$addColumnInfo(name = "successes", title = gettext("Proportion of Successes"), type = "number")
      tablePredictions$addColumns(c(0:tempN)/tempN)
    } else {
      tablePredictions$addColumnInfo(name = "successes", title = gettext("Successes"), type = "integer")
      tablePredictions$addColumns(0:tempN)
    }


    if (ready["models"]) {
      if (options[[ifelse (type == "Prior", "priorPredictivePerformanceDistributionPlotType", "posteriorPredictionDistributionPlotType")]] %in% c("joint", "conditional")) {
        for (i in seq_along(options[["models"]])) {
          tablePredictions$addColumnInfo(name = paste0("hyp_", i), title = gettextf("P(Successes|%s)", options[["models"]][[i]]$name), type = "number")
        }
      } else if (options[[ifelse (type == "Prior", "priorPredictivePerformanceDistributionPlotType", "posteriorPredictionDistributionPlotType")]] == "marginal")
        tablePredictions$addColumnInfo(name = "marginal", title = gettextf("P(Successes)"), type = "number")
    } else
      return()


    if (!ready["data"] && type != "Prior") {

      if ((options[["dataInputType"]] == "variable" && options[["dataVariableSelected"]] != "") ||
          (options[["dataInputType"]] == "sequence" && options[["dataSequenceSequenceOfObservations"]]    != ""))
        tablePredictions$addFootnote(gettext("Please specify successes and failures."))

      return()
    }


    tempResults <- .testBinomialLS(tempData, options[["models"]])
    tempProb    <- NULL

    for (i in 1:length(options[["models"]])) {
      tempProb <- cbind(tempProb, .predictBinomialValuesLS(tempData, options[["models"]][[i]], tempN))
    }

    if (options[[ifelse (type == "Prior", "priorPredictivePerformanceDistributionPlotType", "posteriorPredictionDistributionPlotType")]] == "conditional") {
      for (i in 1:length(options[["models"]])) {
        tablePredictions$addColumns(tempProb[,i])
      }
    } else if (options[[ifelse (type == "Prior", "priorPredictivePerformanceDistributionPlotType", "posteriorPredictionDistributionPlotType")]] == "joint") {
      if (anyNA(tempProb)) {
        tablePredictions$setError(gettext("The table could not be created because the posterior model probabilities are not defined for all models."))
        return()
      }
      for (i in 1:length(options[["models"]])) {
        tablePredictions$addColumns(tempProb[,i]*tempResults[i,"posterior"])
      }
    } else if (options[[ifelse (type == "Prior", "priorPredictivePerformanceDistributionPlotType", "posteriorPredictionDistributionPlotType")]] == "marginal") {
      if (anyNA(tempProb)) {
        tablePredictions$setError(gettext("The table could not be created because the posterior model probabilities are not defined for all models."))
        return()
      }
      tablePredictions$addColumns(apply(tempProb*matrix(tempResults[,"posterior"], byrow = T, ncol = length(options[["models"]]), nrow = tempN + 1), 1, sum))
    }

  }
  return()
}
.plotsPredAccuracyBinomial2LS <- function(jaspResults, data, ready, options) {

  containerPredictiveAccuracy <- .containerPredictiveAccuracyLS(jaspResults, options, "binTest")

  if (is.null(containerPredictiveAccuracy[["plotsPredAccuracy"]])) {

    plotsPredAccuracy <- createJaspPlot(width = 530, height = 400, aspectRatio = 0.7)

    plotsPredAccuracy$position <- 2
    plotsPredAccuracy$dependOn(c(.dataDependenciesBinomialLS, "colorPalette"))

    containerPredictiveAccuracy[["plotsPredAccuracy"]] <- plotsPredAccuracy


    if (!all(ready) || (data[["nSuccesses"]] == 0 && data[["nFailures"]] == 0))
      return()
    else {

      predictionN  <- data[["nSuccesses"]] + data[["nFailures"]]
      tempResults <- .testBinomialLS(data, options[["models"]])

      dfHistAll   <- NULL
      xRange       <- c(0, predictionN)
      xName        <- gettext("Hypothesis")
      yName        <- gettext("Probability")

      if (options[["priorPredictivePerformanceAccuracyPlotType"]] == "conditional")
        tempY <- exp(tempResults[,"logLik"])
      else if (options[["priorPredictivePerformanceAccuracyPlotType"]] == "joint")
        tempY <- exp(tempResults[,"logLik"])*tempResults[,"prior"]
      else if (options[["priorPredictivePerformanceAccuracyPlotType"]] == "marginal")
        tempY <- tempResults[,"posterior"]

      dfHistAll <- data.frame(
        "x" = 1:length(options[["models"]]),
        "y" = tempY,
        "g" = sapply(options[["models"]],function(x)x$name))

      if (anyNA(dfHistAll$y)) {
        plotsPredAccuracy$setError(gettext("The plot could not be created because the posterior model probabilities are not defined for all models."))
        return()
      }

      p <- .plotAccuracyLS(dfHistAll, xName = xName, yName = yName)
      plotsPredAccuracy$plotObject <- p

    }
  }

  return()
}
.plotsIterativeOverlyingBinomial2LS <- function(jaspResults, data, ready, options) {

  containerSequentialTests <- .containerSequentialTestsLS(jaspResults, options, "binTest")

  if (is.null(containerSequentialTests[["sequentialAnalysisPredictivePerformancePlot"]])) {

    plotsIterative <- createJaspPlot(width = 700, height = 400)

    plotsIterative$position <- 2
    plotsIterative$dependOn(c(.dataDependenciesBinomialLS, "colorPalette",
                              "sequentialAnalysisPredictivePerformancePlotBfComparison", "sequentialAnalysisPredictivePerformancePlotBfType", "sequentialAnalysisPredictivePerformancePlotBfVsHypothesis"))
    containerSequentialTests[["sequentialAnalysisPredictivePerformancePlot"]] <- plotsIterative

    if (length(data[["y"]]) == 0)
      return()
    if (!all(ready))
      return()

    if (options[["sequentialAnalysisPredictivePerformancePlotType"]] == "BF") {
      if (options[["sequentialAnalysisPredictivePerformancePlotBfComparison"]] == "vs" &&  options[["sequentialAnalysisPredictivePerformancePlotBfVsHypothesis"]] == "") {
        plotsIterative$setError(gettext("Please specify a hypothesis for comparison."))
        return()
      }
      if (length(options[["models"]]) < 2) {
        plotsIterative$setError("At least 2 hypotheses need to be specified.")
        return()
      }
      if (options[["sequentialAnalysisPredictivePerformancePlotBfComparison"]] == "best")
        theBest <- which.max(.testBinomialLS(data, options[["models"]])$logLik)
    }

    results <- NULL
    iterSeq <- 0:length(data[["y"]])

    for (i in iterSeq) {

      tempData    <- list(
        nSuccesses = sum(data[["y"]][0:i] == 1),
        nFailures  = sum(data[["y"]][0:i] == 0)
      )

      tempResults <- .testBinomialLS(tempData, options[["models"]])

      if (options[["sequentialAnalysisPredictivePerformancePlotType"]] == "conditional") {
        yName  <- gettext("Conditional probability")
        tempY  <- exp(tempResults[,"logLik"])
      } else if (options[["sequentialAnalysisPredictivePerformancePlotType"]] == "joint") {
        yName  <- gettext("Joint probability")
        tempY  <- exp(tempResults[,"logLik"])*tempResults[,"prior"]
      } else if (options[["sequentialAnalysisPredictivePerformancePlotType"]] == "marginal") {
        yName  <- gettext("Posterior probability")
        tempY  <- tempResults[,"posterior"]
      } else if (options[["sequentialAnalysisPredictivePerformancePlotType"]] == "BF") {

        if (options[["sequentialAnalysisPredictivePerformancePlotBfComparison"]] == "inclusion") {
          tempBF <- sapply(1:nrow(tempResults), function(h)
            (tempResults$posterior[h] / (1-tempResults$posterior[h])) / (tempResults$prior[h] / (1-tempResults$prior[h]))
          )
        } else if (options[["sequentialAnalysisPredictivePerformancePlotBfComparison"]] == "best") {
          tempBF <- sapply(1:nrow(tempResults), function(h)
            exp(tempResults$logLik[h]) / exp(tempResults$logLik[theBest])
          )
        } else if (options[["sequentialAnalysisPredictivePerformancePlotBfComparison"]] == "vs") {
          tempBF <- sapply(1:nrow(tempResults), function(h)
            exp(tempResults$logLik[h]) / exp(tempResults$logLik[sapply(options[["models"]], function(p)p$name) == options[["sequentialAnalysisPredictivePerformancePlotBfVsHypothesis"]]])
          )
        }

        if (options[["sequentialAnalysisPredictivePerformancePlotBfType"]] == "BF10")
          tempY <- tempBF
        else if (options[["sequentialAnalysisPredictivePerformancePlotBfType"]] == "BF01")
          tempY <- 1/tempBF
        else if (options[["sequentialAnalysisPredictivePerformancePlotBfType"]] == "LogBF10")
          tempY <- log(tempBF)

        yName <- switch(
          options[["sequentialAnalysisPredictivePerformancePlotBfType"]],
          "BF10"    = bquote("BF"["10"]),
          "BF01"    = bquote("BF"["01"]),
          "LogBF10" = bquote(italic("log")*"(BF"["10"]*")")
        )
      }

      results <- rbind.data.frame(results, tempY)

    }

    if (anyNA(unlist(results))) {
      plotsIterative$setError(gettext("Bayes factors and posterior probabilities could not be computed since the marginal likelihoods are not available for some models."))
      return()
    }

    if (any(is.infinite(unlist(results)))) {
      plotsIterative$setError(gettext("Plotting not possible: One of the Bayes factor is equal to infinity."))
      return()
    }

    plotDataLines <- list()
    for (h in 1:length(options[["models"]])) {
      if (options[["sequentialAnalysisPredictivePerformancePlotType"]] == "BF" && options[["sequentialAnalysisPredictivePerformancePlotBfComparison"]] == "vs") {
        if (options[["sequentialAnalysisPredictivePerformancePlotBfVsHypothesis"]] == options[["models"]][[h]]$name)next
      }

      tempLines   <- NULL
      tempLines   <- rbind(tempLines, data.frame(
        x    = iterSeq,
        y    = results[,h],
        name = options[["models"]][[h]]$name
      ))
      plotDataLines <- c(plotDataLines, list(tempLines))

    }

    xName  <- gettext("Observation")

    if (options[["sequentialAnalysisPredictivePerformancePlotType"]] == "BF")
      BFlog <- options[["sequentialAnalysisPredictivePerformancePlotBfType"]] == "LogBF10"
    else
      BFlog <- NULL

    p <- .plotIterativeLS(plotDataLines, NULL, xName = xName, yName = yName, xStart = 0, palette = options[["colorPalette"]], BFlog = BFlog)

    plotsIterative$plotObject <- p
  }

  return()
}
.tableIterativeBinomial2LS <- function(jaspResults, data, ready, options) {

  containerSequentialTests <- .containerSequentialTestsLS(jaspResults, options, "binTest")

  if (is.null(containerSequentialTests[["tableIterative"]])) {

    tableIterative <- createJaspTable()

    tableIterative$position <- 3
    tableIterative$dependOn(c(.dataDependenciesBinomialLS, "sequentialAnalysisPredictivePerformancePlotUpdatingTable"))
    containerSequentialTests[["tableIterative"]] <- tableIterative

    tableIterative$addColumnInfo(name = "iteration", title = gettext("Observations"), type = "integer")
    if (ready["models"]) {
      for (i in 1:length(options[["models"]])) {
        tableIterative$addColumnInfo(
          name  = options[["models"]][[i]]$name,
          title = options[["models"]][[i]]$name,
          type = "number")
      }
    }


    if (!all(ready))
      return()

    if (options[["sequentialAnalysisPredictivePerformancePlotType"]] == "BF") {
      if (options[["sequentialAnalysisPredictivePerformancePlotBfComparison"]] == "vs" &&  options[["sequentialAnalysisPredictivePerformancePlotBfVsHypothesis"]] == "") {
        tableIterative$setError(gettext("Please specify a hypothesis for comparison."))
        return()
      }
      if (length(options[["models"]]) < 2) {
        tableIterative$setError(gettext("At least 2 hypotheses need to be specified."))
        return()
      }
      if (options[["sequentialAnalysisPredictivePerformancePlotBfComparison"]] == "best")
        theBest <- which.max(.testBinomialLS(data, options[["models"]])$logLik)
    }


    results <- NULL

    if (length(data[["y"]]) > 1)
      iterSeq <- 1:length(data[["y"]])
    else
      iterSeq <- 1

    for (i in iterSeq) {

      tempRow     <- list()
      tempRow[["iteration"]] <- i

      tempData    <- list(
        nSuccesses = sum(data[["y"]][0:i] == 1),
        nFailures  = sum(data[["y"]][0:i] == 0)
      )
      tempResults <- .testBinomialLS(tempData, options[["models"]])

      if (options[["sequentialAnalysisPredictivePerformancePlotType"]] == "conditional")
        tempY <- exp(tempResults[,"logLik"])
      else if (options[["sequentialAnalysisPredictivePerformancePlotType"]] == "joint")
        tempY <- exp(tempResults[,"logLik"])*tempResults[,"prior"]
      else if (options[["sequentialAnalysisPredictivePerformancePlotType"]] == "marginal")
        tempY <- tempResults[,"posterior"]
      else if (options[["sequentialAnalysisPredictivePerformancePlotType"]] == "BF") {

        if (options[["sequentialAnalysisPredictivePerformancePlotBfComparison"]] == "inclusion") {
          tempBF <- sapply(1:nrow(tempResults), function(h)
            (tempResults$posterior[h] / (1-tempResults$posterior[h])) / (tempResults$prior[h] / (1-tempResults$prior[h]))
          )
        } else if (options[["sequentialAnalysisPredictivePerformancePlotBfComparison"]] == "best") {
          tempBF <- sapply(1:nrow(tempResults), function(h)
            exp(tempResults$logLik[h]) / exp(tempResults$logLik[theBest])
          )
        } else if (options[["sequentialAnalysisPredictivePerformancePlotBfComparison"]] == "vs") {
          tempBF <- sapply(1:nrow(tempResults), function(h)
            exp(tempResults$logLik[h]) / exp(tempResults$logLik[sapply(options[["models"]], function(p)p$name) == options[["sequentialAnalysisPredictivePerformancePlotBfVsHypothesis"]]])
          )
        }

        tempY <- switch(
          options[["sequentialAnalysisPredictivePerformancePlotBfType"]],
          "BF10"    = tempBF,
          "BF01"    = 1/tempBF,
          "LogBF10" = log(tempBF)
        )

      }

      for (h in 1:length(options[["models"]])) {
        tempRow[[options[["models"]][[h]]$name]] <- tempY[h]
      }

      tableIterative$addRows(tempRow)
    }
  }

  return()
}
.plotsBothBinomialLS2      <- function(jaspResults, data, ready, options) {

  containerBoth <- .containerPlotsBoth2LS(jaspResults, options, "binTest")

  if (is.null(containerBoth[["priorAndPosteriorDistributionPlot"]])) {

    plotsBoth <- createJaspContainer()

    plotsBoth$position <- 2
    plotsBoth$dependOn(c(.dataDependenciesBinomialLS, "priorAndPosteriorDistributionPlotObservedProportion"))

    containerBoth[["priorAndPosteriorDistributionPlot"]] <- plotsBoth


    if (!all(ready))
      return()

    allLines    <- c()
    allArrows   <- c()
    legend       <- NULL
    tempResults <- .testBinomialLS(data, options[["models"]])

    if (anyNA(tempResults$posterior)) {
      plotsBothError <- createJaspPlot(width = 530, height = 400, aspectRatio = 0.7)
      plotsBoth[["plotsBothError"]] <- plotsBothError
      plotsBothError$setError(gettext("The plot could not be created because the posterior model probabilities are not defined for all models."))
      return()
    }

    for (i in 1:length(options[["models"]])) {

      if (options[["models"]][[i]]$type == "spike") {

        dfArrowPPprior       <- .dataArrowBinomialLS(options[["models"]][[i]])
        dfArrowPPposterior   <- .dataArrowBinomialLS(options[["models"]][[i]])
        dfArrowPPprior$g     <- "Prior"
        dfArrowPPposterior$g <- "Posterior"
        dfArrowPPprior$yEnd     <- exp(log(dfArrowPPprior$yEnd)     + log(tempResults[i, "prior"]))
        dfArrowPPposterior$yEnd <- exp(log(dfArrowPPposterior$yEnd) + log(tempResults[i, "posterior"]))

        allArrows      <- c(allArrows, list(rbind(dfArrowPPposterior, dfArrowPPprior)))

      } else if (options[["models"]][[i]]$type == "beta") {

        dfLinesPP   <- .dataLinesBinomialLS(data, options[["models"]][[i]])
        dfLinesPP$y[dfLinesPP$g == "prior"]     <- exp(log(dfLinesPP$y[dfLinesPP$g == "prior"])+log(tempResults[i, "prior"]))
        dfLinesPP$y[dfLinesPP$g == "posterior"] <- exp(log(dfLinesPP$y[dfLinesPP$g == "posterior"])+log(tempResults[i, "posterior"]))

        allLines   <- c(allLines, list(dfLinesPP))
      }
    }

    if (options[["priorAndPosteriorDistributionPlotObservedProportion"]]) {
      dfPointsPP <- .dataProportionBinomialLS(data)
      if (anyNA(dfPointsPP$x)) dfPointsPP <- NULL
    } else
      dfPointsPP <- NULL

    xName  <- bquote(.(gettext("Population proportion"))~theta)

    if (options[["priorAndPosteriorDistributionPlotType"]] == "joint") {

      spikesI <- 1
      betasI  <- 1

      for (i in 1:length(options[["models"]])) {
        tempPlotsBoth <- createJaspPlot(title = options[["models"]][[i]]$name, width = 530, height = 400, aspectRatio = 0.7)
        plotsBoth[[paste0("plotsBoth_",i)]] <- tempPlotsBoth

        if (options[["models"]][[i]]$type == "spike") {
          tempP   <- .plotPriorPosteriorLS(NULL, allArrows[spikesI], dfPoints = dfPointsPP, xName = xName)
          spikesI <- spikesI + 1
        } else if (options[["models"]][[i]]$type == "beta") {
          tempP   <- .plotPriorPosteriorLS(allLines[betasI], NULL, dfPoints = dfPointsPP, xName = xName)
          betasI  <- betasI + 1
        }

        tempPlotsBoth$plotObject <- tempP
      }


    } else if (options[["priorAndPosteriorDistributionPlotType"]] == "marginal") {

      plotsBothPlot <- createJaspPlot(width = 700, height = 400)
      plotsBoth[["plotsBothPlot"]] <- plotsBothPlot

      allLinesNew <- c()

      if (length(allLines) > 0) {

        for (i in 1:length(allLines)) {

          if (i == 1) {
            allLinesNew[[1]] <- allLines[[i]]
          } else {
            allLinesNew[[1]]$y <- allLinesNew[[1]]$y + allLines[[i]]$y
          }

        }
      }

      p <- .plotPriorPosteriorLS(allLinesNew, allArrows, dfPoints = dfPointsPP, xName = xName)
      plotsBothPlot$plotObject <- p

    }
  }

  return()
}
.plotsBothIndividualBinomial2LS <- function(jaspResults, data, ready, options) {

  containerBoth <- .containerPlotsBoth2LS(jaspResults, options, "binTest")

  if (is.null(containerBoth[["priorAndPosteriorDistributionPlot"]])) {

    plotsBoth <- createJaspContainer()

    plotsBoth$position <- 2
    plotsBoth$dependOn(c(.dataDependenciesBinomialLS, "priorAndPosteriorDistributionPlotObservedProportion"))

    containerBoth[["priorAndPosteriorDistributionPlot"]] <- plotsBoth


    if (all(!ready) || (ready["data"] && !ready["models"])) {

      plotsBoth[[""]] <- createJaspPlot(title = "", width = 530, height = 400, aspectRatio = 0.7)
      return()

    } else if (!ready["data"] && ready["models"]) {

      for (i in 1:length(options[["models"]])) {
        plotsBoth[[options[["models"]][[i]]$name]] <- createJaspPlot(title = options[["models"]][[i]]$name,
                                                                     width = 530, height = 400, aspectRatio = 0.7)
      }
      return()

    } else {

      for (i in 1:length(options[["models"]])) {

        tempPlot <- createJaspPlot(title = options[["models"]][[i]]$name, width = 700, height = 400)

        plotsBoth[[options[["models"]][[i]]$name]] <- tempPlot

        dfArrowPP <- NULL
        dfLinesPP <- NULL

        xName  <- bquote(.(gettext("Population proportion"))~theta)

        if (options[["models"]][[i]]$type == "spike")
          dfArrowPP  <- .dataArrowBinomialLS(options[["models"]][[i]])
        else if (options[["models"]][[i]]$type == "beta") {
          dfLinesPP  <- .dataLinesBinomialLS(data, options[["models"]][[i]])

          if (anyNA(dfLinesPP)) {
            tempPlot$setError(gettextf("Plot could not be produced due to lacking numerical precision for %1$s.", options[["models"]][[i]]$name))
            next
          }

          if (all(dfLinesPP$y[dfLinesPP$g == "Prior"] == dfLinesPP$y[dfLinesPP$g == "Posterior"])) {
            dfLinesPP   <- dfLinesPP[dfLinesPP$g == "Posterior",]
            dfLinesPP$g <- "Prior = Posterior"
          }

        }

        if (options[["priorAndPosteriorDistributionPlotObservedProportion"]]) {
          dfPointsPP <- .dataProportionBinomialLS(data)
          if (anyNA(dfPointsPP$x)) dfPointsPP <- NULL
        } else
          dfPointsPP <- NULL

        p <- .plotPriorPosteriorLS(list(dfLinesPP), list(dfArrowPP), dfPoints = dfPointsPP, xName = xName)
        tempPlot$plotObject <- p
      }
    }
  }

  return()
}
.tablePredictionsBinomialLS2    <- function(jaspResults, data, ready, options) {

  containerPredictions <- .containerPredictionsLS(jaspResults, options, "binTest")

  if (is.null(containerPredictions[["predictionsTable"]])) {

    predictionsTable <- createJaspTable()

    predictionsTable$position <- 2
    predictionsTable$dependOn(c(.dataDependenciesBinomialLS, "posteriorPredictionNumberOfFutureTrials", "posteriorPredictionSummaryTablePointEstimate"))

    estimateText <- .estimateTextLS(options[["posteriorPredictionSummaryTablePointEstimate"]])

    predictionsTable$addColumnInfo(name = "hypothesis",    title = gettext("Model"),                         type = "string")
    predictionsTable$addColumnInfo(name = "posterior",     title = gettextf("Posterior (%s)", "\u03B8"),     type = "string")
    predictionsTable$addColumnInfo(name = "prob",          title = gettext("P(H|data)"),                     type = "number")
    predictionsTable$addColumnInfo(name = "posteriorEst",  title = gettextf("Posterior %s", estimateText),   type = "number")
    predictionsTable$addColumnInfo(name = "predictive",    title = gettextf("Prediction (Successes)"),       type = "string")
    predictionsTable$addColumnInfo(name = "predictiveEst", title = gettextf("Prediction %s", estimateText),  type = "number")

    predictionsTable$setExpectedSize(length(options[["models"]]))

    containerPredictions[["predictionsTable"]] <- predictionsTable

    if (ready["data"] && !ready["models"])
      return()
    else if (!ready["data"]) {

      if ((options[["dataInputType"]] == "variable" && options[["dataVariableSelected"]] != "") ||
          (options[["dataInputType"]] == "sequence" && options[["dataSequenceSequenceOfObservations"]]    != ""))
        predictionsTable$addFootnote(gettext("Please specify successes and failures."))

      return()

    } else {

      tempTests <- .testBinomialLS(data, options[["models"]])
      tempMeans <- NULL
      tableRows <- list()

      # add rows for each hypothesis
      for (i in 1:length(options[["models"]])) {

        tempResults    <- .estimateBinomialLS(data, options[["models"]][[i]])
        tempPrediction <- .predictBinomialLS(data, options[["models"]][[i]], options)

        tempRow <- list(
          hypothesis      = options[["models"]][[i]][["name"]],
          posterior       = tempResults[["distribution"]],
          prob            = tempTests[i, "posterior"],
          posteriorEst    = tempResults[[options[["posteriorPredictionSummaryTablePointEstimate"]]]],
          predictive      = tempPrediction[["distribution"]],
          predictiveEst   = tempPrediction[[options[["posteriorPredictionSummaryTablePointEstimate"]]]]
        )

        if (is.na(tempRow$prob)) {
          marglikIssue <- TRUE
          predictionsTable$addFootnote(
            gettextf(
              "Summary of %1$s could not be computed. The most likely reason is the lack of numerical precision due to the truncation-data conflict (i.e., majority of the posterior distribution lies outside of the truncation range).",
              options[["models"]][[i]]$name),
            symbol = gettext("Warning:"))
        }

        tableRows[[i]] <- tempRow
      }

      tableRows <- do.call(rbind.data.frame, tableRows)
      if (marglikIssue) {
        tableRows$prob <- NA
        predictionsTable$addFootnote(
          gettext("Posterior probabilities and marginal prediction could not be computed since the marginal likelihoods are not available for some models."),
          symbol = gettext("Warning:"))
      } else {
        margEst   <- try(.predictionTableEstimate(data, options, options[["posteriorPredictionSummaryTablePointEstimate"]]))
        tableRows <- rbind(tableRows, data.frame(
          hypothesis     = "Marginal",
          posteriorEst   = margEst[["posteriorEst"]],
          predictiveEst  = margEst[["predictionEst"]]
        ))
      }

      predictionsTable$setData(tableRows)



      # add footnote clarifying what dataset was used
      predictionsTable$addFootnote(gettextf("The prediction for %1$i observation(s) is based on %2$i success(es) and %3$i failure(s)",
                                    options[["posteriorPredictionNumberOfFutureTrials"]],
                                    data[["nSuccesses"]],
                                    data[["nFailures"]]))
    }
  }

  return()
}
.predictionTableEstimate <- function(data, options, estimate) {

  ### posterior estimate
  allLines    <- c()
  allArrows   <- c()
  legend       <- NULL
  tempResults <- .testBinomialLS(data, options[["models"]])
  for (i in 1:length(options[["models"]])) {

    if (options[["models"]][[i]]$type == "spike") {

      dfArrowPP       <- .dataArrowBinomialLS(options[["models"]][[i]])
      dfArrowPP$yEnd <- exp(log(dfArrowPP$yEnd)+log(tempResults[i, "posterior"]))
      dfArrowPP$g     <- options[["models"]][[i]]$name

      allArrows      <- c(allArrows, list(dfArrowPP))
      legend          <- rbind(legend, c(options[["models"]][[i]]$type, options[["models"]][[i]]$name))

    } else if (options[["models"]][[i]]$type == "beta") {

      dfLinesPP   <- .dataLinesBinomialLS(data, options[["models"]][[i]])
      dfLinesPP   <- dfLinesPP[dfLinesPP$g == "Posterior",]
      dfLinesPP$y <- exp(log(dfLinesPP$y)+log(tempResults[i, "posterior"]))
      dfLinesPP$g <- options[["models"]][[i]]$name

      allLines   <- c(allLines, list(dfLinesPP))
      legend      <- rbind(legend, c(options[["models"]][[i]]$type, options[["models"]][[i]]$name))

    }
  }

  allLinesNew <- c()
  allSpikes    <- list()
  if (length(allLines) > 0) {

    for (i in 1:length(allLines)) {

      if (i == 1) {
        allLinesNew[[1]] <- allLines[[i]]
      } else {
        allLinesNew[[1]]$y <- allLinesNew[[1]]$y + allLines[[i]]$y
      }

    }
    allLinesNew[[1]]$g <- "__marginal"
  }

  if (length(allArrows) > 0) {
    for (i in 1:length(allArrows)) {
      allArrows[[i]]$g <- "__marginal"
    }
  }

  tempResults <- .testBinomialLS(data, options[["models"]])
  for (i in 1:length(options[["models"]])) {
    if (options[["models"]][[i]]$type == "spike") {
      allSpikes <- c(
        allSpikes,
        list(data.frame(y = tempResults$posterior[i], x = options[["models"]][[i]]$spikePoint, g = "__marginal"))
      )
    }
  }

  posteriorEst <- .dataPointMarginalBinomial(data, options, allLinesNew[[1]], allSpikes, N = options[["posteriorPredictionNumberOfFutureTrials"]],
                                             type = "parameter", type2 = "Posterior",
                                             estimate = estimate)$x

  ### prediction estimate
  tempResults <- .testBinomialLS(data, options[["models"]])
  allLines  <- c()
  legend     <- NULL

  for (i in 1:length(options[["models"]])) {

    dfHist   <- .dataHistBinomialLS2(data, options[["models"]][[i]], options[["posteriorPredictionNumberOfFutureTrials"]])
    dfHist$g <- options[["models"]][[i]]$name
    dfHist$y <- dfHist$y*tempResults[i,"posterior"]


    # it's not beta, but I'm lazzy to rewrite a function I wanna use
    legend   <- rbind(legend, c("beta", options[["models"]][[i]]$name))
    allLines<- c(allLines, list(dfHist))
  }

  if (length(allLines) > 0) {

    for (i in 1:length(allLines)) {

      if (i == 1) {
        allLinesNew <- allLines[[i]]
      } else {
        allLinesNew$y <- allLinesNew$y + allLines[[i]]$y
      }

    }
    allLinesNew$g <- "__marginal"
  }

  allLinesNew   <- allLinesNew[seq(1,nrow(allLinesNew),2),]
  allLinesNew$x <- allLinesNew$x + .5

  predictionEst <- .dataPointMarginalBinomial(data, options, allLinesNew, NULL, N = options[["posteriorPredictionNumberOfFutureTrials"]],
                                              type = "prediction", type2 = "Posterior",
                                              estimate = estimate)$x

  return(list(
    posteriorEst  = posteriorEst,
    predictionEst = predictionEst
  ))
}
