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



LSbinomialestimation   <- function(jaspResults, dataset, options, state = NULL) {

  options <- .parseAndStoreFormulaOptions(jaspResults, options, c("posteriorDistributionPlotIndividualCiBf", "sequentialAnalysisPointEstimatePlotCiBf"))

  # a vector of two, first for data, second for hypotheses
  ready <- .readyBinomialLS(options)

  # introductory text
  if (options[["introductoryText"]])
    .introductoryTextLS(jaspResults, options, "binEst")

  # evaluate the expressions in models
  if (ready["models"])
    options[["models"]] <- .evaluatePriors(options[["models"]], "binEst")

  # load, check, transform and process data
  if (ready["data"])
    data <- .readDataBinomialLS(dataset, options)

  # data summary table ifrequested (but not ifthe data counts were added directly)
  .summaryBinomialLS(jaspResults, data, options, "binEst")


  ### inference
  # estimated parameter values
  .estimatesBinomialLS(jaspResults, data, ready, options)

  # prior
  if (options[["priorDistributionPlot"]]) {
    if (options[["priorDistributionPlotType"]] != "individual")
      .plotsSimpleBinomialLS(jaspResults, data, ready, options, type = "Prior")
    if (options[["priorDistributionPlotType"]] == "individual")
      .plotsIndividualBinomialLS(jaspResults, data, ready, options, type = "Prior")
  }

  # posterior
  if (options[["posteriorDistributionPlot"]]) {
    if (options[["posteriorDistributionPlotType"]] != "individual")
      .plotsSimpleBinomialLS(jaspResults, data, ready, options, type = "Posterior")
    if (options[["posteriorDistributionPlotType"]] == "individual")
      .plotsIndividualBinomialLS(jaspResults, data, ready, options, type = "Posterior")
  }


  ### sequential analysis
  # point estimate
  if (options[["sequentialAnalysisPointEstimatePlot"]])
    .plotsIterativeOverlyingBinomialLS(jaspResults, data, ready, options)

  if (options[["sequentialAnalysisStackedDistributionsPlot"]])
    .plotsIterativeStackedBinomialLS(jaspResults, data, ready, options)


  # point estimate table
  if (options[["sequentialAnalysisPointEstimatePlot"]] && options[["sequentialAnalysisPointEstimatePlotUpdatingTable"]])
    .tableIterativeBinomialLS(jaspResults, data, ready, options)

  # interval
  if (options[["sequentialAnalysisIntervalEstimatePlot"]]) {
    if (options[["sequentialAnalysisIntervalEstimatePlotType"]] == "overlying")
      .plotsIterativeIntervalOverlyingBinomialLS(jaspResults, data, ready, options)
    if (options[["sequentialAnalysisIntervalEstimatePlotType"]] == "stacked")
      .plotsIterativeIntervalStackedBinomialLS(jaspResults, data, ready, options)
  }

  # interval estimate table
  if (options[["sequentialAnalysisIntervalEstimatePlot"]] && options[["sequentialAnalysisIntervalEstimatePlotUpdatingTable"]])
    .tableIterativeIntervalBinomialLS(jaspResults, data, ready, options)

  # posterior updating table
  if (options[["sequentialAnalysisPosteriorUpdatingTable"]] && options[["dataInputType"]] != "counts")
    .estimatesSequentialBinomialLS(jaspResults, data, ready, options)


  ### prediction
  if (options[["posteriorPredictionSummaryTable"]])
    .tablePredictionsBinomialLS(jaspResults, data, ready, options)

  # plot
  if (options[["posteriorPredictionDistributionPlot"]]) {
    if (options[["posteriorPredictionDistributionPlotType"]] != "individual")
      .plotsPredictionsBinomialLS(jaspResults, data, ready, options)
    if (options[["posteriorPredictionDistributionPlotType"]] == "individual")
      .plotsPredictionsIndividualBinomialLS(jaspResults, data, ready, options)
    if (options[["posteriorPredictionDistributionPlotPredictionsTable"]])
      .tablePosteriorPredictions(jaspResults, data, ready, options)
  }

  return()
}

# main functions
.estimatesBinomialLS               <- function(jaspResults, data, ready, options) {

  estimatesContainer <- .estimatesContainerLS(jaspResults, options, "binEst")

  if (is.null(estimatesContainer[['estimatesTable']])) {

    estimatesTable <- createJaspTable(title = gettextf("Estimation Summary"))

    estimatesTable$position <- 2
    estimatesTable$dependOn(.dataDependenciesBinomialLS)

    estimateText <- .estimateTextLS(options[["priorAndPosteriorPointEstimate"]])

    estimatesTable$addColumnInfo(name = "hypothesis",   title = gettext("Model"),                       type = "string")
    estimatesTable$addColumnInfo(name = "prior",        title = gettextf("Prior (%s)", "\u03B8"),       type = "string")
    estimatesTable$addColumnInfo(name = "priorEst",     title = gettextf("Prior %s", estimateText),     type = "number")
    estimatesTable$addColumnInfo(name = "posterior",    title = gettextf("Posterior (%s)", "\u03B8"),   type = "string")
    estimatesTable$addColumnInfo(name = "posteriorEst", title = gettextf("Posterior %s", estimateText), type = "number")

    estimatesContainer[["estimatesTable"]] <- estimatesTable

    if (ready["data"] && !ready["models"])
      return()
    else if (!ready["data"]) {

      if ((options[["dataInputType"]] == "variable" && options[["dataVariableSelected"]]  != "") ||
          (options[["dataInputType"]] == "sequence" && options[["dataSequenceSequenceOfObservations"]] != ""))
        estimatesTable$addFootnote(gettext("Please specify successes and failures."))

      return()

    } else if (ready["models"]) {

      # add rows for each hypothesis
      for (i in 1:length(options[["models"]])) {
        # add mock data to use only models
        tempData <- list(
          "nSuccesses" = 0,
          "nFailures"  = 0
        )
        tempResults <- .estimateBinomialLS(tempData, options[["models"]][[i]])

        tempRow <- list(
          prior        = tempResults[["distribution"]],
          priorEst     = tempResults[[options[["priorAndPosteriorPointEstimate"]]]],
          hypothesis   = options[["models"]][[i]][["name"]],
          posterior    = "",
          posteriorEst = "")


        if (all(ready)) {
          # and when real data are supplied as well, add posterior information
          tempResults <- .estimateBinomialLS(data, options[["models"]][[i]])

          tempRow["posterior"]    <- tempResults[["distribution"]]
          tempRow["posteriorEst"] <- tempResults[[options[["priorAndPosteriorPointEstimate"]]]]

        }

        estimatesTable$addRows(tempRow)
      }

      # add footnote clarifying what dataset was used
      estimatesTable$addFootnote(gettextf(
        "These results are based on %1$i %2$s and %3$i %4$s.",
        data[["nSuccesses"]], ifelse (data[["nSuccesses"]] == 1, gettext("success"), gettext("successes")),
        data[["nFailures"]],  ifelse (data[["nFailures"]]  == 1, gettext("failure"), gettext("failures"))
      ))

    }
  }

  return()
}
.estimatesSequentialBinomialLS     <- function(jaspResults, data, ready, options) {

  containerIterativeUpdating <- .containerSequentialUpdatingLS(jaspResults, options, "binEst")

  if (is.null(containerIterativeUpdating[["estimatesSequentialTable"]])) {

    estimatesSequentialTable <- createJaspTable()

    estimatesSequentialTable$position <- 2
    estimatesSequentialTable$dependOn(.dataDependenciesBinomialLS)

    estimatesSequentialTable$addColumnInfo(name = "iteration", title = gettext("Observation"), type = "integer")
    containerIterativeUpdating[["estimatesSequentialTable"]] <- estimatesSequentialTable


    estimatesSequentialTable$setExpectedSize(ifelse (ready["data"], length(data[["y"]]) + 1, 1))
    if (ready["models"]) {
      for (i in 1:length(options[["models"]])) {
        estimatesSequentialTable$addColumnInfo(
          name  = options[["models"]][[i]]$name,
          title = options[["models"]][[i]]$name,
          type = "string")
      }
    }


    if (!all(ready))
      return()
    else {
      # add models to the first row
      tempRow <- NULL
      tempRow[["iteration"]] <- 0
      for (h in 1:length(options[["models"]])) {
        tempData    <- list(
          nSuccesses = 0,
          nFailures  = 0
        )
        tempResults <- .estimateBinomialLS(tempData, options[["models"]][[h]])
        tempRow[[options[["models"]][[h]]$name]] <- tempResults$distribution
      }
      estimatesSequentialTable$addRows(tempRow)

      # then update the posteriors as the data go in
      if (length(data[["y"]]) > 0) {
        for (i in 1:length(data[["y"]])) {
          tempRow <- NULL
          tempRow[["iteration"]] <- i
          for (h in 1:length(options[["models"]])) {
            tempData    <- list(
              nSuccesses = sum(data[["y"]][1:i] == 1),
              nFailures  = sum(data[["y"]][1:i] == 0)
            )
            tempResults <- .estimateBinomialLS(tempData, options[["models"]][[h]])
            tempRow[[options[["models"]][[h]]$name]] <- tempResults$distribution
          }
          estimatesSequentialTable$addRows(tempRow)
        }
      }
    }
  }

  return()
}
.plotsSimpleBinomialLS             <- function(jaspResults, data, ready, options, type = c("Prior", "Posterior")) {

  containerPlots <- .containerPlotsLS(jaspResults, options, "binEst", type)

  if (is.null(containerPlots[[paste0("plots",type)]])) {

    plotsSimple <- createJaspPlot(
      width  = if (options[[ifelse (type == "Prior", "priorDistributionPlotType", "posteriorDistributionPlotType")]] == "overlying") 700 else 530,
      height = 400)

    plotsSimple$position <- 2
    plotsSimple$dependOn(c(
      .dataDependenciesBinomialLS,
      ifelse (options[[ifelse (type == "Prior", "priorDistributionPlotType", "posteriorDistributionPlotType")]] == "overlying", "colorPalette", "")))

    containerPlots[[paste0("plots",type)]] <- plotsSimple

    if ((type == "Posterior" && !ready["data"]) || !ready["models"])
      return()

    if (type == "Prior")
      data <- list(
        nSuccesses = 0,
        nFailures  = 0
      )

    allLines  <- c()
    allArrows <- c()
    legend    <- NULL
    for (i in 1:length(options[["models"]])) {

      if (options[["models"]][[i]]$type == "spike") {

        dfArrowPP   <- .dataArrowBinomialLS(options[["models"]][[i]])
        dfArrowPP$g <- options[["models"]][[i]]$name

        allArrows   <- c(allArrows, list(dfArrowPP))
        legend      <- rbind(legend, c(options[["models"]][[i]]$type, options[["models"]][[i]]$name))

      } else if (options[["models"]][[i]]$type == "beta") {

        dfLinesPP   <- .dataLinesBinomialLS(data, options[["models"]][[i]])
        dfLinesPP   <- dfLinesPP[dfLinesPP$g == type,]
        dfLinesPP$g <- options[["models"]][[i]]$name

        allLines    <- c(allLines, list(dfLinesPP))
        legend      <- rbind(legend, c(options[["models"]][[i]]$type, options[["models"]][[i]]$name))

      }
    }

    xName  <- bquote(.(gettext("Population proportion"))~theta)

    if (options[[ifelse (type == "Prior", "priorDistributionPlotType", "posteriorDistributionPlotType")]] == "overlying")
      p <- .plotOverlyingLS(allLines, allArrows, xName = xName, palette = options[["colorPalette"]])
    else
      p <- .plotStackedLS(allLines, allArrows, legend, xName = xName)

    plotsSimple$plotObject <- p
  }

  return()
}
.plotsIndividualBinomialLS         <- function(jaspResults, data, ready, options, type = c("Prior", "Posterior")) {

  containerPlots <- .containerPlotsLS(jaspResults, options, "binEst", type)

  if (is.null(containerPlots[[paste0("plots",type)]])) {

    plotsIndividual <- createJaspContainer()

    plotsIndividual$position <- 2
    if (type == "Prior") {
      dependencies <- c("priorDistributionPlotIndividualPointEstimate", "priorDistributionPlotIndividualPointEstimateType", "priorDistributionPlotIndividualCi",
        "priorDistributionPlotIndividualCiType", "priorDistributionPlotIndividualCiMass", "priorDistributionPlotIndividualCiLower", "priorDistributionPlotIndividualCiUpper")
    } else if (type == "Posterior") {
      dependencies <- c("posteriorDistributionPlotIndividualPointEstimate", "posteriorDistributionPlotIndividualPointEstimateType", "posteriorDistributionPlotIndividualCi",
        "posteriorDistributionPlotIndividualCiType", "posteriorDistributionPlotIndividualCiMass", "posteriorDistributionPlotIndividualCiLower", "posteriorDistributionPlotIndividualCiUpper",
        "posteriorDistributionPlotIndividualCiBf", "posteriorDistributionPloPriorDistribution", "posteriorDistributionPlotObservedProportion"
        )
    }
    plotsIndividual$dependOn(c(dependencies, .dataDependenciesBinomialLS))

    containerPlots[[paste0("plots",type)]] <- plotsIndividual

    if ((type == "Posterior" && !ready["data"]) || !ready["models"]) {

      plotsIndividual[[""]] <- createJaspPlot(title = "", width = 530, height = 400, aspectRatio = 0.7)
      return()

    } else if ((type == "Posterior" && !ready["data"]) || (type == "Posterior" && !ready["models"])) {

      for (i in 1:length(options[["models"]])) {
        plotsIndividual[[options[["models"]][[i]]$name]] <- createJaspPlot(title = options[["models"]][[i]]$name,
                                                                           width = 530, height = 400, aspectRatio = 0.7)
      }
      return()

    } else {

      if (type == "Prior")
        tempData <- list(
          nSuccesses = 0,
          nFailures  = 0
        )
      else if (type == "Posterior")
        tempData <- data



      for (i in 1:length(options[["models"]])) {

        tempPlot <- createJaspPlot(title = options[["models"]][[i]]$name, width = if (type == "Posterior" && (options[["posteriorDistributionPloPriorDistribution"]] || options[["posteriorDistributionPlotObservedProportion"]])) { 700 } else { 530 }, height = 400)

        plotsIndividual[[options[["models"]][[i]]$name]] <- tempPlot

        xName  <- bquote(.(gettext("Population proportion"))~theta)

        dfArrowPP   <- NULL
        dfLinesPP   <- NULL
        dfCI        <- NULL
        dfCILinesPP <- NULL

        if (options[[ifelse (type == "Prior", "priorDistributionPlotIndividualCi", "posteriorDistributionPlotIndividualCi")]]) {

          if (options[[ifelse (type == "Prior", "priorDistributionPlotIndividualCiType", "posteriorDistributionPlotIndividualCiType")]] == "central") {

            dfCI <- .dataCentralBinomialLS(
              tempData,
              options[["models"]][[i]],
              options[[ifelse (type == "Prior", "priorDistributionPlotIndividualCiMass", "posteriorDistributionPlotIndividualCiMass")]],
              type = "parameter"
            )

          } else if (options[[ifelse (type == "Prior", "priorDistributionPlotIndividualCiType", "posteriorDistributionPlotIndividualCiType")]] == "HPD") {

            dfCI <- .dataHPDBinomialLS(
              tempData,
              options[["models"]][[i]],
              options[[ifelse (type == "Prior", "priorDistributionPlotIndividualCiMass", "posteriorDistributionPlotIndividualCiMass")]],
              type = "parameter"
            )

          } else if (options[[ifelse (type == "Prior", "priorDistributionPlotIndividualCiType", "posteriorDistributionPlotIndividualCiType")]] == "custom") {

            dfCI <- .dataCustomBinomialLS(
              tempData, options[["models"]][[i]],
              options[[ifelse (type == "Prior", "priorDistributionPlotIndividualCiLower", "posteriorDistributionPlotIndividualCiLower")]],
              options[[ifelse (type == "Prior", "priorDistributionPlotIndividualCiUpper", "posteriorDistributionPlotIndividualCiUpper")]],
              type = "parameter"
            )

          } else if (options[["posteriorDistributionPlotIndividualCiType"]] == "support") {

            dfCI <- .dataSupportBinomialLS(
              tempData,
              options[["models"]][[i]],
              options[["posteriorDistributionPlotIndividualCiBf"]]
            )

          }
        }


        if (options[["models"]][[i]]$type == "spike") {
          dfArrowPP  <- .dataArrowBinomialLS(options[["models"]][[i]])
          if (type == "Posterior" && options[["posteriorDistributionPloPriorDistribution"]]) {
            dfArrowPP$g <- gettext("Prior = Posterior")
          } else
            dfArrowPP$g <- type
        } else if (options[["models"]][[i]]$type == "beta") {

          dfLinesPP  <- .dataLinesBinomialLS(tempData, options[["models"]][[i]])

          if (type == "Posterior" && options[["posteriorDistributionPloPriorDistribution"]]) {
            if (all(dfLinesPP$y[dfLinesPP$g == "Prior"] == dfLinesPP$y[dfLinesPP$g == "Posterior"])) {
              dfLinesPP   <- dfLinesPP[dfLinesPP$g == "Posterior",]
              dfLinesPP$g <- gettext("Prior = Posterior")
            }
          } else
            dfLinesPP  <- dfLinesPP[dfLinesPP$g == type,]


          if (!is.null(dfCI)) {
            for (r in 1:nrow(dfCI)) {
              tempCILinesPP   <- dfLinesPP[dfLinesPP$x >= dfCI$xStart[r] & dfLinesPP$x <= dfCI$xEnd[r] & dfLinesPP$g %in% c(type, "Prior = Posterior"),]
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

        if (options[[ifelse (type == "Prior", "priorDistributionPlotIndividualPointEstimate", "posteriorDistributionPlotIndividualPointEstimate")]]) {
          dfPointEstimate <- .estimateDataPointBinomial(tempData, options[["models"]][[i]], N = NULL, type = "parameter",
                                                        estimate = options[[ifelse (type == "Prior", "priorDistributionPlotIndividualPointEstimateType", "posteriorDistributionPlotIndividualPointEstimateType")]])
        } else
          dfPointEstimate <- NULL

        if (type == "Posterior" && options[["posteriorDistributionPlotObservedProportion"]]) {
          dfPointsPP <- .dataProportionBinomialLS(tempData)
          if (is.nan(dfPointsPP$x)) dfPointsPP <- NULL
        } else
          dfPointsPP <- NULL

        p <- .plotIndividualLS(allLines = dfLinesPP, allArrows = dfArrowPP,
                               pointEstimate = dfPointEstimate, CI = dfCI, CIallLines = dfCILinesPP,
                               xRange = c(0,1), xName = xName,
                               dfPoints = dfPointsPP, nRound = 3,
                               showLegend = (type == "Posterior" && (options[["posteriorDistributionPlotObservedProportion"]] || options[["posteriorDistributionPloPriorDistribution"]])))
        tempPlot$plotObject <- p
      }
    }

  }

  return()
}
.plotsBothBinomialLS               <- function(jaspResults, data, ready, options) {

  containerBoth <- .containerPlotsBothLS(jaspResults, options, "binEst")

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

        if (options[["models"]][[i]]$type == "spike") {
          dfArrowPP  <- .dataArrowBinomialLS(options[["models"]][[i]])
        } else if (options[["models"]][[i]]$type == "beta") {
          dfLinesPP  <- .dataLinesBinomialLS(data, options[["models"]][[i]])

          if (all(dfLinesPP$y[dfLinesPP$g == "Prior"] == dfLinesPP$y[dfLinesPP$g == "Posterior"])) {
            dfLinesPP   <- dfLinesPP[dfLinesPP$g == "Posterior",]
            dfLinesPP$g <- gettext("Prior = Posterior")
          }

        }

        if (options[["priorAndPosteriorDistributionPlotObservedProportion"]]) {
          dfPointsPP <- .dataProportionBinomialLS(data)
          if (is.nan(dfPointsPP$x))dfPointsPP <- NULL
        } else
          dfPointsPP <- NULL

        p <- .plotPriorPosteriorLS(list(dfLinesPP), list(dfArrowPP), dfPoints = dfPointsPP, xName = xName)
        tempPlot$plotObject <- p
      }
    }
  }

  return()
}
.plotsIterativeOverlyingBinomialLS <- function(jaspResults, data, ready, options) {

  containerIterative <- .containerSequentialOverlyingLS(jaspResults, options, "binEst")

  if (is.null(containerIterative[["plotsIterative"]])) {

    plotsIterative <- createJaspPlot(width = 700, height = 400)

    plotsIterative$position <- 2
    plotsIterative$dependOn(c(.dataDependenciesBinomialLS, "sequentialAnalysisPointEstimatePlotType",
                              "sequentialAnalysisPointEstimatePlotCi", "sequentialAnalysisPointEstimatePlotCiMass", "sequentialAnalysisPointEstimatePlotCiType", "sequentialAnalysisPointEstimatePlotCiBf",
                              "colorPalette"))
    containerIterative[["plotsIterative"]] <- plotsIterative

    if (!all(ready))
      return()

    plotDataLines <- list()
    plotDataCI    <- list()

    # then update the posteriors as the data go in
    for (h in 1:length(options[["models"]])) {

      tempLines   <- NULL
      tempCI      <- NULL
      # for dealing with possible bimodal distributions from HPD
      CIunimodal  <- TRUE
      tempCI1     <- NULL
      tempCI2     <- NULL

      # cheat for getting 2x 0 for the sequantial plot in case of no data
      if (length(data[["y"]]) == 0)
        iterSeq <- c(0, 0.1)
      else
        iterSeq <- 0:length(data[["y"]])

      for (i in iterSeq) {

        tempData    <- list(
          nSuccesses = sum(data[["y"]][0:i] == 1),
          nFailures  = sum(data[["y"]][0:i] == 0)
        )

        tempResults    <- .estimateBinomialLS(tempData, options[["models"]][[h]])
        tempLines      <- rbind(tempLines, data.frame(
          y    = as.numeric(tempResults[[options[["sequentialAnalysisPointEstimatePlotType"]]]]),
          x    = i,
          name = options[["models"]][[h]]$name
        ))

        if (options[["sequentialAnalysisPointEstimatePlotCi"]]) {

          if (options[["sequentialAnalysisPointEstimatePlotCiType"]] == "central") {
            tempCIPP <- .dataCentralBinomialLS(tempData, options[["models"]][[h]],
                                                options[["sequentialAnalysisPointEstimatePlotCiMass"]], type = "parameter")
          } else if (options[["sequentialAnalysisPointEstimatePlotCiType"]] == "HPD") {

            tempCIPP <- .dataHPDBinomialLS(tempData, options[["models"]][[h]],
                                            options[["sequentialAnalysisPointEstimatePlotCiMass"]], type = "parameter")
            if (nrow(tempCIPP) == 2)CIunimodal <- FALSE

          } else if (options[["sequentialAnalysisPointEstimatePlotCiType"]] == "support") {

            tempCIPP <- .dataSupportBinomialLS(tempData, options[["models"]][[h]],
                                               options[["sequentialAnalysisPointEstimatePlotCiBf"]])
            if (nrow(tempCIPP) == 0)tempCIPP <- NULL

          }

          if (nrow(tempCIPP) == 1 && CIunimodal) {

            tempCI <- rbind(tempCI, data.frame(
              y1   = tempCIPP$xStart,
              y2   = tempCIPP$xEnd,
              x    = i,
              name = options[["models"]][[h]]$name
            ))

          } else if (nrow(tempCIPP) == 1 && !CIunimodal) {

            tempCI <- rbind(
              tempCI,
              data.frame(
                y1   = (tempCIPP$xStart + tempCIPP$xEnd)/2,
                y2   = (tempCIPP$xStart + tempCIPP$xEnd)/2,
                x    = i,
                name = tempCI1$name
              ),
              data.frame(
                y1   = c(tempCI1$y2, tempCI1$y1),
                y2   = c(tempCI2$y1, tempCI2$y2),
                x    = rep(tempCI1$x, 2),
                name = rep(tempCI1$name, 2)
              ),
              data.frame(
                y1   = tempCIPP$xStart,
                y2   = tempCIPP$xEnd,
                x    = i,
                name = options[["models"]][[h]]$name
              )
            )
            CIunimodal <- TRUE

          } else if (nrow(tempCIPP) == 2) {

            tempCI1 <- rbind(
              tempCI1,
              data.frame(
                y1   = tempCIPP$xStart[1],
                y2   = tempCIPP$xEnd[1],
                x    = i,
                name = options[["models"]][[h]]$name
              ))

            tempCI2 <- rbind(
              tempCI2,
              data.frame(
                y1   = tempCIPP$xStart[2],
                y2   = tempCIPP$xEnd[2],
                x    = i,
                name = options[["models"]][[h]]$name
              ))

          } else if (nrow(tempCIPP) > 2)
            .quitAnalysis(gettext("More than bimodal CIs are not implemented in the Sequential analysis plot."))
        }

      }

      plotDataLines <- c(plotDataLines, list(tempLines))

      # deal with a possibility of two disjoined CIs
      if (options[["sequentialAnalysisPointEstimatePlotCi"]]) {
        if (CIunimodal) {
          # deal with possible non-existing support intervals
          if (all(is.na(tempCI[,c("y1", "y2")])))
            plotDataCI    <- c(plotDataCI, list(NULL))
          else
            plotDataCI    <- c(plotDataCI, list(tempCI))
        } else
          plotDataCI    <- c(plotDataCI, list(tempCI1), list(tempCI2))
      }

    }

    yName  <- bquote(.(gettext("Population proportion"))~~theta)
    xName  <- gettext("Observation")

    p <- .plotIterativeLS(plotDataLines, plotDataCI, xName = xName, yName = yName, palette = options[["colorPalette"]])

    plotsIterative$plotObject <- p

  }

  return()
}
.plotsIterativeStackedBinomialLS   <- function(jaspResults, data, ready, options) {

  containerIterative <- .containerSequentialStackedLS(jaspResults, options, "binEst")

  if (is.null(containerIterative[["plotsIterative"]])) {
    plotsIterative <- createJaspContainer()

    plotsIterative$position <- 2
    plotsIterative$dependOn(.dataDependenciesBinomialLS)

    containerIterative[["plotsIterative"]] <- plotsIterative


    if (all(!ready) || (ready["data"] && !ready["models"])) {

      plotsIterative[[""]] <- createJaspPlot(title = "", width = 530, height = 400, aspectRatio = 0.7)
      return()

    } else if (!ready["data"] && ready["models"]) {

      for (i in 1:length(options[["models"]])) {
        plotsIterative[[options[["models"]][[i]]$name]] <- createJaspPlot(title = options[["models"]][[i]]$name,
                                                                          width = 530, height = 400, aspectRatio = 0.7)
      }
      return()

    } else {

      #options[["models"]][[i]]$name

      for (i in 1:length(options[["models"]])) {

        tempPlot <- createJaspPlot(title = options[["models"]][[i]]$name, width = 530, height = 400, aspectRatio = 0.7)

        plotsIterative[[options[["models"]][[i]]$name]] <- tempPlot

        allLines  <- c()
        allArrows <- c()
        legend    <- NULL

        # too many iterations crashes JASP
        if (length(data[["y"]]) > 10)
          iterSequence <- round(seq(0, length(data[["y"]]), length.out = 10))
        else
          iterSequence <- 0:length(data[["y"]])

        iterSequence <- rev(iterSequence)

        for (iteration in iterSequence) {

          if (options[["models"]][[i]]$type == "spike") {

            dfArrowPP   <- .dataArrowBinomialLS(options[["models"]][[i]])
            dfArrowPP$g <- as.character(iteration)

            allArrows  <- c(allArrows, list(dfArrowPP))
            legend      <- rbind(legend, c(options[["models"]][[i]]$type, iteration))

          } else if (options[["models"]][[i]]$type == "beta") {

            tempData <- list(
              "nSuccesses" = sum(data[["y"]][0:iteration] == 1),
              "nFailures"  = sum(data[["y"]][0:iteration] == 0)
            )

            dfLinesPP   <- .dataLinesBinomialLS(tempData, options[["models"]][[i]])
            dfLinesPP   <- dfLinesPP[dfLinesPP$g == "Posterior",]
            dfLinesPP$g <- as.character(iteration)

            allLines    <- c(allLines, list(dfLinesPP))
            legend      <- rbind(legend, c(options[["models"]][[i]]$type, iteration))

          }

        }

        xName  <- bquote(.(gettext("Population proportion"))~theta)

        tempPlot$plotObject <- .plotStackedLS(allLines, allArrows, legend, xName = xName)
      }
    }
  }

  return()
}
.plotsIterativeIntervalOverlyingBinomialLS <- function(jaspResults, data, ready, options) {

  containerIterativeInterval <- .containerSequentialIntervalLS(jaspResults, options, "binEst")

  if (is.null(containerIterativeInterval[["sequentialAnalysisIntervalEstimatePlot"]])) {

    plotsIterativeInterval <- createJaspPlot(width = 700, height = 400)

    plotsIterativeInterval$position <- 2
    plotsIterativeInterval$dependOn(c(.dataDependenciesBinomialLS,
                                      "sequentialAnalysisIntervalEstimatePlotLower", "sequentialAnalysisIntervalEstimatePlotUpper", "colorPalette"))
    containerIterativeInterval[["sequentialAnalysisIntervalEstimatePlot"]] <- plotsIterativeInterval

    if (!all(ready))
      return()


    plotDataLines <- list()

    # update the posteriors as the data go in
    for (h in 1:length(options[["models"]])) {

      tempLines   <- NULL

      # cheat for getting 2x 0 for the sequantial plot in case of no data
      if (length(data[["y"]]) == 0)
        iterSeq <- c(0, 0.1)
      else
        iterSeq <- 0:length(data[["y"]])


      for (i in iterSeq) {

        tempData    <- list(
          nSuccesses = sum(data[["y"]][0:i] == 1),
          nFailures  = sum(data[["y"]][0:i] == 0)
        )

        tempResults    <- .dataCustomBinomialLS(tempData, options[["models"]][[h]],
                                                 lCI = options[["sequentialAnalysisIntervalEstimatePlotLower"]],
                                                 uCI = options[["sequentialAnalysisIntervalEstimatePlotUpper"]],
                                                 type = c("parameter"))

        tempLines      <- rbind(tempLines, data.frame(
          y    = tempResults$coverage,
          x    = i,
          name = options[["models"]][[h]]$name
        ))

      }

      plotDataLines <- c(plotDataLines, list(tempLines))

    }

    yName  <- bquote("P("~{.(options[["sequentialAnalysisIntervalEstimatePlotLower"]])<=theta}<=.(options[["sequentialAnalysisIntervalEstimatePlotUpper"]])~")")
    xName  <- gettext("Observation")

    p <- .plotIterativeLS(plotDataLines, allCI = NULL, xName = xName, yName = yName, palette = options[["colorPalette"]])

    plotsIterativeInterval$plotObject <- p
  }

  return()
}
.plotsIterativeIntervalStackedBinomialLS   <- function(jaspResults, data, ready, options) {

  containerIterativeInterval <- .containerSequentialIntervalLS(jaspResults, options, "binEst")

  if (is.null(containerIterativeInterval[["sequentialAnalysisIntervalEstimatePlot"]])) {

    plotsIterativeInterval <- createJaspContainer()

    plotsIterativeInterval$position <- 2
    plotsIterativeInterval$dependOn(c(.dataDependenciesBinomialLS,
                                      "sequentialAnalysisIntervalEstimatePlotLower", "sequentialAnalysisIntervalEstimatePlotUpper", "colorPalette"))


    containerIterativeInterval[["sequentialAnalysisIntervalEstimatePlot"]] <- plotsIterativeInterval


    if (all(!ready) || (ready["data"] && !ready["models"])) {

      plotsIterativeInterval[[""]] <- createJaspPlot(title = "", width = 530, height = 400, aspectRatio = 0.7)
      return()

    } else if (!ready["data"] && ready["models"]) {

      for (i in 1:length(options[["models"]])) {
        plotsIterativeInterval[[options[["models"]][[i]]$name]] <- createJaspPlot(title = options[["models"]][[i]]$name,
                                                                                  width = 530, height = 400, aspectRatio = 0.7)
      }
      return()

    } else {

      #options[["models"]][[i]]$name

      for (i in 1:length(options[["models"]])) {

        tempPlot <- createJaspPlot(title = options[["models"]][[i]]$name, width = 530, height = 400, aspectRatio = 0.7)

        plotsIterativeInterval[[options[["models"]][[i]]$name]] <- tempPlot

        allLines  <- c()
        allArrows <- c()
        legend     <- NULL

        # too many iterations crashes JASP
        if (length(data[["y"]]) > 10)
          iterSequence <- round(seq(0, length(data[["y"]]), length.out = 10))
        else
          iterSequence <- 0:length(data[["y"]])

        iterSequence <- rev(iterSequence)

        for (iteration in iterSequence) {

          if (options[["models"]][[i]]$type == "spike") {

            dfArrowPP   <- .dataArrowBinomialLS(options[["models"]][[i]])
            dfArrowPP$g <- as.character(iteration)

            allArrows   <- c(allArrows, list(dfArrowPP))
            legend      <- rbind(legend, c(options[["models"]][[i]]$type, iteration))

          } else if (options[["models"]][[i]]$type == "beta") {

            tempData <- list(
              "nSuccesses" = sum(data[["y"]][0:iteration] == 1),
              "nFailures"  = sum(data[["y"]][0:iteration] == 0)
            )

            dfLinesPP   <- .dataLinesBinomialLS(tempData, options[["models"]][[i]])
            dfLinesPP   <- dfLinesPP[dfLinesPP$g == "Posterior",]
            dfLinesPP$g <- as.character(iteration)

            allLines    <- c(allLines, list(dfLinesPP))
            legend      <- rbind(legend, c(options[["models"]][[i]]$type, iteration))

          }

        }

        xName  <- bquote(.(gettext("Population proportion"))~theta)

        tempPlot$plotObject <- .plotStackedLS(allLines, allArrows, legend, xName = xName,
                                               lCI = options[["sequentialAnalysisIntervalEstimatePlotLower"]], uCI = options[["sequentialAnalysisIntervalEstimatePlotUpper"]])
      }
    }
  }

  return()
}
.tableIterativeBinomialLS          <- function(jaspResults, data, ready, options) {

  containerIterative <- .containerSequentialOverlyingLS(jaspResults, options, "binEst")

  if (is.null(containerIterative[["tableIterative"]])) {
    tableIterative <- createJaspTable()

    tableIterative$position <- 3
    tableIterative$dependOn(c(.dataDependenciesBinomialLS, "sequentialAnalysisPointEstimatePlotType",
                              "sequentialAnalysisPointEstimatePlotCi", "sequentialAnalysisPointEstimatePlotCiMass", "sequentialAnalysisPointEstimatePlotCiType", "sequentialAnalysisPointEstimatePlotCiBf",
                              "colorPalette", "sequentialAnalysisPointEstimatePlotUpdatingTable"))
    containerIterative[["tableIterative"]] <- tableIterative

    tableIterative$addColumnInfo(name = "iteration", title = gettext("Observation"), type = "integer")
    if (ready["models"]) {
      if (options[["sequentialAnalysisPointEstimatePlotCi"]]) {
        if (options[["sequentialAnalysisPointEstimatePlotCiType"]] == "central")
          CItitle <- gettextf("%i%% CI", options[["sequentialAnalysisPointEstimatePlotCiMass"]]*100)
        else if (options[["sequentialAnalysisPointEstimatePlotCiType"]] == "HPD")
          CItitle <- gettextf("%i%% HPD", options[["sequentialAnalysisPointEstimatePlotCiMass"]]*100)
        else if (options[["sequentialAnalysisPointEstimatePlotCiType"]] == "support")
          CItitle <- gettextf("SI (BF=%s)", options[["sequentialAnalysisPointEstimatePlotCiBf"]])

        for (i in 1:length(options[["models"]])) {
          tableIterative$addColumnInfo(
            name      = paste(options[["models"]][[i]]$name,"center", sep = "_"),
            title     = .estimateTextLS(options[["sequentialAnalysisPointEstimatePlotType"]]),
            overtitle = options[["models"]][[i]]$name,
            type      = "number")
          tableIterative$addColumnInfo(
            name      = paste(options[["models"]][[i]]$name,"CI", sep = "_"),
            title     = CItitle,
            overtitle = options[["models"]][[i]]$name,
            type      = "string")
        }
      } else {
        for (i in 1:length(options[["models"]])) {
          tableIterative$addColumnInfo(
            name  = paste(options[["models"]][[i]]$name,"center", sep = "_"),
            title = options[["models"]][[i]]$name,
            type = "number")
        }
      }
    }

    if (!all(ready))
      return()

    iterSeq <- 0:length(data[["y"]])

    for (i in iterSeq) {

      tempRow     <- list()
      tempRow[["iteration"]] <- i

      tempData    <- list(
        nSuccesses = sum(data[["y"]][0:i] == 1),
        nFailures  = sum(data[["y"]][0:i] == 0)
      )

      for (h in 1:length(options[["models"]])) {

        tempResults <- .estimateBinomialLS(tempData, options[["models"]][[h]])
        tempRow[[paste(options[["models"]][[h]]$name,"center", sep = "_")]] <- tempResults[[options[["sequentialAnalysisPointEstimatePlotType"]]]]

        if (options[["sequentialAnalysisPointEstimatePlotCi"]]) {

          if (options[["sequentialAnalysisPointEstimatePlotCiType"]] == "central") {
            tempCIPP <- .dataCentralBinomialLS(tempData, options[["models"]][[h]],
                                                options[["sequentialAnalysisPointEstimatePlotCiMass"]], type = "parameter")
          } else if (options[["sequentialAnalysisPointEstimatePlotCiType"]] == "HPD") {
            tempCIPP <- .dataHPDBinomialLS(tempData, options[["models"]][[h]],
                                            options[["sequentialAnalysisPointEstimatePlotCiMass"]], type = "parameter")
          } else if (options[["sequentialAnalysisPointEstimatePlotCiType"]] == "support") {
            tempCIPP <- .dataSupportBinomialLS(tempData, options[["models"]][[h]],
                                               options[["sequentialAnalysisPointEstimatePlotCiBf"]])
          }

          if (all(is.na(tempCIPP[1:2]))) {
            tempInt <- "\u2205"
          } else {
            tempInt <- sapply(1:nrow(tempCIPP), function(i)paste(c(
              "[",format(round(tempCIPP$xStart[i], 3), nsmall = 3),", ",format(round(tempCIPP$xEnd[i], 3), nsmall = 3),"]"
            ), collapse = ""))
            tempInt <- paste(tempInt, collapse = " and " )

            tempRow[[paste(options[["models"]][[h]]$name,"CI", sep = "_")]] <- tempInt
          }

        }

      }

      tableIterative$addRows(tempRow)

    }
  }

  return()
}
.tableIterativeIntervalBinomialLS  <- function(jaspResults, data, ready, options) {

  containerIterativeInterval <- .containerSequentialIntervalLS(jaspResults, options, "binEst")

  if (is.null(containerIterativeInterval[["tableIterativeInterval"]])) {

    tableIterativeInterval <- createJaspTable()

    tableIterativeInterval$position <- 3
    tableIterativeInterval$dependOn(c(.dataDependenciesBinomialLS,
                                      "sequentialAnalysisIntervalEstimatePlotLower", "sequentialAnalysisIntervalEstimatePlotUpper", "sequentialAnalysisIntervalEstimatePlotUpdatingTable"))
    containerIterativeInterval[["tableIterativeInterval"]] <- tableIterativeInterval

    tableIterativeInterval$addColumnInfo(name = "iteration", title = gettext("Observation"), type = "integer")
    if (ready["models"]) {
      for (i in 1:length(options[["models"]])) {
        tableIterativeInterval$addColumnInfo(
          name  = paste(options[["models"]][[i]]$name,"center", sep = "_"),
          title = options[["models"]][[i]]$name,
          type = "number")
      }
    }

    if (!all(ready))
      return()

    iterSeq <- 0:length(data[["y"]])

    for (i in iterSeq) {

      tempRow     <- list()
      tempRow[["iteration"]] <- i

      tempData    <- list(
        nSuccesses = sum(data[["y"]][0:i] == 1),
        nFailures  = sum(data[["y"]][0:i] == 0)
      )

      for (h in 1:length(options[["models"]])) {

        tempResults    <- .dataCustomBinomialLS(tempData, options[["models"]][[h]],
                                                 lCI = options[["sequentialAnalysisIntervalEstimatePlotLower"]], uCI = options[["sequentialAnalysisIntervalEstimatePlotUpper"]],
                                                 type = c("parameter"))
        tempRow[[paste(options[["models"]][[h]]$name,"center", sep = "_")]] <- tempResults$coverage
      }

      tableIterativeInterval$addRows(tempRow)
    }
  }

  return()
}
.tablePredictionsBinomialLS        <- function(jaspResults, data, ready, options) {

  containerPredictions <- .containerPredictionsLS(jaspResults, options, "binEst")

  if (is.null(containerPredictions[["predictionsTable"]])) {

    predictionsTable <- createJaspTable()

    predictionsTable$position <- 2
    predictionsTable$dependOn(c(.dataDependenciesBinomialLS, "posteriorPredictionNumberOfFutureTrials"))

    estimateText <- .estimateTextLS(options[["posteriorPredictionSummaryTablePointEstimate"]])

    predictionsTable$addColumnInfo(name = "hypothesis",    title = gettext("Model"),                        type = "string")
    predictionsTable$addColumnInfo(name = "posterior",     title = gettextf("Posterior (%s)", "\u03B8"),    type = "string")
    predictionsTable$addColumnInfo(name = "posteriorEst",  title = gettextf("Posterior %s", estimateText),  type = "number")
    predictionsTable$addColumnInfo(name = "predictive",    title = gettext("Prediction (Successes)"),       type = "string")
    predictionsTable$addColumnInfo(name = "predictiveEst", title = gettextf("Prediction %s", estimateText), type = "number")
    predictionsTable$addColumnInfo(name = "predictiveSD",  title = gettextf("Prediction std. deviation"),   type = "number")

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

      # add rows for each hypothesis
      for (i in 1:length(options[["models"]])) {

        tempResults    <- .estimateBinomialLS(data, options[["models"]][[i]])
        tempPrediction <- .predictBinomialLS(data, options[["models"]][[i]], options)

        tempRow <- list(
          hypothesis     = options[["models"]][[i]][["name"]],
          posterior      = tempResults[["distribution"]],
          posteriorEst   = tempResults[[options[["posteriorPredictionSummaryTablePointEstimate"]]]],
          predictive     = tempPrediction[["distribution"]],
          predictiveEst  = tempPrediction[[options[["posteriorPredictionSummaryTablePointEstimate"]]]],
          predictiveSD   = tempPrediction[["SD"]]
        )

        predictionsTable$addRows(tempRow)
      }

      # add footnote clarifying what dataset was used
      predictionsTable$addFootnote(gettextf(
        "The prediction for %1$s %2$s is based on %3$s %4$s and %5$s %6$s.",
        options[["posteriorPredictionNumberOfFutureTrials"]], ifelse (options[["posteriorPredictionNumberOfFutureTrials"]] == 1, gettext("observation"), gettext("observations")),
        data[["nSuccesses"]], ifelse (data[["nSuccesses"]] == 1, gettext("success"), gettext("successes")),
        data[["nFailures"]],  ifelse (data[["nFailures"]] == 1,  gettext("failure"), gettext("failures"))
      ))
    }
  }

  return()
}
.plotsPredictionsIndividualBinomialLS      <- function(jaspResults, data, ready, options) {


  containerPredictionPlots <- .containerPredictionPlotsLS(jaspResults, options, "binEst")

  if (is.null(containerPredictionPlots[["posteriorPredictionDistributionPlot"]])) {

    plotsPredictions <- createJaspContainer()

    plotsPredictions$position <- 2
    plotsPredictions$dependOn(c(.dataDependenciesBinomialLS, "posteriorPredictionNumberOfFutureTrials",
                                "posteriorPredictionDistributionPlotIndividualCi", "posteriorPredictionDistributionPlotIndividualCiType",
                                "posteriorPredictionDistributionPlotIndividualPointEstimate", "posteriorPredictionDistributionPlotIndividualPointEstimateType",
                                "posteriorPredictionDistributionPlotIndividualCiMass", "posteriorPredictionDistributionPlotIndividualCiLower", "posteriorPredictionDistributionPlotIndividualCiUpper",
                                "posteriorPredictionDistributionPlotAsSampleProportion"))

    containerPredictionPlots[["posteriorPredictionDistributionPlot"]] <- plotsPredictions


    if (!ready["models"]) {

      plotsPredictions[[""]] <- createJaspPlot(title = "", width = 530, height = 400, aspectRatio = 0.7)
      return()

    } else {

      for (i in 1:length(options[["models"]])) {

        tempPlot <- createJaspPlot(title = options[["models"]][[i]]$name, width = 530, height = 400, aspectRatio = 0.7)

        plotsPredictions[[options[["models"]][[i]]$name]] <- tempPlot

        if (options[["posteriorPredictionDistributionPlotAsSampleProportion"]]) {
          xName  <- gettext("Predicted sample proportions")
          yName  <- gettext("Density")
          xRange <- c(-.5/options[["posteriorPredictionNumberOfFutureTrials"]],1 + .5/options[["posteriorPredictionNumberOfFutureTrials"]])
        } else {
          xName  <- gettext("Predicted number of successes")
          yName  <- gettext("Probability")
          xRange <- c(0, options[["posteriorPredictionNumberOfFutureTrials"]])
        }

        dfCI   <- NULL
        dfHist <- NULL

        if (options[["posteriorPredictionDistributionPlotIndividualCi"]]) {

          if (options[["posteriorPredictionDistributionPlotIndividualCiType"]] == "central") {

            dfCI <- .dataCentralBinomialLS(data, options[["models"]][[i]], options[["posteriorPredictionDistributionPlotIndividualCiMass"]],
                                           n = options[["posteriorPredictionNumberOfFutureTrials"]],type = "prediction")

          } else if (options[["posteriorPredictionDistributionPlotIndividualCiType"]] == "HPD") {

            dfCI <- .dataHPDBinomialLS(data, options[["models"]][[i]], options[["posteriorPredictionDistributionPlotIndividualCiMass"]],
                                       n = options[["posteriorPredictionNumberOfFutureTrials"]], type = "prediction")

          } else if (options[["posteriorPredictionDistributionPlotIndividualCiType"]] == "custom") {

            dfCI <- .dataCustomBinomialLS(data, options[["models"]][[i]],
                                          options[["posteriorPredictionDistributionPlotIndividualCiLower"]], options[["posteriorPredictionDistributionPlotIndividualCiUpper"]],
                                          n = options[["posteriorPredictionNumberOfFutureTrials"]], type = "prediction")

            if (options[["posteriorPredictionDistributionPlotIndividualCiUpper"]] > options[["posteriorPredictionNumberOfFutureTrials"]]) {

              plotsPredictionsIndividual[[options[["models"]][[i]]$name]]$setError(
                gettext("The upper CI limit is higher than the number of future observations. Please, change the value of the upper CI limit in the settings panel."))

              return()
            }
            if (options[["posteriorPredictionDistributionPlotIndividualCiLower"]] > options[["posteriorPredictionNumberOfFutureTrials"]]) {

              plotsPredictionsIndividual[[options[["models"]][[i]]$name]]$setError(gettext(
                "The lower CI limit is higher than the number of future observations. Please, change the value of the lower CI limit in the settings panel."))

              return()
            }
            if (options[["posteriorPredictionDistributionPlotIndividualCiLower"]] > options[["posteriorPredictionDistributionPlotIndividualCiUpper"]]) {

              plotsPredictionsIndividual[[options[["models"]][[i]]$name]]$setError(gettext(
                "The lower CI limit is higher than the upper CI limit. Please, change the value of the CI limits in the settings panel."))

              return()
            }
          }
        }

        dfHist  <- .dataHistBinomialLS(data, options[["models"]][[i]], options[["posteriorPredictionNumberOfFutureTrials"]])

        if (options[["posteriorPredictionDistributionPlotAsSampleProportion"]]) {
          dfHist$x <- dfHist$x/options[["posteriorPredictionNumberOfFutureTrials"]]
          if (options[["posteriorPredictionDistributionPlotIndividualCi"]]) {
            dfCI$xStart <- dfCI$xStart/options[["posteriorPredictionNumberOfFutureTrials"]]
            dfCI$xEnd   <- dfCI$xEnd  /options[["posteriorPredictionNumberOfFutureTrials"]]
          }
          nRound <- 3
        } else {
          nRound <- 0
        }


        if (options[["posteriorPredictionDistributionPlotIndividualPointEstimate"]]) {
          dfPointEstimate <- .estimateDataPointBinomial(data, options[["models"]][[i]], N = options[["posteriorPredictionNumberOfFutureTrials"]],
                                                        type = "prediction", estimate = options[["posteriorPredictionDistributionPlotIndividualPointEstimateType"]],
                                                        prop = options[["posteriorPredictionDistributionPlotAsSampleProportion"]])
        } else
          dfPointEstimate <- NULL

        p <- .plotPredictionLS(dfHist, dfPointEstimate, dfCI, xRange, xName, yName, nRound = nRound,
                               proportions = options[["posteriorPredictionDistributionPlotAsSampleProportion"]], predictionN = options[["posteriorPredictionNumberOfFutureTrials"]])
        tempPlot$plotObject <- p
      }

    }
  }

  return()
}
.plotsPredictionsBinomialLS        <- function(jaspResults, data, ready, options) {

  containerPredictionPlots <- .containerPredictionPlotsLS(jaspResults, options, "binEst")

  if (is.null(containerPredictionPlots[["posteriorPredictionDistributionPlot"]])) {

    plotsPredictions <- createJaspPlot(
      width  = if (options[["posteriorPredictionDistributionPlotType"]] == "overlying") 700 else 530,
      height = 400)

    plotsPredictions$position <- 2
    plotsPredictions$dependOn(c(.dataDependenciesBinomialLS, "posteriorPredictionNumberOfFutureTrials",
                                "colorPalette", "posteriorPredictionDistributionPlotAsSampleProportion"))

    containerPredictionPlots[["posteriorPredictionDistributionPlot"]] <- plotsPredictions


    if (!ready["models"])
      return()
    else {

      if (options[["posteriorPredictionDistributionPlotAsSampleProportion"]]) {
        xName  <- gettext("Predicted sample proportions")
        yName  <- gettext("Probability")
        xRange <- c(-.5/options[["posteriorPredictionNumberOfFutureTrials"]],1+.5/options[["posteriorPredictionNumberOfFutureTrials"]])
      } else {
        xName  <- gettext("Predicted number of successes")
        yName  <- gettext("Probability")
        xRange <- c(-.5, options[["posteriorPredictionNumberOfFutureTrials"]]+.5)
      }

      allLines  <- c()
      legend     <- NULL

      for (i in 1:length(options[["models"]])) {

        dfHist   <- .dataHistBinomialLS2(data, options[["models"]][[i]], options[["posteriorPredictionNumberOfFutureTrials"]])
        dfHist$g <- options[["models"]][[i]]$name

        if (options[["posteriorPredictionDistributionPlotAsSampleProportion"]]) {
          dfHist$x <- dfHist$x/options[["posteriorPredictionNumberOfFutureTrials"]]
        }

        # it's not beta, but I'm lazzy to rewrite a function I wanna use
        legend   <- rbind(legend, c("beta", options[["models"]][[i]]$name))
        allLines<- c(allLines, list(dfHist))
      }

      if (options[["posteriorPredictionDistributionPlotType"]] == "overlying") {
        p <- .plotOverlyingLS(allLines, NULL, xName = xName, yName = yName, xRange = xRange, discrete = TRUE,
                              palette = options[["colorPalette"]], proportions = options[["posteriorPredictionDistributionPlotAsSampleProportion"]])
      } else {
        p <- .plotStackedLS(allLines, NULL, legend, xName = xName, xRange = xRange,
                            discrete = TRUE, proportions = options[["posteriorPredictionDistributionPlotAsSampleProportion"]])
      }

      plotsPredictions$plotObject <- p
    }
  }

  return()
}
.tablePosteriorPredictions         <- function(jaspResults, data, ready, options) {

  containerPredictionPlots <- .containerPredictionPlotsLS(jaspResults, options, "binEst")

  if (is.null(containerPredictionPlots[["tablePredictions"]])) {

    tablePredictions <- createJaspTable()

    tablePredictions$position <- 3
    tablePredictions$dependOn(c(.dataDependenciesBinomialLS, "posteriorPredictionNumberOfFutureTrials", "posteriorPredictionDistributionPlotAsSampleProportion", "posteriorPredictionDistributionPlotPredictionsTable"))
    containerPredictionPlots[["tablePredictions"]] <- tablePredictions


    if (options[["posteriorPredictionDistributionPlotAsSampleProportion"]]) {
      tablePredictions$addColumnInfo(name = "successes", title = gettext("Proportion of Successes"), type = "number")
      tablePredictions$addColumns(c(0:options[["posteriorPredictionNumberOfFutureTrials"]])/options[["posteriorPredictionNumberOfFutureTrials"]])
    } else {
      tablePredictions$addColumnInfo(name = "successes", title = gettext("Successes"), type = "integer")
      tablePredictions$addColumns(0:options[["posteriorPredictionNumberOfFutureTrials"]])
    }


    if (ready["models"]) {
      for (i in seq_along(options[["models"]])) {
        tablePredictions$addColumnInfo(name = paste0("hyp_", i), title = options[["models"]][[i]]$name, type = "number")
      }
    } else
      return()


    if (!ready["data"]) {

      if ((options[["dataInputType"]] == "variable" && options[["dataVariableSelected"]]     != "") ||
          (options[["dataInputType"]] == "sequence" && options[["dataSequenceSequenceOfObservations"]]    != ""))
        tablePredictions$addFootnote(gettext("Please specify successes and failures."))

      return()
    }


    tempPred    <- NULL

    for (i in 1:length(options[["models"]])) {
      tempResults <- .dataHistBinomialLS2(data, options[["models"]][[i]], options[["posteriorPredictionNumberOfFutureTrials"]])
      tablePredictions$addColumns(tempResults[1:length(tempResults) %% 2 == 0,"y"])
    }

  }
  return()
}
