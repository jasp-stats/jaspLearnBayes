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

LSgaussianestimation   <- function(jaspResults, dataset, options, state = NULL) {

  options <- .parseAndStoreFormulaOptions(jaspResults, options, c("plotsPosteriorBF", "plotsIterativeBF"))

  # introductory text
  if (options[["introText"]]).introductoryTextLS(jaspResults, options, "gaussEst")

  # a vector of two, first for data, second for hypotheses
  ready <- .readyGaussianLS(options)

  # evaluate the expressions in priors
  if (ready["priors"])options[["priors"]] <- .evaluatePriors(options[["priors"]])

  # load, check, transform and process data
  if (ready["data"]) {
    data <- .readDataGaussianLS(dataset, options)
  } else{
    data <- NULL
  }


  # data summary table if requested (but not if the data counts were added directly)
  .summaryGaussianLS(jaspResults, data, options, "gaussEst")


  ### inference
  # estimated parameter values
  .estimatesGaussianLS(jaspResults, data, ready, options)

  # prior
  if (options[["plotsPrior"]]) {
    if (options[["plotsPriorType"]] != "individual").plotsSimpleGaussianLS(jaspResults, data, ready, options, type = "Prior")
    if (options[["plotsPriorType"]] == "individual").plotsIndividualGaussianLS(jaspResults, data, ready, options, type = "Prior")
  }

  # posterior
  if (options[["plotsPosterior"]]) {
    if (options[["plotsPosteriorType"]] != "individual").plotsSimpleGaussianLS(jaspResults, data, ready, options, type = "Posterior")
    if (options[["plotsPosteriorType"]] == "individual").plotsIndividualGaussianLS(jaspResults, data, ready, options, type = "Posterior")
  }


  ### sequential analysis
  # point estimate
  if (options[["plotsIterative"]]) {
    if (options[["plotsIterativeType"]] == "overlying").plotsIterativeOverlyingGaussianLS(jaspResults, data, ready, options)
    if (options[["plotsIterativeType"]] == "stacked").plotsIterativeStackedGaussianLS(jaspResults, data, ready, options)
  }

  # point estimate table
  if (options[["plotsIterative"]] && options[["plotsIterativeUpdatingTable"]]).tableIterativeGaussianLS(jaspResults, data, ready, options)

  # interval
  if (options[["plotsIterativeInterval"]]) {
    if (options[["plotsIterativeIntervalType"]] == "overlying").plotsIterativeIntervalOverlyingGaussianLS(jaspResults, data, ready, options)
    if (options[["plotsIterativeIntervalType"]] == "stacked").plotsIterativeIntervalStackedGaussianLS(jaspResults, data, ready, options)
  }

  # interval estimate table
  if (options[["plotsIterativeInterval"]] && options[["plotsIterativeIntervalUpdatingTable"]]).tableIterativeIntervalGaussianLS(jaspResults, data, ready, options)

  # posterior updating table
  if (options[["doIterative"]] && options[["dataInputType"]] != "counts").estimatesSequentialGaussianLS(jaspResults, data, ready, options)


  ### prediction
  if (options[["predictionTable"]]).tablepredictionsGaussianLS(jaspResults, data, ready, options)

  # plot
  if (options[["plotsPredictions"]]) {
    if (options[["predictionPlotType"]] != "individual").plotsPredictionsGaussianLS(jaspResults, data, ready, options)
    if (options[["predictionPlotType"]] == "individual").plotsPredictionsIndividualGaussianLS(jaspResults, data, ready, options)
  }

  return()
}


.estimatesGaussianLS               <- function(jaspResults, data, ready, options) {

  if (is.null(jaspResults[["estimatesContainer"]])) {
    estimatesContainer <- createJaspContainer("Model")
    estimatesContainer$position <- 2
    jaspResults[["estimatesContainer"]] <- estimatesContainer
  } else{
    estimatesContainer <- jaspResults[["estimatesContainer"]]
  }


  if (options[["introText"]] && is.null(estimatesContainer[['introText']])) {

    introText <- createJaspHtml()
    introText$dependOn("introText")
    introText$position <- 1

    introText[['text']] <- .explanatoryTextLS("estimates", options, "gaussEst")

    estimatesContainer[['introText']] <- introText
  }


  if (is.null(estimatesContainer[['estimatesTable']])) {

    estimatesTable <- createJaspTable(title = gettext("Estimation Summary"))

    estimatesTable$position <- 2
    estimatesTable$dependOn(.dataDependenciesGaussianLS)

    estimatesTable$addColumnInfo(name = "hypothesis",    title = gettext("Model"),           type = "string")
    estimatesTable$addColumnInfo(name = "prior",         title = gettext("Prior (μ)"),       type = "string")
    estimatesTable$addColumnInfo(name = "priorMean",     title = gettext("Prior Mean"),      type = "number")
    estimatesTable$addColumnInfo(name = "posterior",     title = gettext("Posterior (μ)"),   type = "string")
    estimatesTable$addColumnInfo(name = "posteriorMean", title = gettext("Posterior Mean"),  type = "number")

    estimatesTable$setExpectedSize(length(options[["priors"]]))

    estimatesContainer[["estimatesTable"]] <- estimatesTable

    if (!ready["priors"]) {

      return()

    } else{

      # add rows for each hypothesis
      for (i in 1:length(options[["priors"]])) {

        tempResults <- .estimateGaussianLS(NULL, options[["priors"]][[i]])

        tempRow <- list(
          prior         = tempResults$distribution,
          priorMean     = tempResults$mean,
          hypothesis    = options[["priors"]][[i]]$name,
          posterior     = "",
          posteriorMean = "")


        if (all(ready)) {
          # and when real data are supplied as well, add posterior information
          tempResults <- .estimateGaussianLS(data, options[["priors"]][[i]])

          tempRow["posterior"]     <- tempResults$distribution
          tempRow["posteriorMean"] <- tempResults$mean

        }

        estimatesTable$addRows(tempRow)
      }

    }
  }

  return()
}
.estimatesSequentialGaussianLS     <- function(jaspResults, data, ready, options) {

  containerIterativeUpdating <- .containerSequentialUpdatingLS(jaspResults, options, "bin_est")

  if (is.null(containerIterativeUpdating[["estimatesSequentialTable"]])) {

    estimatesSequentialTable <- createJaspTable()

    estimatesSequentialTable$position <- 2
    estimatesSequentialTable$dependOn(.dataDependenciesGaussianLS)

    estimatesSequentialTable$addColumnInfo(name = "iteration", title = gettext("Observation"), type = "integer")
    containerIterativeUpdating[["estimatesSequentialTable"]] <- estimatesSequentialTable


    estimatesSequentialTable$setExpectedSize(ifelse (ready["data"], length(data$y) + 1, 1))
    if (ready["priors"]) {
      for (i in 1:length(options[["priors"]])) {
        estimatesSequentialTable$addColumnInfo(
          name  = options[["priors"]][[i]]$name,
          title = options[["priors"]][[i]]$name,
          type = "string")
      }
    }


    if (!all(ready)) {
      return()
    } else{
      # add priors to the first row
      tempRow <- NULL
      tempRow[["iteration"]] <- 0
      for (h in 1:length(options[["priors"]])) {
        tempResults <- .estimateGaussianLS(NULL, options[["priors"]][[h]])
        tempRow[[options[["priors"]][[h]]$name]] <- tempResults$distribution
      }
      estimatesSequentialTable$addRows(tempRow)

      # then update the posteriors as the data go in
      if (length(data$y) > 0) {
        for (i in 1:length(data$y)) {
          tempRow <- NULL
          tempRow[["iteration"]] <- i
          for (h in 1:length(options[["priors"]])) {
            tempData    <- list(
              mean = mean(data$y[1:i]),
              N    = length(data$y[1:i]),
              SD   = data$SD
            )
            tempResults <- .estimateGaussianLS(tempData, options[["priors"]][[h]])
            tempRow[[options[["priors"]][[h]]$name]] <- tempResults$distribution
          }
          estimatesSequentialTable$addRows(tempRow)
        }
      }
    }
  }

  return()
}
.plotsSimpleGaussianLS             <- function(jaspResults, data, ready, options, type = c("Prior", "Posterior")) {

  containerPlots <- .containerPlotsLS(jaspResults, options, "gaussEst", type)

  if (is.null(containerPlots[[paste0("plots",type,"simple")]])) {

    plotsSimple <- createJaspPlot(
      width  = if (options[[ifelse (type == "Prior", "plotsPriorType", "plotsPosteriorType")]] == "overlying") 700 else 530,
      height = 400)

    plotsSimple$position <- 2
    plotsSimple$dependOn(c(.dataDependenciesGaussianLS,
                           ifelse (options[[ifelse (type == "Prior", "plotsPriorType", "plotsPosteriorType")]] == "overlying",
                                  "colorPalette", "")))

    containerPlots[[paste0("plots",type,"simple")]] <- plotsSimple

    if (!all(ready))
      return()

    allLines  <- c()
    allArrows <- c()
    legend     <- NULL
    range      <- .rangeGaussiansLS(if (type == "Prior") NULL else data, options[["priors"]])

    for (i in 1:length(options[["priors"]])) {

      if (options[["priors"]][[i]]$type == "spike") {

        dfArrowPP   <- .dataArrowGaussianLS(options[["priors"]][[i]])
        dfArrowPP$g <- options[["priors"]][[i]]$name

        allArrows  <- c(allArrows, list(dfArrowPP))
        legend      <- rbind(legend, c(options[["priors"]][[i]]$type, options[["priors"]][[i]]$name))

      } else if (options[["priors"]][[i]]$type == "normal") {

        dfLinesPP   <- .dataLinesGaussianLS(data, options[["priors"]][[i]], range = range)
        dfLinesPP   <- dfLinesPP[dfLinesPP$g == type,]
        dfLinesPP$g <- options[["priors"]][[i]]$name

        allLines   <- c(allLines, list(dfLinesPP))
        legend      <- rbind(legend, c(options[["priors"]][[i]]$type, options[["priors"]][[i]]$name))

      }
    }

    xName  <- bquote(.(gettext("Population mean"))~mu)

    if (options[[ifelse (type == "Prior", "plotsPriorType", "plotsPosteriorType")]] == "overlying") {
      p <- .plotOverlyingLS(allLines, allArrows, xName = xName, palette = options[["colorPalette"]], xRange = range)
    } else{
      p <- .plotStackedLS(allLines, allArrows, legend, xName = xName, xRange = range)
    }

    plotsSimple$plotObject <- p
  }

  return()
}
.plotsIndividualGaussianLS         <- function(jaspResults, data, ready, options, type = c("Prior", "Posterior")) {

  containerPlots <- .containerPlotsLS(jaspResults, options, "gaussEst", type)

  if (is.null(containerPlots[[paste0("plots",type,"individual")]])) {

    plotsIndividual <- createJaspContainer()

    plotsIndividual$position <- 2
    if (type == "Prior") {
      dependencies <- c("plotsPriorIndividualEstimate", "plotsPriorIndividualEstimateType", "plotsPriorIndividualCI",
        "plotsPriorIndividualType", "plotsPriorCoverage", "plotsPriorLower", "plotsPriorUpper")
    } else if (type == "Posterior") {
      dependencies <- c("plotsPosteriorIndividualEstimate", "plotsPosteriorIndividualEstimateType", "plotsPosteriorIndividualCI",
        "plotsPosteriorIndividualType", "plotsPosteriorCoverage", "plotsPosteriorLower", "plotsPosteriorUpper",
        "plotsPosteriorBF", "plotsPosteriorIndividualPrior", "plotsPosteriorIndividualProportion")
    }
    plotsIndividual$dependOn(c(dependencies, .dataDependenciesGaussianLS))

    containerPlots[[paste0("plots",type,"individual")]] <- plotsIndividual


    if (all(!ready) || (ready["data"] && !ready["priors"])) {

      plotsIndividual[[""]] <- createJaspPlot(title = "", width = 530, height = 400, aspectRatio = 0.7)
      return()

    } else if (!ready["data"] && ready["priors"]) {

      for (i in 1:length(options[["priors"]])) {
        plotsIndividual[[options[["priors"]][[i]]$name]] <- createJaspPlot(title = options[["priors"]][[i]]$name,
                                                                           width = 530, height = 400, aspectRatio = 0.7)
      }
      return()

    } else{

      if (type == "Prior") {
        tempData        <- data
        tempData[["N"]] <- 0
      } else{
        tempData        <- data
      }

      for (i in 1:length(options[["priors"]])) {

        tempPlot <- createJaspPlot(title = options[["priors"]][[i]]$name, width = if (type == "Posterior" && (options[["plotsPosteriorIndividualPrior"]] || options[["plotsPosteriorIndividualProportion"]])) { 700 } else{ 530 }, height = 400)

        plotsIndividual[[options[["priors"]][[i]]$name]] <- tempPlot

        xName  <- bquote(.(gettext("Population mean"))~mu)

        dfArrowPP   <- NULL
        dfLinesPP   <- NULL
        dfCI        <- NULL
        dfCILinesPP <- NULL

        if (type == "Posterior" && options[["plotsPosteriorIndividualCI"]] && options[["plotsPosteriorIndividualType"]] == "support") {
          range <- .rangeGaussianSupportLS(tempData, options[["priors"]][[i]], options[["plotsPosteriorBF"]])
        } else{
          range <- .rangeGaussianLS(if (type == "Prior") NULL else tempData, options[["priors"]][[i]])
        }

        if (options[[ifelse (type == "Prior", "plotsPriorIndividualCI", "plotsPosteriorIndividualCI")]]) {

          if (options[[ifelse (type == "Prior", "plotsPriorIndividualType", "plotsPosteriorIndividualType")]] %in% c("central", "HPD")) {

            dfCI <- .dataCentralGaussianLS(
              if (type == "Prior") NULL else tempData,
              options[["priors"]][[i]],
              options[[ifelse (type == "Prior", "plotsPriorCoverage", "plotsPosteriorCoverage")]],
              type = "parameter"
            )

            if (options[[ifelse (type == "Prior", "plotsPriorIndividualType", "plotsPosteriorIndividualType")]] == "HPD") {
              dfCI$g <- "HPD"
            }

          } else if (options[[ifelse (type == "Prior", "plotsPriorIndividualType", "plotsPosteriorIndividualType")]] == "custom") {

            dfCI <- .dataCustomGaussianLS(
              if (type == "Prior") NULL else tempData,
              options[["priors"]][[i]],
              options[[ifelse (type == "Prior", "plotsPriorLower", "plotsPosteriorLower")]],
              options[[ifelse (type == "Prior", "plotsPriorUpper", "plotsPosteriorUpper")]],
              NULL,
              type = "parameter"
            )

          } else if (options[["plotsPosteriorIndividualType"]] == "support") {

            dfCI <- .dataSupportGaussianLS(
              tempData,
              options[["priors"]][[i]],
              options[["plotsPosteriorBF"]]
            )

          }

          dfCI$parameter <- "mu"
        }


        if (options[["priors"]][[i]]$type == "spike") {
          dfArrowPP  <- .dataArrowGaussianLS(options[["priors"]][[i]])
          if (type == "Posterior" && options[["plotsPosteriorIndividualPrior"]]) {
            dfArrowPP$g <- "Prior = Posterior"
          } else
            dfArrowPP$g <- type
        } else if (options[["priors"]][[i]]$type == "normal") {

          dfLinesPP  <- .dataLinesGaussianLS(data, options[["priors"]][[i]], range = range)

          if (!is.null(dfCI)) {
            for (r in 1:nrow(dfCI)) {
              # wtf?
              tempCILinesPP   <- dfLinesPP[dfLinesPP$x >= dfCI$xStart[r] & dfLinesPP$x <= dfCI$xEnd[r] & dfLinesPP$g == type,]
              tempCILinesPP$g <- paste(c(as.character(dfCI$g), r), collapse = "")
              tempCILinesPP   <- rbind.data.frame(
                data.frame(x = dfCI$xStart[r], y = 0, g = tempCILinesPP$g[1]),
                tempCILinesPP,
                data.frame(x = dfCI$xEnd[r], y = 0, g = tempCILinesPP$g[1])
              )
              dfCILinesPP <- rbind.data.frame(dfCILinesPP, tempCILinesPP)
            }
          }

          if (type == "Posterior" && options[["plotsPosteriorIndividualPrior"]]) {
            if (all(dfLinesPP$y[dfLinesPP$g == "Prior"] == dfLinesPP$y[dfLinesPP$g == "Posterior"])) {
              dfLinesPP   <- dfLinesPP[dfLinesPP$g == "Posterior",]
              dfLinesPP$g <- "Prior = Posterior"
            }
          } else
            dfLinesPP  <- dfLinesPP[dfLinesPP$g == type,]



        }

        if (options[[ifelse (type == "Prior", "plotsPriorIndividualEstimate", "plotsPosteriorIndividualEstimate")]]) {
          dfPointEstimate <- .estimateDataPointGaussian(tempData, options[["priors"]][[i]], N = NULL, type = "parameter",
                                                        estimate = options[[ifelse (type == "Prior", "plotsPriorIndividualEstimateType", "plotsPosteriorIndividualEstimateType")]])
        } else
          dfPointEstimate <- NULL

        if (type == "Posterior" && options[["plotsPosteriorIndividualProportion"]]) {
          dfPointsPP <- data.frame(x = data[["mean"]], y = 0, g = "Sample Mean")
          if (is.nan(dfPointsPP$x))dfPointsPP <- NULL
        } else
          dfPointsPP <- NULL

        p <- .plotIndividualLS(allLines = dfLinesPP, allArrows = dfArrowPP,
                               pointEstimate = dfPointEstimate, CI = dfCI, CIallLines = dfCILinesPP,
                               xRange = range, xName = xName,
                               dfPoints = dfPointsPP, nRound = 3,
                               showLegend = (type == "Posterior" && (options[["plotsPosteriorIndividualProportion"]] || options[["plotsPosteriorIndividualPrior"]])))
        tempPlot$plotObject <- p
      }

      return()
    }

  }

  return()
}
.plotsBothGaussianLS               <- function(jaspResults, data, ready, options) {

  containerBoth <- .containerPlotsBothLS(jaspResults, options, "gaussEst")

  if (is.null(containerBoth[["plotsBoth"]])) {

    plotsBoth <- createJaspContainer()
    plotsBoth$position <- 2
    plotsBoth$dependOn(c(.dataDependenciesGaussianLS, "plotsBothSampleProportion"))

    containerBoth[["plotsBoth"]] <- plotsBoth

    if (all(!ready) || (ready["data"] && !ready["priors"])) {

      plotsBoth[[""]] <- createJaspPlot(title = "", width = 530, height = 400, aspectRatio = 0.7)
      return()

    } else if (!ready["data"] && ready["priors"]) {

      for (i in 1:length(options[["priors"]])) {
        plotsBoth[[options[["priors"]][[i]]$name]] <- createJaspPlot(title = options[["priors"]][[i]]$name,
                                                                     width = 530, height = 400, aspectRatio = 0.7)
      }
      return()

    } else{

      for (i in 1:length(options[["priors"]])) {

        tempPlot <- createJaspPlot(title = options[["priors"]][[i]]$name, width = 530, height = 400, aspectRatio = 0.7)

        plotsBoth[[options[["priors"]][[i]]$name]] <- tempPlot

        dfArrowPP <- NULL
        dfLinesPP <- NULL

        xName  <- bquote(.(gettext("Population mean"))~mu)
        range  <- .rangeGaussianLS(NULL, options[["priors"]][[i]])

        if (options[["priors"]][[i]]$type == "spike") {
          dfArrowPP  <- .dataArrowGaussianLS(options[["priors"]][[i]])
        } else if (options[["priors"]][[i]]$type == "normal") {
          dfLinesPP  <- .dataLinesGaussianLS(data, options[["priors"]][[i]], range = range)

          if (all(dfLinesPP$y[dfLinesPP$g == "Prior"] == dfLinesPP$y[dfLinesPP$g == "Posterior"])) {
            dfLinesPP   <- dfLinesPP[dfLinesPP$g == "Posterior",]
            dfLinesPP$g <- "Prior = Posterior"
          }

        }

        if (options[["plotsBothSampleProportion"]]) {
          dfPointsPP <- .dataObservedGaussianLS(data)
        } else{
          dfPointsPP <- NULL
        }

        p <- .plotPriorPosteriorLS(list(dfLinesPP), list(dfArrowPP), dfPoints = dfPointsPP, xName = xName, xRange = range)
        tempPlot$plotObject <- p
      }
    }
  }

  return()
}
.plotsIterativeOverlyingGaussianLS <- function(jaspResults, data, ready, options) {

  containerIterative <- .containerSequentialPointLS(jaspResults, options, "gaussEst")

  if (is.null(containerIterative[["plotsIterative"]])) {

    plotsIterative <- createJaspPlot(width = 530, height = 400, aspectRatio = 0.7)

    plotsIterative$position <- 2
    plotsIterative$dependOn(c(.dataDependenciesGaussianLS, "plotsIterativeEstimateType",
                              "plotsIterativeOverlyingCI", "plotsIterativeCoverage", "plotsIterativeOverlyingType", "plotsIterativeBF",
                              "colorPalette"))
    containerIterative[["plotsIterative"]] <- plotsIterative

    if (!all(ready)) {
      return()
    }

    plotDataLines <- list()
    plotDataCI    <- list()


    # cheat for getting 2x 0 for the sequential plot in case of no data
    if (length(data$y) == 0) {
      iterSeq <- c(0, 0.1)
    } else{
      iterSeq <- 0:length(data$y)
    }

    # get the plotting range
    range <- NULL
    for (i in iterSeq) {
      if (i < 1) {

        if (options[["plotsIterativeOverlyingCI"]] && options[["plotsIterativeOverlyingType"]] == "support") {
          range <- rbind(range, .rangeGaussiansSupportLS(NULL, options[["priors"]], options[["plotsIterativeBF"]]))
        } else{
          range <- rbind(range, .rangeGaussiansLS(NULL, options[["priors"]]))
        }

      } else{
        tempData <- list(
          mean = mean(data$y[1:i]),
          N    = length(data$y[1:i]),
          SD   = data$SD
        )

        if (options[["plotsIterativeOverlyingCI"]] && options[["plotsIterativeOverlyingType"]] == "support") {
          range <- rbind(range, .rangeGaussiansSupportLS(tempData, options[["priors"]], options[["plotsIterativeBF"]]))
        } else{
          range <- rbind(range, .rangeGaussiansLS(tempData, options[["priors"]]))
        }
      }
    }
    range <- c(min(range[,1]), max(range[,2]))

    # then update the posteriors as the data go in
    for (h in 1:length(options[["priors"]])) {

      tempLines   <- NULL
      tempCI      <- NULL

      for (i in iterSeq) {

        if (i < 1) {
          tempData <- NULL
        } else{
          tempData <- list(
            mean = mean(data$y[1:i]),
            N    = length(data$y[1:i]),
            SD   = data$SD
          )
        }

        tempResults    <- .estimateGaussianLS(tempData, options[["priors"]][[h]])
        tempLines      <- rbind(tempLines, data.frame(
          y    = tempResults[[options[["plotsIterativeEstimateType"]]]],
          x    = i,
          name = options[["priors"]][[h]]$name
        ))

        if (options[["plotsIterativeOverlyingCI"]]) {

          if (options[["plotsIterativeOverlyingType"]] %in% c("central", "HPD")) {

            tempCIPP <- .dataCentralGaussianLS(
              tempData,
              options[["priors"]][[h]],
              options[["plotsIterativeCoverage"]],
              type = "parameter"
            )

            if (options[["plotsIterativeOverlyingType"]] == "HPD") {
              tempCIPP$g <- "HPD"
            }

          } else if (options[["plotsIterativeOverlyingType"]] == "support") {

            tempCIPP <- .dataSupportGaussianLS(
              tempData,
              options[["priors"]][[h]],
              options[["plotsIterativeBF"]],
              range
            )

            if (nrow(tempCIPP) == 0)tempCIPP <- NULL

          }

          tempCI <- rbind(tempCI, data.frame(
            y1   = tempCIPP$xStart,
            y2   = tempCIPP$xEnd,
            x    = i,
            name = options[["priors"]][[h]]$name
          ))

        }

      }

      plotDataLines <- c(plotDataLines, list(tempLines))

      # deal with possible non-existing support intervals
      if (all(is.na(tempCI[,c("y1", "y2")]))) {
        plotDataCI    <- c(plotDataCI, list(NULL))
      } else{
        plotDataCI    <- c(plotDataCI, list(tempCI))
      }

    }

    yName  <- bquote(.(gettext("Population mean"))~~mu)
    xName  <- gettext("Observation")

    p <- .plotIterativeLS(plotDataLines, plotDataCI, xName = xName, yName = yName, palette = options[["colorPalette"]], yRange = range)

    plotsIterative$plotObject <- p
  }

  return()
}
.plotsIterativeStackedGaussianLS   <- function(jaspResults, data, ready, options) {

  containerIterative <- .containerSequentialPointLS(jaspResults, options, "gaussEst")

  if (is.null(containerIterative[["plotsIterative"]])) {

    plotsIterative <- createJaspContainer()

    plotsIterative$position <- 2
    plotsIterative$dependOn(.dataDependenciesGaussianLS)

    containerIterative[["plotsIterative"]] <- plotsIterative


    if (all(!ready) || (ready["data"] && !ready["priors"])) {

      plotsIterative[[""]] <- createJaspPlot(title = "", width = 530, height = 400, aspectRatio = 0.7)
      return()

    } else if (!ready["data"] && ready["priors"]) {

      for (i in 1:length(options[["priors"]])) {
        plotsIterative[[options[["priors"]][[i]]$name]] <- createJaspPlot(title = options[["priors"]][[i]]$name,
                                                                          width = 530, height = 400, aspectRatio = 0.7)
      }
      return()

    } else{

      # too many iterations crashes JASP
      if (length(data$y) > 10) {
        iterSequence <- round(seq(0, length(data$y), length.out = 10))
      } else{
        iterSequence <- 0:length(data$y)
      }
      iterSequence <- rev(iterSequence)


      for (i in 1:length(options[["priors"]])) {

        tempPlot <- createJaspPlot(title = options[["priors"]][[i]]$name, width = 530, height = 400, aspectRatio = 0.7)

        plotsIterative[[options[["priors"]][[i]]$name]] <- tempPlot

        allLines  <- c()
        allArrows <- c()
        legend     <- NULL
        range      <- NULL

        for (iteration in iterSequence) {
          if (iteration < 1) {
            range <- rbind(range, .rangeGaussianLS(NULL, options[["priors"]][[i]]))
          } else{
            tempData <- list(
              mean = mean(data$y[1:iteration]),
              N    = length(data$y[1:iteration]),
              SD   = data$SD
            )
            range <- rbind(range, .rangeGaussianLS(tempData, options[["priors"]][[i]]))
          }
        }
        range <- c(min(range[,1]), max(range[,2]))


        for (iteration in iterSequence) {

          if (iteration < 1) {
            tempData <- NULL
          } else{
            tempData <- list(
              mean = mean(data$y[1:iteration]),
              N    = length(data$y[1:iteration]),
              SD   = data$SD
            )
          }

          if (options[["priors"]][[i]]$type == "spike") {

            dfArrowPP   <- .dataArrowGaussianLS(options[["priors"]][[i]])
            dfArrowPP$g <- as.character(iteration)

            allArrows  <- c(allArrows, list(dfArrowPP))
            legend      <- rbind(legend, c(options[["priors"]][[i]]$type, iteration))

          } else if (options[["priors"]][[i]]$type == "normal") {

            dfLinesPP   <- .dataLinesGaussianLS(tempData, options[["priors"]][[i]], range = range)
            dfLinesPP   <- dfLinesPP[dfLinesPP$g == "Posterior",]
            dfLinesPP$g <- as.character(iteration)

            allLines   <- c(allLines, list(dfLinesPP))
            legend      <- rbind(legend, c(options[["priors"]][[i]]$type, iteration))

          }

        }

        xName  <- bquote(.(gettext("Population mean"))~mu)

        tempPlot$plotObject <- .plotStackedLS(allLines, allArrows, legend, xName = xName, xRange = range)
      }
    }
  }

  return()
}
.plotsIterativeIntervalOverlyingGaussianLS <- function(jaspResults, data, ready, options) {

  containerIterativeInterval <- .containerSequentialIntervalLS(jaspResults, options, "gaussEst")

  if (is.null(containerIterativeInterval[["plotsIterativeInterval"]])) {

    plotsIterativeInterval <- createJaspPlot(width = 530, height = 400, aspectRatio = 0.7)

    plotsIterativeInterval$position <- 2
    plotsIterativeInterval$dependOn(c(.dataDependenciesGaussianLS,
                                      "plotsIterativeIntervalLower", "plotsIterativeIntervalUpper", "colorPalette"))
    containerIterativeInterval[["plotsIterativeInterval"]] <- plotsIterativeInterval

    if (!all(ready)) {
      return()
    }

    plotDataLines <- list()

    # cheat for getting 2x 0 for the sequantial plot in case of no data
    if (length(data$y) == 0) {
      iterSeq <- c(0, 0.1)
    } else{
      iterSeq <- 0:length(data$y)
    }

    # update the posteriors as the data go in
    for (h in 1:length(options[["priors"]])) {

      tempLines   <- NULL

      for (i in iterSeq) {

        if (i < 1) {
          tempData <- NULL
        } else{
          tempData <- list(
            mean = mean(data$y[1:i]),
            N    = length(data$y[1:i]),
            SD   = data$SD
          )
        }

        tempResults <- .dataCustomGaussianLS(
          tempData,
          options[["priors"]][[h]],
          lCI = options[["plotsIterativeIntervalLower"]],
          uCI = options[["plotsIterativeIntervalUpper"]],
          NULL,
          "parameter"
        )

        tempLines  <- rbind(tempLines, data.frame(
          y    = tempResults$coverage,
          x    = i,
          name = options[["priors"]][[h]]$name
        ))

      }

      plotDataLines <- c(plotDataLines, list(tempLines))

    }

    yName  <- bquote("P("~{.(options[["plotsIterativeIntervalLower"]])<=mu}<=.(options[["plotsIterativeIntervalUpper"]])~")")
    xName  <- gettext("Observation")

    p <- .plotIterativeLS(plotDataLines, allCI = NULL, xName = xName, yName = yName, palette = options[["colorPalette"]])


    plotsIterativeInterval$plotObject <- p
  }

  return()
}
.plotsIterativeIntervalStackedGaussianLS   <- function(jaspResults, data, ready, options) {

  containerIterativeInterval <- .containerSequentialIntervalLS(jaspResults, options, "gaussEst")

  if (is.null(containerIterativeInterval[["plotsIterativeInterval"]])) {

    plotsIterativeInterval <- createJaspContainer()

    plotsIterativeInterval$position <- 2
    plotsIterativeInterval$dependOn(c(.dataDependenciesGaussianLS,
                                      "plotsIterativeIntervalLower", "plotsIterativeIntervalUpper", "colorPalette"))
    containerIterativeInterval[["plotsIterativeInterval"]] <- plotsIterativeInterval


    if (all(!ready) || (ready["data"] && !ready["priors"])) {

      plotsIterativeInterval[[""]] <- createJaspPlot(title = "", width = 530, height = 400, aspectRatio = 0.7)
      return()

    } else if (!ready["data"] && ready["priors"]) {

      for (i in 1:length(options[["priors"]])) {
        plotsIterativeInterval[[options[["priors"]][[i]]$name]] <- createJaspPlot(title = options[["priors"]][[i]]$name,
                                                                                  width = 530, height = 400, aspectRatio = 0.7)
      }
      return()

    } else{

      for (i in 1:length(options[["priors"]])) {

        tempPlot <- createJaspPlot(title = options[["priors"]][[i]]$name, width = 530, height = 400, aspectRatio = 0.7)

        plotsIterativeInterval[[options[["priors"]][[i]]$name]] <- tempPlot

        allLines  <- c()
        allArrows <- c()
        legend     <- NULL

        # too many iterations crashes JASP
        if (length(data$y) > 10) {
          iterSequence <- round(seq(0, length(data$y), length.out = 10))
        } else{
          iterSequence <- 0:length(data$y)
        }
        iterSequence <- rev(iterSequence)

        range <- NULL
        for (iteration in iterSequence) {
          if (iteration < 1) {
            range <- rbind(range, .rangeGaussianLS(NULL, options[["priors"]][[i]]))
          } else{
            tempData <- list(
              mean = mean(data$y[1:iteration]),
              N    = length(data$y[1:iteration]),
              SD   = data$SD
            )
            range <- rbind(range, .rangeGaussianLS(tempData, options[["priors"]][[i]]))
          }
        }
        range <- c(min(range[,1]), max(range[,2]))

        for (iteration in iterSequence) {

          if (options[["priors"]][[i]]$type == "spike") {

            dfArrowPP   <- .dataArrowGaussianLS(options[["priors"]][[i]])
            dfArrowPP$g <- as.character(iteration)

            allArrows  <- c(allArrows, list(dfArrowPP))
            legend      <- rbind(legend, c(options[["priors"]][[i]]$type, iteration))

          } else if (options[["priors"]][[i]]$type == "normal") {

            if (iteration < 1) {
              tempData <- NULL
            } else{
              tempData <- list(
                mean = mean(data$y[1:iteration]),
                N    = length(data$y[1:iteration]),
                SD   = data$SD
              )
            }

            dfLinesPP   <- .dataLinesGaussianLS(tempData, options[["priors"]][[i]], "parameter", NULL, range)
            dfLinesPP   <- dfLinesPP[dfLinesPP$g == "Posterior",]
            dfLinesPP$g <- as.character(iteration)

            allLines   <- c(allLines, list(dfLinesPP))
            legend      <- rbind(legend, c(options[["priors"]][[i]]$type, iteration))

          }

        }

        xName  <- bquote(.(gettext("Population mean"))~mu)

        tempPlot$plotObject <- .plotStackedLS(allLines, allArrows, legend, xName = xName, xRange = range,
                                              lCI = options[["plotsIterativeIntervalLower"]],
                                              uCI = options[["plotsIterativeIntervalUpper"]])
      }
    }
  }

  return()
}
.tableIterativeGaussianLS          <- function(jaspResults, data, ready, options) {

  containerIterative <- .containerSequentialPointLS(jaspResults, options, "gaussEst")

  if (is.null(containerIterative[["tableIterative"]])) {

    tableIterative <- createJaspTable()

    tableIterative$position <- 3
    tableIterative$dependOn(c(.dataDependenciesGaussianLS, "plotsIterativeEstimateType",
                              "plotsIterativeOverlyingCI", "plotsIterativeCoverage", "colorPalette", "plotsIterativeUpdatingTable"))
    containerIterative[["tableIterative"]] <- tableIterative

    tableIterative$addColumnInfo(name = "iteration", title = gettext("Observation"), type = "integer")
    if (ready["priors"]) {
      if (options[["plotsIterativeOverlyingCI"]]) {
        if (options[["plotsIterativeOverlyingType"]] == "central") {
          CI_title <- gettextf("%i%% CI", options[["plotsIterativeCoverage"]]*100)
        } else if (options[["plotsIterativeOverlyingType"]] == "HPD") {
          CI_title <- gettextf("%i%% HPD", options[["plotsIterativeCoverage"]]*100)
        } else if (options[["plotsIterativeOverlyingType"]] == "support") {
          CI_title <- gettextf("SI (BF=%s)", options[["plotsIterativeBF"]])
        }
        for (i in 1:length(options[["priors"]])) {
          tableIterative$addColumnInfo(
            name  = paste(options[["priors"]][[i]]$name,"center", sep = "_"),
            title = ifelse (options[["plotsIterativeEstimateType"]] == "mean", gettext("Mean"), gettext("Median")),
            overtitle = options[["priors"]][[i]]$name,
            type = "number")
          tableIterative$addColumnInfo(
            name  = paste(options[["priors"]][[i]]$name,"CI", sep = "_"),
            title = CI_title,
            overtitle = options[["priors"]][[i]]$name,
            type = "string")
        }
      } else{
        for (i in 1:length(options[["priors"]])) {
          tableIterative$addColumnInfo(
            name  = paste(options[["priors"]][[i]]$name,"center", sep = "_"),
            title = options[["priors"]][[i]]$name,
            type = "number")
        }
      }
    }


    if (!all(ready)) {
      return()
    }


    iterSeq <- 0:length(data$y)


    for (i in iterSeq) {

      tempRow     <- list()
      tempRow[["iteration"]] <- i

      if (i < 1) {
        tempData <- NULL
      } else{
        tempData <- list(
          mean = mean(data$y[1:i]),
          N    = length(data$y[1:i]),
          SD   = data$SD
        )
      }

      for (h in 1:length(options[["priors"]])) {

        tempResults <- .estimateGaussianLS(tempData, options[["priors"]][[h]])
        tempRow[[paste(options[["priors"]][[h]]$name,"center", sep = "_")]] <- tempResults[[options[["plotsIterativeEstimateType"]]]]

        if (options[["plotsIterativeOverlyingCI"]]) {

          if (options[["plotsIterativeOverlyingType"]] %in% c("central", "HPD")) {

            tempCIPP <- .dataCentralGaussianLS(
              tempData,
              options[["priors"]][[h]],
              options[["plotsIterativeCoverage"]],
              type = "parameter"
            )

            if (options[["plotsIterativeOverlyingType"]] == "HPD") {
              tempCIPP$g <- "HPD"
            }

          } else if (options[["plotsIterativeOverlyingType"]] == "support") {

            tempCIPP <- .dataSupportGaussianLS(
              tempData,
              options[["priors"]][[h]],
              options[["plotsIterativeBF"]]
            )
          }

          if (all(is.na(tempCIPP[1:2]))) {
            temp_int <- "∅"
          } else{
            temp_int <- sapply(1:nrow(tempCIPP), function(i)paste(c(
              "[",format(round(tempCIPP$xStart[i], 3), nsmall = 3),", ",format(round(tempCIPP$xEnd[i], 3), nsmall = 3),"]"
            ), collapse = ""))
            temp_int <- paste(temp_int, collapse = " and " )

            tempRow[[paste(options[["priors"]][[h]]$name,"CI", sep = "_")]] <- temp_int
          }

        }

      }

      tableIterative$addRows(tempRow)

    }
  }

  return()
}
.tableIterativeIntervalGaussianLS  <- function(jaspResults, data, ready, options) {

  containerIterativeInterval <- .containerSequentialIntervalLS(jaspResults, options, "gaussEst")

  if (is.null(containerIterativeInterval[["tableIterativeInterval"]])) {

    tableIterativeInterval <- createJaspTable()

    tableIterativeInterval$position <- 3
    tableIterativeInterval$dependOn(c(.dataDependenciesGaussianLS,
                                      "plotsIterativeIntervalLower", "plotsIterativeIntervalUpper", "plotsIterativeIntervalUpdatingTable"))
    containerIterativeInterval[["tableIterativeInterval"]] <- tableIterativeInterval

    tableIterativeInterval$addColumnInfo(name = "iteration", title = gettext("Observation"), type = "integer")
    if (ready["priors"]) {
      for (i in 1:length(options[["priors"]])) {
        tableIterativeInterval$addColumnInfo(
          name  = paste(options[["priors"]][[i]]$name,"center", sep = "_"),
          title = options[["priors"]][[i]]$name,
          type = "number")
      }
    }


    if (!all(ready)) {
      return()
    }


    iterSeq <- 0:length(data$y)


    for (i in iterSeq) {

      tempRow     <- list()
      tempRow[["iteration"]] <- i

      if (i < 1) {
        tempData <- NULL
      } else{
        tempData <- list(
          mean = mean(data$y[1:i]),
          N    = length(data$y[1:i]),
          SD   = data$SD
        )
      }

      for (h in 1:length(options[["priors"]])) {

        tempResults <- .dataCustomGaussianLS(
          tempData,
          options[["priors"]][[h]],
          lCI = options[["plotsIterativeIntervalLower"]],
          uCI = options[["plotsIterativeIntervalUpper"]],
          NULL,
          type = "parameter"
        )

        tempRow[[paste(options[["priors"]][[h]]$name,"center", sep = "_")]] <- tempResults$coverage

      }

      tableIterativeInterval$addRows(tempRow)

    }
  }

  return()
}

.tablepredictionsGaussianLS        <- function(jaspResults, data, ready, options) {

  containerPredictions <- .containerPredictionsLS(jaspResults, options, "gaussEst")

  if (is.null(containerPredictions[["predictionsTable"]])) {

    predictionsTable <- createJaspTable()

    predictionsTable$position <- 2
    predictionsTable$dependOn(c(.dataDependenciesGaussianLS, "predictionN"))

    predictionsTable$addColumnInfo(name = "hypothesis",     title = gettext("Model"),                     type = "string")
    predictionsTable$addColumnInfo(name = "posterior",      title = gettextf("Posterior (%s)", "\u03BC"), type = "string")
    predictionsTable$addColumnInfo(name = "posteriorMean",  title = gettext("Posterior Mean"),            type = "number")
    predictionsTable$addColumnInfo(name = "predictive",     title = gettext("Prediction"),                type = "string")
    predictionsTable$addColumnInfo(name = "predictiveMean", title = gettext("Prediction Mean"),           type = "number")
    predictionsTable$setExpectedSize(length(options[["priors"]]))

    containerPredictions[["predictionsTable"]] <- predictionsTable

    if (!ready["priors"] || is.null(data$SD) || data$SD == 0) {

      # TODO: check whether this works properly
      if (is.null(data$SD) || data$SD == 0) {
        predictionsTable$setError(gettext("Please, specify the standard deviation of the data."))
      }

      return()

    } else{

      # add rows for each hypothesis
      for (i in 1:length(options[["priors"]])) {

        tempResults    <- .estimateGaussianLS(data, options[["priors"]][[i]])
        temp_prediction <- .predictGaussianLS(data, options[["priors"]][[i]], options, options[["predictionN"]])

        tempRow <- list(
          hypothesis      = options[["priors"]][[i]]$name,

          posterior       = tempResults$distribution,
          posteriorMean   = tempResults$mean,
          predictive      = temp_prediction$distribution,
          predictiveMean  = temp_prediction$mean
        )


        predictionsTable$addRows(tempRow)
      }

      # add footnote clarifying what dataset was used
      predictionsTable$addFootnote(gettextf(
        "The prediction for %s future %s is based on %s.",
        options[["predictionN"]],
        ifelse (options[["predictionN"]] == 1, gettext("observation"),gettext("observations")),
        if (is.null(data)) gettext("prior") else gettextf(
          "%s past %s",
          data$N,
          ifelse (data$N == 1, gettext("observation"), gettext("observations"))
        )
      ))

    }
  }

  return()
}
.plotsPredictionsIndividualGaussianLS      <- function(jaspResults, data, ready, options) {

  containerPredictionPlots <- .containerPredictionPlotsLS(jaspResults, options, "gaussEst")

  if (is.null(containerPredictionPlots[["plotsPredictions"]])) {

    plotsPredictions <- createJaspContainer()

    plotsPredictions$position <- 2
    plotsPredictions$dependOn(c(.dataDependenciesGaussianLS, "predictionN",
                                "plotsPredictionCI", "plotsPredictionType",
                                "plotsPredictionEstimate", "plotsPredictionEstimateType",
                                "plotsPredictionCoverage", "plotsPredictionLower", "plotsPredictionUpper",
                                "predictionPlotProp"))

    containerPredictionPlots[["plotsPredictions"]] <- plotsPredictions


    if (!ready["priors"] || is.null(data$SD) || data$SD == 0) {

      temp_p <- createJaspPlot(title = "", width = 530, height = 400, aspectRatio = 0.7)

      if (is.null(data$SD) || data$SD == 0) {
        temp_p$setError(gettext("Please, specify the standard deviation of the data."))
      }

      plotsPredictions[[""]] <- temp_p

      return()

    } else{

      for (i in 1:length(options[["priors"]])) {

        tempPlot <- createJaspPlot(title = options[["priors"]][[i]]$name, width = 530, height = 400, aspectRatio = 0.7)

        plotsPredictions[[options[["priors"]][[i]]$name]] <- tempPlot

        yName  <- gettext("Density")
        if (options[["predictionPlotProp"]]) {
          xName  <- gettext("Sample means")
        } else{
          xName  <- gettext("Future data")
        }

        dfCI   <- NULL
        dfHist <- NULL


        range <- .rangeGaussianLS(
          data,
          options[["priors"]][[i]],
          "prediction",
          options[["predictionN"]]
        )

        if (options[["plotsPredictionCI"]]) {

          if (options[["plotsPredictionType"]] %in% c("central","HPD")) {

            dfCI <- .dataCentralGaussianLS(
              data,
              options[["priors"]][[i]],
              options[["plotsPredictionCoverage"]],
              N = if (options[["predictionPlotProp"]]) {options[["predictionN"]]} else{1},
              "prediction"
            )

            if (options[["plotsPredictionType"]] == "HPD") {
              dfCI$g <- "HPD"
            }
          } else if (options[["plotsPredictionType"]] == "custom") {

            dfCI <- .dataCustomGaussianLS(
              data,
              options[["priors"]][[i]],
              options[["plotsPredictionLower"]],
              options[["plotsPredictionUpper"]],
              N = if (options[["predictionPlotProp"]]) {options[["predictionN"]]} else{1},
              "prediction"
            )

          }

          dfCI$parameter <- "mu"
        }

        dfLinesPP  <- .dataLinesGaussianLS(data, options[["priors"]][[i]], "prediction", N = if (options[["predictionPlotProp"]]) {options[["predictionN"]]} else{1}, range = range)
        dfLinesPP  <- dfLinesPP[dfLinesPP$g == "Posterior",]

        if (options[["plotsPredictionEstimate"]]) {
          dfPointEstimate <- .estimateDataPointGaussian(data, options[["priors"]][[i]], N = options[["predictionN"]],
                                                        type = "prediction", estimate = options[["plotsPredictionEstimateType"]],
                                                        prop = options[["predictionPlotProp"]])
        } else
          dfPointEstimate <- NULL


        p <- .plotIndividualLS(allLines = dfLinesPP, allArrows = NULL, pointEstimate = dfPointEstimate, CI = dfCI, CIallLines = NULL, dfPoints = NULL, xRange = range, xName = xName, yName = yName)

        tempPlot$plotObject <- p
      }
    }
  }

  return()
}
.plotsPredictionsGaussianLS        <- function(jaspResults, data, ready, options) {

  containerPredictionPlots <- .containerPredictionPlotsLS(jaspResults, options, "gaussEst")

  if (is.null(containerPredictionPlots[["plotsPredictions"]])) {

    plotsPredictions <- createJaspPlot(width = 530, height = 400, aspectRatio = 0.7)

    plotsPredictions$position <- 2
    plotsPredictions$dependOn(c(.dataDependenciesGaussianLS, "predictionN",
                                "colorPalettePrediction", "predictionPlotProp"))

    containerPredictionPlots[["plotsPredictions"]] <- plotsPredictions


    if (!ready["priors"] || is.null(data$SD) || data$SD == 0) {

      if (is.null(data$SD) || data$SD == 0) {
        plotsPredictions$setError(gettext("Please, specify the standard deviation of the data."))
      }

      return()

    } else{

      range <- .rangeGaussiansLS(
        data,
        options[["priors"]],
        "prediction",
        options[["predictionN"]]
      )

      if (options[["predictionPlotProp"]]) {
        xName  <- gettext("Sample means")
      } else{
        xName  <- gettext("Future data")
      }

      allLines  <- c()
      legend     <- NULL

      for (i in 1:length(options[["priors"]])) {

        dfLinesPP  <- .dataLinesGaussianLS(data, options[["priors"]][[i]], "prediction",  N = if (options[["predictionPlotProp"]]) {options[["predictionN"]]} else{1}, range = range)
        dfLinesPP  <- dfLinesPP[dfLinesPP$g == "Posterior",]

        dfLinesPP$g <- options[["priors"]][[i]]$name

        # it's not beta, but I'm lazzy to rewrite a function I wanna use
        # lol, I was so lazy that I even coppied this comment from the binomial version :D
        legend   <- rbind(legend, c("beta", options[["priors"]][[i]]$name))
        allLines<- c(allLines, list(dfLinesPP))
      }

      if (options[["predictionPlotType"]] == "overlying") {
        p <- .plotOverlyingLS(allLines, NULL, xName = xName, yName = yName, xRange = range, discrete = FALSE,
                              palette = options[["colorPalette"]], proportions = options[["predictionPlotProp"]])
      } else{
        p <- .plotStackedLS(allLines, NULL, legend, xName = xName, xRange = range,
                            discrete = FALSE, proportions = options[["predictionPlotProp"]])
      }

      plotsPredictions$plotObject <- p
    }
  }

  return()
}
