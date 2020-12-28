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

LSgaussianestimation   <- function(jaspResults, dataset, options, state = NULL){

  saveOptions(options)

  # introductory text
  if(options[["introText"]]).introductoryTextLS(jaspResults, options, "gauss_est")

  # a vector of two, first for data, second for hypotheses
  ready <- .readyGaussianLS(options)

  # evaluate the expressions in priors
  if(ready[2])options[["priors"]] <- .evaluate_priors(options[["priors"]])

  # load, check, transform and process data
  if(ready[1]){
    data <- .readDataGaussianLS(dataset, options)
  }else{
    data <- NULL
  }

  # data summary table if requested (but not if the data counts were added directly)
  .summaryGaussianLS(jaspResults, data, options, "gauss_est")


  ### inference
  # estimated parameter values
  .estimatesGaussianLS(jaspResults, data, ready, options)

  # prior
  if(options[["plotsPrior"]]){
    if(options[["plotsPriorType"]] != "individual").plotsSimpleGaussianLS(jaspResults, data, ready, options, type = "Prior")
    if(options[["plotsPriorType"]] == "individual").plotsIndividualGaussianLS(jaspResults, data, ready, options, type = "Prior")
  }

  # posterior
  if(options[["plotsPosterior"]]){
    if(options[["plotsPosteriorType"]] != "individual").plotsSimpleGaussianLS(jaspResults, data, ready, options, type = "Posterior")
    if(options[["plotsPosteriorType"]] == "individual").plotsIndividualGaussianLS(jaspResults, data, ready, options, type = "Posterior")
  }

  # prior and posterior
  if(options[["plotsBoth"]]).plotsBothGaussianLS(jaspResults, data, ready, options)


  ### sequential analysis
  # point estimate
  if(options[["plotsIterative"]]){
    if(options[["plotsIterativeType"]] == "overlying").plotsIterativeOverlyingGaussianLS(jaspResults, data, ready, options)
    if(options[["plotsIterativeType"]] == "stacked").plotsIterativeStackedGaussianLS(jaspResults, data, ready, options)
  }

  # point estimate table
  if(options[["plotsIterative"]] && options[["plotsIterativeUpdatingTable"]]).tableIterativeGaussianLS(jaspResults, data, ready, options)

  # interval
  if(options[["plotsIterativeInterval"]]){
    if(options[["plotsIterativeIntervalType"]] == "overlying").plotsIterativeIntervalOverlyingGaussianLS(jaspResults, data, ready, options)
    if(options[["plotsIterativeIntervalType"]] == "stacked").plotsIterativeIntervalStackedGaussianLS(jaspResults, data, ready, options)
  }

  # interval estimate table
  if(options[["plotsIterativeInterval"]] && options[["plotsIterativeIntervalUpdatingTable"]]).tableIterativeIntervalGaussianLS(jaspResults, data, ready, options)

  # posterior updating table
  if(options[["doIterative"]] && options[["dataType"]] != "dataCounts").estimatesSequentialGaussianLS(jaspResults, data, ready, options)


  ### prediction
  if(options[["predictionTable"]]).tablepredictionsGaussianLS(jaspResults, data, ready, options)

  # plot
  if(options[["plotsPredictions"]]){
    if(options[["predictionPlotType"]] != "individual").plotsPredictionsGaussianLS(jaspResults, data, ready, options)
    if(options[["predictionPlotType"]] == "individual").plotsPredictionsIndividualGaussianLS(jaspResults, data, ready, options)
  }

  return()
}


.estimatesGaussianLS               <- function(jaspResults, data, ready, options){

  if(is.null(jaspResults[["estimatesContainer"]])){
    estimatesContainer <- createJaspContainer("Model")
    estimatesContainer$position <- 2
    jaspResults[["estimatesContainer"]] <- estimatesContainer
  }else{
    estimatesContainer <- jaspResults[["estimatesContainer"]]
  }


  if(options[["introText"]] && is.null(estimatesContainer[['introText']])){

    introText <- createJaspHtml()
    introText$dependOn("introText")
    introText$position <- 1

    introText[['text']] <- .explanatoryTextLS("estimates", NULL, "gauss_est")

    estimatesContainer[['introText']] <- introText
  }


  if(is.null(estimatesContainer[['estimatesTable']])){

    estimatesTable <- createJaspTable(title = gettext("Estimation Summary"))

    estimatesTable$position <- 2
    estimatesTable$dependOn(.GaussianLS_data_dependencies)

    estimatesTable$addColumnInfo(name = "hypothesis",    title = gettext("Model"),           type = "string")
    estimatesTable$addColumnInfo(name = "prior",         title = gettext("Prior (μ)"),       type = "string")
    estimatesTable$addColumnInfo(name = "priorMean",     title = gettext("Prior Mean"),      type = "number")
    estimatesTable$addColumnInfo(name = "posterior",     title = gettext("Posterior (μ)"),   type = "string")
    estimatesTable$addColumnInfo(name = "posteriorMean", title = gettext("Posterior Mean"),  type = "number")

    estimatesTable$setExpectedSize(length(options[["priors"]]))

    estimatesContainer[["estimatesTable"]] <- estimatesTable

    if(!ready[2]){

      return()

    }else{

      # add rows for each hypothesis
      for(i in 1:length(options[["priors"]])){

        temp_results <- .estimateGaussianLS(NULL, options[["priors"]][[i]])

        temp_row <- list(
          prior         = temp_results$distribution,
          priorMean     = temp_results$mean,
          hypothesis    = options[["priors"]][[i]]$name,
          posterior     = "",
          posteriorMean = "")


        if(all(ready)){
          # and when real data are supplied as well, add posterior information
          temp_results <- .estimateGaussianLS(data, options[["priors"]][[i]])

          temp_row["posterior"]     <- temp_results$distribution
          temp_row["posteriorMean"] <- temp_results$mean

        }

        estimatesTable$addRows(temp_row)
      }

    }
  }

  return()
}
.estimatesSequentialGaussianLS     <- function(jaspResults, data, ready, options){

  containerIterativeUpdating <- .containerSequentialUpdatingLS(jaspResults, options, "bin_est")

  if(is.null(containerIterativeUpdating[["estimatesSequentialTable"]])){

    estimatesSequentialTable <- createJaspTable()

    estimatesSequentialTable$position <- 2
    estimatesSequentialTable$dependOn(.GaussianLS_data_dependencies)

    estimatesSequentialTable$addColumnInfo(name = "iteration", title = gettext("Observation"), type = "integer")
    containerIterativeUpdating[["estimatesSequentialTable"]] <- estimatesSequentialTable


    estimatesSequentialTable$setExpectedSize(ifelse(ready[1], length(data$y) + 1, 1))
    if(ready[2]){
      for(i in 1:length(options[["priors"]])){
        estimatesSequentialTable$addColumnInfo(
          name  = options[["priors"]][[i]]$name,
          title = options[["priors"]][[i]]$name,
          type = "string")
      }
    }


    if(!all(ready)){
      return()
    }else{
      # add priors to the first row
      temp_row <- NULL
      temp_row[["iteration"]] <- 0
      for(h in 1:length(options[["priors"]])){
        temp_results <- .estimateGaussianLS(NULL, options[["priors"]][[h]])
        temp_row[[options[["priors"]][[h]]$name]] <- temp_results$distribution
      }
      estimatesSequentialTable$addRows(temp_row)

      # then update the posteriors as the data go in
      if(length(data$y) > 0){
        for(i in 1:length(data$y)){
          temp_row <- NULL
          temp_row[["iteration"]] <- i
          for(h in 1:length(options[["priors"]])){
            temp_data    <- list(
              mean = mean(data$y[1:i]),
              N    = length(data$y[1:i]),
              SD   = data$SD
            )
            temp_results <- .estimateGaussianLS(temp_data, options[["priors"]][[h]])
            temp_row[[options[["priors"]][[h]]$name]] <- temp_results$distribution
          }
          estimatesSequentialTable$addRows(temp_row)
        }
      }
    }
  }

  return()
}
.plotsSimpleGaussianLS             <- function(jaspResults, data, ready, options, type = c("Prior", "Posterior")){

   containerPlots <- .containerPlotsLS(jaspResults, options, "gauss_est", type)

  if(is.null(containerPlots[[paste0("plots",type,"simple")]])){

    plotsSimple <- createJaspPlot(width = 530, height = 400, aspectRatio = 0.7)

    plotsSimple$position <- 2
    plotsSimple$dependOn(c(.GaussianLS_data_dependencies,
                           ifelse(options[[ifelse(type == "Prior", "plotsPriorType", "plotsPosteriorType")]] == "overlying",
                                  "colorPalette", "")))

    containerPlots[[paste0("plots",type,"simple")]] <- plotsSimple

    if (!all(ready))return()

    all_lines  <- c()
    all_arrows <- c()
    legend     <- NULL
    range      <- .rangeGaussiansLS(if(type == "Prior") NULL else data, options[["priors"]])

    for(i in 1:length(options[["priors"]])){

      if(options[["priors"]][[i]]$type == "spike"){

        dfArrowPP   <- .dataArrowGaussianLS(options[["priors"]][[i]])
        dfArrowPP$g <- options[["priors"]][[i]]$name

        all_arrows  <- c(all_arrows, list(dfArrowPP))
        legend      <- rbind(legend, c(options[["priors"]][[i]]$type, options[["priors"]][[i]]$name))

      }else if(options[["priors"]][[i]]$type == "normal"){

        dfLinesPP   <- .dataLinesGaussianLS(data, options[["priors"]][[i]], range = range)
        dfLinesPP   <- dfLinesPP[dfLinesPP$g == type,]
        dfLinesPP$g <- options[["priors"]][[i]]$name

        all_lines   <- c(all_lines, list(dfLinesPP))
        legend      <- rbind(legend, c(options[["priors"]][[i]]$type, options[["priors"]][[i]]$name))

      }
    }

    xName  <- bquote(.(gettext("Population mean"))~mu)

    if(options[[ifelse(type == "Prior", "plotsPriorType", "plotsPosteriorType")]] == "overlying"){
      p <- .plotOverlyingLS(all_lines, all_arrows, xName = xName, palette = options[["colorPalette"]], xRange = range)
    }else{
      p <- .plotStackedLS(all_lines, all_arrows, legend, xName = xName, xRange = range)
    }

    plotsSimple$plotObject <- p
  }

  return()
}
.plotsIndividualGaussianLS         <- function(jaspResults, data, ready, options, type = c("Prior", "Posterior")){

  containerPlots <- .containerPlotsLS(jaspResults, options, "gauss_est", type)

  if(is.null(containerPlots[[paste0("plots",type,"individual")]])){

    plotsIndividual <- createJaspContainer()

    plotsIndividual$position <- 2
    plotsIndividual$dependOn(c(.GaussianLS_data_dependencies,
                               ifelse(type == "Prior", "plotsPriorIndividualCI", "plotsPosteriorIndividualCI"),
                               ifelse(type == "Prior", "plotsPriorCoverage",     "plotsPosteriorCoverage"),
                               ifelse(type == "Prior", "plotsPriorLower",        "plotsPosteriorLower"),
                               ifelse(type == "Prior", "plotsPriorUpper",        "plotsPosteriorUpper")))

    containerPlots[[paste0("plots",type,"individual")]] <- plotsIndividual


    if(all(!ready) || (ready[1] && !ready[2])){

      plotsIndividual[[""]] <- createJaspPlot(title = "", width = 530, height = 400, aspectRatio = 0.7)
      return()

    }else if(!ready[1] && ready[2]){

      for(i in 1:length(options[["priors"]])){
        plotsIndividual[[options[["priors"]][[i]]$name]] <- createJaspPlot(title = options[["priors"]][[i]]$name,
                                                                           width = 530, height = 400, aspectRatio = 0.7)
      }
      return()

    }else{

      for(i in 1:length(options[["priors"]])){

        temp_plot <- createJaspPlot(title = options[["priors"]][[i]]$name, width = 530, height = 400, aspectRatio = 0.7)

        plotsIndividual[[options[["priors"]][[i]]$name]] <- temp_plot

        xName  <- bquote(.(gettext("Population mean"))~mu)

        dfArrowPP   <- NULL
        dfLinesPP   <- NULL
        dfCI        <- NULL
        dfCILinesPP <- NULL

        if(type == "Posterior" && options[["plotsPosteriorIndividualCI"]] && options[["plotsPosteriorIndividualType"]] == "support"){
          range <- .rangeGaussianSupportLS(data, options[["priors"]][[i]], options[["plotsPosteriorBF"]])
        }else{
          range <- .rangeGaussianLS(if(type == "Prior") NULL else data, options[["priors"]][[i]])
        }

        if(options[[ifelse(type == "Prior", "plotsPriorIndividualCI", "plotsPosteriorIndividualCI")]]){

          if(options[[ifelse(type == "Prior", "plotsPriorIndividualType", "plotsPosteriorIndividualType")]] %in% c("central", "HPD")){

            dfCI <- .dataCentralGaussianLS(
              if(type == "Prior") NULL else data,
              options[["priors"]][[i]],
              options[[ifelse(type == "Prior", "plotsPriorCoverage", "plotsPosteriorCoverage")]],
              type = "parameter"
            )

            if(options[[ifelse(type == "Prior", "plotsPriorIndividualType", "plotsPosteriorIndividualType")]] == "HPD"){
              dfCI$g <- "HPD"
            }

          }else if(options[[ifelse(type == "Prior", "plotsPriorIndividualType", "plotsPosteriorIndividualType")]] == "custom"){

            dfCI <- .dataCustomGaussianLS(
              if(type == "Prior") NULL else data,
              options[["priors"]][[i]],
              options[[ifelse(type == "Prior", "plotsPriorLower", "plotsPosteriorLower")]],
              options[[ifelse(type == "Prior", "plotsPriorUpper", "plotsPosteriorUpper")]],
              NULL,
              type = "parameter"
            )

          }else if(options[["plotsPosteriorIndividualType"]] == "support"){

            dfCI <- .dataSupportGaussianLS(
              data,
              options[["priors"]][[i]],
              options[["plotsPosteriorBF"]]
            )

          }
        }


        if(options[["priors"]][[i]]$type == "spike"){

          dfArrowPP  <- .dataArrowGaussianLS(options[["priors"]][[i]])

        }else if(options[["priors"]][[i]]$type == "normal"){

          dfLinesPP  <- .dataLinesGaussianLS(data, options[["priors"]][[i]], range = range)
          dfLinesPP  <- dfLinesPP[dfLinesPP$g == type,]

          if(!is.null(dfCI)){
            for(r in 1:nrow(dfCI)){
              # wtf?
              temp_CILinesPP   <- dfLinesPP[dfLinesPP$x >= dfCI$x_start[r] & dfLinesPP$x <= dfCI$x_end[r],]
              temp_CILinesPP$g <- paste(c(as.character(dfCI$g), r), collapse = "")
              temp_CILinesPP   <- rbind.data.frame(
                data.frame(x = dfCI$x_start[r], y = 0, g = temp_CILinesPP$g[1]),
                temp_CILinesPP,
                data.frame(x = dfCI$x_end[r], y = 0, g = temp_CILinesPP$g[1])
              )
              dfCILinesPP <- rbind.data.frame(dfCILinesPP, temp_CILinesPP)
            }
          }

        }

        p <- .plotIndividualLS(dfLinesPP, dfArrowPP, dfCI, dfCILinesPP, NULL, range, xName, nRound = 3)
        temp_plot$plotObject <- p
      }

      return()
    }

  }

  return()
}
.plotsBothGaussianLS               <- function(jaspResults, data, ready, options){

  containerBoth <- .containerPlotsBothLS(jaspResults, options, "gauss_est")

  if(is.null(containerBoth[["plotsBoth"]])){

    plotsBoth <- createJaspContainer()
    plotsBoth$position <- 2
    plotsBoth$dependOn(c(.GaussianLS_data_dependencies, "plotsBothSampleProportion"))

    containerBoth[["plotsBoth"]] <- plotsBoth

    if(all(!ready) || (ready[1] && !ready[2])){

      plotsBoth[[""]] <- createJaspPlot(title = "", width = 530, height = 400, aspectRatio = 0.7)
      return()

    }else if(!ready[1] && ready[2]){

      for(i in 1:length(options[["priors"]])){
        plotsBoth[[options[["priors"]][[i]]$name]] <- createJaspPlot(title = options[["priors"]][[i]]$name,
                                                                     width = 530, height = 400, aspectRatio = 0.7)
      }
      return()

    }else{

      for(i in 1:length(options[["priors"]])){

        temp_plot <- createJaspPlot(title = options[["priors"]][[i]]$name, width = 530, height = 400, aspectRatio = 0.7)

        plotsBoth[[options[["priors"]][[i]]$name]] <- temp_plot

        dfArrowPP <- NULL
        dfLinesPP <- NULL

        xName  <- bquote(.(gettext("Population mean"))~mu)
        range  <- .rangeGaussianLS(NULL, options[["priors"]][[i]])

        if(options[["priors"]][[i]]$type == "spike"){
          dfArrowPP  <- .dataArrowGaussianLS(options[["priors"]][[i]])
        }else if(options[["priors"]][[i]]$type == "normal"){
          dfLinesPP  <- .dataLinesGaussianLS(data, options[["priors"]][[i]], range = range)

          if(all(dfLinesPP$y[dfLinesPP$g == "Prior"] == dfLinesPP$y[dfLinesPP$g == "Posterior"])){
            dfLinesPP   <- dfLinesPP[dfLinesPP$g == "Posterior",]
            dfLinesPP$g <- "Prior = Posterior"
          }

        }

        if(options[["plotsBothSampleProportion"]]){
          dfPointsPP <- .dataObservedGaussianLS(data)
        }else{
          dfPointsPP <- NULL
        }

        p <- .plotPriorPosteriorLS(list(dfLinesPP), list(dfArrowPP), dfPoints = dfPointsPP, xName = xName, xRange = range)
        temp_plot$plotObject <- p
      }
    }
  }

  return()
}
.plotsIterativeOverlyingGaussianLS <- function(jaspResults, data, ready, options){

  containerIterative <- .containerSequentialPointLS(jaspResults, options, "gauss_est")

  if(is.null(containerIterative[["plotsIterative"]])){

    plotsIterative <- createJaspPlot(width = 530, height = 400, aspectRatio = 0.7)

    plotsIterative$position <- 2
    plotsIterative$dependOn(c(.GaussianLS_data_dependencies, "plotsIterativeCenter",
                              "plotsIterativeIndividualCI", "plotsIterativeCoverage", "colorPalette"))
    containerIterative[["plotsIterative"]] <- plotsIterative

    if (!all(ready)){
      return()
    }

    plot_data_lines <- list()
    plot_data_CI    <- list()


    # cheat for getting 2x 0 for the sequantial plot in case of no data
    if(length(data$y) == 0){
      iter_seq <- c(0, 0.1)
    }else{
      iter_seq <- 0:length(data$y)
    }

    # get the plotting range
    range <- NULL
    for(i in iter_seq){
      if(i < 1){

        if(options[["plotsIterativeIndividualCI"]] && options[["plotsIterativeIndividualType"]] == "support"){
          range <- rbind(range, .rangeGaussiansSupportLS(NULL, options[["priors"]], options[["plotsIterativeBF"]]))
        }else{
          range <- rbind(range, .rangeGaussiansLS(NULL, options[["priors"]]))
        }

      }else{
        temp_data <- list(
          mean = mean(data$y[1:i]),
          N    = length(data$y[1:i]),
          SD   = data$SD
        )

        if(options[["plotsIterativeIndividualCI"]] && options[["plotsIterativeIndividualType"]] == "support"){
          range <- rbind(range, .rangeGaussiansSupportLS(temp_data, options[["priors"]], options[["plotsIterativeBF"]]))
        }else{
          range <- rbind(range, .rangeGaussiansLS(temp_data, options[["priors"]]))
        }
      }
    }
    range <- c(min(range[,1]), max(range[,2]))

    # then update the posteriors as the data go in
    for(h in 1:length(options[["priors"]])){

      temp_lines   <- NULL
      temp_CI      <- NULL

      for(i in iter_seq){

        if(i < 1){
          temp_data <- NULL
        }else{
          temp_data <- list(
            mean = mean(data$y[1:i]),
            N    = length(data$y[1:i]),
            SD   = data$SD
          )
        }

        temp_results    <- .estimateGaussianLS(temp_data, options[["priors"]][[h]])
        temp_lines      <- rbind(temp_lines, data.frame(
          y    = temp_results[[options[["plotsIterativeCenter"]]]],
          x    = i,
          name = options[["priors"]][[h]]$name
        ))

        if(options[["plotsIterativeIndividualCI"]]){

          if(options[["plotsIterativeIndividualType"]] %in% c("central", "HPD")){

            temp_CIPP <- .dataCentralGaussianLS(
              temp_data,
              options[["priors"]][[h]],
              options[["plotsIterativeCoverage"]],
              type = "parameter"
            )

            if(options[["plotsIterativeIndividualType"]] == "HPD"){
              temp_CIPP$g <- "HPD"
            }

          }else if(options[["plotsIterativeIndividualType"]] == "support"){

            temp_CIPP <- .dataSupportGaussianLS(
              temp_data,
              options[["priors"]][[h]],
              options[["plotsIterativeBF"]],
              range
            )

            if(nrow(temp_CIPP) == 0)temp_CIPP <- NULL

          }

          temp_CI <- rbind(temp_CI, data.frame(
            y1   = temp_CIPP$x_start,
            y2   = temp_CIPP$x_end,
            x    = i,
            name = options[["priors"]][[h]]$name
          ))

        }

      }

      plot_data_lines <- c(plot_data_lines, list(temp_lines))

      # deal with possible non-existing support intervals
      if(all(is.na(temp_CI[,c("y1", "y2")]))){
        plot_data_CI    <- c(plot_data_CI, list(NULL))
      }else{
        plot_data_CI    <- c(plot_data_CI, list(temp_CI))
      }

    }

    yName  <- bquote(.(gettext("Population mean"))~~mu)
    xName  <- gettext("Observation")

    p <- .plotIterativeLS(plot_data_lines, plot_data_CI, xName = xName, yName = yName, palette = options[["colorPalette"]], yRange = range)

    plotsIterative$plotObject <- p
  }

  return()
}
.plotsIterativeStackedGaussianLS   <- function(jaspResults, data, ready, options){

  containerIterative <- .containerSequentialPointLS(jaspResults, options, "gauss_est")

  if(is.null(containerIterative[["plotsIterative"]])){

    plotsIterative <- createJaspContainer()

    plotsIterative$position <- 2
    plotsIterative$dependOn(.GaussianLS_data_dependencies)

    containerIterative[["plotsIterative"]] <- plotsIterative


    if(all(!ready) || (ready[1] && !ready[2])){

      plotsIterative[[""]] <- createJaspPlot(title = "", width = 530, height = 400, aspectRatio = 0.7)
      return()

    }else if(!ready[1] && ready[2]){

      for(i in 1:length(options[["priors"]])){
        plotsIterative[[options[["priors"]][[i]]$name]] <- createJaspPlot(title = options[["priors"]][[i]]$name,
                                                                          width = 530, height = 400, aspectRatio = 0.7)
      }
      return()

    }else{

      # too many iterations crashes JASP
      if(length(data$y) > 10){
        iter_sequence <- round(seq(0, length(data$y), length.out = 10))
      }else{
        iter_sequence <- 0:length(data$y)
      }
      iter_sequence <- rev(iter_sequence)


      for(i in 1:length(options[["priors"]])){

        temp_plot <- createJaspPlot(title = options[["priors"]][[i]]$name, width = 530, height = 400, aspectRatio = 0.7)

        plotsIterative[[options[["priors"]][[i]]$name]] <- temp_plot

        all_lines  <- c()
        all_arrows <- c()
        legend     <- NULL
        range      <- NULL

        for(iteration in iter_sequence){
          if(iteration < 1){
            range <- rbind(range, .rangeGaussianLS(NULL, options[["priors"]][[i]]))
          }else{
            temp_data <- list(
              mean = mean(data$y[1:iteration]),
              N    = length(data$y[1:iteration]),
              SD   = data$SD
            )
            range <- rbind(range, .rangeGaussianLS(temp_data, options[["priors"]][[i]]))
          }
        }
        range <- c(min(range[,1]), max(range[,2]))


        for(iteration in iter_sequence){

          if(iteration < 1){
            temp_data <- NULL
          }else{
            temp_data <- list(
              mean = mean(data$y[1:iteration]),
              N    = length(data$y[1:iteration]),
              SD   = data$SD
            )
          }

          if(options[["priors"]][[i]]$type == "spike"){

            dfArrowPP   <- .dataArrowGaussianLS(options[["priors"]][[i]])
            dfArrowPP$g <- as.character(iteration)

            all_arrows  <- c(all_arrows, list(dfArrowPP))
            legend      <- rbind(legend, c(options[["priors"]][[i]]$type, iteration))

          }else if(options[["priors"]][[i]]$type == "normal"){

            dfLinesPP   <- .dataLinesGaussianLS(temp_data, options[["priors"]][[i]], range = range)
            dfLinesPP   <- dfLinesPP[dfLinesPP$g == "Posterior",]
            dfLinesPP$g <- as.character(iteration)

            all_lines   <- c(all_lines, list(dfLinesPP))
            legend      <- rbind(legend, c(options[["priors"]][[i]]$type, iteration))

          }

        }

        xName  <- bquote(.(gettext("Population mean"))~mu)

        temp_plot$plotObject <- .plotStackedLS(all_lines, all_arrows, legend, xName = xName, xRange = range)
      }
    }
  }

  return()
}
.plotsIterativeIntervalOverlyingGaussianLS <- function(jaspResults, data, ready, options){

  containerIterativeInterval <- .containerSequentialIntervalLS(jaspResults, options, "gauss_est")

  if(is.null(containerIterativeInterval[["plotsIterativeInterval"]])){

    plotsIterativeInterval <- createJaspPlot(width = 530, height = 400, aspectRatio = 0.7)

    plotsIterativeInterval$position <- 2
    plotsIterativeInterval$dependOn(c(.GaussianLS_data_dependencies,
                                      "plotsIterativeIntervalLower", "plotsIterativeIntervalUpper", "colorPalette"))
    containerIterativeInterval[["plotsIterativeInterval"]] <- plotsIterativeInterval

    if (!all(ready)){
      return()
    }

    plot_data_lines <- list()

    # cheat for getting 2x 0 for the sequantial plot in case of no data
    if(length(data$y) == 0){
      iter_seq <- c(0, 0.1)
    }else{
      iter_seq <- 0:length(data$y)
    }

    # update the posteriors as the data go in
    for(h in 1:length(options[["priors"]])){

      temp_lines   <- NULL

      for(i in iter_seq){

        if(i < 1){
          temp_data <- NULL
        }else{
          temp_data <- list(
            mean = mean(data$y[1:i]),
            N    = length(data$y[1:i]),
            SD   = data$SD
          )
        }

        temp_results <- .dataCustomGaussianLS(
          temp_data,
          options[["priors"]][[h]],
          lCI = options[["plotsIterativeIntervalLower"]],
          uCI = options[["plotsIterativeIntervalUpper"]],
          NULL,
          "parameter"
        )

        temp_lines  <- rbind(temp_lines, data.frame(
          y    = temp_results$coverage,
          x    = i,
          name = options[["priors"]][[h]]$name
        ))

      }

      plot_data_lines <- c(plot_data_lines, list(temp_lines))

    }

    yName  <- bquote("P("~{.(options[["plotsIterativeIntervalLower"]])<=mu}<=.(options[["plotsIterativeIntervalUpper"]])~")")
    xName  <- gettext("Observation")

    p <- .plotIterativeLS(plot_data_lines, all_CI = NULL, xName = xName, yName = yName, palette = options[["colorPalette"]])


    plotsIterativeInterval$plotObject <- p
  }

  return()
}
.plotsIterativeIntervalStackedGaussianLS   <- function(jaspResults, data, ready, options){

  containerIterativeInterval <- .containerSequentialIntervalLS(jaspResults, options, "gauss_est")

  if(is.null(containerIterativeInterval[["plotsIterativeInterval"]])){

    plotsIterativeInterval <- createJaspContainer()

    plotsIterativeInterval$position <- 2
    plotsIterativeInterval$dependOn(c(.GaussianLS_data_dependencies,
                                      "plotsIterativeIntervalLower", "plotsIterativeIntervalUpper", "colorPalette"))
    containerIterativeInterval[["plotsIterativeInterval"]] <- plotsIterativeInterval


    if(all(!ready) || (ready[1] && !ready[2])){

      plotsIterativeInterval[[""]] <- createJaspPlot(title = "", width = 530, height = 400, aspectRatio = 0.7)
      return()

    }else if(!ready[1] && ready[2]){

      for(i in 1:length(options[["priors"]])){
        plotsIterativeInterval[[options[["priors"]][[i]]$name]] <- createJaspPlot(title = options[["priors"]][[i]]$name,
                                                                                  width = 530, height = 400, aspectRatio = 0.7)
      }
      return()

    }else{

      for(i in 1:length(options[["priors"]])){

        temp_plot <- createJaspPlot(title = options[["priors"]][[i]]$name, width = 530, height = 400, aspectRatio = 0.7)

        plotsIterativeInterval[[options[["priors"]][[i]]$name]] <- temp_plot

        all_lines  <- c()
        all_arrows <- c()
        legend     <- NULL

        # too many iterations crashes JASP
        if(length(data$y) > 10){
          iter_sequence <- round(seq(0, length(data$y), length.out = 10))
        }else{
          iter_sequence <- 0:length(data$y)
        }
        iter_sequence <- rev(iter_sequence)

        range <- NULL
        for(iteration in iter_sequence){
          if(iteration < 1){
            range <- rbind(range, .rangeGaussianLS(NULL, options[["priors"]][[i]]))
          }else{
            temp_data <- list(
              mean = mean(data$y[1:iteration]),
              N    = length(data$y[1:iteration]),
              SD   = data$SD
            )
            range <- rbind(range, .rangeGaussianLS(temp_data, options[["priors"]][[i]]))
          }
        }
        range <- c(min(range[,1]), max(range[,2]))

        for(iteration in iter_sequence){

          if(options[["priors"]][[i]]$type == "spike"){

            dfArrowPP   <- .dataArrowGaussianLS(options[["priors"]][[i]])
            dfArrowPP$g <- as.character(iteration)

            all_arrows  <- c(all_arrows, list(dfArrowPP))
            legend      <- rbind(legend, c(options[["priors"]][[i]]$type, iteration))

          }else if(options[["priors"]][[i]]$type == "normal"){

            if(iteration < 1){
              temp_data <- NULL
            }else{
              temp_data <- list(
                mean = mean(data$y[1:iteration]),
                N    = length(data$y[1:iteration]),
                SD   = data$SD
              )
            }

            dfLinesPP   <- .dataLinesGaussianLS(temp_data, options[["priors"]][[i]], "parameter", NULL, range)
            dfLinesPP   <- dfLinesPP[dfLinesPP$g == "Posterior",]
            dfLinesPP$g <- as.character(iteration)

            all_lines   <- c(all_lines, list(dfLinesPP))
            legend      <- rbind(legend, c(options[["priors"]][[i]]$type, iteration))

          }

        }

        xName  <- bquote(.(gettext("Population mean"))~mu)

        temp_plot$plotObject <- .plotStackedLS(all_lines, all_arrows, legend, xName = xName, xRange = range,
                                               lCI = options[["plotsIterativeIntervalLower"]],
                                               uCI = options[["plotsIterativeIntervalUpper"]])
      }
    }
  }

  return()
}
.tableIterativeGaussianLS          <- function(jaspResults, data, ready, options){

  containerIterative <- .containerSequentialPointLS(jaspResults, options, "gauss_est")

  if(is.null(containerIterative[["tableIterative"]])){

    tableIterative <- createJaspTable()

    tableIterative$position <- 3
    tableIterative$dependOn(c(.GaussianLS_data_dependencies, "plotsIterativeCenter",
                              "plotsIterativeIndividualCI", "plotsIterativeCoverage", "colorPalette", "plotsIterativeUpdatingTable"))
    containerIterative[["tableIterative"]] <- tableIterative

    tableIterative$addColumnInfo(name = "iteration", title = gettext("Observation"), type = "integer")
    if(ready[2]){
      if(options[["plotsIterativeIndividualCI"]]){
        if(options[["plotsIterativeIndividualType"]] == "central"){
          CI_title <- gettextf("%i %% CI", options[["plotsIterativeCoverage"]]*100)
        }else if(options[["plotsIterativeIndividualType"]] == "HPD"){
          CI_title <- gettextf("%i %% HPD", options[["plotsIterativeCoverage"]]*100)
        }else if(options[["plotsIterativeIndividualType"]] == "support"){
          CI_title <- gettextf("SI (BF=%s)", options[["plotsIterativeBF"]])
        }
        for(i in 1:length(options[["priors"]])){
          tableIterative$addColumnInfo(
            name  = paste(options[["priors"]][[i]]$name,"center", sep = "_"),
            title = ifelse(options[["plotsIterativeCenter"]] == "mean", gettext("Mean"), gettext("Median")),
            overtitle = options[["priors"]][[i]]$name,
            type = "number")
          tableIterative$addColumnInfo(
            name  = paste(options[["priors"]][[i]]$name,"CI", sep = "_"),
            title = CI_title,
            overtitle = options[["priors"]][[i]]$name,
            type = "string")
        }
      }else{
        for(i in 1:length(options[["priors"]])){
          tableIterative$addColumnInfo(
            name  = paste(options[["priors"]][[i]]$name,"center", sep = "_"),
            title = options[["priors"]][[i]]$name,
            type = "number")
        }
      }
    }


    if(!all(ready)){
      return()
    }


    iter_seq <- 0:length(data$y)


    for(i in iter_seq){

      temp_row     <- list()
      temp_row[["iteration"]] <- i

      if(i < 1){
        temp_data <- NULL
      }else{
        temp_data <- list(
          mean = mean(data$y[1:i]),
          N    = length(data$y[1:i]),
          SD   = data$SD
        )
      }

      for(h in 1:length(options[["priors"]])){

        temp_results <- .estimateGaussianLS(temp_data, options[["priors"]][[h]])
        temp_row[[paste(options[["priors"]][[h]]$name,"center", sep = "_")]] <- temp_results[[options[["plotsIterativeCenter"]]]]

        if(options[["plotsIterativeIndividualCI"]]){

          if(options[["plotsIterativeIndividualType"]] %in% c("central", "HPD")){

            temp_CIPP <- .dataCentralGaussianLS(
              temp_data,
              options[["priors"]][[h]],
              options[["plotsIterativeCoverage"]],
              type = "parameter"
            )

            if(options[["plotsIterativeIndividualType"]] == "HPD"){
              temp_CIPP$g <- "HPD"
            }

          }else if(options[["plotsIterativeIndividualType"]] == "support"){

            temp_CIPP <- .dataSupportGaussianLS(
              temp_data,
              options[["priors"]][[h]],
              options[["plotsIterativeBF"]]
            )
          }

          if(all(is.na(temp_CIPP[1:2]))){
            temp_int <- "∅"
          }else{
            temp_int <- sapply(1:nrow(temp_CIPP), function(i)paste(c(
              "[",format(round(temp_CIPP$x_start[i], 3), nsmall = 3),", ",format(round(temp_CIPP$x_end[i], 3), nsmall = 3),"]"
            ), collapse = ""))
            temp_int <- paste(temp_int, collapse = " and " )

            temp_row[[paste(options[["priors"]][[h]]$name,"CI", sep = "_")]] <- temp_int
          }

        }

      }

      tableIterative$addRows(temp_row)

    }
  }

  return()
}
.tableIterativeIntervalGaussianLS  <- function(jaspResults, data, ready, options){

  containerIterativeInterval <- .containerSequentialIntervalLS(jaspResults, options, "gauss_est")

  if(is.null(containerIterativeInterval[["tableIterativeInterval"]])){

    tableIterativeInterval <- createJaspTable()

    tableIterativeInterval$position <- 3
    tableIterativeInterval$dependOn(c(.GaussianLS_data_dependencies,
                                      "plotsIterativeIntervalLower", "plotsIterativeIntervalUpper", "plotsIterativeIntervalUpdatingTable"))
    containerIterativeInterval[["tableIterativeInterval"]] <- tableIterativeInterval

    tableIterativeInterval$addColumnInfo(name = "iteration", title = gettext("Observation"), type = "integer")
    if(ready[2]){
      for(i in 1:length(options[["priors"]])){
        tableIterativeInterval$addColumnInfo(
          name  = paste(options[["priors"]][[i]]$name,"center", sep = "_"),
          title = options[["priors"]][[i]]$name,
          type = "number")
      }
    }


    if(!all(ready)){
      return()
    }


    iter_seq <- 0:length(data$y)


    for(i in iter_seq){

      temp_row     <- list()
      temp_row[["iteration"]] <- i

      if(i < 1){
        temp_data <- NULL
      }else{
        temp_data <- list(
          mean = mean(data$y[1:i]),
          N    = length(data$y[1:i]),
          SD   = data$SD
        )
      }

      for(h in 1:length(options[["priors"]])){

        temp_results <- .dataCustomGaussianLS(
          temp_data,
          options[["priors"]][[h]],
          lCI = options[["plotsIterativeIntervalLower"]],
          uCI = options[["plotsIterativeIntervalUpper"]],
          NULL,
          type = "parameter"
        )

        temp_row[[paste(options[["priors"]][[h]]$name,"center", sep = "_")]] <- temp_results$coverage

      }

      tableIterativeInterval$addRows(temp_row)

    }
  }

  return()
}

.tablepredictionsGaussianLS        <- function(jaspResults, data, ready, options){

  containerPredictions <- .containerPredictionsLS(jaspResults, options, "gauss_est")

  if(is.null(containerPredictions[["predictionsTable"]])){

    predictionsTable <- createJaspTable()

    predictionsTable$position <- 2
    predictionsTable$dependOn(c(.GaussianLS_data_dependencies, "predictionN"))

    predictionsTable$addColumnInfo(name = "hypothesis",     title = gettext("Model"),                     type = "string")
    predictionsTable$addColumnInfo(name = "posterior",      title = gettextf("Posterior (%s)", "\u03BC"), type = "string")
    predictionsTable$addColumnInfo(name = "posteriorMean",  title = gettext("Posterior Mean"),            type = "number")
    predictionsTable$addColumnInfo(name = "predictive",     title = gettext("Prediction"),                type = "string")
    predictionsTable$addColumnInfo(name = "predictiveMean", title = gettext("Prediction Mean"),           type = "number")
    predictionsTable$setExpectedSize(length(options[["priors"]]))

    containerPredictions[["predictionsTable"]] <- predictionsTable

    if(!ready[2] || is.null(data$SD) || data$SD == 0){

      # TODO: check whether this works properly
      if(is.null(data$SD) || data$SD == 0){
        predictionsTable$setError(gettext("Please, specify the standard deviation of the data."))
      }

      return()

    }else{

      # add rows for each hypothesis
      for(i in 1:length(options[["priors"]])){

        temp_results    <- .estimateGaussianLS(data, options[["priors"]][[i]])
        temp_prediction <- .predictGaussianLS(data, options[["priors"]][[i]], options, options[["predictionN"]])

        temp_row <- list(
          hypothesis      = options[["priors"]][[i]]$name,

          posterior       = temp_results$distribution,
          posteriorMean   = temp_results$mean,
          predictive      = temp_prediction$distribution,
          predictiveMean  = temp_prediction$mean
        )


        predictionsTable$addRows(temp_row)
      }

      # add footnote clarifying what dataset was used
      predictionsTable$addFootnote(gettextf(
        "The prediction for %s future %s is based on %s.",
        options[["predictionN"]],
        ifelse(options[["predictionN"]] == 1, gettext("observation"),gettext("observations")),
        if(is.null(data)) gettext("prior") else gettextf(
          "%s past %s",
          data$N,
          ifelse(data$N == 1, gettext("observation"), gettext("observations"))
        )
      ))

    }
  }

  return()
}
.plotsPredictionsIndividualGaussianLS      <- function(jaspResults, data, ready, options){

  containerPredictionPlots <- .containerPredictionPlotsLS(jaspResults, options, "gauss_est")

  if(is.null(containerPredictionPlots[["plotsPredictions"]])){

    plotsPredictions <- createJaspContainer()

    plotsPredictions$position <- 2
    plotsPredictions$dependOn(c(.GaussianLS_data_dependencies, "predictionN",
                                "plotsPredictionCI", "plotsPredictionCoverage",
                                "plotsPredictionLower", "plotsPredictionUpper","predictionPlotProp"))

    containerPredictionPlots[["plotsPredictions"]] <- plotsPredictions


    if(!ready[2] || is.null(data$SD) || data$SD == 0){

      temp_p <- createJaspPlot(title = "", width = 530, height = 400, aspectRatio = 0.7)

      if(is.null(data$SD) || data$SD == 0){
        temp_p$setError(gettext("Please, specify the standard deviation of the data."))
      }

      plotsPredictions[[""]] <- temp_p

      return()

    }else{

      for(i in 1:length(options[["priors"]])){

        temp_plot <- createJaspPlot(title = options[["priors"]][[i]]$name, width = 530, height = 400, aspectRatio = 0.7)

        plotsPredictions[[options[["priors"]][[i]]$name]] <- temp_plot

        yName  <- gettext("Density")
        if(options[["predictionPlotProp"]]){
          xName  <- gettext("Sample means")
        }else{
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

        if(options[["plotsPredictionCI"]]){

          if(options[["plotsPredictionType"]] %in% c("central","HPD")){

            dfCI <- .dataCentralGaussianLS(
              data,
              options[["priors"]][[i]],
              options[["plotsPredictionCoverage"]],
              options[["predictionN"]],
              "prediction"
            )

            if(options[["plotsPredictionType"]] == "HPD"){
              dfCI$g <- "HPD"
            }
          }else if(options[["plotsPredictionType"]] == "custom"){

            dfCI <- .dataCustomGaussianLS(
              data,
              options[["priors"]][[i]],
              options[["plotsPredictionLower"]],
              options[["plotsPredictionUpper"]],
              options[["predictionN"]],
              "prediction"
            )

          }
        }

        if(options[["predictionPlotProp"]]){
          dfLinesPP   <- .dataLinesPredGaussianLS(
            data,
            options[["priors"]][[i]],
            "prediction",
            options[["predictionN"]],
            range = range)
          if(options[["plotsPredictionCI"]]){
            temp_m <- ifelse(options[["priors"]][[i]][["type"]] == "spike", options[["priors"]][[i]][["parPoint"]], options[["priors"]][[i]][["parMu"]])
            dfCI$x_start <- temp_m - (temp_m - dfCI$x_start)/sqrt(options[["predictionN"]])
            dfCI$x_end   <- temp_m + (dfCI$x_end - temp_m)/sqrt(options[["predictionN"]])
          }
        }else{

          dfLinesPP  <- .dataLinesGaussianLS(data, options[["priors"]][[i]], "prediction", options[["predictionN"]], range = range)
          dfLinesPP  <- dfLinesPP[dfLinesPP$g == "Posterior",]

        }

        p <- .plotIndividualLS(dfLinesPP, NULL, dfCI, NULL, NULL, range, xName, yName)

        temp_plot$plotObject <- p
      }
    }
  }

  return()
}
.plotsPredictionsGaussianLS        <- function(jaspResults, data, ready, options){

  containerPredictionPlots <- .containerPredictionPlotsLS(jaspResults, options, "gauss_est")

  if(is.null(containerPredictionPlots[["plotsPredictions"]])){

    plotsPredictions <- createJaspPlot(width = 530, height = 400, aspectRatio = 0.7)

    plotsPredictions$position <- 2
    plotsPredictions$dependOn(c(.GaussianLS_data_dependencies, "predictionN",
                                "colorPalettePrediction", "predictionPlotProp"))

    containerPredictionPlots[["plotsPredictions"]] <- plotsPredictions


    if(!ready[2] || is.null(data$SD) || data$SD == 0){

      if(is.null(data$SD) || data$SD == 0){
        plotsPredictions$setError(gettext("Please, specify the standard deviation of the data."))
      }

      return()

    }else{

      range <- .rangeGaussiansLS(
        data,
        options[["priors"]],
        "prediction",
        options[["predictionN"]]
      )

      if(options[["predictionPlotProp"]]){
        xName  <- gettext("Sample means")
      }else{
        xName  <- gettext("Future data")
      }

      all_lines  <- c()
      legend     <- NULL

      for(i in 1:length(options[["priors"]])){

        if(options[["predictionPlotProp"]]){
          dfLinesPP   <- .dataLinesPredGaussianLS(
            data,
            options[["priors"]][[i]],
            "prediction",
            options[["predictionN"]],
            range = range)
        }else{

          dfLinesPP  <- .dataLinesGaussianLS(data, options[["priors"]][[i]], "prediction", options[["predictionN"]], range = range)
          dfLinesPP  <- dfLinesPP[dfLinesPP$g == "Posterior",]

        }

        dfLinesPP$g <- options[["priors"]][[i]]$name

        # it's not beta, but I'm lazzy to rewrite a function I wanna use
        # lol, I was so lazy that I even coppied this comment from the binomial version :D
        legend   <- rbind(legend, c("beta", options[["priors"]][[i]]$name))
        all_lines<- c(all_lines, list(dfLinesPP))
      }

      if(options[["predictionPlotType"]] == "overlying"){
        p <- .plotOverlyingLS(all_lines, NULL, xName = xName, yName = yName, xRange = range, discrete = FALSE,
                              palette = options[["colorPalettePrediction"]], proportions = options[["predictionPlotProp"]])
      }else{
        p <- .plotStackedLS(all_lines, NULL, legend, xName = xName, xRange = range,
                            discrete = FALSE, proportions = options[["predictionPlotProp"]])
      }

      plotsPredictions$plotObject <- p
    }
  }

  return()
}
