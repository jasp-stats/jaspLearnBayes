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



LSbinomialestimation   <- function(jaspResults, dataset, options, state = NULL){

  # a vector of two, first for data, second for hypotheses
  ready <- .readyBinomialLS(options)
  
  # evaluate the expressions in priors
  if(ready[2])options$priors <- .evaluate_priors(options$priors)
  
  # load, check, transform and process data
  if(ready[1])data <- .readDataBinomialLS(dataset, options)
  
  # data summary table if requested (but not if the data counts were added directly)
  if(options$dataSummary & !options$dataType == "dataCounts").summaryBinomialLS(jaspResults, data, ready)
  
  
  ### inference 
  # estimated parameter values
  .estimatesBinomialLS(jaspResults, data, ready, options)
  
  # prior
  if(options$plotsPrior){
    if(options$plotsPriorType != "individual").plotsSimpleBinomialLS(jaspResults, data, ready, options, type = "Prior")
    if(options$plotsPriorType == "individual").plotsIndividualBinomialLS(jaspResults, data, ready, options, type = "Prior")
  }
  
  # posterior
  if(options$plotsPosterior){
    if(options$plotsPosteriorType != "individual").plotsSimpleBinomialLS(jaspResults, data, ready, options, type = "Posterior")
    if(options$plotsPosteriorType == "individual").plotsIndividualBinomialLS(jaspResults, data, ready, options, type = "Posterior")
  }
  
  # prior and posterior
  if(options$plotsBoth).plotsBothBinomialLS(jaspResults, data, ready, options)
  
  ### sequential analysis
  # point estimate
  if(options$plotsIterative){
    if(options$plotsIterativeType == "overlying").plotsIterativeOverlyingBinomialLS(jaspResults, data, ready, options)
    if(options$plotsIterativeType == "stacked").plotsIterativeStackedBinomialLS(jaspResults, data, ready, options)
  }
  
  # point estimate table
  if(options$plotsIterative & options$plotsIterativeUpdatingTable).tableIterativeBinomialLS(jaspResults, data, ready, options)
  
  # interval
  if(options$plotsIterativeInterval){
    if(options$plotsIterativeIntervalType == "overlying").plotsIterativeIntervalOverlyingBinomialLS(jaspResults, data, ready, options)
    if(options$plotsIterativeIntervalType == "stacked").plotsIterativeIntervalStackedBinomialLS(jaspResults, data, ready, options)
  }
  
  # interval estimate table
  if(options$plotsIterativeInterval & options$plotsIterativeIntervalUpdatingTable).tableIterativeIntervalBinomialLS(jaspResults, data, ready, options)
  
  # posterior updating table
  if(options$doIterative & options$dataType != "dataCounts").estimatesSequentialBinomialLS(jaspResults, data, ready, options)
  
  
  ### prediction
  if(options$predictionTable).predictionsBinomialLS(jaspResults, data, ready, options)
  
  # plot
  if(options$plotsPredictions){
    if(options$predictionPlotType != "individual").plotsPredictionsBinomialLS(jaspResults, data, ready, options)
    if(options$predictionPlotType == "individual").plotsPredictionsIndividualBinomialLS(jaspResults, data, ready, options)
  }
  
  return()
}

# main functions
.evaluate_priors       <- function(priors){
  for(p in 1:length(priors)){
    for(i in 1:length(priors[[p]])){
      temp_p <- priors[[p]][[i]]
      if(names(priors[[p]])[i] %in% c("parAlpha", "parBeta", "parPoint", "PH")){
        priors[[p]][[paste(names(priors[[p]])[i],"inp", sep = "_")]] <- priors[[p]][[i]]
        priors[[p]][[i]] <- eval(parse(text = priors[[p]][[i]]))
      }
    }
  }
  return(priors)
}
.scale_priors          <- function(priors){
  unscaled <- sapply(priors, function(x)x$PH)
  scaled   <- unscaled/sum(unscaled)
  for(i in 1:length(priors)){
    priors[[i]]$PH <- scaled[i]
  }
  return(priors)
}
.readyBinomialLS       <- function(options){
  # are data ready
  if(options$dataType == "dataCounts"){
    
    ready <- TRUE
    
  }else if(options$dataType == "dataSequence"){
    
    if(nchar(options$data_sequence) > 0){
      
      if(length(options$key_success_Seq) == 0){
        ready <- FALSE
      }else{
        ready <- TRUE
      }
      
    }else{
      ready <- TRUE
    }
    
  }else if(options$dataType == "dataVariable"){
    
    if(options$selectedVariable != ""){
      
      if(length(options$key_success_Var) == 0){
        ready <- FALSE
      }else{
        ready <- TRUE
      }
      
    }else{
      ready <- TRUE
    }
    
  }
  
  # are priors ready
  ready <- c(ready, length(options[["priors"]]) > 0)
  
  return(ready)
}
.readDataBinomialLS    <- function(dataset, options){
  
  data <- list()
  
  if(options$dataType == "dataCounts"){
    
    data$y <- NULL
    data$nSuccesses <- options$nSuccesses
    data$nFailures  <- options$nFailures
    
  }else{
    
    if((options$dataType == "dataVariable" & options$selectedVariable == "") |
       (options$dataType == "dataSequence" & options$data_sequence == "")){
      
      data$y <- NULL
      
    }else{
      
      if(options$dataType == "dataSequence"){
        
        temp_y <- options$data_sequence
        temp_y <- gsub(",", "\n", temp_y)
        temp_y <- gsub(";", "\n", temp_y)
        temp_y <- unlist(strsplit(temp_y, split = "\n"))
        temp_y <- trimws(temp_y, which = c("both"))
        temp_y <- temp_y[temp_y != ""]
        
      }else if(options$dataType == "dataVariable"){
        
        # this is stupidly written #rework
        if (!is.null(dataset)){
          temp_y <- dataset
        }else{
          temp_y <- .readDataSetToEnd(columns = options$selectedVariable)[,1]
        }
        
      }
      
      data$y <- .cleanDataBinomialLS(temp_y, options)
      
    }
    
    data$nSuccesses <- sum(data$y == 1)
    data$nFailures  <- sum(data$y == 0)
    
  } 
  
  return(data)
  
}
.cleanDataBinomialLS   <- function(x, options){
  
  # doubling the menu allows to store the keys while user switches between different input methods
  if(options$dataType == "dataSequence"){
    key_success <- options$key_success_Seq
    key_failure <- options$key_failure_Seq
  }else{
    key_success <- options$key_success_Var
    key_failure <- options$key_failure_Var
  }
  
  x <- na.omit(x)
  x <- as.character(x)
  
  # treat everything else then success as a failure if only successes are supplied
  if(length(key_failure) == 0){
    
    temp_ks <- x %in% key_success
    
    x[temp_ks]  <- 1
    x[!temp_ks] <- 0
    
  }else{
    # use only variables specified in successes or failures
    
    x <- x[x %in% c(key_success, key_failure)]
    
    temp_ks <- x %in% key_success
    temp_kf <- x %in% key_failure
    
    x[temp_ks] <- 1
    x[temp_kf] <- 0
    
  }
  
  return(as.numeric(x))
}
.summaryBinomialLS     <- function(jaspResults, data, ready){
  summaryTable <- createJaspTable(title = "Data Summary")
  
  summaryTable$position <- 1
  summaryTable$dependOn(c("dataSummary", .BinomialLS_data_dependencies))
  
  summaryTable$addColumnInfo(name = "variable",   title = "",            type = "string")
  summaryTable$addColumnInfo(name = "counts",     title = "Counts",      type = "integer")
  summaryTable$addColumnInfo(name = "proportion", title = "Proportion",  type = "number")
  
  summaryTable$setExpectedSize(3)
  
  jaspResults[["summaryTable"]] <- summaryTable
  
  if(ready[1]){
    summaryTable$addRows(list(variable   = "Successes", 
                              counts     = data$nSuccesses, 
                              proportion = ifelse(is.nan(data$nSuccesses / (data$nSuccesses + data$nFailures)), "",
                                                  data$nSuccesses / (data$nSuccesses + data$nFailures))))
    summaryTable$addRows(list(variable   = "Failures",
                              counts     = data$nFailures, 
                              proportion = ifelse(is.nan(data$nFailures / (data$nSuccesses + data$nFailures)), "",
                                                  data$nFailures / (data$nSuccesses + data$nFailures))))
    summaryTable$addRows(list(variable   = "Total",
                              counts     = data$nSuccesses + data$nFailures, 
                              proportion = "")) #ifelse(is.nan((data$nSuccesses + data$nFailures) / (data$nSuccesses + data$nFailures)), "",
    #        (data$nSuccesses + data$nFailures) / (data$nSuccesses + data$nFailures))))
  }
  
  return()
}
.estimatesBinomialLS   <- function(jaspResults, data, ready, options){
  estimatesTable <- createJaspTable(title = "Estimation Summary")
  
  estimatesTable$position <- 2
  estimatesTable$dependOn(.BinomialLS_data_dependencies)
  
  estimatesTable$addColumnInfo(name = "hypothesis",   title = "Model",           type = "string")
  estimatesTable$addColumnInfo(name = "prior",        title = "Prior (θ)",       type = "string")
  estimatesTable$addColumnInfo(name = "priorMed",     title = "Prior Median",    type = "number")
  estimatesTable$addColumnInfo(name = "posterior",    title = "Posterior (θ)",   type = "string")
  estimatesTable$addColumnInfo(name = "posteriorMed", title = "Posterior Median",type = "number")
  
  estimatesTable$setExpectedSize(length(options$priors))
  
  jaspResults[["estimatesTable"]] <- estimatesTable
  
  if(ready[1] & !ready[2]){
    
    return()
    
  }else if(!ready[1]){
    
    jaspResults[["estimatesTable"]]$setError("Please specify successes and failures.")
    
  }else if(ready[2]){
    
    # add rows for each hypothesis
    for(i in 1:length(options$priors)){
      # add mock data to use only priors
      temp_data <- list(
        "nSuccesses" = 0,
        "nFailures"  = 0
      )
      temp_results <- .estimateBinomialLS(temp_data, options$priors[[i]])
      
      temp_row <- list(
        prior        = temp_results$distribution,
        priorMed     = temp_results$median,
        hypothesis   = options$priors[[i]]$name, 
        posterior    = "", 
        posteriorMed = "")
      
      
      if(all(ready)){
        # and when real data are supplied as well, add posterior information
        temp_results <- .estimateBinomialLS(data, options$priors[[i]])
        
        temp_row["posterior"]    <- temp_results$distribution
        temp_row["posteriorMed"] <- temp_results$median
        
      }
      
      estimatesTable$addRows(temp_row)
    }
    
    # add footnote clarifying what dataset was used
    estimatesTable$addFootnote(paste0("These results are based on ", data$nSuccesses," ", ifelse(data$nSuccesses == 1, "success", "successes"),
                                      "  and ", data$nFailures, " ",ifelse(data$nFailures == 1, "failure", "failures"), "."))
    
  }
  
}
.estimatesSequentialBinomialLS <- function(jaspResults, data, ready, options){
  estimatesSequentialTable <- createJaspTable(title = "Sequential Posterior Updating")
  
  estimatesSequentialTable$position <- 10
  estimatesSequentialTable$dependOn(c("doIterative", .BinomialLS_data_dependencies))
  
  estimatesSequentialTable$addColumnInfo(name = "iteration", title = "Observations", type = "integer")
  jaspResults[["estimatesSequentialTable"]] <- estimatesSequentialTable
  
  
  estimatesSequentialTable$setExpectedSize(ifelse(ready[1], length(data$y) + 1, 1))
  if(ready[2]){
    for(i in 1:length(options$priors)){
      estimatesSequentialTable$addColumnInfo(
        name  = options$priors[[i]]$name,  
        title = options$priors[[i]]$name,
        type = "string")
    }
  }
  
  
  if(!all(ready)){
    return()
  }else{
    # add priors to the first row
    temp_row <- NULL
    temp_row[["iteration"]] <- 0
    for(h in 1:length(options$priors)){
      temp_data    <- list(
        nSuccesses = 0,
        nFailures  = 0
      )
      temp_results <- .estimateBinomialLS(temp_data, options$priors[[h]])
      temp_row[[options$priors[[h]]$name]] <- temp_results$distribution
    }
    estimatesSequentialTable$addRows(temp_row)
    
    # then update the posteriors as the data go in
    if(length(data$y) > 0){
      for(i in 1:length(data$y)){
        temp_row <- NULL
        temp_row[["iteration"]] <- i
        for(h in 1:length(options$priors)){
          temp_data    <- list(
            nSuccesses = sum(data$y[1:i] == 1),
            nFailures  = sum(data$y[1:i] == 0)
          )
          temp_results <- .estimateBinomialLS(temp_data, options$priors[[h]])
          temp_row[[options$priors[[h]]$name]] <- temp_results$distribution
        }
        estimatesSequentialTable$addRows(temp_row)
      }
    }
    
  }
}
.plotsSimpleBinomialLS <- function(jaspResults, data, ready, options, type = c("Prior", "Posterior")){
  
  plotsSimple <- createJaspPlot(title = paste0(type, " Plots"), width = 530, height = 400, aspectRatio = 0.7)
  
  plotsSimple$position <- ifelse(type == "Prior", 3, 4)
  plotsSimple$dependOn(c(.BinomialLS_data_dependencies,
                         ifelse(type == "Prior", "plotsPrior", "plotsPosterior"),
                         ifelse(type == "Prior", "plotsPriorType", "plotsPosteriorType"),
                         ifelse(options[[ifelse(type == "Prior", "plotsPriorType", "plotsPosteriorType")]] == "overlying",
                                "colorPalette", "")))
  
  jaspResults[[paste0("plots",type,"simple")]] <- plotsSimple
  
  if (!all(ready))return()
  
  all_lines  <- c()
  all_arrows <- c()
  legend     <- NULL
  for(i in 1:length(options$priors)){
    
    if(options$priors[[i]]$type == "spike"){
      
      dfArrowPP   <- .dataArrowPPLS(options$priors[[i]])
      dfArrowPP$g <- options$priors[[i]]$name
      
      all_arrows  <- c(all_arrows, list(dfArrowPP))
      legend      <- rbind(legend, c(options$priors[[i]]$type, options$priors[[i]]$name))
      
    }else if(options$priors[[i]]$type == "beta"){
      
      dfLinesPP   <- .dataLinesPPLS(data, options$priors[[i]])
      dfLinesPP   <- dfLinesPP[dfLinesPP$g == type,]
      dfLinesPP$g <- options$priors[[i]]$name
      
      all_lines   <- c(all_lines, list(dfLinesPP))
      legend      <- rbind(legend, c(options$priors[[i]]$type, options$priors[[i]]$name))
      
    }
  }
  
  xName  <- bquote("Population proportion"~theta)
  
  if(options[[ifelse(type == "Prior", "plotsPriorType", "plotsPosteriorType")]] == "overlying"){
    p <- .plotOverlyingLS(all_lines, all_arrows, xName = xName, palette = options$colorPalette)
  }else{
    p <- .plotStackedLS(all_lines, all_arrows, legend, xName = xName)
  }
  
  jaspResults[[paste0("plots",type,"simple")]]$plotObject <- p
  
  return()
}
.plotsBothBinomialLS   <- function(jaspResults, data, ready, options){
  
  plotsBoth <- createJaspContainer(title = "Prior and Posterior Plots")
  
  plotsBoth$position <- 5
  plotsBoth$dependOn(c(.BinomialLS_data_dependencies, "plotsBoth", "plotsBothSampleProportion"))
  
  jaspResults[["plotsBoth"]] <- plotsBoth
  
  
  if(all(!ready) | (ready[1] & !ready[2])){
    
    plotsBoth[[""]] <- createJaspPlot(title = "", width = 530, height = 400, aspectRatio = 0.7)
    return()
    
  }else if(!ready[1] & ready[2]){
    
    for(i in 1:length(options$priors)){
      plotsBoth[[options$priors[[i]]$name]] <- createJaspPlot(title = options$priors[[i]]$name,
                                                              width = 530, height = 400, aspectRatio = 0.7)
    }
    return()
    
  }else{
    
    for(i in 1:length(options$priors)){
      
      temp_plot <- createJaspPlot(title = options$priors[[i]]$name, width = 530, height = 400, aspectRatio = 0.7)
      
      plotsBoth[[options$priors[[i]]$name]] <- temp_plot
      
      dfArrowPP <- NULL
      dfLinesPP <- NULL
      
      xName  <- bquote("Population proportion"~theta)
      
      if(options$priors[[i]]$type == "spike"){
        dfArrowPP  <- .dataArrowPPLS(options$priors[[i]])
      }else if(options$priors[[i]]$type == "beta"){
        dfLinesPP  <- .dataLinesPPLS(data, options$priors[[i]])
        
        if(all(dfLinesPP$y[dfLinesPP$g == "Prior"] == dfLinesPP$y[dfLinesPP$g == "Posterior"])){
          dfLinesPP   <- dfLinesPP[dfLinesPP$g == "Posterior",]
          dfLinesPP$g <- "Prior = Posterior"
        }
        
      }
      
      if(options$plotsBothSampleProportion){
        dfPointsPP <- .dataProportionPPLS(data)
        if(is.nan(dfPointsPP$x))dfPointsPP <- NULL
      }else{
        dfPointsPP <- NULL 
      }
      
      p <- .plotPriorPosteriorLS(list(dfLinesPP), list(dfArrowPP), dfPoints = dfPointsPP, xName = xName)
      temp_plot$plotObject <- p
    }
    
    return()
  }
  
  
}
.plotsIndividualBinomialLS     <- function(jaspResults, data, ready, options, type = c("Prior", "Posterior")){
  
  plotsIndividual <- createJaspContainer(title = paste0(type, " Plots"))
  
  plotsIndividual$position <- ifelse(type == "Prior", 3, 4)
  plotsIndividual$dependOn(c(.BinomialLS_data_dependencies,
                             ifelse(type == "Prior", "plotsPrior",             "plotsPosterior"),
                             ifelse(type == "Prior", "plotsPriorType",         "plotsPosteriorType"),
                             ifelse(type == "Prior", "plotsPriorIndividualCI", "plotsPosteriorIndividualCI"),
                             ifelse(type == "Prior", "plotsPriorCoverage",     "plotsPosteriorCoverage"),
                             ifelse(type == "Prior", "plotsPriorLower",        "plotsPosteriorLower"),
                             ifelse(type == "Prior", "plotsPriorUpper",        "plotsPosteriorUpper")))
  
  jaspResults[[paste0("plots",type,"individual")]] <- plotsIndividual
  
  
  if(all(!ready) | (ready[1] & !ready[2])){
    
    plotsIndividual[[""]] <- createJaspPlot(title = "", width = 530, height = 400, aspectRatio = 0.7)
    return()
    
  }else if(!ready[1] & ready[2]){
    
    for(i in 1:length(options$priors)){
      plotsIndividual[[options$priors[[i]]$name]] <- createJaspPlot(title = options$priors[[i]]$name,
                                                                    width = 530, height = 400, aspectRatio = 0.7)
    }
    return()
    
  }else{
    
    if(type == "Prior"){
      temp_data <- list(
        nSuccesses = 0,
        nFailures = 0
      )
    }else{
      temp_data <- data
    }
    
    for(i in 1:length(options$priors)){
      
      temp_plot <- createJaspPlot(title = options$priors[[i]]$name, width = 530, height = 400, aspectRatio = 0.7)
      
      plotsIndividual[[options$priors[[i]]$name]] <- temp_plot
      
      xName  <- bquote("Population proportion"~theta)
      
      dfArrowPP   <- NULL
      dfLinesPP   <- NULL
      dfCI        <- NULL
      dfCILinesPP <- NULL
      
      if(options[[ifelse(type == "Prior", "plotsPriorIndividualCI", "plotsPosteriorIndividualCI")]]){
        
        if(options[[ifelse(type == "Prior", "plotsPriorIndividualType", "plotsPosteriorIndividualType")]] == "central"){
          
          dfCI <- .dataCentralPPLS(temp_data, options$priors[[i]], options[[ifelse(type == "Prior", "plotsPriorCoverage", "plotsPosteriorCoverage")]], type = "parameter")
          
        }else if(options[[ifelse(type == "Prior", "plotsPriorIndividualType", "plotsPosteriorIndividualType")]] == "HPD"){
          
          dfCI <- .dataHPDPPLS(temp_data, options$priors[[i]], options[[ifelse(type == "Prior", "plotsPriorCoverage", "plotsPosteriorCoverage")]], type = "parameter")
          
        }else if(options[[ifelse(type == "Prior", "plotsPriorIndividualType", "plotsPosteriorIndividualType")]] == "custom"){
          
          dfCI <- .dataCustomPPLS(temp_data, options$priors[[i]], options[[ifelse(type == "Prior", "plotsPriorLower", "plotsPosteriorLower")]],
                                  options[[ifelse(type == "Prior", "plotsPriorUpper", "plotsPosteriorUpper")]], type = "parameter")  
          
        }else if(options$plotsPosteriorIndividualType == "support"){
          
          dfCI <- .dataSupportPPLS(temp_data, options$priors[[i]], options$plotsPosteriorBF)  
          
        }
      }
      
      
      if(options$priors[[i]]$type == "spike"){
        
        dfArrowPP  <- .dataArrowPPLS(options$priors[[i]])
        
      }else if(options$priors[[i]]$type == "beta"){
        
        dfLinesPP  <- .dataLinesPPLS(data, options$priors[[i]])
        dfLinesPP  <- dfLinesPP[dfLinesPP$g == type,]
        
        if(!is.null(dfCI)){
          for(r in 1:nrow(dfCI)){
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
      
      p <- .plotIndividualLS(dfLinesPP, dfArrowPP, dfCI, dfCILinesPP, c(0,1), xName, nRound = 3)
      temp_plot$plotObject <- p
    }
    
    return()
  }
  
  
}
.plotsIterativeOverlyingBinomialLS <- function(jaspResults, data, ready, options){
  
  plotsIterative <- createJaspPlot(title = "Sequential Analysis: Point Estimate", width = 530, height = 400, aspectRatio = 0.7)
  
  plotsIterative$position <- 6
  plotsIterative$dependOn(c(.BinomialLS_data_dependencies, "plotsIterative", "plotsIterativeCenter",
                            "plotsIterativeIndividualCI", "plotsIterativeCoverage", "colorPalette"))
  
  
  if (!all(ready)){
    jaspResults[["plotsIterative"]] <- plotsIterative
    return()
  }
  
  plot_data_lines <- list()
  plot_data_CI    <- list()
  
  # then update the posteriors as the data go in
  for(h in 1:length(options$priors)){
    
    temp_lines   <- NULL
    temp_CI      <- NULL
    # for dealing with possible bimodal distributions from HPD
    CI_unimodal  <- TRUE
    temp_CI1     <- NULL
    temp_CI2     <- NULL
    
    # cheat for getting 2x 0 for the sequantial plot in case of no data
    if(length(data$y) == 0){
      iter_seq <- c(0, 0.1)
    }else{
      iter_seq <- 0:length(data$y)
    }
    
    for(i in iter_seq){
      
      temp_data    <- list(
        nSuccesses = sum(data$y[0:i] == 1),
        nFailures  = sum(data$y[0:i] == 0)
      )
      
      temp_results    <- .estimateBinomialLS(temp_data, options$priors[[h]])
      temp_lines      <- rbind(temp_lines, data.frame(
        y    = temp_results[[options$plotsIterativeCenter]],
        x    = i,
        name = options$priors[[h]]$name
      ))
      
      if(options$plotsIterativeIndividualCI){
        
        if(options$plotsIterativeIndividualType == "central"){
          temp_CIPP <- .dataCentralPPLS(temp_data, options$priors[[h]],
                                        options$plotsIterativeCoverage, type = "parameter")
        }else if(options$plotsIterativeIndividualType == "HPD"){
          
          temp_CIPP <- .dataHPDPPLS(temp_data, options$priors[[h]],
                                    options$plotsIterativeCoverage, type = "parameter")
          if(nrow(temp_CIPP) == 2)CI_unimodal <- FALSE
          
        }else if(options$plotsIterativeIndividualType == "support"){
          
          temp_CIPP <- .dataSupportPPLS(temp_data, options$priors[[h]],
                                        options$plotsIterativeBF)
          if(nrow(temp_CIPP) == 0)temp_CIPP <- NULL
          
        }
        
        if(nrow(temp_CIPP) == 1 & CI_unimodal){
          
          temp_CI <- rbind(temp_CI, data.frame(
            y1   = temp_CIPP$x_start,
            y2   = temp_CIPP$x_end,
            x    = i,
            name = options$priors[[h]]$name
          ))
          
        }else if(nrow(temp_CIPP) == 1 & !CI_unimodal){
          
          temp_CI <- rbind(
            temp_CI,
            data.frame(
              y1   = (temp_CIPP$x_start + temp_CIPP$x_end)/2,
              y2   = (temp_CIPP$x_start + temp_CIPP$x_end)/2,
              x    = i,
              name = temp_CI1$name
            ),
            data.frame(
              y1   = c(temp_CI1$y2, temp_CI1$y1),
              y2   = c(temp_CI2$y1, temp_CI2$y2),
              x    = rep(temp_CI1$x, 2),
              name = rep(temp_CI1$name, 2)
            ),
            data.frame(
              y1   = temp_CIPP$x_start,
              y2   = temp_CIPP$x_end,
              x    = i,
              name = options$priors[[h]]$name
            )
          )
          CI_unimodal <- TRUE
          
        }else if(nrow(temp_CIPP) == 2){
          
          temp_CI1 <- rbind(
            temp_CI1,
            data.frame(
              y1   = temp_CIPP$x_start[1],
              y2   = temp_CIPP$x_end[1],
              x    = i,
              name = options$priors[[h]]$name
            ))
          
          temp_CI2 <- rbind(
            temp_CI2,
            data.frame(
              y1   = temp_CIPP$x_start[2],
              y2   = temp_CIPP$x_end[2],
              x    = i,
              name = options$priors[[h]]$name
            ))
          
        }else if(nrow(temp_CIPP) > 2){
          stop("More than bimodal CIs are not implemented in the Sequential analysis plot.")
        }
      }
      
    }
    
    plot_data_lines <- c(plot_data_lines, list(temp_lines))
    
    # deal with a possibility of two disjoined CIs
    if(options$plotsIterativeIndividualCI){
      if(CI_unimodal){
        # deal with possible non-existing support intervals
        if(all(is.na(temp_CI[,c("y1", "y2")]))){
          plot_data_CI    <- c(plot_data_CI, list(NULL))          
        }else{
          plot_data_CI    <- c(plot_data_CI, list(temp_CI))
        }
      }else{
        plot_data_CI    <- c(plot_data_CI, list(temp_CI1), list(temp_CI2))
      }
    }
    
  }
  
  yName  <- bquote("Population proportion"~theta)
  xName  <- "Observations"
  
  p <- .plotIterativeLS(plot_data_lines, plot_data_CI, xName = xName, yName = yName, palette = options$colorPalette)
  
  
  plotsIterative$plotObject <- p
  #jaspResults[["plotsIterative"]]$plotObject <- p
  #plotsIterative$addFootnote("Posterior median and 95% CI")
  
  jaspResults[["plotsIterative"]] <- plotsIterative
  return()
}
.plotsIterativeStackedBinomialLS   <- function(jaspResults, data, ready, options){
  
  plotsIterative <- createJaspContainer(title = "Sequential Analysis: Point Estimate")
  
  plotsIterative$position <- 6
  plotsIterative$dependOn(c(.BinomialLS_data_dependencies, "plotsIterative"))
  
  jaspResults[["plotsIterative"]] <- plotsIterative
  
  
  if(all(!ready) | (ready[1] & !ready[2])){
    
    plotsIterative[[""]] <- createJaspPlot(title = "", width = 530, height = 400, aspectRatio = 0.7)
    return()
    
  }else if(!ready[1] & ready[2]){
    
    for(i in 1:length(options$priors)){
      plotsIterative[[options$priors[[i]]$name]] <- createJaspPlot(title = options$priors[[i]]$name,
                                                                   width = 530, height = 400, aspectRatio = 0.7)
    }
    return()
    
  }else{
    
    #options$priors[[i]]$name
    
    for(i in 1:length(options$priors)){
      
      temp_plot <- createJaspPlot(title = options$priors[[i]]$name, width = 530, height = 400, aspectRatio = 0.7)
      
      plotsIterative[[options$priors[[i]]$name]] <- temp_plot
      
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
      
      for(iteration in iter_sequence){
        
        if(options$priors[[i]]$type == "spike"){
          
          dfArrowPP   <- .dataArrowPPLS(options$priors[[i]])
          dfArrowPP$g <- as.character(iteration)
          
          all_arrows  <- c(all_arrows, list(dfArrowPP))
          legend      <- rbind(legend, c(options$priors[[i]]$type, iteration))
          
        }else if(options$priors[[i]]$type == "beta"){
          
          temp_data <- list(
            "nSuccesses" = sum(data$y[0:iteration] == 1),
            "nFailures"  = sum(data$y[0:iteration] == 0)
          )
          
          dfLinesPP   <- .dataLinesPPLS(temp_data, options$priors[[i]])
          dfLinesPP   <- dfLinesPP[dfLinesPP$g == "Posterior",]
          dfLinesPP$g <- as.character(iteration)
          
          all_lines   <- c(all_lines, list(dfLinesPP))
          legend      <- rbind(legend, c(options$priors[[i]]$type, iteration))
          
        }
        
      }
      
      xName  <- bquote("Population proportion"~theta)
      
      temp_plot$plotObject <- .plotStackedLS(all_lines, all_arrows, legend, xName = xName)
    }
    
    return()
  }
}
.plotsIterativeIntervalOverlyingBinomialLS <- function(jaspResults, data, ready, options){
  
  plotsIterativeInterval <- createJaspPlot(title = "Sequential Analysis: Interval", width = 530, height = 400, aspectRatio = 0.7)
  
  plotsIterativeInterval$position <- 8
  plotsIterativeInterval$dependOn(c(.BinomialLS_data_dependencies, "plotsIterativeInterval",
                                    "plotsIterativeIntervalLower", "plotsIterativeIntervalUpper", "colorPalette"))
  
  
  if (!all(ready)){
    jaspResults[["plotsIterativeInterval"]] <- plotsIterativeInterval
    return()
  }
  
  plot_data_lines <- list()
  
  # update the posteriors as the data go in
  for(h in 1:length(options$priors)){
    
    temp_lines   <- NULL
    
    # cheat for getting 2x 0 for the sequantial plot in case of no data
    if(length(data$y) == 0){
      iter_seq <- c(0, 0.1)
    }else{
      iter_seq <- 0:length(data$y)
    }
    
    for(i in iter_seq){
      
      temp_data    <- list(
        nSuccesses = sum(data$y[0:i] == 1),
        nFailures  = sum(data$y[0:i] == 0)
      )
      
      temp_results    <- .dataCustomPPLS(temp_data, options$priors[[h]],
                                         lCI = options$plotsIterativeIntervalLower, uCI = options$plotsIterativeIntervalUpper,
                                         type = c("parameter"))
      
      temp_lines      <- rbind(temp_lines, data.frame(
        y    = temp_results$coverage,
        x    = i,
        name = options$priors[[h]]$name
      ))
      
    }
    
    plot_data_lines <- c(plot_data_lines, list(temp_lines))
    
  }
  
  yName  <- bquote("P("~{.(options$plotsIterativeIntervalLower)<=theta}<=.(options$plotsIterativeIntervalUpper)~")")
  xName  <- "Observations"
  
  p <- .plotIterativeLS(plot_data_lines, all_CI = NULL, xName = xName, yName = yName, palette = options$colorPalette)
  
  
  plotsIterativeInterval$plotObject <- p
  
  jaspResults[["plotsIterativeInterval"]] <- plotsIterativeInterval
  return()
}
.plotsIterativeIntervalStackedBinomialLS   <- function(jaspResults, data, ready, options){
  
  plotsIterativeInterval <- createJaspContainer(title = "Sequential Analysis: Interval")
  
  plotsIterativeInterval$position <- 8
  plotsIterativeInterval$dependOn(c(.BinomialLS_data_dependencies, "plotsIterativeInterval",
                                    "plotsIterativeIntervalLower", "plotsIterativeIntervalUpper", "colorPalette"))
  
  
  jaspResults[["plotsIterativeInterval"]] <- plotsIterativeInterval
  
  
  if(all(!ready) | (ready[1] & !ready[2])){
    
    plotsIterativeInterval[[""]] <- createJaspPlot(title = "", width = 530, height = 400, aspectRatio = 0.7)
    return()
    
  }else if(!ready[1] & ready[2]){
    
    for(i in 1:length(options$priors)){
      plotsIterativeInterval[[options$priors[[i]]$name]] <- createJaspPlot(title = options$priors[[i]]$name,
                                                                           width = 530, height = 400, aspectRatio = 0.7)
    }
    return()
    
  }else{
    
    #options$priors[[i]]$name
    
    for(i in 1:length(options$priors)){
      
      temp_plot <- createJaspPlot(title = options$priors[[i]]$name, width = 530, height = 400, aspectRatio = 0.7)
      
      plotsIterativeInterval[[options$priors[[i]]$name]] <- temp_plot
      
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
      
      for(iteration in iter_sequence){
        
        if(options$priors[[i]]$type == "spike"){
          
          dfArrowPP   <- .dataArrowPPLS(options$priors[[i]])
          dfArrowPP$g <- as.character(iteration)
          
          all_arrows  <- c(all_arrows, list(dfArrowPP))
          legend      <- rbind(legend, c(options$priors[[i]]$type, iteration))
          
        }else if(options$priors[[i]]$type == "beta"){
          
          temp_data <- list(
            "nSuccesses" = sum(data$y[0:iteration] == 1),
            "nFailures"  = sum(data$y[0:iteration] == 0)
          )
          
          dfLinesPP   <- .dataLinesPPLS(temp_data, options$priors[[i]])
          dfLinesPP   <- dfLinesPP[dfLinesPP$g == "Posterior",]
          dfLinesPP$g <- as.character(iteration)
          
          all_lines   <- c(all_lines, list(dfLinesPP))
          legend      <- rbind(legend, c(options$priors[[i]]$type, iteration))
          
        }
        
      }
      
      xName  <- bquote("Population proportion"~theta)
      
      temp_plot$plotObject <- .plotStackedLS(all_lines, all_arrows, legend, xName = xName,
                                             lCI = options$plotsIterativeIntervalLower, uCI = options$plotsIterativeIntervalUpper)
    }
    
    return()
  }
}
.predictionsBinomialLS <- function(jaspResults, data, ready, options){
  predictionsTable <- createJaspTable(title = "Prediction Summary")
  
  predictionsTable$position <- 11
  predictionsTable$dependOn(c(.BinomialLS_data_dependencies, "predictionTable", "predictionN"))
  
  predictionsTable$addColumnInfo(name = "hypothesis",     title = "Model",                  type = "string")
  predictionsTable$addColumnInfo(name = "posterior",      title = "Posterior (θ)",          type = "string")
  predictionsTable$addColumnInfo(name = "posteriorMean",  title = "Posterior Mean",         type = "number")
  predictionsTable$addColumnInfo(name = "predictive",     title = "Prediction (Successes)", type = "string")
  predictionsTable$addColumnInfo(name = "predictiveMean", title = "Prediction Mean",        type = "number")
  
  # title <- paste0(options$predictionTableCI, "% Prediction Interval")
  # predictionsTable$addColumnInfo(name = "lowerCI", type = "number", format = "sf:4;dp:3", title = "Lower", overtitle = title)
  # predictionsTable$addColumnInfo(name = "upperCI", type = "number", format = "sf:4;dp:3", title = "Upper", overtitle = title)
  
  
  predictionsTable$setExpectedSize(length(options$priors))
  
  jaspResults[["predictionsTable"]] <- predictionsTable
  
  if(ready[1] & !ready[2]){
    
    return()
    
  }else if(!ready[1]){
    
    jaspResults[["predictionsTable"]]$setError("Please specify successes and failures.")
    
  }else if(ready[2]){
    
    # add rows for each hypothesis
    for(i in 1:length(options$priors)){
      
      temp_results    <- .estimateBinomialLS(data, options$priors[[i]])
      temp_prediction <- .predictBinomialLS(data, options$priors[[i]], options)
      
      temp_row <- list(
        hypothesis      = options$priors[[i]]$name,
        
        posterior       = temp_results$distribution,
        posteriorMean   = temp_results$mean,
        predictive      = temp_prediction$distribution,
        predictiveMean  = temp_prediction$mean
        #lowerCI      = temp_prediction$lCI, 
        #upperCI      = temp_prediction$uCI
      )
      
      
      predictionsTable$addRows(temp_row)
    }
    
    # add footnote clarifying what dataset was used
    predictionsTable$addFootnote(paste0("The prediction for ", options$predictionN, " ", ifelse(options$predictionN == 1, "observation", "observations"), 
                                        " is based on ", data$nSuccesses," ", ifelse(data$nSuccesses == 1, "success", "successes"),
                                        "  and ", data$nFailures, " ",ifelse(data$nFailures == 1, "failure", "failures"), "."))
    
  }
  
}
.plotsPredictionsIndividualBinomialLS      <- function(jaspResults, data, ready, options){
  
  plotsPredictionsIndividual <- createJaspContainer(title = "Prediction Plots")
  
  plotsPredictionsIndividual$position <- 12
  plotsPredictionsIndividual$dependOn(c(.BinomialLS_data_dependencies, "predictionN",
                                        "plotsPredictions", "predictionPlotType",
                                        "plotsPredictionCI","plotsPredictionType", "plotsPredictionCoverage",
                                        "plotsPredictionLower", "plotsPredictionUpper","predictionPlotProp"))
  
  jaspResults[["plotsPredictionsIndividual"]] <- plotsPredictionsIndividual
  
  
  if(all(!ready) | (ready[1] & !ready[2])){
    
    plotsPredictionsIndividual[[""]] <- createJaspPlot(title = "", width = 530, height = 400, aspectRatio = 0.7)
    return()
    
  }else if(!ready[1] & ready[2]){
    
    for(i in 1:length(options$priors)){
      plotsPredictionsIndividual[[options$priors[[i]]$name]] <- createJaspPlot(title = options$priors[[i]]$name,
                                                                               width = 530, height = 400, aspectRatio = 0.7)
    }
    return()
    
  }else{
    
    for(i in 1:length(options$priors)){
      
      temp_plot <- createJaspPlot(title = options$priors[[i]]$name, width = 530, height = 400, aspectRatio = 0.7)
      
      plotsPredictionsIndividual[[options$priors[[i]]$name]] <- temp_plot
      
      if(options$predictionPlotProp){
        xName  <- "Sample proportions"
        yName  <- "Density"
        xRange <- c(-.5/options$predictionN,1 + .5/options$predictionN)
      }else{
        xName  <- "Number of successes"
        yName  <- "Probability"
        xRange <- c(0, options$predictionN)
      }
      
      
      dfCI   <- NULL
      dfHist <- NULL
      
      if(options$plotsPredictionCI){
        
        if(options$plotsPredictionType == "central"){
          
          dfCI <- .dataCentralPPLS(data, options$priors[[i]], options$plotsPredictionCoverage,
                                   n = options$predictionN,type = "prediction")
          
        }else if(options$plotsPredictionType == "HPD"){
          
          dfCI <- .dataHPDPPLS(data, options$priors[[i]], options$plotsPredictionCoverage,
                               n = options$predictionN, type = "prediction")
          
        }else if(options$plotsPredictionType == "custom"){
          
          dfCI <- .dataCustomPPLS(data, options$priors[[i]],
                                  options$plotsPredictionLower, options$plotsPredictionUpper,
                                  n = options$predictionN, type = "prediction")
          
          if(options$plotsPredictionUpper > options$predictionN){
            
            plotsPredictionsIndividual[[options$priors[[i]]$name]]$setError("The upper CI limit is higher than the number of future 
                                                                            observations. Please, change the value of the upper CI limit 
                                                                            in the settings panel.")
            
            return()
          }
          if(options$plotsPredictionLower > options$predictionN){
            
            plotsPredictionsIndividual[[options$priors[[i]]$name]]$setError("The lower CI limit is higher than the number of future 
                                                                            observations. Please, change the value of the lower CI limit 
                                                                            in the settings panel.")
            
            return()
          }
          if(options$plotsPredictionLower > options$plotsPredictionUpper){
            
            plotsPredictionsIndividual[[options$priors[[i]]$name]]$setError("The lower CI limit is higher than the upper CI limit.
                                                                             Please, change the value of the CI limits 
                                                                             in the settings panel.")
            
            return()
          }
          
        }
      }
      
      dfHist  <- .dataHistPPLS(data, options$priors[[i]], options$predictionN)
      
      if(options$predictionPlotProp){
        dfHist$x <- dfHist$x/options$predictionN
        if(options$plotsPredictionCI){
          dfCI$x_start <- dfCI$x_start/options$predictionN
          dfCI$x_end   <- dfCI$x_end  /options$predictionN
        }
        nRound <- 3
      }else{
        nRound <- 0
      }
      
      p <- .plotPredictionLS(dfHist, dfCI, xRange, xName, yName, nRound = nRound,
                             proportions = options$predictionPlotProp, predictionN = options$predictionN)
      temp_plot$plotObject <- p
    }
    
    return()
  }
  
  
}
.plotsPredictionsBinomialLS    <- function(jaspResults, data, ready, options){
  
  plotsPredictions <- createJaspPlot(title = "Prediction Plots", width = 530, height = 400, aspectRatio = 0.7)
  
  plotsPredictions$position <- 12
  plotsPredictions$dependOn(c(.BinomialLS_data_dependencies, "predictionN",
                              "plotsPredictions", "predictionPlotType", "colorPalettePrediction", "predictionPlotProp"))
  
  jaspResults[["plotsPredictions"]] <- plotsPredictions
  
  
  if(!all(ready)){
    return()
  }else{
    
    if(options$predictionPlotProp){
      xName  <- "Sample proportions"
      yName  <- "Density"
      xRange <- c(-.5/options$predictionN,1+.5/options$predictionN)
    }else{
      xName  <- "Number of successes"
      yName  <- "Probability"
      xRange <- c(-.5, options$predictionN+.5)
    }
    
    all_lines  <- c()
    legend     <- NULL
    
    for(i in 1:length(options$priors)){
      
      dfHist   <- .dataHistPPLS2(data, options$priors[[i]], options$predictionN)
      dfHist$g <- options$priors[[i]]$name
      
      if(options$predictionPlotProp){
        dfHist$x <- dfHist$x/options$predictionN
      }
      
      # it's not beta, but I'm lazzy to rewrite a function I wanna use
      legend   <- rbind(legend, c("beta", options$priors[[i]]$name))
      all_lines<- c(all_lines, list(dfHist))
    }
    
    if(options$predictionPlotType == "overlying"){
      p <- .plotOverlyingLS(all_lines, NULL, xName = xName, yName = yName, xRange = xRange, discrete = TRUE,
                            palette = options$colorPalettePrediction, proportions = options$predictionPlotProp)
    }else{
      p <- .plotStackedLS(all_lines, NULL, legend, xName = xName, xRange = xRange,
                          discrete = TRUE, proportions = options$predictionPlotProp)
    }
    
    jaspResults[["plotsPredictions"]]$plotObject <- p
    return()
  }
  
}
.tableIterativeBinomialLS      <- function(jaspResults, data, ready, options){
  
  tableIterative <- createJaspTable(title = "Sequential Analysis: Point Estimate")
  
  tableIterative$position <- 7
  tableIterative$dependOn(c(.BinomialLS_data_dependencies, "plotsIterative", "plotsIterativeCenter",
                            "plotsIterativeIndividualCI", "plotsIterativeCoverage", "colorPalette", "plotsIterativeUpdatingTable"))
  
  
  tableIterative$addColumnInfo(name = "iteration", title = "Observations", type = "integer")
  if(ready[2]){
    if(options$plotsIterativeIndividualCI){
      if(options$plotsIterativeIndividualType == "central"){
        CI_title <- paste(options$plotsIterativeCoverage*100, "% CI", sep = "")
      }else if(options$plotsIterativeIndividualType == "HPD"){
        CI_title <- paste(options$plotsIterativeCoverage*100, "% HPD", sep = "")
      }else if(options$plotsIterativeIndividualType == "support"){
        CI_title <- paste("SI (BF=",options$plotsIterativeBF,")", sep = "")
      }
      for(i in 1:length(options$priors)){
        tableIterative$addColumnInfo(
          name  = paste(options$priors[[i]]$name,"center", sep = "_"),
          title = ifelse(options$plotsIterativeCenter == "mean", "Mean", "Median"),
          overtitle = options$priors[[i]]$name,
          type = "number")
        tableIterative$addColumnInfo(
          name  = paste(options$priors[[i]]$name,"CI", sep = "_"),
          title = CI_title,
          overtitle = options$priors[[i]]$name,
          type = "string")
      }
    }else{
      for(i in 1:length(options$priors)){
        tableIterative$addColumnInfo(
          name  = paste(options$priors[[i]]$name,"center", sep = "_"),  
          title = options$priors[[i]]$name,
          type = "number")
      }
    }
  }
  
  
  if(!all(ready)){
    jaspResults[["tableIterative"]] <- tableIterative
    return()
  }
  
  
  iter_seq <- 0:length(data$y)
  
  
  for(i in iter_seq){
    
    temp_row     <- list() 
    temp_row[["iteration"]] <- i
    
    temp_data    <- list(
      nSuccesses = sum(data$y[0:i] == 1),
      nFailures  = sum(data$y[0:i] == 0)
    )
    
    for(h in 1:length(options$priors)){
      
      temp_results <- .estimateBinomialLS(temp_data, options$priors[[h]])
      temp_row[[paste(options$priors[[h]]$name,"center", sep = "_")]] <- temp_results[[options$plotsIterativeCenter]]
      
      if(options$plotsIterativeIndividualCI){
        
        if(options$plotsIterativeIndividualType == "central"){
          temp_CIPP <- .dataCentralPPLS(temp_data, options$priors[[h]],
                                        options$plotsIterativeCoverage, type = "parameter")
        }else if(options$plotsIterativeIndividualType == "HPD"){
          temp_CIPP <- .dataHPDPPLS(temp_data, options$priors[[h]],
                                    options$plotsIterativeCoverage, type = "parameter")
        }else if(options$plotsIterativeIndividualType == "support"){
          temp_CIPP <- .dataSupportPPLS(temp_data, options$priors[[h]],
                                        options$plotsIterativeBF)
        }
        
        if(all(is.na(temp_CIPP[1:2]))){
          temp_int <- "∅"
        }else{
          temp_int <- sapply(1:nrow(temp_CIPP), function(i)paste(c(
            "[",format(round(temp_CIPP$x_start[i], 3), nsmall = 3),", ",format(round(temp_CIPP$x_end[i], 3), nsmall = 3),"]"
          ), collapse = ""))
          temp_int <- paste(temp_int, collapse = " and " )
          
          temp_row[[paste(options$priors[[h]]$name,"CI", sep = "_")]] <- temp_int
        }
        
      }
      
    }
    
    tableIterative$addRows(temp_row)
    
  }
  
  jaspResults[["tableIterative"]] <- tableIterative
  return()
}
.tableIterativeIntervalBinomialLS  <- function(jaspResults, data, ready, options){
  
  tableIterativeInterval <- createJaspTable(title = "Sequential Analysis: Interval")
  
  tableIterativeInterval$position <- 9
  tableIterativeInterval$dependOn(c(.BinomialLS_data_dependencies, "plotsIterativeInterval",
                                    "plotsIterativeIntervalLower", "plotsIterativeIntervalUpper", "plotsIterativeIntervalUpdatingTable"))
  
  
  tableIterativeInterval$addColumnInfo(name = "iteration", title = "Observations", type = "integer")
  if(ready[2]){
    for(i in 1:length(options$priors)){
      tableIterativeInterval$addColumnInfo(
        name  = paste(options$priors[[i]]$name,"center", sep = "_"),  
        title = options$priors[[i]]$name,
        type = "number")
    }
  }
  
  
  if(!all(ready)){
    jaspResults[["tableIterativeInterval"]] <- tableIterativeInterval
    return()
  }
  
  
  iter_seq <- 0:length(data$y)
  
  
  for(i in iter_seq){
    
    temp_row     <- list() 
    temp_row[["iteration"]] <- i
    
    temp_data    <- list(
      nSuccesses = sum(data$y[0:i] == 1),
      nFailures  = sum(data$y[0:i] == 0)
    )
    
    for(h in 1:length(options$priors)){
      
      temp_results    <- .dataCustomPPLS(temp_data, options$priors[[h]],
                                         lCI = options$plotsIterativeIntervalLower, uCI = options$plotsIterativeIntervalUpper,
                                         type = c("parameter"))
      temp_row[[paste(options$priors[[h]]$name,"center", sep = "_")]] <- temp_results$coverage
      
    }
    
    tableIterativeInterval$addRows(temp_row)
    
  }
  
  jaspResults[["tableIterativeInterval"]] <- tableIterativeInterval
  return()
}

# computational functions
.estimateBinomialLS    <- function(data, prior){
  
  if(prior$type == "spike"){
    
    output <- list(
      distribution = paste0("spike at ", prior$parPoint_inp),
      mean         = prior$parPoint,
      median       = prior$parPoint,
      lCI          = prior$parPoint,
      uCI          = prior$parPoint
    )
    
    return(output)
    
  }else if(prior$type == "beta"){
    
    # in order to keep decimals as decimals if user fills them that way
    if(!is.na(as.numeric(prior$parAlpha_inp))){
      text_Alpha <- prior$parAlpha + data$nSuccesses
    }else{
      text_Alpha <- MASS::fractions(prior$parAlpha + data$nSuccesses)
    }
    if(!is.na(as.numeric(prior$parBeta_inp))){
      text_Beta <- prior$parBeta + data$nFailures
    }else{
      text_Beta <- MASS::fractions(prior$parBeta + data$nFailures)
    }
    
    output <- list(
      distribution = paste0("beta (",text_Alpha, ", ",text_Beta, ")"),
      mean         = (prior$parAlpha + data$nSuccesses) / (prior$parAlpha + data$nSuccesses + prior$parBeta + data$nFailures),
      median       = qbeta(.5,   prior$parAlpha + data$nSuccesses, prior$parBeta + data$nFailures),
      lCI          = qbeta(.025, prior$parAlpha + data$nSuccesses, prior$parBeta + data$nFailures),
      uCI          = qbeta(.975, prior$parAlpha + data$nSuccesses, prior$parBeta + data$nFailures)
    )
    
    
    return(output)
  }
}
.predictBinomialLS     <- function(data, prior, options){
  
  if(prior$type == "spike"){
    
    output <- list(
      distribution = paste0("binomial (", options$predictionN, ", ", prior$parPoint_inp,")"),
      mean         = prior$parPoint * options$predictionN,
      median       = qbinom(.5, options$predictionN, prior$parPoint),
      lCI          = qbinom(    (1 - options$predictionTableCI)/2, options$predictionN, prior$parPoint),
      uCI          = qbinom(1 - (1 - options$predictionTableCI)/2, options$predictionN, prior$parPoint)
    )
    
    return(output)
    
  }else if(prior$type == "beta"){
    
    # in order to keep decimals as decimals if user fills them that way
    if(!is.na(as.numeric(prior$parAlpha_inp))){
      text_Alpha <- prior$parAlpha + data$nSuccesses
    }else{
      text_Alpha <- MASS::fractions(prior$parAlpha + data$nSuccesses)
    }
    if(!is.na(as.numeric(prior$parBeta_inp))){
      text_Beta <- prior$parBeta + data$nSuccesses
    }else{
      text_Beta <- MASS::fractions(prior$parBeta + data$nFailures)
    }
    
    output <- list(
      distribution = paste0("beta-binomial (",options$predictionN, ", ", text_Alpha, ", ",  text_Beta, ")"),
      mean         = (prior$parAlpha + data$nSuccesses) * options$predictionN / (prior$parAlpha + data$nSuccesses + prior$parBeta + data$nFailures),
      median       = .qbetabinomLS(.5, options$predictionN, prior$parAlpha + data$nSuccesses, prior$parBeta + data$nFailures),
      lCI          = .qbetabinomLS(    (1 - options$predictionTableCI)/2, options$predictionN, prior$parAlpha + data$nSuccesses, prior$parBeta + data$nFailures),
      uCI          = .qbetabinomLS(1 - (1 - options$predictionTableCI)/2, options$predictionN, prior$parAlpha + data$nSuccesses, prior$parBeta + data$nFailures)
    )
    
    return(output)
  }
}
.betaHDILS             <- function(alpha, beta, coverage){
  
  if(alpha == 1 & beta == 1){
    
    # do central in case that alpha & beta == 1, the interval is weird otherwise
    HDI <- c(.5 - coverage/2, .5 + coverage/2)
    
  }else if(alpha >= 1 & beta >= 1){
    
    HDI <- HDInterval::hdi(qbeta, coverage, shape1 = alpha, shape2 = beta)
    
  }else{
    # new density approach - instead of pdf, use scaled cdf
    # x_density <- seq(0, 1, .00001)
    # y_density <- dbeta(x_density, shape1 = alpha, shape2 = beta)
    # y_density[c(1, length(y_density))] <- 0
    
    den_beta <- .dbetaLS(alpha, beta)
    class(den_beta) <- "density"
    
    HDI <- HDInterval::hdi(den_beta, coverage, allowSplit = T)
    HDI <- round(HDI, 5) # dealing with precission 
    HDI[HDI[,1] <= min(den_beta$x),1] <- 0
    HDI[HDI[,2] >= max(den_beta$x),2] <- 1
    
  }
  
  HDI <- matrix(as.vector(HDI), ncol = 2)
  return(HDI)
}
.binomialHDILS         <- function(n, theta, coverage){
  
  # this doesn't work in some cases for some reason
  # HDI <- HDInterval::hdi(qbinom, coverage, size = n, prob = theta)
  
  x_density <- 0:n
  y_density <- dbinom(x_density, n, theta)
  y_density <- round(y_density, 10)
  den_binom <- list(
    x = x_density,
    y = y_density
  )
  class(den_binom) <- "density"
  HDI <- HDInterval::hdi(den_binom, coverage, allowSplit = T)
  
  HDI <- matrix(as.vector(HDI), ncol = 2)
  return(HDI)
}
.betabinomialHDILS     <- function(n, alpha, beta, coverage){
  
  if(alpha == 1 & beta == 1){
    
    HDI <-     x <- c(
      .qbetabinomLS((1 - coverage)/2 + 1e-5,  n, alpha, beta),
      .qbetabinomLS(1 - (1 - coverage)/2,     n, alpha, beta)
    )
    
  }else{
    
    x_density <- 0:n
    y_density <- sapply(x_density,function(s)extraDistr::dbbinom(s, n, alpha, beta))
    y_density <- round(y_density, 10)
    den_beta <- list(
      x = x_density,
      y = y_density
    )
    class(den_beta) <- "density"
    HDI <- HDInterval::hdi(den_beta, coverage, allowSplit = T)
    
  }
  
  HDI <- matrix(as.vector(HDI), ncol = 2)
  return(HDI)
}
.qbetabinomLS          <- function(p, n, alpha, beta){
  # the rounding is due to numerical imprecission in extraDistr::pbbinom
  return(c(0:n)[match(TRUE, round(sapply(0:n, function(s)extraDistr::pbbinom(s, n, alpha, beta)),10) >= p)])
}
.betaSupportLS         <- function(alpha, beta, successses, failures, BF){
  
  # old way
  # x_seq  <- seq(.001,.999,.001)
  # bf_res <- dbeta(x_seq, alpha + successses, beta + failures)/dbeta(x_seq, alpha, beta)
  
  temp_post  <- .dbetaLS(alpha + successses, beta + failures)
  temp_prior <- .dbetaLS(alpha, beta)
  
  x_seq   <- temp_post$x
  y_post  <- temp_post$y
  y_prior <- temp_prior$y
  
  bf_res  <- y_post/y_prior
  
  TF_seq <- bf_res>BF
  
  support <- .aproximateSupportLS(x_seq, TF_seq)
  
  support$lCI[support$lCI == min(x_seq)] <- 0
  support$uCI[support$uCI == max(x_seq)] <- 1
  
  return(support)
  
}
.aproximateSupportLS   <- function(x_seq, TF_seq){
  x_start <- NULL
  x_end   <- NULL
  
  r <- rle(TF_seq)
  
  if(length(r$values) > 0){
    for(i in 1:length(r$values)){
      if(r$values[i]){
        if(i == 1){
          x_start <- c(x_start, 1)
          x_end   <- c(x_end,   r$lengths[1])
        }else{
          x_start <- c(x_start, sum(r$lengths[1:(i-1)])+1)
          x_end   <- c(x_end,   sum(r$lengths[1:i]))
        }
      } 
    }
  }else{
    x_start <- NA
    x_end   <- NA
  }
  
  return(cbind.data.frame("lCI" = x_seq[x_start], "uCI" = x_seq[x_end]))
} 

.dbetaLS <- function(alpha, beta){

  y <- c(
    pbeta(.001, alpha, beta)*1000,
    dbeta(seq(.0015, .9985, .001), alpha, beta),
    pbeta(.999, alpha, beta, lower.tail = F)*1000
  )
  x <- c(.0005, seq(.0015, .9985, .001), .9995)
  
  return(list(
    x = x,
    y = y
  ))
}

# plotting functions
.dataLinesPPLS         <- function(data, prior){
  
  x_seq   <- seq(.0005, .9995, .001)
  y_post  <- round((pbeta(x_seq + .0005, prior$parAlpha + data$nSuccesses, prior$parBeta + data$nFailures) - pbeta(x_seq - .0005, prior$parAlpha + data$nSuccesses, prior$parBeta + data$nFailures))*length(x_seq),10)
  y_prior <- round((pbeta(x_seq + .0005, prior$parAlpha, prior$parBeta) - pbeta(x_seq - .0005, prior$parAlpha, prior$parBeta))*length(x_seq),10)
  
  linesGroup <- c(y_post, y_prior)
  thetaGroup <- c(x_seq, x_seq)
  nameGroup  <- c(rep("Posterior", length(x_seq)), rep("Prior", length(x_seq)))
  
  dat        <- data.frame(x = thetaGroup, y = linesGroup, g = nameGroup)
  return(dat)
}
.dataPointsPPLS        <- function(data, prior){
  
  theta <- data$nSuccesses / (data$nSuccesses + data$nFailures)
  
  pointXVal <- c(theta, theta)
  
  if(prior$type == "spike"){
    if(prior$parPoint == theta){
      pointYVal <- c(1, 1)
    }else{
      pointYVal <- c(0, 0)
    }
  }else if(prior$type == "beta"){
    pointYVal <- c(dbeta(theta, prior$parAlpha + data$nSuccesses, prior$parBeta + data$nFailures),
                   dbeta(theta, prior$parAlpha, prior$parBeta))
  }
  
  nameGroup <- c("Observed", "Observed")
  dat       <- data.frame(x = pointXVal, y = pointYVal, g = nameGroup)
  return(dat)
}
.dataHPDPPLS           <- function(data, prior, coverage, n = NULL, type = c("parameter", "prediction")){
  
  if(type == "parameter"){
    
    if(prior$type == "spike"){
      x <- matrix(prior$parPoint, ncol = 2, nrow = 1)
    }else if(prior$type == "beta"){
      x <- .betaHDILS(prior$parAlpha + data$nSuccesses, prior$parBeta + data$nFailures, coverage)
    }
    
  }else if(type == "prediction"){
    
    if(prior$type == "spike"){
      x <- .binomialHDILS(n, prior$parPoint, coverage)
    }else if(prior$type == "beta"){
      x <- .betabinomialHDILS(n, prior$parAlpha + data$nSuccesses, prior$parBeta + data$nFailures, coverage)
    }
    
  }
  
  dat       <- data.frame(x_start = x[,1], x_end = x[,2], g = "HPD", coverage = coverage)
  return(dat)
}
.dataCentralPPLS       <- function(data, prior, coverage, n = NULL, type = c("parameter", "prediction")){
  
  if(type == "parameter"){
    
    if(prior$type == "spike"){
      x <- matrix(prior$parPoint, ncol = 2, nrow = 1)
    }else if(prior$type == "beta"){
      x <- qbeta(c((1 - coverage)/2, 1 - (1 - coverage)/2), prior$parAlpha + data$nSuccesses, prior$parBeta + data$nFailures)
    }
    
  }else if(type == "prediction"){
    # adding  (+ 1e-5) to the first lower bound because the quantile function is not inverse of cumulatiove
    # distribution function and the lower boundary is not part of the interval. Wanted to write custom 
    # quantile function for the lower bound, however, the aproximation in R reusults in inability to fix
    # the borderline cases: CI for BinomialLS distribution with 3 trials, probabily .5 and coverage 75% 
    if(prior$type == "spike"){
      x <- qbinom(c((1 - coverage)/2 + 1e-5, 1 - (1 - coverage)/2), n, prior$parPoint)
    }else if(prior$type == "beta"){
      x <- c(
        .qbetabinomLS((1 - coverage)/2 + 1e-5, n, prior$parAlpha + data$nSuccesses, prior$parBeta + data$nFailures),
        .qbetabinomLS(1 - (1 - coverage)/2,     n , prior$parAlpha + data$nSuccesses, prior$parBeta + data$nFailures)
      )
    }
    
  }
  
  dat       <- data.frame(x_start = x[1], x_end = x[2], g = "central", coverage = coverage)
  return(dat)
}
.dataCustomPPLS        <- function(data, prior, lCI, uCI, n = NULL, type = c("parameter", "prediction")){
  
  if(type == "parameter"){
    
    if(prior$type == "spike"){
      coverage <- ifelse(lCI <= prior$parPoint & prior$parPoint <= uCI, 1, 0)
    }else if(prior$type == "beta"){
      coverage <- pbeta(uCI, prior$parAlpha + data$nSuccesses, prior$parBeta + data$nFailures) -
        pbeta(lCI, prior$parAlpha + data$nSuccesses, prior$parBeta + data$nFailures)
    }
    
  }else if(type == "prediction"){
    
    if(prior$type == "spike"){
      
      coverage <- sum(sapply(lCI:uCI, function(s)dbinom(s, n, prior$parPoint)))
      
    }else if(prior$type == "beta"){
      
      coverage <- sum(sapply(lCI:uCI, function(s)
        extraDistr::dbbinom(s, n, prior$parAlpha + data$nSuccesses, prior$parBeta + data$nFailures)))
    }
    
  }
  
  dat       <- data.frame(x_start = lCI, x_end = uCI, g = "custom", coverage = coverage)
  return(dat)
}
.dataSupportPPLS       <- function(data, prior, BF){
  
  if(prior$type == "spike"){
    coverage <- 1
    lCI      <- prior$parPoint
    uCI      <- prior$parPoint
  }else if(prior$type == "beta"){
    
    x        <- .betaSupportLS(prior$parAlpha, prior$parBeta, data$nSuccesses, data$nFailures, BF)
    
    if(nrow(x) > 0){
      lCI      <- x$lCI
      uCI      <- x$uCI
      coverage <- sum(sapply(1:length(lCI),function(i){
        pbeta(uCI[i], prior$parAlpha + data$nSuccesses, prior$parBeta + data$nFailures) - 
          pbeta(lCI[i], prior$parAlpha + data$nSuccesses, prior$parBeta + data$nFailures)
      }))
    }else{
      lCI      <- NA
      uCI      <- NA
      coverage <- 0
    }
  }
  
  dat       <- data.frame(x_start = lCI, x_end = uCI, g = "support", coverage = coverage, BF = BF)
  return(dat)
}
.dataProportionPPLS    <- function(data){
  
  theta <- data$nSuccesses / (data$nSuccesses + data$nFailures)
  dat   <- data.frame(x = theta, y = 0, g = "Sample proportion")
  
  return(dat)
}
.dataHistPPLS          <- function(data, prior, n){
  
  x <- 0:n
  
  if(prior$type == "spike"){
    y <- dbinom(x, n, prior$parPoint)
  }else if(prior$type == "beta"){
    
    y <- sapply(x, function(s)extraDistr::dbbinom(s, n, prior$parAlpha + data$nSuccesses, prior$parBeta + data$nFailures))
  }
  
  dat <- data.frame(x = x, y = y)
  return(dat)
}
.dataHistPPLS2         <- function(data, prior, n){
  
  x <- 0:n
  
  if(prior$type == "spike"){
    y <- dbinom(x, n, prior$parPoint)
  }else if(prior$type == "beta"){
    y <- sapply(x, function(s)extraDistr::dbbinom(s, n, prior$parAlpha + data$nSuccesses, prior$parBeta + data$nFailures))
  }
  
  x_new <- x[sort(rep(1:length(x),2))] + c(-.5, +.5)
  y_new <- y[sort(rep(1:length(x),2))]
  
  dat <- data.frame(x = x_new, y = y_new)
  return(dat)
}
.dataArrowPPLS         <- function(prior){
  dat       <- data.frame(x = prior$parPoint, y_start = 0, y_end = 1, g = "Prior = Posterior")
  return(dat)
}
.CI_labelLS            <- function(CI, nRound){
  temp_int <- sapply(1:nrow(CI), function(i){
    if(is.na(CI$x_start[i])){
      x <- "none"
      #x <- "~symbol(\\306)"
    }else if(CI$x_start[i] == CI$x_end[i]){
      x <- paste(c(
        "[",format(round(CI$x_start[i], nRound), nsmall = nRound),"]"
      ), collapse = "")
    }else{
      x <- paste(c(
        "[",format(round(CI$x_start[i], nRound), nsmall = nRound),", ",format(round(CI$x_end[i], nRound), nsmall = nRound),"]"
      ), collapse = "")
    }
    return(x)
  }
  )
  temp_int <- paste(temp_int, collapse = " and " )
  temp_int <- paste("'",temp_int,"'")
  
  # text for the coverage
  temp_cov <- paste(c("'",round(CI$coverage[1]*100), "% CI'"), collapse = "")
  
  
  if(CI$g[1] == "HPD"){
    temp_label <- paste(c(temp_cov,"['HPD']:",temp_int), collapse = "")
  }else if(CI$g[1] == "custom"){
    temp_label  <- paste(c("P({",CI$x_start,"<=x}<=",(CI$x_end),")","=='",round(CI$coverage[1]*100)," %'"), collapse = "")
  }else if(CI$g[1] == "support"){
    temp_label <- paste(c("SI['[BF = ",CI$BF[1],"]']:",temp_int), collapse = "")
  }else if(CI$g[1] == "central"){
    temp_label <- paste(c(temp_cov,":",temp_int), collapse = "")
  }
  
  if(nchar(temp_label) > 75){
    temp_o <- gregexpr(" and", substr(temp_label, 1, 65))
    temp_label <- c(paste(substr(temp_label, 1, temp_o[[1]][length(temp_o[[1]])]-1), "'", sep = ""), 
                    paste("'",substr(temp_label, temp_o[[1]][length(temp_o[[1]])], nchar(temp_label)), sep = ""))
  }
  
  return(temp_label)
}
.getYMax               <- function(all_lines, all_arrows){
  if(!is.null(all_lines) & !is.null(all_arrows)){
    y_breaks  <- JASPgraphs::getPrettyAxisBreaks(c(0, max(all_lines$y)))
    y_max     <- max(c(all_lines$y, y_breaks))
  }else if(!is.null(all_lines)){
    y_breaks  <- JASPgraphs::getPrettyAxisBreaks(c(0, max(all_lines$y)))
    y_max     <- max(c(all_lines$y, y_breaks))
  }else{
    y_breaks  <- JASPgraphs::getPrettyAxisBreaks(c(0, max(all_arrows$y_end)))
    y_max     <- max(c(all_arrows$y_end, y_breaks))
  }
  return(y_max)
}
.scalingSpikes         <- function(all_lines, all_arrows){
  if(!is.null(all_lines) & !is.null(all_arrows)){
    y_max     <- .getYMax(all_lines, all_arrows)
    y_breaks2 <- JASPgraphs::getPrettyAxisBreaks(c(0, max(all_arrows$y_end)))
    return(y_max/max(y_breaks2))
  }else{
    return(1)
  }
}
.plotYAxis             <- function(all_lines, all_arrows, CI){
  
  y_max <- .getYMax(all_lines, all_arrows)
  
  if(!is.null(all_lines) & !is.null(all_arrows)){
    y_breaks  <- JASPgraphs::getPrettyAxisBreaks(c(0, max(all_lines$y)))
    y_breaks2 <- JASPgraphs::getPrettyAxisBreaks(c(0, max(all_arrows$y_end)))
    y_pos2    <- y_breaks2/(max(y_breaks2)/y_max)
  }else if(!is.null(all_lines)){
    y_breaks  <- JASPgraphs::getPrettyAxisBreaks(c(0, max(all_lines$y)))
  }else{
    y_breaks  <- JASPgraphs::getPrettyAxisBreaks(c(0, max(all_arrows$y_end)))
  }
  
  # set the y-scale plotting range
  if(!is.null(CI)){
    y_range    <- c(0, y_max * 1.20) 
  }else{
    y_range    <- c(0, y_max) 
  }
  
  if(!is.null(all_lines) & !is.null(all_arrows)){
    return(ggplot2::scale_y_continuous(
      "Density",
      breaks = y_breaks,
      limits = y_range,
      sec.axis = ggplot2::sec_axis(
        ~ .,
        name   = "Probability",
        breaks = y_pos2,
        labels = y_breaks2)
    ))
  }else{
    return(ggplot2::scale_y_continuous(
      ifelse(is.null(all_lines), "Probability", "Density"),
      breaks = y_breaks,
      limits = y_range
    ))
  }
}
.plotThemePlus         <- function(all_lines, all_arrows){
  if(!is.null(all_lines) & !is.null(all_arrows)){
    return(
      ggplot2::theme(
        axis.title.y.right = ggplot2::element_text(vjust = 3),
        plot.margin = ggplot2::margin(t = 3, r = 10, b = 0, l = 1))
    )    
  }else{
    return(
      ggplot2::theme(
        axis.title.y.right = ggplot2::element_text(vjust = 3.5),
        plot.margin = ggplot2::margin(t = 3, r = 0, b = 0, l = 1))
    )
  }
}

.plotPriorPosteriorLS  <- function(all_lines, all_arrows, dfPoints = NULL, xName = NULL, yName = "Density"){
  
  mappingArrow <- ggplot2::aes(x = x, xend = x, y = y_start, yend = y_end, color = g)
  mappingLines <- ggplot2::aes(x = x, y = y, color = g)
  mappingPoint <- ggplot2::aes(x = x, y = y, color = g)

  if(!is.null(all_lines))all_lines   <- do.call("rbind", all_lines)
  if(!is.null(all_arrows))all_arrows <- do.call("rbind", all_arrows)

  # get the y_axis max
  y_max <- .getYMax(all_lines, all_arrows)
  
  g <- ggplot2::ggplot() 
  
  if(!is.null(all_arrows)){
    
    for(i in nrow(all_arrows):1){

      temp_arrow       <- all_arrows[i,]
      temp_arrow$y_end <- temp_arrow$y_end * .scalingSpikes(all_lines, all_arrows)
      
      g <- g + ggplot2::geom_segment(
        data        = temp_arrow,
        mapping     = mappingArrow,
        size        = 1,
        linetype    = 1,
        arrow       = ggplot2::arrow(length = ggplot2::unit(0.5, "cm")),
        show.legend = F) +
        ggplot2::geom_segment(
          data    = temp_arrow,
          mapping = mappingArrow,
          size    = 1)
    }
    
    x_high <- max(all_arrows$x)
  }
  
  if(!is.null(all_lines)){
    
    for(i in 1:length(unique(all_lines$g))){
      
      temp_line <- all_lines[all_lines$g == unique(all_lines$g)[i], ]
      temp_type <- i
      
      g <- g + ggplot2::geom_line(
        data     = temp_line,
        mapping  = mappingLines,
        size     = 1,
        linetype = temp_type)
    }
    
    x_high <- all_lines$x[which.max(all_lines$y)]
  }

  g <- g + ggplot2::scale_x_continuous(xName, limits = c(0, 1))
  g <- g + .plotYAxis(all_lines, all_arrows, NULL)
  
  if(!is.null(dfPoints)){
    
    g <- g + ggplot2::geom_point(data = dfPoints, mapping = mappingPoint, show.legend = TRUE,
                                 inherit.aes = FALSE, size = 4, shape = 4, 
                                 stroke = 1.25, fill = "grey")
    
    if(!is.null(all_arrows)){
      g <- g + ggplot2::scale_color_manual("",
                                           values  = c(c("black", "gray")[1:length(unique(all_arrows$g))], "black"),
                                           breaks  = c(as.character(all_arrows$g), as.character(unique(dfPoints$g))),
                                           guide   = ggplot2::guide_legend(override.aes = list(
                                             linetype = if(length(unique(all_arrows$g)) == 1) c(1, NA) else c(1, 2, NA),
                                             shape    = if(length(unique(all_arrows$g)) == 1) c(NA, 4) else c(NA, NA, 4)
                                           )))  
    }else{
      g <- g + ggplot2::scale_color_manual("",
                                           values  = c("black", "gray", "black")[c(1:length(unique(all_lines$g)), 3)],
                                           breaks  = c(unique(as.character(all_lines$g)), as.character(unique(dfPoints$g))),
                                           guide   = ggplot2::guide_legend(override.aes = list(
                                             linetype = c( 1,  2, NA)[c(1:length(unique(all_lines$g)), 3)],
                                             shape    = c(NA, NA,  4)[c(1:length(unique(all_lines$g)), 3)]
                                           ))) 
    }
    
  }else{
    
    if(!is.null(all_arrows)){
      g <- g + ggplot2::scale_color_manual("",
                                           values  = c("black", "gray")[1:length(unique(all_arrows$g))],
                                           breaks  = as.character(all_arrows$g),
                                           guide   = ggplot2::guide_legend(override.aes = list(
                                             linetype = if(length(unique(all_arrows$g)) == 1) c(1) else c(1, 2),
                                             shape    = if(length(unique(all_arrows$g)) == 1) c(NA) else c(NA, NA)
                                           ))) 
    }else{
      g <- g + ggplot2::scale_color_manual("",
                                           values  = c( "black", "gray")[1:length(unique(all_lines$g))],
                                           breaks  = c(unique(as.character(all_lines$g))),
                                           guide   = ggplot2::guide_legend(override.aes = list(
                                             linetype = c( 1,  2)[1:length(unique(all_lines$g))],
                                             shape    = c(NA, NA)[1:length(unique(all_lines$g))]
                                           ))) 
    }
    
  }
  

  if (x_high > .5) {
    legend.position = c(0.25, 1)
  }else {
    legend.position = c(0.75, 1)
  }

  g <- g + JASPgraphs::themeJaspRaw(legend.position = legend.position)
  g <- g + JASPgraphs::geom_rangeframe(sides = if(!is.null(all_lines) & !is.null(all_arrows)) "lbr" else "lb") +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(), 
      legend.text  = ggplot2::element_text(margin = ggplot2::margin(0, 0, 2, 0)),
      legend.key.height = ggplot2::unit(1, "cm"),
      legend.key.width  = ggplot2::unit(1.5,"cm")) + 
    .plotThemePlus(all_lines, all_arrows)
  
  plot <- g
  class(plot) <- c("JASPgraphs", class(plot))
  
  return(plot)
}
.plotOverlyingLS       <- function(all_lines, all_arrows, dfPoints = NULL, CI = NULL, xName = NULL, yName = NULL,
                                   xRange = c(0,1), palette = "colorblind", no_legend = FALSE, nRound = 3, discrete = FALSE,
                                   proportions = FALSE){

  mappingLines   <- ggplot2::aes(x = x, y = y, group = g, color = g)
  mappingArrows  <- ggplot2::aes(x = x , xend = x, y = y_start, yend = y_end, group = g, color = g)
  mappingArrows1 <- ggplot2::aes(x = x_start , xend = x_end, y = y, yend = y, group = g)
  mappingArrows2 <- ggplot2::aes(x = x_end , xend = x_start, y = y, yend = y, group = g)
  mappingText    <- ggplot2::aes(x = x, y = y, label = label)
  mappingPoint   <- ggplot2::aes(x = x, y = y)
  
  if(!is.null(all_lines))all_lines   <- do.call("rbind", all_lines)
  if(!is.null(all_arrows))all_arrows <- do.call("rbind", all_arrows)

  # get the y_axis max
  y_max <- .getYMax(all_lines, all_arrows)
  
  # set the CI text
  if(!is.null(CI)){
    # text for the interval
    temp_label <- .CI_labelLS(CI, nRound)
    CI         <- cbind.data.frame(CI, "y" = y_max * 1.05)
  }
  
  if(discrete){
    xBreaks <- JASPgraphs::getPrettyAxisBreaks(c(ceiling(xRange[1]),floor(xRange[2])))
    xBreaks[length(xBreaks)] <- floor(xRange[2])
    if(!proportions){
      xBreaks <- round(xBreaks)
    }
  }else{
    xBreaks <- JASPgraphs::getPrettyAxisBreaks(xRange)
  }
  
  g <- ggplot2::ggplot()
  
  if(!is.null(all_arrows)){
    
    all_arrows_scaled        <- all_arrows
    all_arrows_scaled$y_end  <- all_arrows_scaled$y_end * .scalingSpikes(all_lines, all_arrows)
    
    g <- g + ggplot2::geom_segment(
      data        = all_arrows_scaled,
      mapping     = mappingArrows,
      size        = 1,
      arrow       = ggplot2::arrow(length = ggplot2::unit(0.5, "cm")),
      show.legend = F)
    g <- g + ggplot2::geom_segment(
        data    = all_arrows_scaled,
        mapping = mappingArrows,
        size    = 1)
  }
  if(!is.null(all_lines)){
    g <- g + ggplot2::geom_line(data = all_lines, mapping = mappingLines, size = 1,)
  }
  
  if(!is.null(dfPoints)){
    g <- g + ggplot2::geom_point(data = dfPoints, mapping = mappingPoint, show.legend = FALSE,
                                 inherit.aes = FALSE, size = 4, shape = 4, 
                                 stroke = 1.25, fill = "grey")
  }
  
  if(no_legend == TRUE){
    g <- g + ggplot2::scale_colour_manual(values = "black")
  }else{
    g <- g + JASPgraphs::scale_JASPcolor_discrete(palette)
  }
  
  # axes
  g <- g + ggplot2::scale_x_continuous(xName, limits = xRange)
  g <- g + .plotYAxis(all_lines, all_arrows, CI)
  
  # legend
  if(!is.null(all_lines)){
    xr   <- range(all_lines$x)
    idx  <- which.max(all_lines$y)
    xmax <- all_lines$x[idx]
  }else{
    xr   <- range(all_arrows$x)
    idx  <- which.max(all_arrows$y_end)
    xmax <- all_arrows$x[idx]
  }
  
  if(!is.null(CI)){
    if(!is.na(CI$x_start[1])){
      g <- g + ggplot2::geom_segment(
        data    = CI,
        mapping = mappingArrows1, size = 1,
        arrow   = ggplot2::arrow(angle = 90, length = ggplot2::unit(0.25, "cm")),
        color   = "black") + ggplot2::geom_segment(
          data    = CI,
          mapping = mappingArrows2, size = 1,
          arrow   = ggplot2::arrow(angle = 90, length = ggplot2::unit(0.25, "cm")),
          color   = "black")
    }
    
    label_y    <- if(length(temp_label) == 1) 1.10 else 1.25 - .07 * c(1:length(temp_label)) 
    for(i in 1:length(temp_label)){
      
      temp_text <- data.frame(
        label = temp_label[i],
        x = (xRange[1] + xRange[2])/2,
        y = y_max * label_y[i]
      )
      
      g <- g + ggplot2::geom_text(
        data    = temp_text,
        mapping = mappingText,
        parse   = TRUE,
        hjust   = .5, vjust = 0, size = 6
      )
    }
  }
  
  
  if (xmax > mean(xr)) {
    legend.position = c(0.15, 0.875)
  }else{
    legend.position = c(0.8,  0.875)
  }
  
  if(no_legend == FALSE){
    g <- g + JASPgraphs::themeJaspRaw(legend.position = legend.position)
  }else{
    g <- g + JASPgraphs::themeJaspRaw()
  }
  g <- g + JASPgraphs::geom_rangeframe(sides = if(!is.null(all_lines) & !is.null(all_arrows)) "lbr" else "lb") +  
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.text  = ggplot2::element_text(margin = ggplot2::margin(0, 0, 2, 0)),
      legend.key.height = ggplot2::unit(1, "cm"),
      legend.key.width  = ggplot2::unit(1.5,"cm")) + 
    .plotThemePlus(all_lines, all_arrows)

  plot <- g
  class(plot) <- c("JASPgraphs", class(plot))
  
  return(plot)
}
.plotStackedLS         <- function(all_lines, all_arrows, legend, dfPoints = NULL, xName = NULL, yName = "Density",
                                   xRange = c(0,1), lCI = NULL, uCI = NULL, discrete = FALSE, proportions = FALSE){
  
  mappingLines  <- ggplot2::aes(x = x, y = y, group = g, color = g)
  mappingArrows <- ggplot2::aes(x = x , xend = x, y = y_start, yend = y_end, group = g, color = g)
  mappingLegend <- ggplot2::aes(x = x, y = y, label = name)
  mappingPoint  <- ggplot2::aes(x = x, y = y)
  
  if(discrete){
    xBreaks <- JASPgraphs::getPrettyAxisBreaks(c(ceiling(xRange[1]),floor(xRange[2])))
    if(!proportions){
      xBreaks <- round(xBreaks)
      xBreaks[length(xBreaks)] <- floor(xRange[2])
    }
  }else{
    xBreaks <- JASPgraphs::getPrettyAxisBreaks(xRange)
  }
  
  if(!is.null(all_lines)){
    
    all_linesD <- all_lines
    all_linesL <- all_lines
    
    for(i in 1:length(all_linesD)){
      if(is.null(lCI) & is.null(uCI)){
        all_linesD[[i]] <- rbind.data.frame(
          data.frame(x = xRange[1], y = 0, g = all_linesD[[i]]$g[1]),
          all_linesD[[i]],
          data.frame(x = xRange[2], y = 0, g = all_linesD[[i]]$g[1])     
        )
      }else{
        all_linesD[[i]] <- rbind.data.frame(
          data.frame(x = lCI, y = 0, g = all_linesD[[i]]$g[1]),
          all_linesD[[i]][all_linesD[[i]]$x > lCI & all_linesD[[i]]$x < uCI,],
          data.frame(x = uCI, y = 0, g = all_linesD[[i]]$g[1])     
        )
      }
      all_linesL[[i]] <- data.frame(x = xRange, y = rep(0, 2), g = all_linesD[[i]]$g[1] )
    }
    
    all_lines  <- do.call("rbind", all_lines)
    all_linesD <- do.call("rbind", all_linesD)
    all_linesL <- do.call("rbind", all_linesL)
  }
  
  if(!is.null(all_arrows)){
    
    all_arrowsL <- list()
    for(i in 1:length(all_arrows)){
      all_arrowsL[[i]] <- data.frame(y = rep(all_arrows[[i]]$y_start, 2), x = xRange,
                                     g = rep(all_arrows[[i]]$g, 2))
    }
    
    all_arrows <- do.call("rbind", all_arrows)
    all_arrowsL<- do.call("rbind", all_arrowsL)
  }
  
  legend      <- data.frame(legend)
  colnames(legend) <- c("type", "name")
  legend$type <- as.character(legend$type)
  legend$name <- as.character(legend$name)
  
  if(!is.null(all_lines)){
    obsYmax <- max(all_lines$y)
    if(!is.null(all_arrows)){
      all_arrows$y_end <- obsYmax
    }
  }else{
    obsYmax <- max(all_arrows$y_end)    
  }
  yBreak  <- obsYmax/3 
  newymax <- obsYmax + yBreak*nrow(legend)
  
  legend$y <- yBreak*(0:(nrow(legend)-1))
  legend$x <- xRange[1]
  
  # changing y-coordinates to "stack" the plots
  for(i in 1:nrow(legend)){
    if(legend$type[i] == "spike"){
      all_arrows[all_arrows$g == legend[i,2], "y_start"] <- all_arrows[all_arrows$g == legend[i,2], "y_start"] + yBreak*(i-1)
      all_arrows[all_arrows$g == legend[i,2], "y_end"]   <- all_arrows[all_arrows$g == legend[i,2], "y_end"]   + yBreak*(i-1)
      all_arrowsL[all_arrowsL$g == legend[i,2], "y"]     <- all_arrowsL[all_arrowsL$g == legend[i,2], "y"]     + yBreak*(i-1)
    }else if(legend$type[i] == "beta"){
      all_lines[all_lines$g == legend[i,2], "y"]   <- all_lines[all_lines$g == legend[i,2], "y"]   + yBreak*(i-1)
      all_linesD[all_linesD$g == legend[i,2], "y"] <- all_linesD[all_linesD$g == legend[i,2], "y"] + yBreak*(i-1)
      all_linesL[all_linesL$g == legend[i,2], "y"] <- all_linesL[all_linesL$g == legend[i,2], "y"] + yBreak*(i-1)
    }
  }
  
  g <- ggplot2::ggplot()
  
  for(i in nrow(legend):1){
    if(legend$type[i] == "spike"){
      g <- g + ggplot2::geom_segment(
        data = all_arrows[all_arrows$g == legend$name[i],],
        mapping = mappingArrows, size = 1,
        arrow = ggplot2::arrow(length = ggplot2::unit(0.5, "cm"))) +
        ggplot2::geom_line(
          data = all_arrowsL[all_arrowsL$g == legend$name[i],],
          mapping = mappingLines)
    }
    if(legend$type[i] == "beta"){
      g <- g + ggplot2::geom_line(
        data = all_lines[all_lines$g == legend$name[i],],
        mapping = mappingLines, size = 1) + 
        ggplot2::geom_polygon(
          data = all_linesD[all_linesD$g == legend$name[i],],
          mapping = mappingLines, fill = "grey60", alpha = .8)
      
      if(!is.null(lCI) & !is.null(uCI)){
        g <- g + ggplot2::geom_line(
          data = all_linesL[all_linesL$g == legend$name[i],],
          mapping = mappingLines
        )
      }
      
    }
  }
  
  if(!is.null(dfPoints)){
    g <- g + ggplot2::geom_point(data = dfPoints, mapping = mappingPoint, show.legend = FALSE,
                                 inherit.aes = FALSE, size = 4, shape = 4, 
                                 stroke = 1.25, fill = "grey")
  }
  
  #legend$name <- sapply(legend$name, function(x)paste(c(x, "    "), collapse = ""))
  #g <- g + ggplot2::geom_text(data = legend, mapping = mappingLegend,
  #                            size = 8, hjust = 1, vjust = 0)
  
  g <- g + ggplot2::scale_colour_manual(values = rep("black", nrow(legend))) +
    ggplot2::scale_x_continuous(xName, limits = xRange, breaks = xBreaks) +
    ggplot2::scale_y_continuous(yName, limits = c(0, newymax),breaks = legend$y, labels = legend$name) + 
    ggplot2::coord_cartesian(clip = 'off')
  
  
  g <- g + JASPgraphs::themeJaspRaw() + 
    JASPgraphs::geom_rangeframe(sides = 'b') + 
    ggplot2::theme(
      legend.title = ggplot2::element_blank(), 
      legend.text  = ggplot2::element_text(margin = ggplot2::margin(0, 0, 2, 0)),
      legend.key.height = ggplot2::unit(1, "cm"),
      legend.key.width  = ggplot2::unit(1.5,"cm"),
      
      axis.line.y  = ggplot2::element_blank(),
      #axis.text.y  = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      legend.position = "none"
      #plot.margin  = ggplot2::unit(c(0,0,0,max(sapply(legend$name,nchar))/60), "npc")
    )
  
  plot <- g
  class(plot) <- c("JASPgraphs", class(plot))
  
  return(plot)
}
.plotIterativeLS       <- function(all_lines, all_CI, xName = "Observations", yName = NULL, x_start = 0,
                                   palette = "colorblind", BF_log = NULL){

  all_lines      <- do.call("rbind", all_lines)
  all_lines$name <- factor(all_lines$name, levels = sort(levels(all_lines$name)))
  
  obsXmax    <- max(all_lines$x)
  newXmax    <- obsXmax
  if(obsXmax > 10){
    xBreaks <- round(seq(x_start, obsXmax, length.out = 7))
  }else{
    xBreaks <- x_start:obsXmax
  }
  
  if(is.null(BF_log)){
    yRange <- c(0, 1)
  }else if(BF_log){
    yRange <- range(c(all_lines$y, 0))
  }else if(!BF_log){
    yRange <- range(c(all_lines$y, 1))
  }
  yBreaks <- JASPgraphs::getPrettyAxisBreaks(yRange)
  
  
  mappingLines   <- ggplot2::aes(x = x, y = y, 
                                 group = name, color = name)
  mappinglCI     <- ggplot2::aes(x = x, y = y1, 
                                 group = name, color = name)
  mappinguCI     <- ggplot2::aes(x = x, y = y2, 
                                 group = name, color = name)
  mappingPolygon <- ggplot2::aes(x = x, y = y, group = name, fill = name)
  
  clr  <- scales::gradient_n_pal(JASPgraphs::JASPcolors(palette))(seq(0, 1, length.out = length(unique(all_lines$name))))
  #clr  <- JASPgraphs::colorBrewerJasp(n = length(unique(all_lines$name)))
  if(length(all_CI) > 0){
    names_CI <- NULL
    for(i in 1:length(all_CI)){
      names_CI <- c(names_CI, as.character(unique(all_CI[[i]]$name)))
    }
    clr1 <- clr[order(order(names_CI))]
  }
  
  
  g <- ggplot2::ggplot()
  
  if(length(all_CI) > 0){
    for(i in length(all_CI):1){
      if(is.null(all_CI[[i]]))next
      temp_data <- all_CI[[i]]
      temp_poly <- data.frame(
        x = c(temp_data$x, rev(temp_data$x)),
        y = c(temp_data$y1, rev(temp_data$y2)),
        name = rep(temp_data$name,2)
      )
      
      g <- g + 
        ggplot2::geom_polygon(
          data    = temp_poly,
          mapping = mappingPolygon, fill = clr1[i], alpha = .3) +
        ggplot2::geom_path(
          data    = temp_data,
          mapping = mappinguCI, size = 1, linetype = 2) +
        ggplot2::geom_path(
          data    = temp_data,
          mapping = mappinglCI, size = 1, linetype = 2)
    }
  }
  
  if(!is.null(BF_log)){
    if(BF_log){
      g <- g +
        ggplot2::geom_line(
          data    = data.frame(x = c(1, newXmax), y = c(0, 0)),
          mapping = ggplot2::aes(x = x, y = y), size = 1, show.legend = F, linetype = 3)
    }else if(!BF_log){
      g <- g +
        ggplot2::geom_line(
          data    = data.frame(x = c(1, newXmax), y = c(1, 1)),
          mapping = ggplot2::aes(x = x, y = y), size = 1, show.legend = F, linetype = 3)
    }
  }
  
  g <- g +
    ggplot2::geom_line(
      data    = all_lines,
      mapping = mappingLines, size = 1)
  
  g <- g +
    ggplot2::scale_x_continuous(xName, limits = c(0, newXmax), breaks = xBreaks) +
    ggplot2::scale_y_continuous(yName, limits = yRange, breaks = yBreaks) +
    ggplot2::scale_colour_manual(values = clr)
  
  
  if (mean(all_lines$y[all_lines$x == max(all_lines$x)]) > .5) {
    legend.position = c(0.8, 0.03 + length(unique(all_lines$name))/10)
  }else{
    legend.position = c(0.8, 1.03)
  }
  
  g <- g + JASPgraphs::themeJaspRaw(legend.position = legend.position) + 
    JASPgraphs::geom_rangeframe(sides = 'lb') + 
    ggplot2::theme(
      legend.title = ggplot2::element_blank(), 
      legend.text  = ggplot2::element_text(margin = ggplot2::margin(0, 2, 2, 0)),
      legend.key.height = ggplot2::unit(1, "cm"),
      legend.key.width  = ggplot2::unit(1.5,"cm"),
    )
  
  plot <- g
  class(plot) <- c("JASPgraphs", class(plot))
  
  return(plot)
}
.plotIndividualLS      <- function(all_lines, all_arrows, CI, CIall_lines, xRange, xName, yName = NULL, nRound = 3){ 
  
  mappingLines   <- ggplot2::aes(x = x, y = y, group = g,)
  mappingArrows  <- ggplot2::aes(x = x , xend = x, y = y_start, yend = y_end, group = g)
  mappingArrows1 <- ggplot2::aes(x = x_start , xend = x_end, y = y, yend = y, group = g)
  mappingArrows2 <- ggplot2::aes(x = x_end , xend = x_start, y = y, yend = y, group = g)
  mappingText    <- ggplot2::aes(x = x, y = y, label = label)
  
  # get the y_axis max
  y_max <- .getYMax(all_lines, all_arrows)
  
  # set the CI text
  if(!is.null(CI)){
    # text for the interval
    temp_label <- .CI_labelLS(CI, nRound)
    CI         <- cbind.data.frame(CI, "y" = y_max * 1.05)
  }
  
  g <- ggplot2::ggplot()
  
  if(!is.null(all_arrows)){
    
    temp_arrows        <- all_arrows
    temp_arrows$y_end  <- temp_arrows$y_end * .scalingSpikes(all_lines, all_arrows)
      
    g <- g + ggplot2::geom_segment(
      data    = all_arrows,
      mapping = mappingArrows, size = 1,
      arrow   = ggplot2::arrow(length = ggplot2::unit(0.5, "cm")),
      color   = "black")
  }
  
  if(!is.null(all_lines)){
    if(!is.null(CIall_lines)){
      g <- g + ggplot2::geom_polygon(
        data = CIall_lines,
        mapping = mappingLines, fill = "grey60", alpha = .8)
    }
    g <- g + ggplot2::geom_line(
      data    = all_lines,
      mapping = mappingLines, size = 1, color = "black") 
  }
  
  if(!is.null(CI)){
    if(!is.na(CI$x_start[1])){
      g <- g + ggplot2::geom_segment(
        data    = CI,
        mapping = mappingArrows1, size = 1,
        arrow   = ggplot2::arrow(angle = 90, length = ggplot2::unit(0.25, "cm")),
        color   = "black") + ggplot2::geom_segment(
          data    = CI,
          mapping = mappingArrows2, size = 1,
          arrow   = ggplot2::arrow(angle = 90, length = ggplot2::unit(0.25, "cm")),
          color   = "black")
    }
    

    label_y    <- if(length(temp_label) == 1) 1.10 else 1.25 - .07 * c(1:length(temp_label)) 
    for(i in 1:length(temp_label)){
      
      temp_text <- data.frame(
        label = temp_label[i],
        x = (xRange[1] + xRange[2])/2,
        y = y_max * label_y[i]
      )
      
      g <- g + ggplot2::geom_text(
        data    = temp_text,
        mapping = mappingText,
        parse   = TRUE,
        hjust   = .5, vjust = 0, size = 6
      )
    }
  }
  
  
  # x-axes
  g <- g + ggplot2::scale_x_continuous(xName, limits = xRange)
  g <- g + .plotYAxis(all_lines, all_arrows, CI)

  
  g <- g + JASPgraphs::themeJaspRaw() + 
    JASPgraphs::geom_rangeframe(sides = if(!is.null(all_lines) & !is.null(all_arrows)) "lbr" else "lb") +  
    ggplot2::theme(
      legend.title = ggplot2::element_blank(), 
      legend.text  = ggplot2::element_text(margin = ggplot2::margin(0, 0, 2, 0)),
      legend.key.height = ggplot2::unit(1, "cm"),
      legend.key.width  = ggplot2::unit(1.5,"cm")) + 
    .plotThemePlus(all_lines, all_arrows)
  
  plot <- g
  class(plot) <- c("JASPgraphs", class(plot))
  return(plot)
}
.plotPredictionLS      <- function(dfHist, CI, xRange, xName, yName, nRound = 0, xBlacked = NULL,
                                   proportions = FALSE, predictionN = NULL){
  
  mappingHistogram  <- ggplot2::aes(x = x, y = y, fill = col)
  mappingArrows1    <- ggplot2::aes(x = x_start_adj , xend = x_end_adj, y = y, yend = y, group = g)
  mappingArrows2    <- ggplot2::aes(x = x_end_adj,  xend = x_start_adj, y = y, yend = y, group = g)
  mappingText       <- ggplot2::aes(x = x, y = y, label = label)
  
  if(!is.null(CI)){
    # text for the interval
    temp_label <- .CI_labelLS(CI, nRound)
    
    y_max_multiplier <- ifelse(length(temp_label) == 1, 1.15, 1.25)
  }
  
  yBreaks  <- JASPgraphs::getPrettyAxisBreaks(c(0, dfHist$y))
  if(proportions){
    xBreaks    <- JASPgraphs::getPrettyAxisBreaks(xRange)
    xBreaks[1] <- 0
    xBreaks[length(xBreaks)] <- 1
  }else{
    xBreaks  <- round(JASPgraphs::getPrettyAxisBreaks(xRange))
    xBreaks[length(xBreaks)] <- predictionN
  }
  
  
  if(xBreaks[length(xBreaks)] > xRange[2])xBreaks[length(xBreaks)] <- xRange[2]
  
  obsYmax    <- max(dfHist$y)
  breaksYmax <- yBreaks[length(yBreaks)]
  newymax    <- max(ifelse(!is.null(CI), y_max_multiplier + .05, 1.10) * obsYmax, breaksYmax)
  
  dfHist$col <- "a"
  if(!is.null(CI)){
    
    CI <- cbind.data.frame(CI, "y" = obsYmax * 1.10)
    
    if(!proportions){
      CI$x_start_adj <- CI$x_start - .5
      CI$x_end_adj   <- CI$x_end   + .5
    }else{
      CI$x_start_adj <- CI$x_start - .5/(predictionN + 1)
      CI$x_end_adj   <- CI$x_end   + .5/(predictionN + 1)
    }
    
    for(i in 1:nrow(CI)){
      dfHist$col[dfHist$x >= CI$x_start[i] & dfHist$x <= CI$x_end[i]] <- "b"
    }
  }
  if(!is.null(xBlacked)){
    dfHist[dfHist$x == xBlacked,"col"] <- "c"
  }
  
  g <- ggplot2::ggplot()
  g <- g + ggplot2::geom_bar(
    data     = dfHist,
    mapping  = mappingHistogram,
    #fill     = "grey",
    col      = "black",
    stat     = "identity"
  )
  
  
  if(!is.null(CI)){
    if(!is.na(CI$x_start[1])){
      g <- g + ggplot2::geom_segment(
        data    = CI,
        mapping = mappingArrows1, size = 1,
        arrow   = ggplot2::arrow(angle = 90, length = ggplot2::unit(0.25, "cm")),
        color   = "black") + ggplot2::geom_segment(
          data    = CI,
          mapping = mappingArrows2, size = 1,
          arrow   = ggplot2::arrow(angle = 90, length = ggplot2::unit(0.25, "cm")),
          color   = "black")
    }
    
    r <- 0
    for(i in 1:length(temp_label)){
      
      temp_text <- data.frame(
        label = temp_label[i],
        x = (xRange[1] + xRange[2])/2,
        y = obsYmax * (y_max_multiplier-r)
      )
      
      g <- g + ggplot2::geom_text(
        data    = temp_text,
        mapping = mappingText,
        parse   = TRUE,
        hjust   = .5, vjust = 0, size = 6
      )
      
      r <- r + .10
      
    }
  }
  
  # control fill
  if(is.null(CI)){
    fillColor <- c("grey90") 
  }else{
    if(nrow(CI) == 1){
      if(all(xRange[1]:xRange[2] %in% CI$x_start:CI$x_end)){
        fillColor <- c("grey50") 
      }else{
        fillColor <- c("grey90", "grey50") 
      }
    }else{
      if(all(xRange[1]:xRange[2] %in% c(unlist(sapply(1:nrow(CI), function(i)CI$x_start[i]:CI$x_end[i]))))){
        fillColor <- c("grey50") 
      }else{
        fillColor <- c("grey90", "grey50") 
      }
    }
  }
  if(!is.null(xBlacked)){
    fillColor <- c(fillColor, "black")
  }
  
  
  if(!proportions){
    g <- g + ggplot2::scale_x_continuous(xName, breaks = xBreaks, limits = c(xRange[1] - .5, xRange[2] + .5))
  }else{
    g <- g + ggplot2::scale_x_continuous(xName, breaks = xBreaks, limits = c(xRange[1] - .5/(predictionN+1), xRange[2] + .5/(predictionN+1)))
  }
  g <- g + ggplot2::scale_y_continuous(yName, breaks = yBreaks, limits = c(0, newymax)) 
  g <- g + ggplot2::scale_colour_manual(values = fillColor, aesthetics = "fill")
  
  
  g <- g + JASPgraphs::themeJaspRaw() + 
    JASPgraphs::geom_rangeframe(sides = 'lb') +  
    ggplot2::theme(
      legend.title = ggplot2::element_blank(), 
      legend.text  = ggplot2::element_text(margin = ggplot2::margin(0, 0, 2, 0)),
      legend.key.height = ggplot2::unit(1, "cm"),
      legend.key.width  = ggplot2::unit(1.5,"cm"))
  
  plot <- g
  class(plot) <- c("JASPgraphs", class(plot))
  return(plot)
}


# all settings dependent on data input
.BinomialLS_data_dependencies <- c("dataType",
                                   "nSuccesses", "nFailures",                                 # for Counts
                                   "data_sequence",    "key_success_Seq", "key_failure_Seq",  # for Sequence
                                   "selectedVariable", "key_success_Var", "key_failure_Var")  # for Variable
