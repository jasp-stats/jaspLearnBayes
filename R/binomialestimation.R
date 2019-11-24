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


binomialEstimation   <- function(jaspResults, dataset, options, state = NULL){
  
  # a vector of two, first for data, second for hypotheses
  ready <- .readyBinomial(options)
  
  # load, check, transform and process data
  if(ready[1])data <- .readDataBinomial(dataset, options)
  
  # data summary table if requested (but not if the data counts were added directly)
  if(options$dataSummary & !options$dataType == "dataCounts").summaryBinomial(jaspResults, data, ready)
  
  ### output 
  # estimated parameter values
  .estimatesBinomial(jaspResults, data, ready, options)
  
  # sequentially estimated parameter values
  if(options$doIterative & !options$dataType == "dataCounts").estimatesSequentialBinomial(jaspResults, data, ready, options)
  
  # prior
  if(options$plotsPrior){
    if(options$plotsPriorType != "individual").plotsSimpleBinomial(jaspResults, data, ready, options, type = "Prior")
    if(options$plotsPriorType == "individual").plotsIndividualBinomial(jaspResults, data, ready, options, type = "Prior")
  }
  
  # posterior
  if(options$plotsPosterior){
    if(options$plotsPosteriorType != "individual").plotsSimpleBinomial(jaspResults, data, ready, options, type = "Posterior")
    if(options$plotsPosteriorType == "individual").plotsIndividualBinomial(jaspResults, data, ready, options, type = "Posterior")
  }
  
  # prior and posterior
  if(options$plotsBoth).plotsBothBinomial(jaspResults, data, ready, options)
  
  # sequential
  if(options$plotsIterative){
    if(options$plotsIterativeType == "overlying").plotsIterativeOverlyingBinomial(jaspResults, data, ready, options)
    if(options$plotsIterativeType == "stacked").plotsIterativeStackedBinomial(jaspResults, data, ready, options)
  }
  
  ### prediction
  if(options$predictionTable).predictionsBinomial(jaspResults, data, ready, options)
  
  # plot
  if(options$predictionPlot).plotsPredictionsBinomial(jaspResults, data, ready, options)
  
  return()
}

# main functions
.readyBinomial       <- function(options){
  # are data ready
  if(options$dataType == "dataCounts"){
    ready <- TRUE # options$nSuccesses > 0 | options$nFailures > 0
  }else if(options$dataType == "dataSequence"){
    ready <- TRUE # nchar(options$data_sequence) > 0
  }else if(options$dataType == "dataVariable"){
    ready <- TRUE # options$selectedVariable != ""
  }
  
  # are priors ready
  ready <- c(ready, length(options[["priors"]]) > 0)
  return(ready)
}
.readDataBinomial    <- function(dataset, options){
  
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
        
      }else if(options$dataType == "dataVariable"){
        
        # this is stupidly written #rework
        if (!is.null(dataset)){
          temp_y <- dataset
        }else{
          temp_y <- .readDataSetToEnd(columns = options$selectedVariable)[,1]
        }
        
      }
      
      data$y          <- .cleanDataBinomial(temp_y, options)
    
    }
    
    data$nSuccesses <- sum(data$y == 1)
    data$nFailures  <- sum(data$y == 0)
    
  } 
  
  return(data)
  
}
.cleanDataBinomial   <- function(x, options){
  
  key <- list(
    c("T", "F"),
    c("S", "F"),
    c("TRUE", "FALSE"),
    c("correct", "incorrect"),
    c("hit", "miss")
  )
  
  # doubling the menu allows to store the keys while user switches between different input methods
  key_success <- ifelse(options$dataType == "dataSequence", options$key_success_Seq, options$key_success_Var)
  key_failure <- ifelse(options$dataType == "dataSequence", options$key_failure_Seq, options$key_failure_Var)
  
  x <- na.omit(x)
  x <- as.character(x)
  
  # use user provided keys if possible
  if(key_success != "" & key_failure != "" & options$dataType != "dataCounts"){
    
    # split the sequence into individual parts
    if(options$dataType == "dataSequence"){
      
      if(grepl(",", x)){
        x <- unlist(strsplit(x, split = ","))
      }else{
        x <- unlist(strsplit(x, split = ";"))        
      }
      x <- x[x != ""] # remove accidental double separators
      
    }
    
    if(!all(x == key_success | x == key_failure)){
      stop(paste0("The encoding does not match the supplied data, the problematic imput: ",
                  paste(unique(x[!(x == key_success | x == key_failure)]), collapse = "; ")))
    }
    
    x <- ifelse(toupper(x) %in% toupper(key_success), 1, 0)
    
  }else if(!all(x == 1 | x == 0)){
    # if not, and all of the variables aren't already 1s or 0s, try the recoding provided in key object
    
    # split the sequence into individual parts
    if(options$dataType == "dataSequence"){
      
      if(grepl(",", x)){
        x <- unlist(strsplit(x, split = ","))
      }else if(grepl(";", x)){
        x <- unlist(strsplit(x, split = ";"))
      }else{
        x <- unlist(strsplit(x, split = ""))
      }
      x <- x[x != ""] # remove accidental double separators
      
    }
    
    for(i in 1:length(key)){
      if(all( toupper(x) %in% toupper(key[[i]][1]) | toupper(x) %in% toupper(key[[i]][2]))){
        x <- ifelse(toupper(x) %in% toupper(key[[i]][1]), 1,
                    ifelse(toupper(x) %in% toupper(key[[i]][2]), 0, x))
      }
    }
    
    if(!all(x == 1 | x == 0)){
      stop(paste0("The data input was not recognized. Input successes as '1' and failures as '0' or 
                  specify an appropriate encoding of individual factors.\tUncregnized input: ",
                  paste(unique(x[!(x == 0 | x == 1)]), collapse = "; ")))
      }
    
  }
  
  return(as.numeric(x))
}
.summaryBinomial     <- function(jaspResults, data, ready){
  summaryTable <- createJaspTable(title = "Data Summary")
  
  summaryTable$position <- 1
  summaryTable$dependOn(c("dataSummary", .binomial_data_dependencies))
  
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
.estimatesBinomial   <- function(jaspResults, data, ready, options){
  estimatesTable <- createJaspTable(title = "Estimation Summary")
  
  estimatesTable$position <- 2
  estimatesTable$dependOn(.binomial_data_dependencies)
  
  estimatesTable$addColumnInfo(name = "hypothesis",   title = "Model"    ,       type = "string")
  estimatesTable$addColumnInfo(name = "prior",        title = "Prior (θ)",       type = "string")
  estimatesTable$addColumnInfo(name = "priorMed",     title = "Prior Median",    type = "number")
  estimatesTable$addColumnInfo(name = "posterior",    title = "Posterior (θ)",   type = "string")
  estimatesTable$addColumnInfo(name = "posteriorMed", title = "Posterior Median",type = "number")
  estimatesTable$addColumnInfo(name = "likelihood",   title = "Likelihood",      type = "number")
  
  estimatesTable$setExpectedSize(length(options$priors))
  
  jaspResults[["estimatesTable"]] <- estimatesTable
  
  if(all(!ready) | (ready[1] & !ready[2])){
    return()
  }else if(ready[2]){
    
    # add rows for each hypothesis
    for(i in 1:length(options$priors)){
      # add mock data to use only priors
      temp_data <- list(
        "nSuccesses" = 0,
        "nFailures"  = 0
      )
      temp_results <- .estimateBinomial(temp_data, options$priors[[i]])
      
      temp_row <- list(
        prior        = temp_results$distribution,
        priorMed     = temp_results$median,
        hypothesis   = options$priors[[i]]$name, 
        posterior    = "", 
        posteriorMed = "",
        likelihood   = "")
      
      
      if(all(ready)){
        # and when real data are supplied as well, add posterior information
        temp_results <- .estimateBinomial(data, options$priors[[i]])
        
        temp_row["posterior"]    <- temp_results$distribution
        temp_row["posteriorMed"] <- temp_results$median
        temp_row["likelihood"]   <- temp_results$likelihood
        
      }
      
      estimatesTable$addRows(temp_row)
    }
    
    # add footnote clarifying what dataset was used
    estimatesTable$addFootnote(paste0("The output is based on ", data$nSuccesses," ", ifelse(data$nSuccesses == 1, "success", "successes"),
                                      "  and ", data$nFailures, " ",ifelse(data$nFailures == 1, "failure", "failures"), "."))
    
  }
  
}
.estimatesSequentialBinomial <- function(jaspResults, data, ready, options){
  estimatesSequentialTable <- createJaspTable(title = "Iterative Posterior Updating")
  
  estimatesSequentialTable$position <- 3
  estimatesSequentialTable$dependOn(c("doIterative", .binomial_data_dependencies))
  
  estimatesSequentialTable$addColumnInfo(name = "iteration", title = "Iteration", type = "integer")
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
      temp_results <- .estimateBinomial(temp_data, options$priors[[h]])
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
          temp_results <- .estimateBinomial(temp_data, options$priors[[h]])
          temp_row[[options$priors[[h]]$name]] <- temp_results$distribution
        }
        estimatesSequentialTable$addRows(temp_row)
      }
    }
    
  }
}
.plotsSimpleBinomial <- function(jaspResults, data, ready, options, type = c("Prior", "Posterior")){
  
  plotsSimple <- createJaspPlot(title = paste0(type, " Plots"), width = 530, height = 400, aspectRatio = 0.7)
  
  plotsSimple$position <- ifelse(type == "Prior", 4, 5)
  plotsSimple$dependOn(c(.binomial_data_dependencies,
                         ifelse(type == "Prior", "plotsPrior", "plotsPosterior"),
                         ifelse(type == "Prior", "plotsPriorType", "plotsPosteriorType"),
                         ifelse(options[[ifelse(type == "Prior", "plotsPriorType", "plotsPosteriorType")]] == "overlying",
                                "colorPalette", "")))
  
  jaspResults[[type]] <- plotsSimple
  
  if (!all(ready))return()
  
  all_lines  <- c()
  all_arrows <- c()
  legend     <- NULL
  for(i in 1:length(options$priors)){
    
    if(options$priors[[i]]$type == "point"){
      
      dfArrowPP   <- .dataArrowPP(options$priors[[i]])
      dfArrowPP$g <- options$priors[[i]]$name
      
      all_arrows  <- c(all_arrows, list(dfArrowPP))
      legend      <- rbind(legend, c(options$priors[[i]]$type, options$priors[[i]]$name))
      
    }else if(options$priors[[i]]$type == "beta"){
      
      dfLinesPP   <- .dataLinesPP(data, options$priors[[i]])
      dfLinesPP   <- dfLinesPP[dfLinesPP$g == type,]
      dfLinesPP$g <- options$priors[[i]]$name
      
      all_lines   <- c(all_lines, list(dfLinesPP))
      legend      <- rbind(legend, c(options$priors[[i]]$type, options$priors[[i]]$name))
      
    }
  }
  
  xName  <- expression(paste("Population proportion ", theta))
  
  if(options[[ifelse(type == "Prior", "plotsPriorType", "plotsPosteriorType")]] == "overlying"){
    p <- .plotOverlying(all_lines, all_arrows, xName = xName, palette = options$colorPalette)
  }else{
    p <- .plotStacked(all_lines, all_arrows, legend, xName = xName)
  }
  
  jaspResults[[type]]$plotObject <- p
  
  return()
}
.plotsBothBinomial   <- function(jaspResults, data, ready, options){
  
  plotsBoth <- createJaspContainer(title = "Prior and Posterior Plots")
  
  plotsBoth$position <- 6
  plotsBoth$dependOn(c(.binomial_data_dependencies, "plotsBoth", "plotsBothSampleProportion"))
  
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
      
      xName  <- expression(paste("Population proportion ", theta))
      
      if(options$priors[[i]]$type == "point"){
        dfArrowPP  <- .dataArrowPP(options$priors[[i]])
      }else if(options$priors[[i]]$type == "beta"){
        dfLinesPP  <- .dataLinesPP(data, options$priors[[i]])
        
        if(all(dfLinesPP$y[dfLinesPP$g == "Prior"] == dfLinesPP$y[dfLinesPP$g == "Posterior"])){
          dfLinesPP   <- dfLinesPP[dfLinesPP$g == "Posterior",]
          dfLinesPP$g <- "Prior = Posterior"
        }
        
      }
      
      if(options$plotsBothSampleProportion){
        dfPointsPP <- .dataProportionPP(data, options$priors[[i]])
      }else{
        dfPointsPP <- NULL 
      }
      
      p <- .plotPriorPosterior(dfLine = dfLinesPP, dfArrow = dfArrowPP, dfPoints = dfPointsPP, xName = xName)
      temp_plot$plotObject <- p
    }
    
    return()
  }
  
  
}
.plotsIndividualBinomial     <- function(jaspResults, data, ready, options, type = c("Prior", "Posterior")){
  
  plotsIndividual <- createJaspContainer(title = paste0(type, " Plots"))
  
  plotsIndividual$position <- ifelse(type == "Prior", 4, 5)
  plotsIndividual$dependOn(c(.binomial_data_dependencies,
                       ifelse(type == "Prior", "plotsPrior",             "plotsPosterior"),
                       ifelse(type == "Prior", "plotsPriorType",         "plotsPosteriorType"),
                       ifelse(type == "Prior", "plotsPriorIndividualCI", "plotsPosteriorIndividualCI"),
                       ifelse(type == "Prior", "plotsPriorCoverage",     "plotsPosteriorCoverage"),
                       ifelse(type == "Prior", "plotsPriorLower",        "plotsPosteriorLower"),
                       ifelse(type == "Prior", "plotsPriorUpper",        "plotsPosteriorUpper")))
  
  jaspResults[["plotsIndividual"]] <- plotsIndividual
  
  
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
      
      xName  <- expression(paste("Population proportion ", theta))
      
      dfArrowPP   <- NULL
      dfLinesPP   <- NULL
      dfCI        <- NULL
      dfCILinesPP <- NULL

      if(options[[ifelse(type == "Prior", "plotsPriorIndividualCI", "plotsPosteriorIndividualCI")]]){
        
        if(options[[ifelse(type == "Prior", "plotsPriorIndividualType", "plotsPosteriorIndividualType")]] == "Central"){
          
          dfCI <- .dataCentralPP(temp_data, options$priors[[i]], options[[ifelse(type == "Prior", "plotsPriorCoverage", "plotsPosteriorCoverage")]]/100, type = "parameter")
          
        }else if(options[[ifelse(type == "Prior", "plotsPriorIndividualType", "plotsPosteriorIndividualType")]] == "HPD"){
          
          dfCI <- .dataHPDPP(temp_data, options$priors[[i]], options[[ifelse(type == "Prior", "plotsPriorCoverage", "plotsPosteriorCoverage")]]/100, type = "parameter")
          
        }else if(options[[ifelse(type == "Prior", "plotsPriorIndividualType", "plotsPosteriorIndividualType")]] == "Custom"){
          
          dfCI <- .dataCustomPP(temp_data, options$priors[[i]], options[[ifelse(type == "Prior", "plotsPriorLower", "plotsPosteriorLower")]],
                                options[[ifelse(type == "Prior", "plotsPriorUpper", "plotsPosteriorUpper")]], type = "parameter")  
          
        }
      }
      
      
      if(options$priors[[i]]$type == "point"){
        
        dfArrowPP  <- .dataArrowPP(options$priors[[i]])
        
      }else if(options$priors[[i]]$type == "beta"){
        
        dfLinesPP  <- .dataLinesPP(data, options$priors[[i]])
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
      
      p <- .plotIndividual(dfLinesPP, dfArrowPP, dfCI, dfCILinesPP, c(0,1), xName, "Density", nRound = 3)
      temp_plot$plotObject <- p
    }
    
    return()
  }
  
  
}
.plotsIterativeOverlyingBinomial <- function(jaspResults, data, ready, options){
  
  plotsIterative <- createJaspPlot(title = "Iterative Posterior Updating", width = 530, height = 400, aspectRatio = 0.7)
  
  plotsIterative$position <- 7
  plotsIterative$dependOn(c(.binomial_data_dependencies, "plotsIterative", "plotsIterativeCenter",
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
        nFailures  = sum(data$y[1:i] == 0)
      )
      
      temp_results    <- .estimateBinomial(temp_data, options$priors[[h]])
      temp_lines      <- rbind(temp_lines, data.frame(
        y    = temp_results[[options$plotsIterativeCenter]],
        x    = i,
        name = options$priors[[h]]$name
      ))
      
      if(options$plotsIterativeIndividualCI){
        
        if(options$plotsIterativeIndividualType == "Central"){
          temp_CIPP <- .dataCentralPP(temp_data, options$priors[[h]],
                                      options$plotsIterativeCoverage/100, type = "parameter")
        }else if(options$plotsIterativeIndividualType == "HPD"){
          
          temp_CIPP <- .dataHPDPP(temp_data, options$priors[[h]],
                                  options$plotsIterativeCoverage/100, type = "parameter")
          if(nrow(temp_CIPP) == 2)CI_unimodal <- FALSE
          
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
         
        }else{
          stop("More than bimodal CIs are not implemented in the Sequential analysis plot.")
        }
      }
      
    }
    
    plot_data_lines <- c(plot_data_lines, list(temp_lines))
    
    # deal with a possibility of two disjoined CIs
    if(options$plotsIterativeIndividualCI){
      if(CI_unimodal){
        plot_data_CI    <- c(plot_data_CI, list(temp_CI))
      }else{
        plot_data_CI    <- c(plot_data_CI, list(temp_CI1), list(temp_CI2))
      }
    }
    
  }
  
  yName  <- expression(paste("Population proportion  ", theta))
  xName  <- "Iteration"
  
  p <- .plotIterative(plot_data_lines, plot_data_CI, xName = xName, yName = yName, palette = options$colorPalette)
  
  
  plotsIterative$plotObject <- p
  #jaspResults[["plotsIterative"]]$plotObject <- p
  #plotsIterative$addFootnote("Posterior median and 95% CI")
  
  jaspResults[["plotsIterative"]] <- plotsIterative
  return()
}
.plotsIterativeStackedBinomial   <- function(jaspResults, data, ready, options){
  
  plotsIterative <- createJaspContainer(title = "Iterative Posterior Updating")
  
  plotsIterative$position <- 7
  plotsIterative$dependOn(c(.binomial_data_dependencies, "plotsIterative"))
  
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
      
      for(iteration in iter_sequence){
        
        if(options$priors[[i]]$type == "point"){
          
          dfArrowPP   <- .dataArrowPP(options$priors[[i]])
          dfArrowPP$g <- as.character(iteration)
          
          all_arrows  <- c(all_arrows, list(dfArrowPP))
          legend      <- rbind(legend, c(options$priors[[i]]$type, iteration))
          
        }else if(options$priors[[i]]$type == "beta"){
          
          temp_data <- list(
            "nSuccesses" = sum(data$y[0:iteration] == 1),
            "nFailures"  = sum(data$y[0:iteration] == 0)
          )
          
          dfLinesPP   <- .dataLinesPP(temp_data, options$priors[[i]])
          dfLinesPP   <- dfLinesPP[dfLinesPP$g == "Posterior",]
          dfLinesPP$g <- as.character(iteration)
          
          all_lines   <- c(all_lines, list(dfLinesPP))
          legend      <- rbind(legend, c(options$priors[[i]]$type, iteration))
          
        }
        
      }
      
      xName  <- expression(paste("Population proportion ", theta))
      
      temp_plot$plotObject <- .plotStacked(all_lines, all_arrows, legend, xName = xName)
    }
    
    return()
  }
}
.predictionsBinomial <- function(jaspResults, data, ready, options){
  predictionsTable <- createJaspTable(title = "Prediction Summary")
  
  predictionsTable$position <- 8
  predictionsTable$dependOn(c(.binomial_data_dependencies, "predictionTable", "predictionN"))
  
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
  
  if(all(!ready) | (ready[1] & !ready[2])){
    return()
  }else if(ready[2]){
    
    # add rows for each hypothesis
    for(i in 1:length(options$priors)){
      
      temp_results    <- .estimateBinomial(data, options$priors[[i]])
      temp_prediction <- .predictBinomial(data, options$priors[[i]], options)
      
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
    predictionsTable$addFootnote(paste0("Prediction of ", options$predictionN, " ", ifelse(options$predictionN == 1, "observation", "observations"), 
                                        " based on ", data$nSuccesses," ", ifelse(data$nSuccesses == 1, "success", "successes"),
                                        "  and ", data$nFailures, " ",ifelse(data$nFailures == 1, "failure", "failures"), "."))
    
  }
  
}
.plotsPredictionsBinomial    <- function(jaspResults, data, ready, options){
  
  plotsPredictions <- createJaspContainer(title = "Prediction Plots")
  
  plotsPredictions$position <- 9
  plotsPredictions$dependOn(c(.binomial_data_dependencies, "predictionPlot", "predictionN",
                              "plotsPredictionCI","plotsPredictionType", "plotsPredictionCoverage",
                              "plotsPredictionLower", "plotsPredictionUpper"))
  
  jaspResults[["plotsPredictions"]] <- plotsPredictions
  
  
  if(all(!ready) | (ready[1] & !ready[2])){
    
    plotsPredictions[[""]] <- createJaspPlot(title = "", width = 530, height = 400, aspectRatio = 0.7)
    return()
    
  }else if(!ready[1] & ready[2]){
    
    for(i in 1:length(options$priors)){
      plotsPredictions[[options$priors[[i]]$name]] <- createJaspPlot(title = options$priors[[i]]$name,
                                                                    width = 530, height = 400, aspectRatio = 0.7)
    }
    return()
    
  }else{
    
    for(i in 1:length(options$priors)){
      
      temp_plot <- createJaspPlot(title = options$priors[[i]]$name, width = 530, height = 400, aspectRatio = 0.7)
      
      plotsPredictions[[options$priors[[i]]$name]] <- temp_plot
      
      xName  <- "Number of successes"
      yName  <- "Probability"
      xRange <- c(0, options$predictionN)
        
      dfCI   <- NULL
      dfHist <- NULL
      
      if(options$plotsPredictionCI){
        
        if(options$plotsPredictionType == "Central"){
          
          dfCI <- .dataCentralPP(data, options$priors[[i]], options$plotsPredictionCoverage/100,
                                 n = options$predictionN,type = "prediction")
          
        }else if(options$plotsPredictionType == "HPD"){
          
          dfCI <- .dataHPDPP(data, options$priors[[i]], options$plotsPredictionCoverage/100,
                             n = options$predictionN, type = "prediction")
          
        }else if(options$plotsPredictionType == "Custom"){
          
          dfCI <- .dataCustomPP(data, options$priors[[i]],
                                options$plotsPredictionLower, options$plotsPredictionUpper,
                                n = options$predictionN, type = "prediction")  
          
        }
      }
      
      dfHist  <- .dataHistPP(data, options$priors[[i]], options$predictionN)
        
      p <- .plotPrediction(dfHist, dfCI, xRange, xName, yName, nRound = 0)
      temp_plot$plotObject <- p
    }
    
    return()
  }
  
  
}

# computational functions
.estimateBinomial    <- function(data, prior){
  
  if(prior$type == "point"){
    
    output <- list(
      distribution = paste0("point at ", prior$parPoint),
      mean         = prior$parPoint,
      median       = prior$parPoint,
      lCI          = prior$parPoint,
      uCI          = prior$parPoint
    )
    
    if(data$nSuccesses + data$nFailures > 0){
      output$likelihood <- stats::dbinom(data$nSuccesses, data$nSuccesses + data$nFailures, prior$parPoint)
    }else{
      output$likelihood <- NA
    }
    
    return(output)
    
  }else if(prior$type == "beta"){
    
    output <- list(
      distribution = paste0("beta (", prior$parAlpha + data$nSuccesses, ", ",  prior$parBeta + data$nFailures, ")"),
      mean         = (prior$parAlpha + data$nSuccesses) / (prior$parAlpha + data$nSuccesses + prior$parBeta + data$nFailures),
      median       = qbeta(.5,   prior$parAlpha + data$nSuccesses, prior$parBeta + data$nFailures),
      lCI          = qbeta(.025, prior$parAlpha + data$nSuccesses, prior$parBeta + data$nFailures),
      uCI          = qbeta(.975, prior$parAlpha + data$nSuccesses, prior$parBeta + data$nFailures)
    )
    
    if(data$nSuccesses + data$nFailures > 0){
      output$likelihood <- (factorial(data$nSuccesses+data$nFailures)/(factorial(data$nSuccesses)*factorial(data$nFailures)))*
        beta(prior$parAlpha + data$nSuccesses, prior$parBeta + data$nFailures)/beta(prior$parAlpha, prior$parBeta)
    }else{
      output$likelihood <- NA
    }
    
    return(output)
  }
}
.predictBinomial     <- function(data, prior, options){
  
  if(prior$type == "point"){
    
    output <- list(
      distribution = paste0("binomial (", options$predictionN, ", ", prior$parPoint,")"),
      mean         = prior$parPoint * options$predictionN,
      median       = qbinom(.5, options$predictionN, prior$parPoint),
      lCI          = qbinom(    (1 - options$predictionTableCI/100)/2, options$predictionN, prior$parPoint),
      uCI          = qbinom(1 - (1 - options$predictionTableCI/100)/2, options$predictionN, prior$parPoint)
    )
    
    return(output)
    
  }else if(prior$type == "beta"){
    
    output <- list(
      distribution = paste0("beta-binomial (",options$predictionN, ", ", prior$parAlpha + data$nSuccesses, ", ",  prior$parBeta + data$nFailures, ")"),
      mean         = (prior$parAlpha + data$nSuccesses) * options$predictionN / (prior$parAlpha + data$nSuccesses + prior$parBeta + data$nFailures),
      median       = .qbetabinom(.5, options$predictionN, prior$parAlpha + data$nSuccesses, prior$parBeta + data$nFailures),
      lCI          = .qbetabinom(    (1 - options$predictionTableCI/100)/2, options$predictionN, prior$parAlpha + data$nSuccesses, prior$parBeta + data$nFailures),
      uCI          = .qbetabinom(1 - (1 - options$predictionTableCI/100)/2, options$predictionN, prior$parAlpha + data$nSuccesses, prior$parBeta + data$nFailures)
    )
    
    return(output)
  }
}
.betaHDI             <- function(alpha, beta, coverage){
  
  if(alpha >= 1 & beta >= 1){
    
    HDI <- HDInterval::hdi(qbeta, coverage, shape1 = alpha, shape2 = beta)
    
  }else{
    
    x_density <- seq(0, 1, .00001)
    y_density <- dbeta(x_density, shape1 = alpha, shape2 = beta)
    y_density[c(1, length(y_density))] <- 0
    den_beta <- list(
      x = x_density,
      y = y_density
    )
    class(den_beta) <- "density"
    HDI <- HDInterval::hdi(den_beta, coverage, allowSplit = T)
    HDI[HDI == .99999] <- 1
    HDI[HDI == .00001] <- 0
    
  }
  
  HDI <- matrix(as.vector(HDI), ncol = 2)
  return(HDI)
}
.binomialHDI         <- function(n, theta, coverage){
  
  HDI <- HDInterval::hdi(qbinom, coverage, size = n, prob = theta)
  
  HDI <- matrix(as.vector(HDI), ncol = 2)
  return(HDI)
}
.betabinomialHDI     <- function(n, alpha, beta, coverage){
  
  x_density <- 0:n
  y_density <- sapply(x_density,function(s).dbetabinom(s, n, alpha, beta))
  den_beta <- list(
    x = x_density,
    y = y_density
  )
  class(den_beta) <- "density"
  HDI <- HDInterval::hdi(den_beta, coverage, allowSplit = T)
  
  HDI <- matrix(as.vector(HDI), ncol = 2)
  return(HDI)
}
.dbetabinom          <- function(s, n, alpha, beta){
  return((factorial(n)/(factorial(s)*factorial(n - s)))*
           beta(alpha + s, beta + (n - s))/beta(alpha, beta))
}
.pbetabinom          <- function(s, n, alpha, beta){
  return(sum(sapply(0:s, function(i).dbetabinom(i, n, alpha, beta))))
}
.qbetabinom          <- function(p, n, alpha, beta){
  return(c(0:n)[match(TRUE, sapply(0:n, function(s).pbetabinom(s, n, alpha, beta)) >= p)])
}
# plotting functions
.dataLinesPP         <- function(data, prior){
  
  if (prior$parAlpha == 1 && prior$parBeta == 1) {
    
    theta <- seq(0, 1, length.out = 1000)
    
  } else {
    
    theta <- seq(0.001, 0.999, length.out = 1000)
  }
  
  size <- length(theta)
  linesGroup <- c(dbeta(theta, prior$parAlpha + data$nSuccesses, prior$parBeta + data$nFailures),
                  dbeta(theta, prior$parAlpha, prior$parBeta))
  
  
  thetaGroup <- c(theta, theta)
  nameGroup  <- c(rep("Posterior", length(theta)), rep("Prior", length(theta)))
  dat        <- data.frame(x = thetaGroup, y = linesGroup, g = nameGroup)
  return(dat)
}
.dataPointsPP        <- function(data, prior){
  
  theta <- data$nSuccesses / (data$nSuccesses + data$nFailures)
  
  pointXVal <- c(theta, theta)
  
  if(prior$type == "point"){
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
.dataHPDPP           <- function(data, prior, coverage, n = NULL, type = c("parameter", "prediction")){
  
  if(type == "parameter"){
    
    if(prior$type == "point"){
      x <- matrix(prior$parPoint, ncol = 2, nrow = 1)
    }else if(prior$type == "beta"){
      x <- .betaHDI(prior$parAlpha + data$nSuccesses, prior$parBeta + data$nFailures, coverage)
    }
    
  }else if(type == "prediction"){
    
    if(prior$type == "point"){
      x <- .binomialHDI(n, prior$parPoint, coverage)
    }else if(prior$type == "beta"){
      x <- .betabinomialHDI(n, prior$parAlpha + data$nSuccesses, prior$parBeta + data$nFailures, coverage)
    }
    
  }
  
  dat       <- data.frame(x_start = x[,1], x_end = x[,2], g = "HPD", coverage = coverage)
  return(dat)
}
.dataCentralPP       <- function(data, prior, coverage, n = NULL, type = c("parameter", "prediction")){
  
  if(type == "parameter"){
    
    if(prior$type == "point"){
      x <- matrix(prior$parPoint, ncol = 2, nrow = 1)
    }else if(prior$type == "beta"){
      x <- qbeta(c((1 - coverage)/2, 1 - (1 - coverage)/2), prior$parAlpha + data$nSuccesses, prior$parBeta + data$nFailures)
    }
    
  }else if(type == "prediction"){
    
    if(prior$type == "point"){
      x <- qbinom(c((1 - coverage)/2, 1 - (1 - coverage)/2), n, prior$parPoint)
    }else if(prior$type == "beta"){
      x <- c(
        .qbetabinom((1 - coverage)/2,     n, prior$parAlpha + data$nSuccesses, prior$parBeta + data$nFailures),
        .qbetabinom(1 - (1 - coverage)/2, n, prior$parAlpha + data$nSuccesses, prior$parBeta + data$nFailures)
      )
    }
    
  }
  
  dat       <- data.frame(x_start = x[1], x_end = x[2], g = "central", coverage = coverage)
  return(dat)
}
.dataCustomPP        <- function(data, prior, lCI, uCI, n = NULL, type = c("parameter", "prediction")){
  
  if(type == "parameter"){
    
    if(prior$type == "point"){
      coverage <- ifelse(lCI <= prior$parPoint & prior$parPoint <= uCI, 1, 0)
    }else if(prior$type == "beta"){
      coverage <- pbeta(uCI, prior$parAlpha + data$nSuccesses, prior$parBeta + data$nFailures) -
        pbeta(lCI, prior$parAlpha + data$nSuccesses, prior$parBeta + data$nFailures)
    }
    
  }else if(type == "prediction"){
    
    if(prior$type == "point"){
      
      coverage <- sum(sapply(lCI:uCI, function(s)dbinom(s, n, prior$parPoint)))
        
    }else if(prior$type == "beta"){

      coverage <- sum(sapply(lCI:uCI, function(s)
        .dbetabinom(s, n, prior$parAlpha + data$nSuccesses, prior$parBeta + data$nFailures)))
    }
    
  }
  
  dat       <- data.frame(x_start = lCI, x_end = uCI, g = "custom", coverage = coverage)
  return(dat)
}
.dataProportionPP    <- function(data, prior){
  
  theta <- data$nSuccesses / (data$nSuccesses + data$nFailures)
  dat   <- data.frame(x = theta, y = 0, g = "Sample proportion")
  
  return(dat)
}
.dataHistPP          <- function(data, prior, n){
  
  x <- 0:n
  
  if(prior$type == "point"){
    y <- dbinom(x, n, prior$parPoint)
  }else if(prior$type == "beta"){
    y <- sapply(x, function(s).dbetabinom(s, n, prior$parAlpha + data$nSuccesses, prior$parBeta + data$nFailures))
  }
  
  dat <- data.frame(x = x, y = y)
  return(dat)
}
.dataArrowPP         <- function(prior){
  dat       <- data.frame(x = prior$parPoint, y_start = 0, y_end = 1, g = "Prior = Posterior")
  return(dat)
}
.plotPriorPosterior  <- function(dfLine, dfArrow, dfPoints = NULL, xName = NULL, yName = "Density"){
  
  mappingArrow <- ggplot2::aes(x = x, xend = x, y = y_start, yend = y_end, color = g)
  mappingLines <- ggplot2::aes(x = x, y = y, color = g)
  mappingPoint <- ggplot2::aes(x = x, y = y, color = g)
  
  
  g <- ggplot2::ggplot() 
  
  if(!is.null(dfArrow)){
    g <- g +  ggplot2::geom_segment(
      data = dfArrow, mappingArrow,
      size = 1,
      arrow = ggplot2::arrow(length = ggplot2::unit(0.5, "cm")),
      show.legend = F) +
      ggplot2::geom_segment(data = dfArrow, mappingArrow, size = 1)
    
    y_max    <- dfArrow$y_end
    y_breaks <- c(0, dfArrow$y_end)
    y_labels <- c(0, "\U221E" )
    x_high   <- dfArrow$x
  }else{
    
    for(i in 1:length(unique(dfLine$g))){
      temp_line <- dfLine[dfLine$g == unique(dfLine$g)[i], ]
      temp_type <- i
      g <- g + ggplot2::geom_line(
        data = temp_line, mappingLines,
        size = 1, linetype = temp_type)
    }
    
    y_max <- max(dfLine$y)
    y_breaks <- JASPgraphs::getPrettyAxisBreaks(c(0, y_max))
    y_labels <- y_breaks
    x_high   <- dfLine$x[which.max(dfLine$y)]
  }
  
  g <- g + ggplot2::scale_x_continuous(xName, limits = c(0, 1)) + 
    ggplot2::scale_y_continuous(yName,
                                breaks = y_breaks,
                                limits = c(0, y_max),
                                labels = y_labels) 
  
  if(!is.null(dfPoints)){
    
    g <- g + ggplot2::geom_point(data = dfPoints, mapping = mappingPoint, show.legend = TRUE,
                                 inherit.aes = FALSE, size = 4, shape = 4, 
                                 stroke = 1.25, fill = "grey")
    
    if(!is.null(dfArrow)){
      g <- g + ggplot2::scale_color_manual("",
                                           values  = c("black", "black"),
                                           breaks  = c(as.character(dfArrow$g), as.character(unique(dfPoints$g))),
                                           guide   = ggplot2::guide_legend(override.aes = list(
                                             linetype = c(1, NA),
                                             shape    = c(NA, 4)
                                           )))  
    }else{
      g <- g + ggplot2::scale_color_manual("",
                                           values  = c("black", "black", "black")[c(1:length(unique(dfLine$g)), 3)],
                                           breaks  = c(unique(as.character(dfLine$g)), as.character(unique(dfPoints$g))),
                                           guide   = ggplot2::guide_legend(override.aes = list(
                                             linetype = c( 1,  2, NA)[c(1:length(unique(dfLine$g)), 3)],
                                             shape    = c(NA, NA,  4)[c(1:length(unique(dfLine$g)), 3)]
                                           ))) 
    }
    
  }else{
    
    if(!is.null(dfArrow)){
      g <- g + ggplot2::scale_color_manual("",
                                           values  = "black",
                                           breaks  = as.character(dfArrow$g),
                                           guide   = ggplot2::guide_legend(override.aes = list(
                                             linetype = c(1),
                                             shape    = c(NA)
                                           ))) 
    }else{
      g <- g + ggplot2::scale_color_manual("",
                                           values  = c( "black", "black")[1:length(unique(dfLine$g))],
                                           breaks  = c(unique(as.character(dfLine$g))),
                                           guide   = ggplot2::guide_legend(override.aes = list(
                                             linetype = c( 1,  2)[1:length(unique(dfLine$g))],
                                             shape    = c(NA, NA)[1:length(unique(dfLine$g))]
                                           ))) 
    }

  }
  
  
  if (x_high > .5) {
    legend.position = c(0.2, 0.875)
  }else {
    legend.position = c(0.8, 0.875)
  }
  
  g <- g + JASPgraphs::themeJaspRaw(legend.position = legend.position) + 
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
.plotOverlying       <- function(all_lines, all_arrows, dfPoints = NULL, xName = NULL, yName = "Density",
                                 palette = "colorblind"){
  
  mappingLines  <- ggplot2::aes(x = x, y = y, group = g, color = g)
  mappingArrows <- ggplot2::aes(x = x , xend = x, y = y_start, yend = y_end, group = g, color = g)
  
  if(!is.null(all_lines))all_lines  <- do.call("rbind", all_lines)
  if(!is.null(all_arrows))all_arrows <- do.call("rbind", all_arrows)
  
  if(!is.null(all_lines)){
    yBreaks  <- JASPgraphs::getPrettyAxisBreaks(c(0, all_lines$y))  
    obsYmax  <- max(all_lines$y)
  }else{
    yBreaks  <- JASPgraphs::getPrettyAxisBreaks(c(0, all_arrows$y_end))  
    obsYmax  <- max(all_arrows$y_end)
  }
  
  breaksYmax <- yBreaks[length(yBreaks)]
  newymax <- max(1.1 * obsYmax, breaksYmax)
  if(!is.null(all_arrows))all_arrows$y_end <- newymax
  
  g <- ggplot2::ggplot()
  
  if(!is.null(all_arrows)){
    g <- g + ggplot2::geom_segment(
      data = all_arrows, mapping = mappingArrows, size = 1,
      arrow = ggplot2::arrow(length = ggplot2::unit(0.5, "cm")), show.legend = F) +
      ggplot2::geom_segment(data = all_arrows, mappingArrows, size = 1)
  }
  if(!is.null(all_lines)){
    g <- g + ggplot2::geom_line(data = all_lines, mapping = mappingLines, size = 1,)
  }
  
  g <- g + 
    JASPgraphs::scale_JASPcolor_discrete(palette) +
    #ggplot2::scale_colour_manual(values = JASPgraphs::colorBrewerJasp(n = length(unique(all_lines$g)) + length(unique(all_arrows$g)))) +
    ggplot2::scale_x_continuous(xName, limits = c(0, 1))
  
  
  if(!is.null(all_lines)){
    g <- g + ggplot2::scale_y_continuous(yName,
                                         breaks = yBreaks,
                                         limits = c(0, newymax)) 
  }else{
    g <- g + ggplot2::scale_y_continuous(yName,
                                         breaks = c(0, newymax),
                                         limits = c(0, newymax),
                                         labels = c(0, "\U221E" )) 
  }
  
  if(!is.null(all_lines)){
    xr <- range(all_lines$x)
    idx <- which.max(all_lines$y)
    xmax <- all_lines$x[idx]
  }else{
    xr <- range(all_arrows$x)
    idx <- which.max(all_arrows$y_end)
    xmax <- all_arrows$x[idx]
  }
  
  if (xmax > mean(xr)) {
    legend.position = c(0.15, 0.875)
  }else{
    legend.position = c(0.8, 0.875)
  }
  
  g <- g + JASPgraphs::themeJaspRaw(legend.position = legend.position) + 
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
.plotStacked         <- function(all_lines, all_arrows, legend, xName = NULL, yName = "Density"){
  
  mappingLines  <- ggplot2::aes(x = x, y = y, group = g, color = g)
  mappingArrows <- ggplot2::aes(x = x , xend = x, y = y_start, yend = y_end, group = g, color = g)
  mappingLegend <- ggplot2::aes(x = x, y = y, label = name)
  
  if(!is.null(all_lines)){
    
    all_linesD <- all_lines
    for(i in 1:length(all_linesD)){
      all_linesD[[i]] <- rbind.data.frame(
        data.frame(x = 0, y = 0, g = all_linesD[[i]]$g[1]),
        all_linesD[[i]],
        data.frame(x = 1, y = 0, g = all_linesD[[i]]$g[1])     
      )
    }
    
    all_lines  <- do.call("rbind", all_lines)
    all_linesD <- do.call("rbind", all_linesD)
  }
  
  if(!is.null(all_arrows)){
    
    all_arrowsL <- list()
    for(i in 1:length(all_arrows)){
      all_arrowsL[[i]] <- data.frame(y = rep(all_arrows[[i]]$y_start, 2), x = c(0, 1),
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
  legend$x <- 0
  
  # changing y-coordinates to "stack" the plots
  for(i in 1:nrow(legend)){
    if(legend$type[i] == "point"){
      all_arrows[all_arrows$g == legend[i,2], "y_start"] <- all_arrows[all_arrows$g == legend[i,2], "y_start"] + yBreak*(i-1)
      all_arrows[all_arrows$g == legend[i,2], "y_end"]   <- all_arrows[all_arrows$g == legend[i,2], "y_end"]   + yBreak*(i-1)
      all_arrowsL[all_arrowsL$g == legend[i,2], "y"]     <- all_arrowsL[all_arrowsL$g == legend[i,2], "y"]     + yBreak*(i-1)
    }else if(legend$type[i] == "beta"){
      all_lines[all_lines$g == legend[i,2], "y"]   <- all_lines[all_lines$g == legend[i,2], "y"]   + yBreak*(i-1)
      all_linesD[all_linesD$g == legend[i,2], "y"] <- all_linesD[all_linesD$g == legend[i,2], "y"] + yBreak*(i-1)
    }
  }
  
  g <- ggplot2::ggplot()
  
  for(i in nrow(legend):1){
    if(legend$type[i] == "point"){
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
    }
  }
  
  legend$name <- sapply(legend$name, function(x)paste(c(x, "   "), collapse = ""))
  g <- g + ggplot2::geom_text(data = legend, mapping = mappingLegend,
                              size = 8, hjust = 1, vjust = 0, fontface = 1)
  
  g <- g + ggplot2::scale_colour_manual(values = rep("black", nrow(legend))) +
    ggplot2::scale_x_continuous(xName, limits = c(0, 1)) +
    ggplot2::scale_y_continuous(yName) + 
    ggplot2::coord_cartesian(clip = 'off')
  
  
  
  g <- g + JASPgraphs::themeJaspRaw() + 
    JASPgraphs::geom_rangeframe(sides = 'b') + 
    ggplot2::theme(
      legend.title = ggplot2::element_blank(), 
      legend.text  = ggplot2::element_text(margin = ggplot2::margin(0, 0, 2, 0)),
      legend.key.height = ggplot2::unit(1, "cm"),
      legend.key.width  = ggplot2::unit(1.5,"cm"),
      
      axis.line.y  = ggplot2::element_blank(),
      axis.text.y  = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      legend.position = "none", 
      plot.margin  = ggplot2::unit(c(0,0,0,max(sapply(legend$name,nchar))/60), "npc")
    )
  
  plot <- g
  class(plot) <- c("JASPgraphs", class(plot))
  
  return(plot)
}
.plotIterative       <- function(all_lines, all_CI, xName = "Iteration", yName = NULL,
                                 palette = "colorblind"){
  
  all_lines      <- do.call("rbind", all_lines)
  all_lines$name <- factor(all_lines$name, levels = sort(levels(all_lines$name)))
  
  obsXmax    <- max(all_lines$x)
  newXmax    <- obsXmax
  if(obsXmax > 7){
    xBreaks <- round(seq(0, obsXmax, length.out = 7))
  }else{
    xBreaks <- 0:obsXmax
  }
  
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

  g <- g +
    ggplot2::geom_line(
      data    = all_lines,
      mapping = mappingLines, size = 1)
  
  g <- g +
    ggplot2::scale_x_continuous(xName, limits = c(0, newXmax), breaks = xBreaks) +
    ggplot2::scale_y_continuous(yName, limits = c(0, 1)) +
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
.plotIndividual      <- function(LinesPP, ArrowPP, CI, CILinesPP, xRange, xName, yName, nRound = 3){
  
  mappingLines   <- ggplot2::aes(x = x, y = y, group = g,)
  mappingArrows  <- ggplot2::aes(x = x , xend = x, y = y_start, yend = y_end, group = g)
  mappingArrows1 <- ggplot2::aes(x = x_start , xend = x_end, y = y, yend = y, group = g)
  mappingArrows2 <- ggplot2::aes(x = x_end , xend = x_start, y = y, yend = y, group = g)
  mappingText    <- ggplot2::aes(x = x, y = y, label = label)
  
  
  if(!is.null(LinesPP)){
    yBreaks  <- JASPgraphs::getPrettyAxisBreaks(c(0, LinesPP$y))  
    obsYmax  <- max(LinesPP$y)
  }else{
    yBreaks  <- JASPgraphs::getPrettyAxisBreaks(c(0, ArrowPP$y_end))  
    obsYmax  <- max(ArrowPP$y_end)
  }
  
  breaksYmax <- yBreaks[length(yBreaks)]
  newymax <- max(ifelse(!is.null(CI), 1.25, 1.10) * obsYmax, breaksYmax)
  
  if(is.null(CI) & !is.null(ArrowPP))ArrowPP$y_end <- newymax 
  
  if(!is.null(CI)){
    # 'opening' arrows in case the CI is a point
    CI <- cbind.data.frame(CI, "y" = obsYmax * 1.1)
    for(i in 1:nrow(CI)){
      
      if(CI$x_start[i] == CI$x_end[i]){
        
        if(CI$x_start[i] > xRange[1] + .0001 & CI$x_start[i] < xRange[2] - .0001){
          CI$x_start[i] <- CI$x_start[i] - .0001
          CI$x_end[i]   <- CI$x_end[i]   + .0001
        }else if(CI_data$x_start[i] <= xRange[1] + .0001){
          CI$x_start[i] <- xRange[1]
          CI$x_end[i]   <- xRange[1] + .0001
        }else if(CI_data$x_start[i] <= xRange[2] - .0001){
          CI$x_start[i] <- xRange[2] - .0001
          CI$x_end[i]   <- xRange[2]
        }
      }
    }
  }
  
  
  g <- ggplot2::ggplot()
  
  if(!is.null(ArrowPP)){
    g <- g + ggplot2::geom_segment(
      data    = ArrowPP,
      mapping = mappingArrows, size = 1,
      arrow   = ggplot2::arrow(length = ggplot2::unit(0.5, "cm")),
      color   = "black")
  }
  
  if(!is.null(LinesPP)){
    if(!is.null(CILinesPP)){
      g <- g + ggplot2::geom_polygon(
        data = CILinesPP,
        mapping = mappingLines, fill = "grey60", alpha = .8)
    }
    g <- g + ggplot2::geom_line(
      data    = LinesPP,
      mapping = mappingLines, size = 1, color = "black") 
  }
  
  if(!is.null(CI)){
    g <- g + ggplot2::geom_segment(
      data    = CI,
      mapping = mappingArrows1, size = 1,
      arrow   = ggplot2::arrow(length = ggplot2::unit(0.5, "cm")),
      color   = "black") + ggplot2::geom_segment(
        data    = CI,
        mapping = mappingArrows2, size = 1,
        arrow   = ggplot2::arrow(length = ggplot2::unit(0.5, "cm")),
        color   = "black")
    
    
    temp_label <- sapply(1:nrow(CI), function(i)paste(c(
      "[",format(round(CI$x_start[i], nRound), nsmall = nRound),", ",format(round(CI$x_end[i], nRound), nsmall = nRound),"]"
    ), collapse = ""))
    temp_label <- paste(temp_label, collapse = " ∪ " )
    temp_label <- paste(c(round(CI$coverage[1]*100), "% CI: ", temp_label), collapse = "")
    
    temp_text <- data.frame(
      x     = ifelse(nrow(CI) > 1,
                     (xRange[1] + xRange[2])/2,
                     ifelse((CI$x_start + CI$x_end)/2 < xRange[1] + (xRange[2] - xRange[1])/7, 0, 
                            ifelse((CI$x_start + CI$x_end)/2 > xRange[2] - (xRange[2] - xRange[1])/7, 1,
                                   (CI$x_start + CI$x_end)/2))
      ),
      y     = obsYmax * 1.2,
      label = temp_label)
    
    g <- g + ggplot2::geom_text(
      data    = temp_text,
      mapping = mappingText,
      hjust   = ifelse(temp_text$x < xRange[1] + (xRange[2] - xRange[1])/7, 0, 
                       ifelse(temp_text$x > xRange[2] - (xRange[2] - xRange[1])/7, 1, .5)),
      vjust   = .5, size = 6
    )
    
  }
  
  
  g <- g + ggplot2::scale_x_continuous(xName, limits = xRange)
  
  if(!is.null(LinesPP)){
    g <- g + ggplot2::scale_y_continuous(yName,
                                         breaks = yBreaks,
                                         limits = c(0, newymax)) 
  }else{
    g <- g + ggplot2::scale_y_continuous(yName,
                                         breaks = c(0, newymax),
                                         limits = c(0, newymax),
                                         labels = c(0, "\U221E" )) 
  }
  
  
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
.plotPrediction      <- function(dfHist, CI, xRange, xName, yName, nRound = 0){
  
  mappingHistogram  <- ggplot2::aes(x = x, y = y, fill = col)
  mappingArrows1    <- ggplot2::aes(x = x_start_adj , xend = x_end_adj, y = y, yend = y, group = g)
  mappingArrows2    <- ggplot2::aes(x = x_end_adj,  xend = x_start_adj, y = y, yend = y, group = g)
  mappingText       <- ggplot2::aes(x = x, y = y, label = label)
  
  
  yBreaks  <- JASPgraphs::getPrettyAxisBreaks(c(0, dfHist$y))
  xBreaks  <- round(JASPgraphs::getPrettyAxisBreaks(xRange))
  obsYmax  <- max(dfHist$y)
  
  breaksYmax <- yBreaks[length(yBreaks)]
  newymax    <- max(ifelse(!is.null(CI), 1.25, 1.10) * obsYmax, breaksYmax)
  
  dfHist$col <- "a"
  if(!is.null(CI)){
    
    CI <- cbind.data.frame(CI, "y" = obsYmax * 1.1)
    
    CI$x_start_adj <- CI$x_start - .5
    CI$x_end_adj   <- CI$x_end   + .5
    
    for(i in 1:nrow(CI)){
      dfHist$col[dfHist$x >= CI$x_start[i] & dfHist$x <= CI$x_end[i]] <- "b"
    }
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
    g <- g + ggplot2::geom_segment(
      data    = CI,
      mapping = mappingArrows1, size = 1,
      arrow   = ggplot2::arrow(length = ggplot2::unit(0.5, "cm")),
      color   = "black") + ggplot2::geom_segment(
        data    = CI,
        mapping = mappingArrows2, size = 1,
        arrow   = ggplot2::arrow(length = ggplot2::unit(0.5, "cm")),
        color   = "black")
    
    
    temp_label <- sapply(1:nrow(CI), function(i)paste(c(
      "[",format(round(CI$x_start[i], nRound), nsmall = nRound),", ",format(round(CI$x_end[i], nRound), nsmall = nRound),"]"
    ), collapse = ""))
    temp_label <- paste(temp_label, collapse = " ∪ " )
    temp_label <- paste(c(round(CI$coverage[1]*100), "% CI: ", temp_label), collapse = "")
    
    temp_text <- data.frame(
      x     = ifelse(nrow(CI) > 1,
                     (xRange[1] + xRange[2])/2,
                     ifelse((CI$x_start + CI$x_end)/2 < xRange[1] + (xRange[2] - xRange[1])/7,  xRange[1] - .5, 
                            ifelse((CI$x_start + CI$x_end)/2 > xRange[2] - (xRange[2] - xRange[1])/7,  xRange[2] + .5,
                                   (CI$x_start + CI$x_end)/2))
      ),
      y     = obsYmax * 1.2,
      label = temp_label)
    
    g <- g + ggplot2::geom_text(
      data    = temp_text,
      mapping = mappingText,
      hjust   = ifelse(temp_text$x < xRange[1] + (xRange[2] - xRange[1])/7, 0, 
                       ifelse(temp_text$x > xRange[2] - (xRange[2] - xRange[1])/7, 1, .5)),
      vjust   = .5, size = 6
    )
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

  
  g <- g + ggplot2::scale_x_continuous(xName, breaks = xBreaks, limits = c(xRange[1] - .5, xRange[2] + .5))
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
# all settings depenedent on data input
.binomial_data_dependencies <- c("dataType",
                                 "nSuccesses", "nFailures",                                 # for Counts
                                 "data_sequence", "key_success_Seq", "key_failure_Seq",     # for Sequence
                                 "selectedVariable", "key_success_Var", "key_failure_Var")  # for Variable