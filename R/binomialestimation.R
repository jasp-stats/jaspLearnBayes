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

binomialEstimation <- function(jaspResults, dataset, options, state = NULL){

  # lazy test
  if(F){
    options[["priors"]] <- list(
      list(
        name = "H1",
        type = "point",
        parPoint = 0.5),
      list(
        name = "REpetia",
        type = "point",
        parPoint = 0.3),
      list(
        name = "H2",
        type = "beta",
        parAlpha = 1,
        parBeta = 1),
      list(
        name = "H3",
        type = "beta",
        parAlpha = 5,
        parBeta = 1),
      list(
        name = "H4",
        type = "beta",
        parAlpha = 5,
        parBeta = 1),
      list(
        name = "H5",
        type = "beta",
        parAlpha = 5,
        parBeta = 1)
    )
  }
  
  # a vector of two, first for data, second for hypotheses
  ready <- .readyBinomial(options)
  
  # load, check, transform and process data
  if(ready[1])data <- .readDataBinomial(dataset, options)
  
  # summary table if requested (but not if the data counts were added directly)
  if(options$dataSummary & !options$dataType == "dataCounts").summaryBinomial(jaspResults, data, ready)
  
  # estimated parameter values
  .estimatesBinomial(jaspResults, data, ready, options)
  
  # sequentially estimated parameter values
  if(options$doIterative & !options$dataType == "dataCounts").estimatesSequentialBinomial(jaspResults, data, ready, options)
  
  # prior plots
  if(options$plotsPrior).plotsSimpleBinomial(jaspResults, data, ready, options, type = "Prior")
  
  # prior plots
  if(options$plotsPosterior).plotsSimpleBinomial(jaspResults, data, ready, options, type = "Posterior")
  
  # prior and posterior plots
  if(options$plotsBoth).plotsBothBinomial(jaspResults, data, ready, options)
  
  # Iterative plots
  if(options$plotsIterative){
    if(options$plotsIterativeType == "overlying").plotsIterativeOverlyingBinomial(jaspResults, data, ready, options)
    if(options$plotsIterativeType == "stacked").plotsIterativeStackedBinomial(jaspResults, data, ready, options)
  }
  
  return()
}

.readyBinomial       <- function(options){
  # are data ready
  if(options$dataType == "dataCounts"){
    ready <- options$nSuccesses > 0 | options$nFailures > 0
  }else if(options$dataType == "dataSequence"){
    ready <- nchar(options$data_sequence) > 0
  }else if(options$dataType == "dataVariable"){
    ready <- options$selectedVariable != ""
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
    
    if(options$dataType == "dataSequence"){
      
      y <- options$data_sequence

    }else if(options$dataType == "dataVariable"){
      # this is stupidly written #rework
      if (!is.null(dataset)){
        y <- dataset
      }else{
        y <- .readDataSetToEnd(columns = options$selectedVariable)[,1]
      }
      
    }
    
    cleaned_out     <- .cleanDataBinomial(y, options)
    data$y          <- cleaned_out$y
    data$nSuccesses <- sum(data$y == 1)
    data$nFailures  <- sum(data$y == 0)
    data$lSuccesses <- cleaned_out$lSuccesses
    data$lFailures  <- cleaned_out$lFailures
  } 
  
  return(data)
  
}
.cleanDataBinomial   <- function(x, options){
  
  lSuccesses <- NULL -> lFailures
  key <- list(
    c("T", "F"),
    c("S", "F"),
    c("TRUE", "FALSE"),
    c("correct", "incorrect"),
    c("hit", "miss"),
    c(2, 1)
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
    lSuccesses <- key_success
    lFailures <- key_failure
    
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
  
  return(list(
    y          = as.numeric(x),
    lSuccesses = lSuccesses,
    lFailures  = lFailures
    )
  )
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
    summaryTable$addRows(list(variable   = ifelse(!is.null(data$lSuccesses), data$lSuccesses, "Successes"), 
                              counts     = data$nSuccesses, 
                              proportion = data$nSuccesses / (data$nSuccesses + data$nFailures)))
    summaryTable$addRows(list(variable   = ifelse(!is.null(data$lFailures), data$lFailures, "Failures"),
                              counts     = data$nFailures, 
                              proportion = data$nFailures / (data$nSuccesses + data$nFailures)))
    summaryTable$addRows(list(variable   = "Total",
                              counts     = data$nSuccesses + data$nFailures, 
                              proportion = (data$nSuccesses + data$nFailures) / (data$nSuccesses + data$nFailures)))
  }
  
  return()
}
.estimatesBinomial   <- function(jaspResults, data, ready, options){
  estimatesTable <- createJaspTable(title = "Estimation Summary")
  
  estimatesTable$position <- 2
  estimatesTable$dependOn(.binomial_data_dependencies)
  
  estimatesTable$addColumnInfo(name = "hypothesis",   title = "Hypothesis",      type = "string")
  estimatesTable$addColumnInfo(name = "prior",        title = "Prior",           type = "string")
  estimatesTable$addColumnInfo(name = "priorMed",     title = "Prior Median",    type = "number")
  estimatesTable$addColumnInfo(name = "posterior",    title = "Posterior",       type = "string")
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
      temp_results <- .computeBinomial(temp_data, options$priors[[i]])
      
      temp_row <- list(
        prior        = temp_results$distribution,
        priorMed     = temp_results$median,
        hypothesis   = options$priors[[i]]$name, 
        posterior    = "", 
        posteriorMed = "",
        likelihood   = "")
      
      
      if(all(ready)){
        # and when real data are supplied as well, add posterior information
        temp_results <- .computeBinomial(data, options$priors[[i]])
        
        temp_row["posterior"]    <- temp_results$distribution
        temp_row["posteriorMed"] <- temp_results$median
        temp_row["likelihood"]   <- temp_results$likelihood
        
      }
      
      estimatesTable$addRows(temp_row)
    }
    
    # add footnote clarifying what the theta paramater symbolizes
    if(!is.null(data$lSuccesses)){
      estimatesTable$addFootnote(paste0("The proportion of '", data$lSuccesses, "' is used through the analysis."))
    }
    
    
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
      temp_results <- .computeBinomial(data, options$priors[[h]])
      temp_row[[options$priors[[h]]$name]] <- temp_results$distribution
    }
    estimatesSequentialTable$addRows(temp_row)
    
    # then update the posteriors as the data go in
    for(i in 1:length(data$y)){
      temp_row <- NULL
      temp_row[["iteration"]] <- i
      for(h in 1:length(options$priors)){
        temp_data    <- list(
          nSuccesses = sum(data$y[1:i] == 1),
          nFailures  = sum(data$y[1:i] == 0)
        )
        temp_results <- .computeBinomial(temp_data, options$priors[[h]])
        temp_row[[options$priors[[h]]$name]] <- temp_results$distribution
      }
      estimatesSequentialTable$addRows(temp_row)
    }
    
  }
}
.computeBinomial     <- function(data, prior){
  
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
.plotsSimpleBinomial <- function(jaspResults, data, ready, options, type = c("Prior", "Posterior")){
  
  plotsSimple <- createJaspPlot(title = paste0(type, " Plots"), width = 530, height = 400, aspectRatio = 0.7)
  
  plotsSimple$position <- ifelse(type == "Prior", 4, 5)
  plotsSimple$dependOn(c(.binomial_data_dependencies,
                         ifelse(type == "Prior", "plotsPrior", "plotsPosterior"),
                         ifelse(type == "Prior", "plotsPriorType", "plotsPosteriorType")))
  
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
    p <- .plotOverlying(all_lines, all_arrows, xName = xName)
  }else{
    p <- .plotStacked(all_lines, all_arrows, legend, xName = xName)
  }
  
  jaspResults[[type]]$plotObject <- p
  
  return()
}
.plotsBothBinomial   <- function(jaspResults, data, ready, options){
  
  plotsBoth <- createJaspContainer(title = "Prior and Posterior Plots")
  
  plotsBoth$position <- 6
  plotsBoth$dependOn(c(.binomial_data_dependencies, "plotsBoth"))
  
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
      
      xName  <- expression(paste("Population proportion ", theta))
      
      if(options$priors[[i]]$type == "point"){
        
        dfArrowPP <- .dataArrowPP(options$priors[[i]])
        dfPointsPP<- .dataPointsPP(data, options$priors[[i]])
        p <- .plotArrow(dfArrow = dfArrowPP, dfPoints = dfPointsPP, xName = xName)
        
      }else if(options$priors[[i]]$type == "beta"){
        
        dfLinesPP <- .dataLinesPP(data, options$priors[[i]])
        dfPointsPP<- .dataPointsPP(data, options$priors[[i]])
        p <- JASPgraphs::PlotPriorAndPosterior(dfLines = dfLinesPP, dfPoints = dfPointsPP, xName = xName)
        
      }
      
      temp_plot$plotObject <- p
    }
    
    return()
  }
  
  
}
.plotsIterativeOverlyingBinomial <- function(jaspResults, data, ready, options){
  
  plotsIterative <- createJaspPlot(title = "Iterative Posterior Updating", width = 530, height = 400, aspectRatio = 0.7)
  
  plotsIterative$position <- 7
  plotsIterative$dependOn(c(.binomial_data_dependencies, "plotsIterative"))
  
  
  
  if (!all(ready)){
    jaspResults[["plotsIterative"]] <- plotsIterative
    return()
  }
  
  plot_data <- NULL
  for(h in 1:length(options$priors)){
    temp_data    <- list(
      nSuccesses = 0,
      nFailures  = 0
    )
    temp_results <- .computeBinomial(temp_data, options$priors[[h]])
    plot_data <- rbind(plot_data, cbind(data.frame(temp_results), "name" = options$priors[[h]]$name, "i" = 0))
  }
  
  # then update the posteriors as the data go in
  for(i in 1:length(data$y)){
    for(h in 1:length(options$priors)){
      temp_data    <- list(
        nSuccesses = sum(data$y[1:i] == 1),
        nFailures  = sum(data$y[1:i] == 0)
      )
      temp_results <- .computeBinomial(temp_data, options$priors[[h]])
      plot_data <- rbind(plot_data, cbind(data.frame(temp_results), "name" = options$priors[[h]]$name, "i" = i))
    }
  }
  
  yName  <- expression(paste("Population proportion ", theta))
  xName  <- "Iteration"
  
  p <- .plotIterative(plot_data, xName = xName, yName = yName)
  
  
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
.dataArrowPP         <- function(prior){
  dat       <- data.frame(x = prior$parPoint, y_start = 0, y_end = 1, g = "Prior = Posterior")
  return(dat)
}
.plotArrow           <- function(dfArrow, dfPoints = NULL, xName = NULL, yName = "Density",
                                 lineColors = NULL){
  
  mappingArrow <- ggplot2::aes(x = x ,xend = x, y = y_start, yend = y_end, color = g)
  mappingPoint <- ggplot2::aes(x = x, y = y, color = g)
  
  
  g <- ggplot2::ggplot() + 
    ggplot2::geom_segment(
      data = dfArrow, mappingArrow,
      size = 1,
      arrow = ggplot2::arrow(length = ggplot2::unit(0.5, "cm")),
      show.legend = F) +
    ggplot2::geom_segment(data = dfArrow, mappingArrow, size = 1) +
    ggplot2::scale_x_continuous(xName, limits = c(0, 1)) + 
    ggplot2::scale_y_continuous(yName,
                                breaks = c(0, dfArrow$y_end),
                                limits = c(0, dfArrow$y_end),
                                labels = c(0, "\U221E" )) 
  
  if(!is.null(dfPoints)){
    g <- g + ggplot2::geom_point(data = dfPoints, mapping = mappingPoint, show.legend = TRUE,
                                 inherit.aes = FALSE, size = 4, shape = 21, 
                                 stroke = 1.25, fill = "grey")
  }
  
  
  if (dfArrow$x > .5) {
    legend.position = c(0.2, 0.875)
  }else {
    legend.position = c(0.8, 0.875)
  }
  
  g <- g + ggplot2::scale_color_manual("",
                                       values  = c("black", "black"),
                                       breaks  = c(as.character(dfArrow$g), as.character(unique(dfPoints$g))),
                                       guide   = ggplot2::guide_legend(override.aes = list(
                                         linetype = c(1, NA),
                                         shape    = c(NA, 21)
                                         #arrow    = rep(NA, 2)
                                       ))) 
  
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
                                 lineColors = NULL){
  
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
  
  g <- g + ggplot2::scale_colour_manual(values = JASPgraphs::colorBrewerJasp(n = length(unique(all_lines$g)) + length(unique(all_arrows$g)))) +
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
.plotStacked         <- function(all_lines, all_arrows, legend, xName = NULL, yName = "Density",
                                 lineColors = NULL){
  
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
.plotIterative       <- function(plot_data, xName = "Iteration", yName = NULL,
                                 lineColors = NULL){
  
  obsXmax    <- max(plot_data$i)
  newXmax    <- obsXmax
  if(obsXmax > 7){
    xBreaks <- round(seq(0, obsXmax, length.out = 7))
  }else{
    xBreaks <- 0:obsXmax
  }
  
  mappingLines   <- ggplot2::aes(x = i, y = median, 
                                 group = name, color = name)
  mappinglCI     <- ggplot2::aes(x = i, y = lCI, 
                                 group = name, color = name)
  mappinguCI     <- ggplot2::aes(x = i, y = uCI, 
                                 group = name, color = name)
  mappingPolygon <- ggplot2::aes(x = x, y = y, group = name, fill = name)
  
  clr  <- JASPgraphs::colorBrewerJasp(n = length(unique(plot_data$name)))
  clr1 <- clr[order(order(levels(plot_data$name)))]
  
  g <- ggplot2::ggplot()
  
  for(i in length(unique(plot_data$name)):1){
    temp_data <- plot_data[plot_data$name == unique(plot_data$name)[i], ]
    temp_poly <- data.frame(
      x = c(temp_data$i, rev(temp_data$i)),
      y = c(temp_data$lCI, rev(temp_data$uCI)),
      name = rep(temp_data$name,2)
    )
    
    g <- g + 
      ggplot2::geom_polygon(
        data = temp_poly,
        mapping = mappingPolygon, fill = clr1[i], alpha = .3) +
      ggplot2::geom_line(
        data = temp_data,
        mapping = mappinguCI, size = 1, linetype = 2) +
      ggplot2::geom_line(
        data = temp_data,
        mapping = mappinglCI, size = 1, linetype = 2)
  }
  
  for(i in length(unique(plot_data$name)):1){
    temp_data <- plot_data[plot_data$name == unique(plot_data$name)[i], ]
    g <- g +
      ggplot2::geom_line(
        data = temp_data,
        mapping = mappingLines, size = 1)
  }
  
  g <- g +
    ggplot2::scale_x_continuous(xName, limits = c(0, newXmax), breaks = xBreaks) +
    ggplot2::scale_y_continuous(yName, limits = c(0, 1)) +
    ggplot2::scale_colour_manual(values = clr)
  
  if (mean(plot_data$median[plot_data$i == max(plot_data$i)]) > .5) {
    legend.position = c(0.8, 0.03 + length(unique(plot_data$name))/10)
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
# all settings depenedent on data input
.binomial_data_dependencies <- c("dataType",
                                 "nSuccesses", "nFailures",                                 # for Counts
                                 "data_sequence", "key_success_Seq", "key_failure_Seq",     # for Sequence
                                 "selectedVariable", "key_success_Var", "key_failure_Var")  # for Variable