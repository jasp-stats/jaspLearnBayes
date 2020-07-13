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

LSbinomialtesting   <- function(jaspResults, dataset, options, state = NULL){
  
  # a vector of two, first for data, second for hypotheses
  ready <- .readyBinomialLS(options)
  
  # evaluate the expressions in priors
  if(ready[2])options$priors <- .evaluate_priors(options$priors)
  # scale the prior probabilities
  if(ready[2])options$priors <- .scale_priors(options$priors)
  
  # load, check, transform and process data
  if(ready[1])data <- .readDataBinomialLS(dataset, options)
  
  # data summary table if requested (but not if the data counts were added directly)
  if(options$dataSummary & !options$dataType == "dataCounts").summaryBinomialLS(jaspResults, data, ready)
  
  ### inference
  # summary table
  .testsBinomialLS(jaspResults, data, ready, options)
  
  # prior parameter
  if(options$plotsPrior){
    if(options$plotsPriorType != "conditional").plotsSimpleBinomial2LS(jaspResults, data, ready, options, type = "Prior")
    if(options$plotsPriorType == "conditional").plotsIndividualBinomial2LS(jaspResults, data, ready, options, type = "Prior")
  }
  
  # prior predictive
  if(options$plotsPredictions){
    if(options$plotsPredictionType != "conditional").plotsPredictionsBinomial2LS(jaspResults, data, ready, options, type = "Prior")
    if(options$plotsPredictionType == "conditional").plotsPredictionsIndividualBinomial2LS(jaspResults, data, ready, options, type = "Prior")
  }
  
  # predictive accuracy
  if(options$plotsPredictiveAccuracy).plotsPredAccuracyBinomial2LS(jaspResults, data, ready, options)
  
  # posterior parameter
  if(options$plotsPosterior){
    if(options$plotsPosteriorType != "conditional").plotsSimpleBinomial2LS(jaspResults, data, ready, options, type = "Posterior")
    if(options$plotsPosteriorType == "conditional").plotsIndividualBinomial2LS(jaspResults, data, ready, options, type = "Posterior")
  }
  
  ### sequential analysis
  if(options$plotsIterative).plotsIterativeOverlyingBinomial2LS(jaspResults, data, ready, options)
  
  if(options$plotsIterative & options$plotsIterativeUpdatingTable).tableIterativeBinomial2LS(jaspResults, data, ready, options)
  
  # posterior predictive
  if(options$plotsPredictionsPost){
    if(options$predictionPostPlotType != "conditional").plotsPredictionsBinomial2LS(jaspResults, data, ready, options, type = "Posterior")
    if(options$predictionPostPlotType == "conditional").plotsPredictionsIndividualBinomial2LS(jaspResults, data, ready, options, type = "Posterior")
  }
  
  return()
}


.testsBinomialLS   <- function(jaspResults, data, ready, options){
  testsTable <- createJaspTable(title = "Testing Summary")
  
  testsTable$position <- 2
  testsTable$dependOn(.BinomialLS_data_dependencies)
  
  testsTable$addColumnInfo(name = "hypothesis",   title = "Hypothesis",          type = "string")
  testsTable$addColumnInfo(name = "prior",        title = "P(H)",                type = "number")
  testsTable$addColumnInfo(name = "log_lik",      title = "log(likelihood)",     type = "number")
  testsTable$addColumnInfo(name = "posterior",    title = "P(H|data)",           type = "number")
  
  testsTable$setExpectedSize(length(options$priors))
  
  jaspResults[["testsTable"]] <- testsTable
  
  if(ready[1] & !ready[2]){
    
    return()
    
  }else if(!ready[1]){
    
    jaspResults[["testsTable"]]$setError("Please specify successes and failures.")
    
  }else if(ready[2]){
    
    temp_results <- .testBinomialLS(data, options$priors)
    
    for(i in 1:length(options$priors)){
      
      temp_row <- list(
        hypothesis  = options$priors[[i]]$name,
        prior       = temp_results$prior[i],
        log_lik     = temp_results$log_lik[i], 
        posterior   = temp_results$posterior[i])
      
      testsTable$addRows(temp_row)
    }
    
    # add footnote clarifying what dataset was used
    testsTable$addFootnote(paste0("These results are based on ", data$nSuccesses," ", ifelse(data$nSuccesses == 1, "success", "successes"),
                                  "  and ", data$nFailures, " ",ifelse(data$nFailures == 1, "failure", "failures"), "."))
    
  }
  
}
.plotsSimpleBinomial2LS       <- function(jaspResults, data, ready, options, type = c("Prior", "Posterior")){
  
  title <- paste0(
    tools::toTitleCase(options[[ifelse(type == "Prior", "plotsPriorType", "plotsPosteriorType")]]),
    " ",type, " ", "Plot")
  plotsSimple <- createJaspPlot(title = title, width = 530, height = 400, aspectRatio = 0.7)
  
  plotsSimple$position <- ifelse(type == "Prior", 3, 6)
  plotsSimple$dependOn(c(.BinomialLS_data_dependencies,
                         ifelse(type == "Prior", "plotsPrior",                 "plotsPosterior"),
                         ifelse(type == "Prior", "plotsPriorJointType",        "plotsPosteriorJointType"),
                         ifelse(type == "Prior", "plotsPrior",                 "plotsPosterior"),
                         ifelse(type == "Prior", "plotsPriorMarginalCI",       "plotsPosteriorMarginalCI"),
                         ifelse(type == "Prior", "plotsPriorMarginalCoverage", "plotsPosteriorMarginalCoverage"),
                         ifelse(type == "Prior", "plotsPriorMarginalLower",    "plotsPosteriorMarginalLower"),
                         ifelse(type == "Prior", "plotsPriorMarginalUpper",    "plotsPosteriorMarginalUpper"),
                         "colorPalette", "scaleSpikes"))
  
  
  jaspResults[[type]] <- plotsSimple
  
  if (!all(ready))return()
  
  all_lines    <- c()
  all_arrows   <- c()
  legend       <- NULL
  temp_results <- .testBinomialLS(data, options$priors)
  
  for(i in 1:length(options$priors)){
    
    if(options$priors[[i]]$type == "spike"){
      
      dfArrowPP   <- .dataArrowPPLS(options$priors[[i]])
      dfArrowPP$g <- options$priors[[i]]$name
      
      all_arrows  <- c(all_arrows, list(dfArrowPP))
      legend      <- rbind(legend, c(options$priors[[i]]$type, options$priors[[i]]$name))
      
    }else if(options$priors[[i]]$type == "beta"){
      
      dfLinesPP   <- .dataLinesPPLS(data, options$priors[[i]])
      dfLinesPP   <- dfLinesPP[dfLinesPP$g == type,]
      dfLinesPP$y <- exp(log(dfLinesPP$y)+log(temp_results[i, tolower(type)]))
      dfLinesPP$g <- options$priors[[i]]$name
      
      all_lines   <- c(all_lines, list(dfLinesPP))
      legend      <- rbind(legend, c(options$priors[[i]]$type, options$priors[[i]]$name))
      
    }
  }
  
  if(options$plotsPredictionsObserved){
    dfPoints <- data.frame(
      x = data$nSuccesses/(data$nSuccesses + data$nFailures),
      y = 0,
      g = "Observed"
    )
  }else{
    dfPoints <- NULL
  }
  
  xName  <- bquote("Population proportion"~theta)
  
  if(options[[ifelse(type == "Prior", "plotsPriorType", "plotsPosteriorType")]] == "joint"){
    
    if(options[[ifelse(type == "Prior", "plotsPriorJointType", "plotsPosteriorJointType")]] == "overlying"){
      p <- .plotOverlyingLS(all_lines, all_arrows, dfPoints, xName = xName, palette = options$colorPalette)  
    }else if(options[[ifelse(type == "Prior", "plotsPriorJointType", "plotsPosteriorJointType")]] == "stacked"){
      p <- .plotStackedLS(all_lines, all_arrows, legend, xName = xName)
    }
    
    
  }else if(options[[ifelse(type == "Prior", "plotsPriorType", "plotsPosteriorType")]] == "marginal"){
    
    all_lines_new <- c()
    
    if(length(all_lines) > 0){
      
      for(i in 1:length(all_lines)){
        
        if(i == 1){
          all_lines_new[[1]] <- all_lines[[i]]
        }else{
          all_lines_new[[1]]$y <- all_lines_new[[1]]$y + all_lines[[i]]$y
        }
        
      }
      all_lines_new[[1]]$g <- "__marginal"
    }
    
    if(length(all_arrows) > 0){
      for(i in 1:length(all_arrows)){
        all_arrows[[i]]$g <- "__marginal"
      }
    }
    
    if(options[[ifelse(type == "Prior", "plotsPriorMarginalCI", "plotsPosteriorMarginalCI")]]){
      
      all_spikes   <- list()
      if(type == "Prior"){
        for(i in 1:length(options$priors)){
          if(options$priors[[i]]$type == "spike"){
            all_spikes <- c(
              all_spikes, 
              list(data.frame(y = options$priors[[i]]$PH, x = options$priors[[i]]$parPoint, g = "__marginal"))
            )
          }
        }
      }else{
        temp_results <- .testBinomialLS(data, options$priors)
        for(i in 1:length(options$priors)){
          if(options$priors[[i]]$type == "spike"){
            all_spikes <- c(
              all_spikes, 
              list(data.frame(y = temp_results$posterior[i], x = options$priors[[i]]$parPoint, g = "__marginal"))
            )
          }
        }
        
      }
      
      if(options[[ifelse(type == "Prior", "plotsPriorMarginalType", "plotsPosteriorMarginalType")]] == "central"){
        
        dfCI <- .marginalCentralLS(all_lines_new[[1]], all_spikes, 
                                   options[[ifelse(type == "Prior", "plotsPriorMarginalCoverage", "plotsPosteriorMarginalCoverage")]])
        
      }else if(options[[ifelse(type == "Prior", "plotsPriorMarginalType", "plotsPosteriorMarginalType")]] == "HPD"){
        
        dfCI <- .marginalHPDLS(all_lines_new[[1]], all_spikes,
                               options[[ifelse(type == "Prior", "plotsPriorMarginalCoverage", "plotsPosteriorMarginalCoverage")]])    
        
      }else if(options[[ifelse(type == "Prior", "plotsPriorMarginalType", "plotsPosteriorMarginalType")]] == "custom"){
        
        dfCI <- .marginalCustomLS(all_lines_new[[1]], all_spikes,
                                  lCI = options[[ifelse(type == "Prior", "plotsPriorMarginalLower", "plotsPosteriorMarginalLower")]],
                                  uCI = options[[ifelse(type == "Prior", "plotsPriorMarginalUpper", "plotsPosteriorMarginalUpper")]])
        
      }else if(options[[ifelse(type == "Prior", "plotsPriorMarginalType", "plotsPosteriorMarginalType")]] == "support"){
        
        dfCI <- .marginalSupportLS(data, options$priors, all_lines_new[[1]], all_spikes, options$plotsPosteriorMarginalBF)
        
      }
      
    }else{
      dfCI <- NULL
    }
    
    p <- .plotOverlyingLS(all_lines_new, all_arrows, dfPoints, CI = dfCI, xName = xName, no_legend = T)
    
  }
  
  jaspResults[[type]]$plotObject <- p
  
  return()
}
.plotsIndividualBinomial2LS   <- function(jaspResults, data, ready, options, type = c("Prior", "Posterior")){
  
  plotsIndividual <- createJaspContainer(title = paste0("Conditional ", type, " Plots"))
  
  plotsIndividual$position <- ifelse(type == "Prior", 3, 6)
  plotsIndividual$dependOn(c(.BinomialLS_data_dependencies,
                             ifelse(type == "Prior", "plotsPrior",             "plotsPosterior"),
                             ifelse(type == "Prior", "plotsPriorType",         "plotsPosteriorType"),
                             ifelse(type == "Prior", "plotsPriorTypeCI",       "plotsPosteriorTypeCI"),
                             ifelse(type == "Prior", "plotsPriorCoverage",     "plotsPosteriorCoverage"),
                             ifelse(type == "Prior", "plotsPriorLower",        "plotsPosteriorLower"),
                             ifelse(type == "Prior", "plotsPriorUpper",        "plotsPosteriorUpper"),
                             "scaleSpikes"))
  
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
    
    temp_results <- .testBinomialLS(data, options$priors)
    
    for(i in 1:length(options$priors)){
      
      temp_plot <- createJaspPlot(title = options$priors[[i]]$name, width = 530, height = 400, aspectRatio = 0.7)
      
      plotsIndividual[[options$priors[[i]]$name]] <- temp_plot
      
      xName  <- bquote("Population proportion"~theta)
      
      dfArrowPP   <- NULL
      dfLinesPP   <- NULL
      dfCI        <- NULL
      dfCILinesPP <- NULL
      
      if(options[[ifelse(type == "Prior", "plotsPriorCI", "plotsPosteriorCI")]]){
        
        if(options[[ifelse(type == "Prior", "plotsPriorTypeCI", "plotsPosteriorTypeCI")]] == "central"){
          
          dfCI <- .dataCentralPPLS(temp_data, options$priors[[i]], options[[ifelse(type == "Prior", "plotsPriorCoverage", "plotsPosteriorCoverage")]], type = "parameter")
          
        }else if(options[[ifelse(type == "Prior", "plotsPriorTypeCI", "plotsPosteriorTypeCI")]] == "HPD"){
          
          dfCI <- .dataHPDPPLS(temp_data, options$priors[[i]], options[[ifelse(type == "Prior", "plotsPriorCoverage", "plotsPosteriorCoverage")]], type = "parameter")
          
        }else if(options[[ifelse(type == "Prior", "plotsPriorTypeCI", "plotsPosteriorTypeCI")]] == "custom"){
          
          dfCI <- .dataCustomPPLS(temp_data, options$priors[[i]], options[[ifelse(type == "Prior", "plotsPriorLower", "plotsPosteriorLower")]],
                                  options[[ifelse(type == "Prior", "plotsPriorUpper", "plotsPosteriorUpper")]], type = "parameter")  
          
        }else if(options[[ifelse(type == "Prior", "plotsPriorTypeCI", "plotsPosteriorTypeCI")]] == "support"){
          
          dfCI <- .dataSupportPPLS(temp_data, options$priors[[i]], options$plotsPosteriorBF)  
          
        }
      }
      
      
      if(options$priors[[i]]$type == "spike"){
        
        dfArrowPP  <- .dataArrowPPLS(options$priors[[i]])
        
      }else if(options$priors[[i]]$type == "beta"){
        
        dfLinesPP   <- .dataLinesPPLS(data, options$priors[[i]])
        dfLinesPP   <- dfLinesPP[dfLinesPP$g == type,]
        dfLinesPP$y <- dfLinesPP$y*temp_results[i, tolower(type)]
        
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
      
      p <- .plotIndividualLS(dfLinesPP, dfArrowPP, dfCI, dfCILinesPP, c(0,1), xName, "Density", nRound = 3)
      temp_plot$plotObject <- p
    }
    
    return()
  }
  
  
}
.plotsPredictionsBinomial2LS  <- function(jaspResults, data, ready, options, type = c("Prior", "Posterior")){
  
  title <- paste0(
    tools::toTitleCase(options[[ifelse(type == "Prior", "plotsPredictionType", "plotsPredictionPostType")]]),
    " ",type, " ", "Plot")
  plotsPredictions <- createJaspPlot(title = title, width = 530, height = 400, aspectRatio = 0.7)
  
  plotsPredictions$position <- ifelse(type == "Prior",4,7)
  plotsPredictions$dependOn(c(.BinomialLS_data_dependencies,
                              ifelse(type == "Prior", "plotsPredictions",                  "plotsPredictionsPost"),
                              ifelse(type == "Prior", "plotsPredictionType",               "plotsPredictionPostType"),
                              ifelse(type == "Prior", "plotsPredictionPostMarginalTypeCI", "plotsPredictionPostMarginalTypeCI"),
                              ifelse(type == "Prior", "plotsPredictionMarginalCoverage",   "plotsPredictionPostMarginalCoverage"),
                              ifelse(type == "Prior", "plotsPredictionMarginalLower",      "plotsPredictionPostMarginalLower"),
                              ifelse(type == "Prior", "plotsPredictionMarginalUpper",      "plotsPredictionPostMarginalUpper"),
                              ifelse(type == "Prior", "plotsPredictionsObserved",          "predictionPostPlotProp"),
                              "colorPalette"))
  
  jaspResults[["plotsPredictions"]] <- plotsPredictions
  
  
  if(!all(ready) | (data$nSuccesses == 0 & data$nFailures == 0)){
    return()
  }else{
    
    if(type == "Prior"){
      predictionN  <- data$nSuccesses + data$nFailures
      temp_results <- .testBinomialLS(data, options$priors)
      temp_data    <- data.frame(
        nSuccesses = 0,
        nFailures  = 0
      )
    }else if(type == "Posterior"){
      predictionN  <- options$predictionN
      temp_results <- .testBinomialLS(data, options$priors)
      temp_data    <- data
    }
    
    if(type == "Posterior" & options$predictionPostPlotProp){
      xName  <- "Sample proportions"
      yName  <- "Density"
      xRange <- c(-.5/predictionN, 1 + .5/predictionN)
      proportions <- options$predictionPostPlotProp
      nRound <- 3
    }else{
      xName  <- "Number of successes"
      yName  <- "Probability"
      xRange <- c(-.5, predictionN + .5)
      nRound <- 0
      proportions <- FALSE
    }
    
    
    all_lines  <- c()
    legend     <- NULL
    
    for(i in 1:length(options$priors)){
      
      dfHist   <- .dataHistPPLS2(temp_data, options$priors[[i]], predictionN)
      dfHist$g <- options$priors[[i]]$name
      dfHist$y <- dfHist$y*temp_results[i,ifelse(type == "Prior","prior","posterior")]
      
      if(type == "Posterior" & options$predictionPostPlotProp){
        dfHist$x <- dfHist$x/predictionN
      }
      
      # it's not beta, but I'm lazzy to rewrite a function I wanna use
      legend   <- rbind(legend, c("beta", options$priors[[i]]$name))
      all_lines<- c(all_lines, list(dfHist))
    }
    
    if(type == "Prior"){
      if(options$plotsPredictionsObserved){
        dfPoint <- data.frame(x = data$nSuccesses, y = 0)
      }else{
        dfPoint <- NULL
      }
    }else{
      dfPoint <- NULL
    }
    
    
    if(options[[ifelse(type == "Prior","predictionPlotType", "predictionPostPlotType")]] == "joint"){
      
      if(options[[ifelse(type == "Prior", "plotsPriorJointType", "plotsPrJointType")]] == "overlying"){
        p <- .plotOverlyingLS(all_lines, all_arrows, dfPoints, xName = xName, palette = options$colorPalette)  
      }else if(options[[ifelse(type == "Prior", "plotsPriorType", "plotsPriorJointType")]] == "stacked"){
        p <- .plotStackedLS(all_lines, all_arrows, legend, xName = xName)
      }
      
      p <- .plotOverlyingLS(all_lines, NULL, dfPoints = dfPoint, xName = xName, yName = yName, xRange = xRange,
                            palette = options$colorPalette, nRound = nRound, proportions = proportions)
      
    }else if(options[[ifelse(type == "Prior","predictionPlotType", "predictionPostPlotType")]] == "marginal"){
      
      if(length(all_lines) > 0){
        
        for(i in 1:length(all_lines)){
          
          if(i == 1){
            all_lines_new <- all_lines[[i]]
          }else{
            all_lines_new$y <- all_lines_new$y + all_lines[[i]]$y
          }
          
        }
        all_lines_new$g <- "__marginal"
      }
      
      all_lines_new   <- all_lines_new[seq(1,nrow(all_lines_new),2),]
      if(type == "Posterior" & options$predictionPostPlotProp){
        all_lines_new$x <- all_lines_new$x + .5/predictionN
      }else{
        all_lines_new$x <- all_lines_new$x + .5
      }
      
      if(type == "Prior"){
        if(options$plotsPredictionsObserved){
          xBlacked <- data$nSuccesses
        }else{
          xBlacked <- NULL
        }
      }else{
        xBlacked <- NULL
      }
      
      
      if(options[[ifelse(type == "Prior", "plotsPredictionMarginalCI", "plotsPredictionPostMarginalCI")]]){
        
        if(options[[ifelse(type == "Prior", "plotsPredictionMarginalTypeCI", "plotsPredictionPostMarginalTypeCI")]] == "central"){
          
          dfCI <- .marginalCentralLS(all_lines_new, NULL, options$plotsPredictionMarginalCoverage, 0, predictionN, TRUE)        
          
        }else if(options[[ifelse(type == "Prior", "plotsPredictionMarginalTypeCI", "plotsPredictionPostMarginalTypeCI")]] == "HPD"){
          
          dfCI <- .marginalHPDLS(all_lines_new, list(),
                                 options[[ifelse(type == "Prior", "plotsPredictionMarginalCoverage", "plotsPredictionPostMarginalCoverage")]],
                                 0, predictionN, TRUE)    
          
        }else if(options[[ifelse(type == "Prior", "plotsPredictionMarginalTypeCI", "plotsPredictionPostMarginalTypeCI")]] == "custom"){
          
          dfCI <- .marginalCustomLS(all_lines_new, list(),
                                    lCI = options[[ifelse(type == "Prior", "plotsPredictionMarginalLower", "plotsPredictionPostMarginalLower")]],
                                    uCI = options[[ifelse(type == "Prior", "plotsPredictionMarginalUpper", "plotsPredictionPostMarginalUpper")]],
                                    TRUE)
          
          if(options[[ifelse(type == "Prior", "plotsPredictionMarginalUpper", "plotsPredictionPostMarginalUpper")]]
             > predictionN){
            
            plotsPredictions$setError("The upper CI limit is higher than the number of future 
                                       observations. Please change the value of the upper CI limit 
                                       in the settings panel.")
            
            return()
          }
          if(options[[ifelse(type == "Prior", "plotsPredictionMarginalLower", "plotsPredictionPostMarginalLower")]]
             > predictionN){
            
            plotsPredictions$setError("The lower CI limit is higher than the number of future 
                                       observations. Please change the value of the lower CI limit 
                                       in the settings panel.")
            
            return()
          }
          if(options[[ifelse(type == "Prior", "plotsPredictionMarginalLower", "plotsPredictionPostMarginalLower")]] > 
             options[[ifelse(type == "Prior", "plotsPredictionMarginalUpper", "plotsPredictionPostMarginalUpper")]]){
            
            plotsPredictions$setError("The lower CI limit is higher than the upper CI limit.
                                       Please change the value of the CI limits 
                                       in the settings panel.")
            
            return()
          }
          
        }
      }else{
        dfCI <- NULL
      }
      
      if(type == "Posterior" & options$predictionPostPlotProp){
        if(options$plotsPredictionPostMarginalCI){
          dfCI$x_start <- dfCI$x_start / predictionN
          dfCI$x_end   <- dfCI$x_end   / predictionN
        }
        xRange <- c(-.5/predictionN, 1 + .5/predictionN)
      }else{
        xRange <- c(0, predictionN)
      }
      
      
      p <- .plotPredictionLS(all_lines_new, dfCI, xRange = xRange, xName = xName, yName = yName, nRound = nRound, xBlacked = xBlacked,
                             proportions = proportions, predictionN = predictionN)
      
    }else{
      p <- .plotStackedLS(all_lines, NULL, legend, dfPoints = dfPoint, xName = xName, xRange = xRange, proportions = proportions)
    }
    
    jaspResults[["plotsPredictions"]]$plotObject <- p
    return()
  }
  
}
.plotsPredictionsIndividualBinomial2LS  <- function(jaspResults, data, ready, options, type = c("Prior", "Posterior")){
  
  plotsPredictionsIndividual <- createJaspContainer(title = paste0("Conditional ",type," Prediction Plots"))
  
  plotsPredictionsIndividual$position <- ifelse(type == "Prior",4,7)
  plotsPredictionsIndividual$dependOn(c(.BinomialLS_data_dependencies,
                                        ifelse(type == "Prior", "plotsPredictions",       "plotsPredictionsPost"),
                                        ifelse(type == "Prior", "plotsPredictionType",    "plotsPredictionPostType"),
                                        ifelse(type == "Prior", "plotsPredictionCI",      "plotsPredictionPostCI"),
                                        ifelse(type == "Prior", "plotsPredictionTypeCI",  "plotsPredictionPostTypeCI"),
                                        ifelse(type == "Prior", "plotsPredictionCoverage","plotsPredictionPostCoverage"),
                                        ifelse(type == "Prior", "plotsPredictionLower",   "plotsPredictionPostLower"),
                                        ifelse(type == "Prior", "plotsPredictionUpper",   "plotsPredictionPostUpper"),
                                        ifelse(type == "Prior", "",                       "predictionPostPlotProp")
  ))
  
  
  jaspResults[["plotsPredictionsIndividual"]] <- plotsPredictionsIndividual
  
  
  if(all(!ready) | (ready[1] & !ready[2])){
    
    plotsPredictionsIndividual[[""]] <- createJaspPlot(title = "", width = 530, height = 400, aspectRatio = 0.7)
    return()
    
  }else if((!ready[1] & ready[2]) | (data$nSuccesses == 0 & data$nFailures == 0)){
    
    for(i in 1:length(options$priors)){
      plotsPredictionsIndividual[[options$priors[[i]]$name]] <- createJaspPlot(title = options$priors[[i]]$name,
                                                                               width = 530, height = 400, aspectRatio = 0.7)
    }
    return()
    
  }else{
    
    if(type == "Prior"){
      predictionN  <- data$nSuccesses + data$nFailures
      temp_results <- .testBinomialLS(data, options$priors)
      temp_data    <- data.frame(
        nSuccesses = 0,
        nFailures  = 0
      )
    }else if(type == "Posterior"){
      predictionN  <- options$predictionN
      temp_results <- .testBinomialLS(data, options$priors)
      temp_data    <- data
    }
    
    for(i in 1:length(options$priors)){
      
      temp_plot <- createJaspPlot(title = options$priors[[i]]$name, width = 530, height = 400, aspectRatio = 0.7)
      
      plotsPredictionsIndividual[[options$priors[[i]]$name]] <- temp_plot
      
      if(type == "Posterior" & options$predictionPostPlotProp){
        xName  <- "Sample proportions"
        yName  <- "Density"
        xRange <- c(-.5/predictionN, 1 + .5/predictionN)
        proportions <- options$predictionPostPlotProp
      }else{
        xName  <- "Number of successes"
        yName  <- "Probability"
        xRange <- c(0, predictionN)
        proportions <- FALSE
      }
      
      
      dfCI   <- NULL
      dfHist <- NULL
      
      if(options[[ifelse(type == "Prior","plotsPredictionCI","plotsPredictionPostCI")]]){
        
        if(options[[ifelse(type == "Prior","plotsPredictionTypeCI","plotsPredictionPostTypeCI")]] == "central"){
          
          dfCI <- .dataCentralPPLS(data, options$priors[[i]],
                                   options[[ifelse(type == "Prior","plotsPredictionCoverage","plotsPredictionPostCoverage")]],
                                   n = predictionN,type = "prediction")
          
        }else if(options[[ifelse(type == "Prior","plotsPredictionTypeCI","plotsPredictionPostTypeCI")]] == "HPD"){
          
          dfCI <- .dataHPDPPLS(data, options$priors[[i]],
                               options[[ifelse(type == "Prior","plotsPredictionCoverage","plotsPredictionPostCoverage")]],
                               n = predictionN, type = "prediction")
          
        }else if(options[[ifelse(type == "Prior","plotsPredictionTypeCI","plotsPredictionPostTypeCI")]] == "custom"){
          
          dfCI <- .dataCustomPPLS(data, options$priors[[i]],
                                  options[[ifelse(type == "Prior","plotsPredictionLower","plotsPredictionPostLower")]],
                                  options[[ifelse(type == "Prior","plotsPredictionUpper","plotsPredictionPostUpper")]],
                                  n = predictionN, type = "prediction")
          
          if(options[[ifelse(type == "Prior","plotsPredictionUpper","plotsPredictionPostUpper")]] > predictionN){
            
            plotsPredictionsIndividual[[options$priors[[i]]$name]]$setError("The upper CI limit is higher than the number of future 
                                                                            observations. Please, change the value of the upper CI limit 
                                                                            in the settings panel.")
            
            return()
          }
          if(options[[ifelse(type == "Prior","plotsPredictionLower","plotsPredictionPostLower")]]  > predictionN){
            
            plotsPredictionsIndividual[[options$priors[[i]]$name]]$setError("The lower CI limit is higher than the number of future 
                                                                            observations. Please, change the value of the lower CI limit 
                                                                            in the settings panel.")
            
            return()
          }
          if(options[[ifelse(type == "Prior","plotsPredictionLower","plotsPredictionPostLower")]] 
             > options[[ifelse(type == "Prior","plotsPredictionUpper","plotsPredictionPostUpper")]]){
            
            plotsPredictionsIndividual[[options$priors[[i]]$name]]$setError("The lower CI limit is higher than the upper CI limit.
                                                                             Please, change the value of the CI limits 
                                                                             in the settings panel.")
            
            return()
          }
          
        }
      }
      
      dfHist  <- .dataHistPPLS(temp_data, options$priors[[i]], predictionN)
      
      dfHist$y<- dfHist$y*temp_results[i,ifelse(type == "Prior","prior","posterior")]
      
      if(type == "Prior"){
        if(options$plotsPredictionsObserved){
          xBlacked <- data$nSuccesses
        }else{
          xBlacked <- NULL
        }
      }else{
        xBlacked <- NULL
      }
      
      if(type == "Posterior" & options$predictionPostPlotProp){
        dfHist$x <- dfHist$x/predictionN
        if(options$plotsPredictionPostCI){
          dfCI$x_start <- dfCI$x_start/predictionN
          dfCI$x_end   <- dfCI$x_end  /predictionN
        }
        nRound <- 3
      }else{
        nRound <- 0
      }
      
      p <- .plotPredictionLS(dfHist, dfCI, xRange, xName, yName, nRound = nRound, xBlacked = xBlacked,
                             proportions = proportions, predictionN = predictionN)
      temp_plot$plotObject <- p
    }
    
    return()
  }
  
  
}
.plotsPredAccuracyBinomial2LS <- function(jaspResults, data, ready, options){
  
  title <- paste0(tools::toTitleCase(options$predictiveAccuracyType), " Predictive Accuracy Plot")
  
  plotsPredAccuracy <- createJaspPlot(title = title, width = 530, height = 400, aspectRatio = 0.7)
  
  plotsPredAccuracy$position <- 5
  plotsPredAccuracy$dependOn(c(.BinomialLS_data_dependencies,
                               "plotsPredictiveAccuracy", "predictiveAccuracyType", "colorPalette"))
  
  jaspResults[["plotsPredAccuracy"]] <- plotsPredAccuracy
  
  
  if(!all(ready) | (data$nSuccesses == 0 & data$nFailures == 0)){
    return()
  }else{
    
    predictionN  <- data$nSuccesses + data$nFailures
    temp_results <- .testBinomialLS(data, options$priors)
    
    dfHist_all   <- NULL
    xRange       <- c(0, predictionN)
    xName        <- "Hypothesis"
    yName        <- "Probability"
    
    
    if(options$predictiveAccuracyType == "conditional"){
      temp_y <- exp(temp_results[,"log_lik"])
    }else if(options$predictiveAccuracyType == "joint"){
      temp_y <- exp(temp_results[,"log_lik"])*temp_results[,"prior"]       
    }else if(options$predictiveAccuracyType == "marginal"){
      temp_y <- temp_results[,"posterior"]
    }
    
    dfHist_all <- data.frame(
      "x" = 1:length(options$priors),
      "y" = temp_y,
      "g" = sapply(options$priors,function(x)x$name))
    
    p <- .plotAccuracyLS(dfHist_all, xName = xName, yName = yName)
    jaspResults[["plotsPredAccuracy"]]$plotObject <- p
    
    return()
    
  }
  
}
.plotsIterativeOverlyingBinomial2LS <- function(jaspResults, data, ready, options){
  
  plotsIterative <- createJaspPlot(title = "Sequential Analysis", width = 530, height = 400, aspectRatio = 0.7)
  
  plotsIterative$position <- 7
  plotsIterative$dependOn(c(.BinomialLS_data_dependencies, "plotsIterative", "plotsIterativeType",
                            "colorPalette"))
  
  
  if(length(data$y) == 0){
    jaspResults[["plotsIterative"]] <- plotsIterative
    return()
  }
  if(!all(ready)){
    jaspResults[["plotsIterative"]] <- plotsIterative
    return()
  }
  if(options$plotsIterativeType == "BF"){
    if(options$BF_comparison == ""){
      plotsIterative$setError("Please specify a hypothesis for comparison.")
      jaspResults[["plotsIterative"]] <- plotsIterative
      return()
    }
    if(length(options$priors) < 2){
      plotsIterative$setError("At least 2 hypotheses need to be specified.")
      jaspResults[["plotsIterative"]] <- plotsIterative
      return()
    }
  }
  
  results <- NULL
  if(length(data$y) == 1){
    iter_seq <- c(1, 1.1)
  }else{
    iter_seq <- 1:length(data$y)
  }
  
  for(i in iter_seq){
    
    temp_data    <- list(
      nSuccesses = sum(data$y[0:i] == 1),
      nFailures  = sum(data$y[0:i] == 0)
    )
    
    temp_results <- .testBinomialLS(temp_data, options$priors)
    
    if(options$plotsIterativeType == "conditional"){
      yName  <- "Conditional probability"
      temp_y <- exp(temp_results[,"log_lik"])
    }else if(options$plotsIterativeType == "joint"){
      yName  <- "Joint probability"
      temp_y <- exp(temp_results[,"log_lik"])*temp_results[,"prior"]       
    }else if(options$plotsIterativeType == "marginal"){
      yName  <- "Marginal probability"
      temp_y <- temp_results[,"posterior"]
    }else if(options$plotsIterativeType == "BF"){

      temp_y <- temp_results[,"log_lik"] - temp_results[temp_results$name == options$BF_comparison,"log_lik"]
      
      if(!options$BF_log){
        yName  <- paste("BF against", options$BF_comparison)
        temp_y <- exp(temp_y)
      }else{
        yName  <- paste("log(BF) against", options$BF_comparison)
      }
      
    }
    
    results <- rbind.data.frame(results, temp_y)
    
  }
  
  plot_data_lines <- list()
  for(h in 1:length(options$priors)){
    
    if(options$plotsIterativeType == "BF"){
      if(options$BF_comparison == options$priors[[h]]$name)next
    }
    
    temp_lines   <- NULL
    temp_lines   <- rbind(temp_lines, data.frame(
      x    = iter_seq,
      y    = results[,h],
      name = options$priors[[h]]$name
    ))
    plot_data_lines <- c(plot_data_lines, list(temp_lines))
    
  }
  
  xName  <- "Observations"
  
  if(options$plotsIterativeType == "BF"){
    BF_log <- options$BF_log
  }else{
    BF_log <- NULL
  }
  
  p <- .plotIterativeLS(plot_data_lines, NULL, xName = xName, yName = yName, x_start = 1, palette = options$colorPalette,
                        BF_log = BF_log)
  
  
  plotsIterative$plotObject <- p
  
  jaspResults[["plotsIterative"]] <- plotsIterative
  return()
}
.tableIterativeBinomial2LS <- function(jaspResults, data, ready, options){
  
  title <- paste0(
    "Sequential Analysis ",
    "[", ifelse(options$BF_log & options$plotsIterativeType == "BF", "log(", ""),
    tools::toTitleCase(options$plotsIterativeType),
    ifelse(options$BF_log & options$plotsIterativeType == "BF", ")", ""), "]",
    ": Updating Table")
  tableIterative <- createJaspTable(title = title)
  
  tableIterative$position <- 8
  tableIterative$dependOn(c(.BinomialLS_data_dependencies, "plotsIterative", "plotsIterativeType",
                            "plotsIterativeUpdatingTable"))
  
  
  tableIterative$addColumnInfo(name = "iteration", title = "Observations", type = "integer")
  if(ready[2]){
    for(i in 1:length(options$priors)){
      tableIterative$addColumnInfo(
        name  = options$priors[[i]]$name,  
        title = options$priors[[i]]$name,
        type = "number")
    }
  }
  
  
  if(!all(ready)){
    jaspResults[["tableIterative"]] <- tableIterative
    return()
  }
  if(options$plotsIterativeType == "BF"){
    if(options$BF_comparison == ""){
      tableIterative$setError("Please specify a hypothesis for comparison.")
      jaspResults[["tableIterative"]] <- tableIterative
      return()
    }
    if(length(options$priors) < 2){
      tableIterative$setError("At least 2 hypotheses need to be specified.")
      jaspResults[["tableIterative"]] <- tableIterative
      return()
    }
  }
  
  
  results <- NULL
  
  if(length(data$y) > 1){
    iter_seq <- 1:length(data$y)
  }else{
    iter_seq <- 1
  }
  
  for(i in iter_seq){
    
    temp_row     <- list() 
    temp_row[["iteration"]] <- i
    
    temp_data    <- list(
      nSuccesses = sum(data$y[0:i] == 1),
      nFailures  = sum(data$y[0:i] == 0)
    )
    temp_results <- .testBinomialLS(temp_data, options$priors)
    
    if(options$plotsIterativeType == "conditional"){
      temp_y <- exp(temp_results[,"log_lik"])
    }else if(options$plotsIterativeType == "joint"){
      temp_y <- exp(temp_results[,"log_lik"])*temp_results[,"prior"]       
    }else if(options$plotsIterativeType == "marginal"){
      temp_y <- temp_results[,"posterior"]
    }else if(options$plotsIterativeType == "BF"){
      
      temp_y <- temp_results[,"log_lik"] - temp_results[temp_results$name == options$BF_comparison,"log_lik"]
      
      if(!options$BF_log){
        yName  <- paste("BF against", options$BF_comparison)
        temp_y <- exp(temp_y)
      }else{
        yName  <- paste("log(BF) against", options$BF_comparison)
      }
      
    }
    
    for(h in 1:length(options$priors)){
      
      temp_row[[options$priors[[h]]$name]] <- temp_y[h]
      
    }
    
    tableIterative$addRows(temp_row)
    
  }
  
  jaspResults[["tableIterative"]] <- tableIterative
  return()
  
}

.testBinomialLS    <- function(data, priors){
  
  names     <- rep(NA, length(priors))
  prior     <- rep(NA, length(priors))
  log_lik   <- rep(NA, length(priors))
  
  obs_prop  <- data$nSuccesses / (data$nSuccesses + data$nFailures)
  
  for(i in 1:length(priors)){
    
    temp_prior <- priors[[i]]
    prior[i]   <- temp_prior$PH
    names[i]   <- temp_prior$name
    
    if(data$nSuccesses + data$nFailures > 0){
      
      if(temp_prior$type == "spike"){
        
        log_lik[i]   <- stats::dbinom(data$nSuccesses, data$nSuccesses + data$nFailures, temp_prior$parPoint, log = TRUE)
        
      }else if(temp_prior$type == "beta"){
        
        log_lik[i]   <- extraDistr::dbbinom(data$nSuccesses, data$nSuccesses + data$nFailures, 
                                            temp_prior$parAlpha, temp_prior$parBeta, log = TRUE)
        
      }
      
      
    }
    
  }
  
  if(data$nSuccesses + data$nFailures > 0){
    
    PH_log_lik <- log(prior) + log_lik
    norm_const <- log(sum(exp(PH_log_lik))) 
    posterior  <- exp(PH_log_lik - norm_const)
    
  }else{
    
    posterior  <- prior
    
  }
  
  return(data.frame(
    prior       = prior,
    log_lik     = log_lik,
    posterior   = posterior,
    name        = names
  ))
  
}


.marginalCentralLS <- function(density, spikes, coverage, l.bound = 0, u.bound = 1, density_discrete = FALSE){
  
  if(!is.null(density)){
    if(!density_discrete)density$y <- density$y/nrow(density)    
  }else{
    density <- data.frame("y" = NULL, "x" = NULL)
  }
  
  if(length(spikes) != 0){
    for(i in 1:length(spikes)){
      density <- rbind(density[density$x <= spikes[[i]]$x,], spikes[[i]], density[spikes[[i]]$x < density$x,])
    }
  }
  
  cs  <- cumsum(density$y)
  css <- rev(cumsum(rev(density$y)))
  
  lower <- density$x[cs  > (1-coverage)/2]
  lower <- lower[1]
  if(is.na(lower))lower <- l.bound
  
  upper <- density$x[(1-coverage)/2  <  css]
  upper <- upper[length(upper)]
  if(length(upper) == 0)upper <- u.bound
  
  return(cbind.data.frame(x_start = lower, x_end = upper, g = "central", coverage = coverage))
}
.marginalHPDLS     <- function(density, spikes, coverage, l.bound = 0, u.bound = 1, density_discrete = FALSE){
  
  HDI      <- NULL
  temp.cov <- 0
  
  # spikes have always the highest density - use them first
  if(length(spikes) != 0){
    spikes.df   <- do.call(rbind, spikes)
    spikes.df   <- spikes.df[order(spikes.df$y),]
    
    i        <- 1
    while(temp.cov < coverage & i <= nrow(spikes.df)){
      HDI      <- rbind(HDI, rep(spikes.df$x[i],2))
      temp.cov <- temp.cov + spikes.df$y[i]
      i        <- i + 1
    }
    
    # remove duplicious spikes
    HDI <- HDI[!duplicated(HDI[,1]),]
    HDI <- matrix(as.vector(HDI), ncol = 2)
  }
  
  # add continous density
  if(!is.null(density) & temp.cov < coverage){
    
    # if we have only spikes and density, the probability mass of density is 1 - spikes
    sum_dens_prob <- 1 - temp.cov
    # proportion of density needed to finish the coverage
    prop_density  <- (coverage-temp.cov)/sum_dens_prob
    
    # deal with flat density
    if(all(round(density$y,10) == round(density$y[1],10))){
      
      if(density_discrete){
        n.bars  <- u.bound-l.bound+1
        HDI2    <- c((u.bound-l.bound)/2 - prop_density*n.bars/2 + .5, (u.bound-l.bound)/2 + prop_density*n.bars/2 - .5)
        HDI2[1] <- floor(HDI2[1])
        HDI2[2] <- ceiling(HDI2[2])
      }else{
        HDI2 <- c((u.bound-l.bound)/2-(u.bound-l.bound)*prop_density/2, (u.bound-l.bound)/2+(u.bound-l.bound)*prop_density/2)
      }
      
    }else{
      
      den_marginal <- list(
        x = density$x,
        y = density$y
      )
      class(den_marginal) <- "density"
      HDI2 <- HDInterval::hdi(den_marginal, prop_density, allowSplit = T)
      
    }
    
    HDI2 <- matrix(as.vector(HDI2), ncol = 2)
    HDI2[HDI2 >= u.bound - .001] <- u.bound
    HDI2[HDI2 <= l.bound + .001] <- l.bound
    
    # remove spikes covered by density
    if(length(spikes) != 0){
      for(i in nrow(HDI):1){
        if(any(HDI[i,1] >= HDI2[,1] & HDI2[,2] >= HDI[i,1]))HDI <- HDI[-i,]
      }
    }
    
    HDI  <- rbind(HDI, HDI2)
    
  }
  
  HDI <- HDI[order(HDI[,1]),]
  HDI <- matrix(as.vector(HDI), ncol = 2)
  
  return(cbind.data.frame(x_start = HDI[,1], x_end = HDI[,2], g = "HPD", coverage = coverage))
}
.marginalCustomLS  <- function(density, spikes, lCI, uCI, density_discrete = FALSE){
  
  if(!is.null(density)){
    if(!density_discrete)density$y <- density$y/nrow(density)    
  }else{
    density <- data.frame("y" = NULL, "x" = NULL)
  }
  
  if(length(spikes) != 0){
    for(i in 1:length(spikes)){
      density <- rbind(density[density$x <= spikes[[i]]$x,], spikes[[i]], density[spikes[[i]]$x < density$x,])
    }
  }
  
  coverage <- sum(density$y[density$x >= lCI & density$x <= uCI])
  
  return(cbind.data.frame(x_start = lCI, x_end = uCI, g = "custom", coverage = coverage))
}
.marginalSupportLS <- function(data, priors, post_density, post_spikes, BF){
  
  # posterior spikes and density are already computed, we just need to get priors
  prior_spikes   <- list()
  density_i      <- 0
  prior_density  <- NULL
  temp_results   <- .testBinomialLS(data, priors)
  for(i in 1:length(priors)){
    if(priors[[i]]$type == "spike"){
      prior_spikes <- c(
        prior_spikes, 
        list(data.frame(y = priors[[i]]$PH, x = priors[[i]]$parPoint, g = "__marginal"))
      )
    }else if(priors[[i]]$type == "beta"){
      dfLinesPP   <- .dataLinesPPLS(data, priors[[i]])
      dfLinesPP   <- dfLinesPP[dfLinesPP$g == "Prior",]
      dfLinesPP$y <- exp(log(dfLinesPP$y)+log(temp_results[i, "prior"]))
      dfLinesPP$g <- priors[[i]]$name
      
      if(density_i == 0){
        prior_density   <- dfLinesPP
      }else{
        prior_density$y <- prior_density$y + dfLinesPP$y
      }
      density_i <- density_i + 1
    }
  }
  
  
  # compute BFs
  bf_spikes <- list()
  if(!is.null(prior_density)){
    bf_density <- data.frame(
      y = exp(log(post_density$y) - log(prior_density$y)),
      x = post_density$x
      )
    bf_density$y[post_density$y == 0] <- 0 # dealing with NaN's due to density aproximation
  }else{
    bf_density <- data.frame(y = NULL, x = NULL)
  }
  if(length(prior_spikes) != 0){
    for(i in 1:length(prior_spikes)){
      bf_spikes[[i]] <- data.frame(
        x = post_spikes[[i]]$x,
        y = post_spikes[[i]]$y / prior_spikes[[i]]$y 
      )
    }
  }
  
  
  if(length(bf_spikes) != 0){
    for(i in 1:length(bf_spikes)){
      bf_density <- rbind(bf_density[bf_density$x <= bf_spikes[[i]]$x,], bf_spikes[[i]], bf_density[bf_spikes[[i]]$x < bf_density$x,])
    }
  }
  
  
  support <- .aproximateSupportLS(bf_density$x, bf_density$y > BF)
  
  support$lCI[support$lCI == .0005] <- 0
  support$uCI[support$uCI == .9995] <- 1
  
  if(nrow(support) > 0){
    lCI      <- support$lCI
    uCI      <- support$uCI
    coverage <- 666 # not implemented
  }else{
    lCI      <- NA
    uCI      <- NA
    coverage <- 0
  }
  
  dat       <- data.frame(x_start = lCI, x_end = uCI, g = "support", coverage = coverage, BF = BF)
  
  return(dat)
}
.plotAccuracyLS    <- function(dfHist, xName = xName, yName = yName){
  
  mappingHistogram  <- ggplot2::aes(x = x, y = y, fill = col)
  
  yBreaks  <- JASPgraphs::getPrettyAxisBreaks(c(0, dfHist$y))
  xBreaks  <- 1:nrow(dfHist)
  xRange   <- c(.5, nrow(dfHist) + .5)
  
  obsYmax  <- max(dfHist$y)
  breaksYmax <- yBreaks[length(yBreaks)]
  newymax    <- max(1.10 * obsYmax, breaksYmax)
  
  dfHist$col <- "a"
  
  g <- ggplot2::ggplot()
  g <- g + ggplot2::geom_bar(
    data     = dfHist,
    mapping  = mappingHistogram,
    #fill     = "grey",
    col      = "black",
    stat     = "identity"
  )
  
  
  g <- g + ggplot2::scale_x_continuous(xName, breaks = xBreaks, labels = dfHist$g, limits = xRange)
  g <- g + ggplot2::scale_y_continuous(yName, breaks = yBreaks, limits = c(0, newymax)) 
  g <- g + ggplot2::scale_colour_manual(values = "grey90", aesthetics = "fill")
  
  
  g <- g + JASPgraphs::themeJaspRaw() + 
    JASPgraphs::geom_rangeframe(sides = 'lb') +  
    ggplot2::theme(
      legend.title = ggplot2::element_blank(), 
      legend.text  = ggplot2::element_text(margin = ggplot2::margin(0, 0, 2, 0)),
      legend.key.height = ggplot2::unit(1, "cm"),
      legend.key.width  = ggplot2::unit(1.5,"cm"),
      axis.text.x       = ggplot2::element_text(angle = 45))
  
  plot <- g
  class(plot) <- c("JASPgraphs", class(plot))
  return(plot)
  
}
