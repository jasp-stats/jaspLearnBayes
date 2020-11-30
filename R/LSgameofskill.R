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


LSgameofskill   <- function(jaspResults, dataset, options, state = NULL){
  ## some transformation to match previous code
  input <- list(
    "n"     = length(options[["players"]]),
    "alpha" = sapply(options[["players"]], function(p)p$values[[1]]),
    "k"     = sapply(options[["players"]], function(p)p$values[[2]]),
    "t"     = options[["winPoints"]], 
    "s"     = options[["nSims"]],
    "check" = options[["CI"]])
  alpha <- sapply(options[["players"]], function(p)p$values[[1]])
  k     <- sapply(options[["players"]], function(p)p$values[[2]])
  

  ## check errors
  if(input$n < 2)
    .quitAnalysis(gettextf(
      "Warning: The number of players must be at least 2. Adjust the inputs!"
    ))
  
  if(input$n != length(k))
    .quitAnalysis(gettextf(
      "The number of players (%1$i) does not equal the numbers of points for each player when interrupted (%2$i). Please check the appropriate settings.",
      input$n,
      length(k)
    ))
  
  if(max(k) >= input$t)
    .quitAnalysis(gettextf(
      "Warning: Player %1$i has already won the game. Adjust the inputs!",
      which(k == max(k))
    ))
  
  if(sum(c(k,alpha) > 0) != length(c(k,alpha)))
    .quitAnalysis(gettextf(
      "Warning: No negative input values! Adjust the inputs!"
    ))
  
  
  ## Summary Table
  summaryTable <- createJaspTable(title = gettext("Summary Table"))
  
  summaryTable$dependOn(c("players", "nSims", "winPoints", "CI"))
  summaryTable$addCitation("JASP Team (2018). JASP (Version 0.9.2) [Computer software].")
  
  summaryTable$addColumnInfo(name = "players",   title = gettext("Players"),   type = "string")
  summaryTable$addColumnInfo(name = "prior",   title = gettext("Prior Skill"),   type = "string")
  summaryTable$addColumnInfo(name = "points",   title = gettext("Points Gained"),   type = "string")
  summaryTable$addColumnInfo(name = "pA",   title = gettext("Analytical"),   type = "number", 
                             overtitle = gettext("p(win the game)"))
  summaryTable$addColumnInfo(name = "pS",   title = gettext("Simulated"),   type = "number", 
                             overtitle = gettext("p(win the game)"))
  

  ## Credible Interval Plot
  CIPlot <- createJaspPlot(title = "Probability of Player 1 Winning",  width = 480, height = 320)
  CIPlot$dependOn(c("players", "nSims", "winPoints", "CI"))
  CIPlot$addCitation("JASP Team (2018). JASP (Version 0.9.2) [Computer software].")
  
  # column specification
  CIPlot0 <- ggplot2::ggplot(data= NULL) + 
    #ggtitle("Probability of Player 1 Winning") +
    ggplot2::xlab("Number of Simulated Games") + 
    ggplot2::ylab("Pr(Winning the Game)") +
    ggplot2::coord_cartesian(xlim = c(0, input$s), ylim = c(0, 1)) 
  
  ## fill in the table and the plot 
  if (input$n == 2 & max(k) < input$t){
    
    # output of compare_function3, when there are two players
    result <- compare_function3(k[1],k[2],input$t,alpha[1],alpha[2],input$s)
    
    
    
    # fill in the table
    summaryTable$addRows(list(players = 1, prior = alpha[1], points = k[1], # 
                              pA = result[[2]], pS = result[[1]]))
    summaryTable$addRows(list(players = 2, prior = alpha[2], points = k[2],# 
                              pA = 1-result[[2]], pS = 1-result[[1]]))
    
    # fill in the plot
    if (input$check){ # whether plot CI or not
      
      # Credibility interval (highest posterior density interval)
      SimulResult <- result[[4]] # store the simulated result
      SimulMatrix <- matrix(0, nrow = 1000, ncol = input$s) # the matrix of samples from posterior distribution based on simulated result
      
      for (i in 1:input$s){  
        SimulMatrix[, i] <- rbeta(1000, SimulResult[i]*i+alpha[1], i-SimulResult[i]*i+alpha[2])
      }
      CredInt <- apply(SimulMatrix, 2, HDInterval::hdi) # record the credibility interval
      y.upper <- CredInt[1,]
      y.lower <- CredInt[2,]
      CIPlot0 <- CIPlot0 + 
        ggplot2::geom_polygon(ggplot2::aes(x = c(1:input$s,input$s:1), y = c(y.upper, rev(y.lower))), 
                     fill = "lightsteelblue")  # CI
    }
    
    CIPlot$plotObject <- jaspGraphs::themeJasp(CIPlot0) + 
      ggplot2::geom_line(color = "darkred", ggplot2::aes(x = c(1:input$s), y = rep(result[[2]], input$s))) +  # analytical prob
      ggplot2::geom_line(data= NULL, ggplot2::aes(x = c(1:input$s), y = result[[4]])) # simulated prob
    
    
  }else if (input$n >= 3 & max(k) < input$t){
    # output of compare_function4, when there are three or more players
    result <- compare_function4(k, input$t, alpha, input$s)
    
    # a vector of analytical p for all players, calculated by switching with player 1
    Analytical_Prob <- vector()
    for (i in 1:length(k)){
      k_copy <- replace(k, c(1, i), k[c(i, 1)])
      alpha_copy <- replace(alpha, c(1, i), alpha[c(i, 1)])
      Analytical_Prob[i] <- compare_function4(k_copy, input$t, alpha_copy, input$s)[[2]] 
    }
    
    # fill in the table
    for (i in 1:input$n){
      summaryTable$addRows(list(players = i, prior = alpha[i], points = k[i],
                                pA = Analytical_Prob[i], pS = result[[1]][i]))
    }

    # fill in the plot
    if (input$check){ # whether plot CI or not
      
      # Credibility interval (highest posterior density interval)
      SimulResult <- result[[4]] # store the simulated result
      SimulMatrix <- matrix(0, nrow = 1000, ncol = input$s) # the matrix of samples from posterior distribution based on simulated result
      
      for (i in 1:input$s){  
        SimulMatrix[, i] <- MCMCpack::rdirichlet(1000, result[[4]][,i]*i+alpha)[,1]
      }
      CredInt <- apply(SimulMatrix, 2, HDInterval::hdi) # record the credibility interval
      y.upper <- CredInt[1,]
      y.lower <- CredInt[2,]
      CIPlot0 <- CIPlot0 + 
        ggplot2::geom_polygon(ggplot2::aes(x = c(1:input$s,input$s:1), y = c(y.upper, rev(y.lower))), 
                     fill = "lightsteelblue")  # CI
    }
    
    CIPlot$plotObject <- jaspGraphs::themeJasp(CIPlot0) + 
      ggplot2::geom_line(color = "darkred", ggplot2::aes(x = c(1:input$s), y = rep(result[[2]], input$s))) +   # analytical prob
      ggplot2::geom_line(data= NULL, ggplot2::aes(x = c(1:input$s), y = result[[4]][1,])) # simulated prob
    
  }
  jaspResults[["summaryTable"]] <- summaryTable
  jaspResults[["CIPlot"]] <- CIPlot
  
  return()
}
