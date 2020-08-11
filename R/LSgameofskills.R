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


#library(MGLM)
#library(HDInterval)
#library(DT)
#library(MCMCpack)
#source("combinations.R")


sum_p_n <- function(y, a){  
  dDirichletnmultinom <- function(y, a){
    #y <- c(4,3,2)
    #a <- c(2,3,4)
    y_sum <- sum(y)
    a_sum <- sum(a)
    p1 <- gamma(y_sum) / (gamma(y[1]) * prod(gamma(y+1))/gamma(y[1]+1))
    p2 <- gamma(a_sum) / (prod(gamma(a)))
    p3 <- (prod(gamma(y + a))) / gamma(y_sum + a_sum)
    return (p1 * p2 * p3)
  }
  
  #calculate the prob
  p_calculated <- 0
  for (l in 1:(prod(y)/y[1])){         #generate all the situations that player 1 wins
    p_calculated <- p_calculated + dDirichletnmultinom(combinations(y)[l,], a) 
  }
  return(p_calculated)
}

combinations <- function(k){
  if (length(k) == 3){ # the number of players
    #k <- c(1,2,3)
    num <- prod(k)/k[1] #total number of sets
    #sets <- vector(mode = "list", length = num)
    sets <- matrix(nrow = num, ncol = length(k))
    for (i in 0:(k[2]-1)){
      for (j in 0:(k[3]-1)){
        sets[i*k[3]+j+1,] <- c(k[1],i,j)
      }
    }
    return(sets)
  }else{
    #k <- c(1,2,3,4)
    #x1 <- sets
    x1 <- combinations(k[1:(length(k) - 1)]) # k without k[length(k)]
    x2 <- k[length(k)]  
    y <- matrix(ncol = length(k), nrow = prod(k)/k[1])
    for (i in 1:length(x1[,1])){
      for (j in 1:k[length(k)]){
        y[(i-1)*(k[length(k)])+j,] <- append(x1[i,],j-1)
      }
    }
    return(y)
  }
} 


compare_function3 <- function(m, n, t, alpha = 1, beta = 1, simulation){
  
  # first estimating the probability that player 1 wins
  record_game <- vector()
  for (i in 1:simulation){
    # copy m and n
    m_copy <- m
    n_copy <- n
    # keep running the game until m_copy or n_copy reach 0
    while (m_copy != t & n_copy != t){
      if(rbinom(1,1,(m_copy + alpha)/(m_copy + n_copy + alpha + beta)) == 1){ # alpha = 1, beta = 1 for the noninformative prior
        m_copy <- m_copy+1
      }else{
        n_copy <- n_copy+1
      }
      
    }
    # if m_copy = t, record 1,player 1 wins the single trial; if n_copy = t, record 0, player 2 win the trial 
    record_game[i] <- (t-n_copy)^(t-m_copy)
  }
  p_simulated <- sum(record_game)/simulation    # estimated probability that player 1 wins
  
  # to plot the simulated probability over the number of times the simulation is performed
  p_cumulative <- record_game
  for (i in 2:simulation){
    p_cumulative[i] <- (p_cumulative[i] + p_cumulative[i-1]*(i-1))/i
  }
  
  # calculate the probability that player 1 wins using beta negative binomial distribution
  # "x" is the number of failures, "size" is the number of successes
  p_calculated <- pbnbinom(q = t-n-1, size = t-m, alpha = m+alpha, beta = n+beta)
  
  # calculating the difference between the simulated and the calculated value
  dif = abs(p_simulated - p_calculated)
  return (list(p_simulated,p_calculated,dif,p_cumulative))
}

compare_function4 <- function(k,t,alpha,simulation){
  # first estimating the probability that player 1 wins
  record_game <- vector()
  for (i in 1:simulation){
    # record the result of every trial 
    record_trial <- matrix(k,nrow = length(k),ncol = 1)
    # keep running the game until one of the player wins t trials
    while ( prod(record_trial - t) != 0){
      # alpha as the prior in the Dirichlet distribution 
      p <- (record_trial + alpha)/(sum(record_trial)+sum(alpha))
      as.vector(p)
      record_trial <- record_trial + rmultinom(1,1,p) 
    }
    # transfer the record of trial to the record of game
    record_game[i] <- which(record_trial == t, arr.ind = TRUE)[1,"row"]
  }
  # estimated probability that player 1 wins
  # p_simulated <- length(which(record_game == 1))/simulation   
  
  # estimated probability that each player wins
  p_simulated <- vector()
  for (i in 1:length(p)){
    p_simulated[i] <- length(which(record_game == i))/simulation 
  }
  
  # plot the simulated probability over the number of times the simulation is performed
  p_cumulative <- record_game
  p_cumulative[which(p_cumulative != 1)] <- 0 # if other than player 1 wins, change the record to 0
  
  for (i in 2:simulation){
    p_cumulative[i] <- (p_cumulative[i] + p_cumulative[i-1]*(i-1))/i
  }
  
  ## sum the results to plot the simulated probability over simulations. Make it a matrix for CI
  p_cumulative <- matrix(0, nrow = length(k), ncol = simulation)
  p_cumulative[record_game[1],1] <- 1
  for (i in 1:length(k)){    
    for (j in 2:simulation){
      if (record_game[j] == i){
        p_cumulative[i, j] <- (p_cumulative[i, j-1]*(j-1)+1)/j
      }else{
        p_cumulative[i, j] <- p_cumulative[i, j-1]*(j-1)/j
      }
    }
  }
  
  # calculate the probability that player 1 wins using Dirichlet negative multinomial distribution
  p_calculated <- sum_p_n(t-k,k+alpha)
  
  # calculating the difference between the simulated and the calculated value
  dif = abs(p_simulated - p_calculated)
  return (list(p_simulated,p_calculated,dif,p_cumulative))
}





LSgameofskills   <- function(jaspResults, dataset, options, state = NULL){
  ## some transformation to match previous code
  input <- list("n" = options$n, "alpha" = options$alpha, "k" = options$k, "t" = options$t, 
                "s" = options$s, "check" = options$check)
  # input <- list("p" = "1,1", "k" = "1,1", "t" = 2, "s" = 100, "n"= 2)
  alpha <- as.numeric(unlist(strsplit(input$alpha,",")))
  k <- as.numeric(unlist(strsplit(input$k,",")))
  
  ## Summary Table
  summaryTable <- createJaspTable(title = "Summary Table")
  
  summaryTable$dependOn(c("n", "alpha", "k", "t", "s", "check"))
  summaryTable$addCitation("JASP Team (2018). JASP (Version 0.9.2) [Computer software].")
  
  summaryTable$addColumnInfo(name = "players",   title = "Players",   type = "string")
  summaryTable$addColumnInfo(name = "prior",   title = "Prior Belief",   type = "string")
  summaryTable$addColumnInfo(name = "points",   title = "Points Obtained",   type = "string")
  summaryTable$addColumnInfo(name = "pA",   title = "Analytical",   type = "string", 
                             overtitle = "Pr(win the game)")
  summaryTable$addColumnInfo(name = "pS",   title = "Simulated",   type = "string", 
                             overtitle = "Pr(win the game)")
  
  ## Credible Interval Plot
  CIPlot <- createJaspPlot(title = "Probability of Player 1 Winning",  width = 480, height = 320)
  CIPlot$dependOn(c("n", "k", "t", "p", "s", "check"))
  CIPlot$addCitation("JASP Team (2018). JASP (Version 0.9.2) [Computer software].")
  
  # column specification
  CIPlot0 <- ggplot2::ggplot(data= NULL) + 
    ggtitle("Probability of Player 1 Winning") +
    xlab("Number of Simulated Games") + 
    ylab("Pr(Winning the Game)") +
    coord_cartesian(xlim = c(0, input$s), ylim = c(0, 1)) 
  
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
      CredInt <- apply(SimulMatrix, 2, hdi) # record the credibility interval
      y.upper <- CredInt[1,]
      y.lower <- CredInt[2,]
      CIPlot0 <- CIPlot0 + 
        geom_polygon(aes(x = c(1:input$s,input$s:1), y = c(y.upper, rev(y.lower))), 
                     fill = "lightsteelblue")  # CI
    }
    
    CIPlot$plotObject <- CIPlot0 + 
      geom_line(color = "darkred", aes(x = c(1:input$s), y = rep(result[[2]], input$s))) +  # analytical prob
      geom_line(data= NULL, aes(x = c(1:input$s), y = result[[4]])) # simulated prob
    
    
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
        SimulMatrix[, i] <- rdirichlet(1000, result[[4]][,i]*i+alpha)[,1]
      }
      CredInt <- apply(SimulMatrix, 2, hdi) # record the credibility interval
      y.upper <- CredInt[1,]
      y.lower <- CredInt[2,]
      CIPlot0 <- CIPlot0 + 
        geom_polygon(aes(x = c(1:input$s,input$s:1), y = c(y.upper, rev(y.lower))), 
                     fill = "lightsteelblue")  # CI
    }
    
    CIPlot$plotObject <- CIPlot0 + 
      geom_line(color = "darkred", aes(x = c(1:input$s), y = rep(result[[2]], input$s))) +   # analytical prob
      geom_line(data= NULL, aes(x = c(1:input$s), y = result[[4]][1,])) # simulated prob
    
  }
  jaspResults[["summaryTable"]] <- summaryTable
  jaspResults[["CIPlot"]] <- CIPlot
  
  return()
}
