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


#source("combinations.R")



compare_function1 <- function(p,m1,n1,t,simulation){
  
  # first estimating the probability that player 1 wins by simulating the game for a given number of times.
  m <- t - m1 # m: the number of success that player 1 should have to win the game
  n <- t - n1 # n: the number of success that player 2 should have to win the game
  
  record_game <- vector()
  for (i in 1:simulation){
    # copy m and n
    record_trial <- rbind(m,n)
    # keep running the game until one of them wins
    while (record_trial[1,]*record_trial[2,]!= 0){
      record_trial <- record_trial - rmultinom(1,1,c(p,1-p)) #updating the recorded result 
    }
    
    # if record_trial[1,] = 0, record 1,player 1 wins this single trial; 
    # if record_trial[2,] = 0, record 0, player 2 wins 
    record_game[i] <- record_trial[2,]^record_trial[1,]
    
  }
  p_simulated <- sum(record_game)/simulation    # estimated probability that player 1 wins
  
  # plot the simulated probability over the number of times the simulation is performed
  p_cumulative <- record_game
  for (i in 2:simulation){
    p_cumulative[i] <- (p_cumulative[i] + p_cumulative[i-1]*(i-1))/i
  }
  #plot(p_cumulative,
  #main="Simulated probability of player 1 winning over simulations", 
  #xlab = "Number of simulations",ylab="Probability of winning",type="l",
  #xlim = c(0, simulation), ylim = c(0, 1),)
  
  # calculating the probabiltiy that player 1 wins from negative binomial distribution
  p_calculated <- pnbinom(n-1,m,p)  
  
  # difference between simulated and calculated probability
  dif = abs(p_simulated - p_calculated) 
  return (list(p_simulated,p_calculated,dif,p_cumulative))
}

compare_function2 <- function(k1, t, p, simulation){
  # first estimating the probability that player 1 wins
  k <- t-k1 # k: the vector regarding the number of more successes that player 1, 2, ..., n should have to win the game
  
  # normalize the probability if they do not sum to one
  p <- p/sum(p)
  
  record_game <- vector()# initializing the vector to record the results of all the games
  for (i in 1:simulation){
    # copy the trials players need to succeed and update after each trial
    record_trial <- k # each row stands for simulations
    # keep running the game until one record of the trial reaches 0
    while (prod(record_trial) != 0){
      record_trial <- record_trial - t(rmultinom(1,1,p)) #updating the recorded result
    }
    # transfer the record of trial to the record of game
    record_game[i] <- which(record_trial == 0, arr.ind = TRUE)[1,"col"]
  }
  
  # estimated probability that each player wins
  p_simulated <- vector()
  for (i in 1:length(p)){
    p_simulated[i] <- length(which(record_game == i))/simulation 
  }
  
  p_cumulative <- matrix(0, nrow = length(p), ncol = simulation)
  p_cumulative[record_game[1],1] <- 1
  for (i in 1:length(p)){    
    for (j in 2:simulation){
      
      if (record_game[j] == i){
        p_cumulative[i, j] <- (p_cumulative[i, j-1]*(j-1)+1)/j
      }else{
        p_cumulative[i, j] <- p_cumulative[i, j-1]*(j-1)/j
      }
    }
  }
  
  ## Calculating the probability using negative multinomial distribution and resursive function
  p_calculated <- 0
  for (l in 1:(prod(k)/k[1])){
    p_calculated <- p_calculated + exp(MGLM::dnegmn(Y = combinations(k)[l,2:length(k)],
                                              beta = combinations(k)[1], prob = p[2:length(p)]))
  }
  
  # calculating difference between the simulated and the calculated probability of player 1 winning
  dif <- abs(p_simulated - p_calculated)
  return (list(p_simulated, p_calculated, dif, p_cumulative))
}


#l <- compare_function2(c(1,1,1), 2, c(0.2,0.4,0.4), 10)
#l

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

##
LSgameofchance   <- function(jaspResults, dataset, options, state = NULL){

  #ready <- (length(options$variables) > 0)
  
  
  ## some transformation to match previous code
  input <- list("n" = options$n, "k" = options$k, "t" = options$t, "p" = options$p,
                "s" = options$s, "check" = options$check)
  # input <- list("p" = "1,1", "k" = "1,1", "t" = 2, "s" = 100, "n"= 2)
  k <- as.numeric(unlist(strsplit(input$k,",")))
  p <- as.numeric(unlist(strsplit(input$p,",")))/sum(as.numeric(unlist(strsplit(input$p,","))))
  
  ## check errors
  #need(max(k()) < input$t, paste("Warning: Player", which(k() == max(k())),
  #                               " has already won the game. Adjust the inputs!")),
  #.hasErrors(dataset, type = "limits",
  #           message = paste("Warning: Player", which(k == max(k)),
  #                           " has already won the game. Adjust the inputs!"),  
  #           limits.target = input$t,
  #           limits.max = max(k) - 1,
  #           exitAnalysisIfErrors = TRUE)
  
  
  

  
  ## Summary Table
  summaryTable <- createJaspTable(title = "Summary Table")
  
  summaryTable$dependOn(c("n", "k", "t", "p", "s", "check"))
  summaryTable$addCitation("JASP Team (2018). JASP (Version 0.9.2) [Computer software].")
  
  summaryTable$addColumnInfo(name = "players",   title = "Players",   type = "string")
  summaryTable$addColumnInfo(name = "pPoint",   title = "Pr(win a point)",   type = "string")
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
    ggplot2::xlab("Number of Simulated Games") + 
    ggplot2::ylab("Pr(Winning the Game)") +
    ggplot2::coord_cartesian(xlim = c(0, input$s), ylim = c(0, 1)) 
  
  ## fill in the table and the plot 
  if (input$n == 2 & max(k) < input$t){
  
    # output of compare_function1, when there are two players
    result <- compare_function1(p[1],k[1],k[2],input$t,input$s)
    
    # fill in the table
    summaryTable$addRows(list(players = 1, pPoint = p[1], pA = result[[2]], pS = result[[1]]))
    summaryTable$addRows(list(players = 2, pPoint = 1-p[1], pA = 1-result[[2]], pS = 1-result[[1]]))
    
    # fill in the plot
    if (input$check){ # whether plot CI or not
      
      # Credibility interval (highest posterior density interval)
      SimulResult <- result[[4]] # store the simulated result
      SimulMatrix <- matrix(0, nrow = 1000, ncol = input$s) # the matrix of samples from posterior distribution based on simulated result
      
      for (i in 1:input$s){  
        SimulMatrix[, i] <- rbeta(1000, SimulResult[i]*i+1, i-SimulResult[i]*i+1)
      }
      CredInt <- apply(SimulMatrix, 2, hdi) # record the credibility interval
      y.upper <- CredInt[1,]
      y.lower <- CredInt[2,]
      CIPlot0 <- CIPlot0 + 
        ggplot2::geom_polygon(ggplot2::aes(x = c(1:input$s,input$s:1), y = c(y.upper, rev(y.lower))), 
                     fill = "lightsteelblue")  # CI
    }
    
    CIPlot0 <- CIPlot0 + 
      ggplot2::geom_line(color = "darkred", aes(x = c(1:input$s), y = rep(result[[2]], input$s))) +  # analytical prob
      ggplot2::geom_line(data= NULL, aes(x = c(1:input$s), y = result[[4]])) # simulated prob
    
    CIPlot$plotObject <- JASPgraphs::themeJasp(CIPlot0)
    
    
  }else if (input$n >= 3 & max(k) < input$t){
    # output of compare_function2, when there are three or more players
    result <- compare_function2(k, input$t, p, input$s)
    
    # a vector of analytical p for all players, calculated by switching with player 1
    Analytical_Prob <- vector()
    for (i in 1:length(p)){
      k_copy <- replace(k, c(1, i), k[c(i, 1)])
      p_copy <- replace(p, c(1, i), p[c(i, 1)])
      Analytical_Prob[i] <- compare_function2(k_copy, input$t, p_copy, input$s)[[2]] 
    }
    
    # fill in the table
    for (i in 1:input$n){
      summaryTable$addRows(list(players = i, pPoint = p[i], 
                                pA = Analytical_Prob[i], pS = result[[1]][i]))
    }
    
    # fill in the plot
    if (input$check){ # whether plot CI or not
      
      # Credibility interval (highest posterior density interval)
      SimulResult <- result[[4]] # store the simulated result
      SimulMatrix <- matrix(0, nrow = 1000, ncol = input$s) # the matrix of samples from posterior distribution based on simulated result
      
      for (i in 1:input$s){  
        SimulMatrix[, i] <- rdirichlet(1000, result[[4]][,i]*i+1)[,1]
      }
      CredInt <- apply(SimulMatrix, 2, hdi) # record the credibility interval
      y.upper <- CredInt[1,]
      y.lower <- CredInt[2,]
      CIPlot0 <- CIPlot0 + 
        ggplot2::geom_polygon(ggplot2::aes(x = c(1:input$s,input$s:1), y = c(y.upper, rev(y.lower))), 
                     fill = "lightsteelblue")  # CI
    }
    
    CIPlot0 <- CIPlot0 + 
      ggplot2::geom_line(color = "darkred", ggplot2::aes(x = c(1:input$s), y = rep(result[[2]], input$s))) +   # analytical prob
      ggplot2::geom_line(data= NULL, ggplot2::aes(x = c(1:input$s), y = result[[4]][1,])) # simulated prob
    
    CIPlot$plotObject <- JASPgraphs::themeJasp(CIPlot0)
    
  }
  
  jaspResults[["summaryTable"]] <- summaryTable
  jaspResults[["CIPlot"]] <- CIPlot

  

  return()
}
