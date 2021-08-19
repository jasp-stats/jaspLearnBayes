## This is a function calculating the probability mass function
# of Dirichlet negative multinomial distribution

## Input:
# y: a vector containing the number of trials each player needs to win
# a: a vector containing all the parameters for the Dirichlet multinomial distribution, indicating the competence of each player

dDirichletNegMultinom <- function(y, a, log = FALSE) {

  ySum <- sum(y)
  aSum <- sum(a)

  lp1 <- lgamma(ySum) - (lgamma(y[1]) + sum(lgamma(y+1)) - lgamma(y[1]+1))
  lp2 <- lgamma(aSum) - sum(lgamma(a))
  lp3 <- sum(lgamma(y + a)) - lgamma(ySum + aSum)

  logDensity <- lp1 + lp2 + lp3
  if (log)
    return(logDensity)
  else
    return(exp(logDensity))

}

pDirichletNegMultinom <- function(y, a){


  #calculate the prob
  pCalculated <- 0
  for (l in 1:(prod(y)/y[1])){         #generate all the situations that player 1 wins
    pCalculated <- pCalculated + dDirichletNegMultinom(combinations(y)[l, ], a)
  }
  return(pCalculated)
}



# This is a recursive function returning all situations (combinations of points obtained
# by each player) in which player 1 wins the game.
# Input k: a vector of containing the number of trials that each player needs to win the game.

combinations <- function(k){
  n <- length(k)

  if (n == 3){ # the number of players
    num <- prod(k)/k[1] #total number of sets
    sets <- matrix(nrow = num, ncol = n)
    for (i in 0:(k[2]-1)){
      for (j in 0:(k[3]-1)){
        sets[i*k[3]+j+1, ] <- c(k[1],i,j)
      }
    }
    return(sets)
  }else{
    x1 <- combinations(k[1:(n-1)]) # recursion
    x2 <- k[n]
    sets <- matrix(ncol = n, nrow = prod(k)/k[1])
    for (i in 1:length(x1[ ,1])){
      for (j in 1:k[n]){
        sets[(i-1)*(k[n])+j, ] <- c(x1[i, ],j-1)
      }
    }

    return(sets)
  }
}

compareChanceTwoPlayers <- function(p,m1,n1,t,simulation){

  # first estimating the probability that player 1 wins by simulating the game for a given number of times.
  m <- t - m1 # m: the number of success that player 1 should have to win the game
  n <- t - n1 # n: the number of success that player 2 should have to win the game

  recordGame <- numeric(simulation)
  for (i in 1:simulation){
    # copy m and n
    recordTrial <- rbind(m, n)
    # keep running the game until one of them wins
    while (recordTrial[1, ] * recordTrial[2, ] != 0){
      recordTrial <- recordTrial - rmultinom(1, 1, c(p, 1 - p)) #updating the recorded result
    }

    # if recordTrial[1, ] = 0, record 1,player 1 wins this single trial;
    # if recordTrial[2, ] = 0, record 0, player 2 wins
    # recordGame[i] <- recordTrial[2, ]^recordTrial[1, ]
    recordGame[i] <- if (recordTrial[2, ] == 0) 0 else 1

  }
  pSimulated <- mean(recordGame)    # estimated probability that player 1 wins

  # plot the simulated probability over the number of times the simulation is performed
  pCumulative <- recordGame
  for (i in 2:simulation){
    pCumulative[i] <- (pCumulative[i] + pCumulative[i-1]*(i-1))/i
  }

  # calculating the probabiltiy that player 1 wins from negative binomial distribution
  pCalculated <- pnbinom(n-1,m,p)

  # difference between simulated and calculated probability
  dif = abs(pSimulated - pCalculated)
  return (list(pSimulated,pCalculated,dif,pCumulative))
}

compareChanceNPlayers <- function(k1, t, p, simulation){
  # first estimating the probability that player 1 wins
  k <- t-k1 # k: the vector regarding the number of more successes that player 1, 2, ..., n should have to win the game

  # normalize the probability if they do not sum to one
  p <- p/sum(p)

  recordGame <- numeric(simulation)# initializing the vector to record the results of all the games
  for (i in 1:simulation){
    # copy the trials players need to succeed and update after each trial
    recordTrial <- k # each row stands for simulations
    # keep running the game until one record of the trial reaches 0
    while (prod(recordTrial) != 0){
      recordTrial <- recordTrial - t(rmultinom(1,1,p)) #updating the recorded result
    }
    # transfer the record of trial to the record of game
    recordGame[i] <- which(recordTrial == 0, arr.ind = TRUE)[1,"col"]
  }

  # estimated probability that each player wins
  pSimulated <- vector()
  for (i in 1:length(p)){
    pSimulated[i] <- length(which(recordGame == i))/simulation
  }

  pCumulative <- matrix(0, nrow = length(p), ncol = simulation)
  pCumulative[recordGame[1],1] <- 1
  for (i in 1:length(p)){
    for (j in 2:simulation){
      if (recordGame[j] == i){
        pCumulative[i, j] <- (pCumulative[i, j-1]*(j-1)+1)/j
      }else{
        pCumulative[i, j] <- pCumulative[i, j-1]*(j-1)/j
      }
    }
  }

  ## Calculating the probability using negative multinomial distribution and resursive function
  pCalculated <- 0
  for (l in 1:(prod(k)/k[1])){
    pCalculated <- pCalculated + exp(MGLM::dnegmn(Y = combinations(k)[l,2:length(k)],
                                              beta = combinations(k)[1], prob = p[2:length(p)]))
  }

  # calculating difference between the simulated and the calculated probability of player 1 winning
  dif <- abs(pSimulated - pCalculated)
  return (list(pSimulated, pCalculated, dif, pCumulative))
}

compareSkillTwoPlayers <- function(m, n, t, alpha = 1, beta = 1, simulation){

  # first estimating the probability that player 1 wins
  recordGame <- numeric(simulation)
  for (i in 1:simulation){
    # copy m and n
    mCopy <- m
    nCopy <- n
    # keep running the game until mCopy or nCopy reach 0
    while (mCopy != t & nCopy != t){
      if(rbinom(1,1,(mCopy + alpha)/(mCopy + nCopy + alpha + beta)) == 1){ # alpha = 1, beta = 1 for the noninformative prior
        mCopy <- mCopy+1
      }else{
        nCopy <- nCopy+1
      }

    }

    # if mCopy == t, record 1,player 1 wins the single trial;
    # if nCopy == t, record 0, player 2 win the trial
    #recordGame[i] <- (t-nCopy)^(t-mCopy)
    recordGame[i] <- if (nCopy == t) 0 else 1.
  }
  pSimulated <- sum(recordGame)/simulation    # estimated probability that player 1 wins

  # to plot the simulated probability over the number of times the simulation is performed
  pCumulative <- recordGame
  for (i in 2:simulation){
    pCumulative[i] <- (pCumulative[i] + pCumulative[i-1]*(i-1))/i
  }

  # calculate the probability that player 1 wins using beta negative binomial distribution
  # "x" is the number of failures, "size" is the number of successes
  pCalculated <- extraDistr::pbnbinom(q = t-n-1, size = t-m, alpha = m+alpha, beta = n+beta)

  # calculating the difference between the simulated and the calculated value
  dif = abs(pSimulated - pCalculated)
  return (list(pSimulated,pCalculated,dif,pCumulative))
}

compareSkillNPlayers <- function(k, t, alpha, simulation){
  # first estimating the probability that player 1 wins
  recordGame <- numeric(simulation)
  for (i in 1:simulation){
    # record the result of every trial
    recordTrial <- matrix(k,nrow = length(k),ncol = 1)
    # keep running the game until one of the player wins t trials
    while ( prod(recordTrial - t) != 0){
      # alpha as the prior in the Dirichlet distribution
      p <- (recordTrial + alpha)/(sum(recordTrial)+sum(alpha))
      as.vector(p)
      recordTrial <- recordTrial + rmultinom(1,1,p)
    }
    # transfer the record of trial to the record of game
    recordGame[i] <- which(recordTrial == t, arr.ind = TRUE)[1,"row"]
  }
  # estimated probability that player 1 wins
  # pSimulated <- length(which(recordGame == 1))/simulation

  # estimated probability that each player wins
  pSimulated <- vector()
  for (i in 1:length(p)){
    pSimulated[i] <- length(which(recordGame == i))/simulation
  }

  # plot the simulated probability over the number of times the simulation is performed
  pCumulative <- recordGame
  pCumulative[which(pCumulative != 1)] <- 0 # if other than player 1 wins, change the record to 0

  for (i in 2:simulation){
    pCumulative[i] <- (pCumulative[i] + pCumulative[i-1]*(i-1))/i
  }

  ## sum the results to plot the simulated probability over simulations. Make it a matrix for CI
  pCumulative <- matrix(0, nrow = length(k), ncol = simulation)
  pCumulative[recordGame[1],1] <- 1
  for (i in 1:length(k)){
    for (j in 2:simulation){
      if (recordGame[j] == i){
        pCumulative[i, j] <- (pCumulative[i, j-1]*(j-1)+1)/j
      }else{
        pCumulative[i, j] <- pCumulative[i, j-1]*(j-1)/j
      }
    }
  }

  # calculate the probability that player 1 wins using Dirichlet negative multinomial distribution
  pCalculated <- pDirichletNegMultinom(t-k,k+alpha)

  # calculating the difference between the simulated and the calculated value
  dif = abs(pSimulated - pCalculated)
  return (list(pSimulated,pCalculated,dif,pCumulative))
}
