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

compare_function1 <- function(p,m1,n1,t,simulation){

  # first estimating the probability that player 1 wins by simulating the game for a given number of times.
  m <- t - m1 # m: the number of success that player 1 should have to win the game
  n <- t - n1 # n: the number of success that player 2 should have to win the game

  record_game <- vector()
  for (i in 1:simulation){
    # copy m and n
    record_trial <- rbind(m, n)
    # keep running the game until one of them wins
    while (record_trial[1, ] * record_trial[2, ] != 0){
      record_trial <- record_trial - rmultinom(1, 1, c(p, 1 - p)) #updating the recorded result
    }

    # if record_trial[1,] = 0, record 1,player 1 wins this single trial;
    # if record_trial[2,] = 0, record 0, player 2 wins
    record_game[i] <- record_trial[2, ]^record_trial[1, ]

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
  p_calculated <- extraDistr::pbnbinom(q = t-n-1, size = t-m, alpha = m+alpha, beta = n+beta)

  # calculating the difference between the simulated and the calculated value
  dif = abs(p_simulated - p_calculated)
  return (list(p_simulated,p_calculated,dif,p_cumulative))
}

compare_function4 <- function(k, t, alpha, simulation){
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
