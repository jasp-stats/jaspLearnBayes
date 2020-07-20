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
.marginalCentralLS     <- function(density, spikes, coverage, l.bound = 0, u.bound = 1, density_discrete = FALSE){
  
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
.marginalHPDLS         <- function(density, spikes, coverage, l.bound = 0, u.bound = 1, density_discrete = FALSE){
  
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
.marginalCustomLS      <- function(density, spikes, lCI, uCI, density_discrete = FALSE){
  
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
.marginalSupportLS     <- function(data, priors, post_density, post_spikes, BF){
  
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
.dbetaLS               <- function(alpha, beta){
  
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
