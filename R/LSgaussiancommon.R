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

# data load and summary
.readyGaussianLS       <- function(options){
  # are data ready
  if(options[["dataType"]] == "dataCounts"){

    ready <- !(is.null(options[["SD_summary"]]) || options[["SD_summary"]] == 0 || is.null(options[["N"]]) || options[["N"]] == 0)

  }else if(options[["dataType"]] == "dataSequence"){

    ready <- !(nchar(options[["data_sequence"]]) == 0 || is.null(options[["SD_sequence"]]) || options[["SD_sequence"]] == 0)

  }else if(options[["dataType"]] == "dataVariable"){

    ready <- !(options[["selectedVariable"]] == "" || is.null(options[["SD_variable"]]) || options[["SD_variable"]] == 0)

  }

  # are priors ready
  ready <- c(ready, length(options[["priors"]]) > 0)

  return(ready)
}
.readDataGaussianLS    <- function(dataset, options){

  data <- list()

  if(options[["dataType"]] == "dataCounts"){

    data$y    <- NULL
    data$mean <- options[["mean"]]
    data$SD   <- options[["SD_summary"]]
    data$N    <- options[["N"]]

  }else{

    if(options[["dataType"]] == "dataSequence"){

      temp_y   <- .clean_sequence(options[["data_sequence"]])
      data$SD  <- options[["SD_sequence"]]

    }else if(options[["dataType"]] == "dataVariable"){

      # this is stupidly written #rework
      if (!is.null(dataset)){
        temp_y <- dataset
      }else{
        temp_y <- .readDataSetToEnd(columns = options[["selectedVariable"]])[,1]
      }

      data$SD  <- options[["SD_variable"]]

    }

    data$y    <- .cleanDataGaussianLS(temp_y, options)
    data$mean <- mean(data$y)
    data$N    <- length(data$y)

  }

  return(data)

}
.cleanDataGaussianLS   <- function(x, options){

  x <- na.omit(x)

  if(anyNA(as.numeric(x))){
    JASP:::.quitAnalysis(gettextf("Only numeric values are allowed in the input."))
  }

  return(as.numeric(x))
}
.summaryGaussianLS     <- function(jaspResults, data, options, analysis){

  if(!options[["dataSummary"]] && !options[["introText"]])
    return()

  if(is.null(jaspResults[["summaryContainer"]])){
    summaryContainer <- createJaspContainer("Data Input")
    summaryContainer$position <- 1
    jaspResults[["summaryContainer"]] <- summaryContainer
  }else{
    summaryContainer <- jaspResults[["summaryContainer"]]
  }


  if(options[["introText"]] && is.null(summaryContainer[['summaryText']])){

    summaryText <- createJaspHtml()
    summaryText$dependOn(c("introText", "dataSummary"))
    summaryText$position <- 1

    summaryText[['text']] <- .explanatoryTextLS("data", options, analysis)

    summaryContainer[['summaryText']] <- summaryText
  }


  if(options[["dataSummary"]] && options[["dataType"]] != "dataCounts" && is.null(summaryContainer[['summaryTable']])){

    summaryTable <- createJaspTable(title = gettext("Data Summary"))

    summaryTable$position <- 1
    summaryTable$dependOn(c("dataSummary", .GaussianLS_data_dependencies))

    summaryTable$addColumnInfo(name = "observations",  title = gettext("Observations"),  type = "integer")
    summaryTable$addColumnInfo(name = "mean",          title = gettext("Mean"),          type = "number")
    summaryTable$addColumnInfo(name = "sd",            title = gettext("SD"),            type = "number")

    summaryContainer[["summaryTable"]] <- summaryTable

    if(.readyGaussianLS(options)){

      summaryTable$addRows(list(observations = data$N,
                                mean         = data$mean,
                                sd           = data$SD))
    }
  }

  return()
}

# computational functions
.estimateGaussianLS         <- function(data, prior, CI = .95){

  if(prior[["type"]] == "spike"){

    output <- list(
      distribution = gettextf("spike at %s", prior[["parPoint_inp"]]),
      mean         = prior[["parPoint"]],
      median       = prior[["parPoint"]],
      lCI          = prior[["parPoint"]],
      uCI          = prior[["parPoint"]]
    )

    return(output)

  }else if(prior[["type"]] == "normal"){

    if(is.null(data)){

      output <- list(
        distribution = gettextf("normal (%s, %s)", prior[["parMu_inp"]], prior[["parSigma_inp"]]),
        mean         = prior[["parMu"]],
        median       = prior[["parMu"]],
        lCI          = stats::qnorm(  (1-CI)/2, prior[["parMu"]], prior[["parSigma"]]),
        uCI          = stats::qnorm(1-(1-CI)/2, prior[["parMu"]], prior[["parSigma"]])
      )

    }else{

      est_mean <- .estimateGaussianMeanLS(prior, data)
      est_SD   <- .estimateGaussianSDLS(prior, data)

      output <- list(
        distribution = gettextf("normal (%.2f, %.2f)", est_mean, est_SD),
        mean         = est_mean,
        median       = est_mean,
        lCI          = stats::qnorm(  (1-CI)/2, est_mean, est_SD),
        uCI          = stats::qnorm(1-(1-CI)/2, est_mean, est_SD)
      )

    }

    return(output)
  }
}
.estimateGaussianMeanLS     <- function(prior, data){
  return(
    ( prior[["parSigma"]]^2 * data$mean)         / ( (data$SD^2/data$N) +  prior[["parSigma"]]^2) +
    ( data$SD^2             * prior[["parMu"]] ) / ( (data$SD^2/data$N) +  prior[["parSigma"]]^2)
  )
}
.estimateGaussianSDLS       <- function(prior, data){
  return(sqrt(
    1/( 1 / prior[["parSigma"]]^2 + data$N / data$SD^2)
  ))
}
.testGaussianLS             <- function(data, priors){

  names     <- rep(NA, length(priors))
  prior     <- rep(NA, length(priors))
  log_lik   <- rep(NA, length(priors))

  for(i in 1:length(priors)){

    temp_prior <- priors[[i]]
    prior[i]   <- temp_prior$PH
    names[i]   <- temp_prior$name

    if(!is.null(data)){

      if(temp_prior[["type"]] == "spike"){

        log_lik[i]   <- stats::dnorm(data$mean, temp_prior[["parPoint"]], data$SD/sqrt(data$N), log = TRUE)

      }else if(temp_prior[["type"]] == "normal"){

        log_lik[i]   <- stats::dnorm(data$mean, prior[["parMu"]], sqrt( data$SD^2/data$N + prior[["parSigma"]]^2 ), log = TRUE)

      }

    }

  }


  PH_log_lik <- log(prior) + log_lik
  norm_const <- log(sum(exp(PH_log_lik)))
  posterior  <- exp(PH_log_lik - norm_const)


  return(data.frame(
    prior       = prior,
    log_lik     = log_lik,
    posterior   = posterior,
    name        = names
  ))

}
.predictGaussianLS          <- function(data, prior, options, N, CI = .95){

  if(prior[["type"]] == "spike"){

    output <- list(
      distribution = gettextf("normal (%s, %.2f)", prior[["parPoint_inp"]], data$SD/sqrt(N)),
      mean         = prior[["parPoint"]],
      median       = prior[["parPoint"]],
      lCI          = stats::qnorm(  (1-CI)/2, prior[["parPoint"]], data$SD/sqrt(N)),
      uCI          = stats::qnorm(1-(1-CI)/2, prior[["parPoint"]], data$SD/sqrt(N))
    )

    return(output)

  }else if(prior[["type"]] == "normal"){

    if(is.null(data)){

      pred_mean <- prior[["parMu"]]
      pred_SD   <- sqrt( data$SD^2/N + prior[["parSigma"]]^2 )

    }else{

      pred_mean <- .estimateGaussianMeanLS(prior, data)
      pred_SD   <- sqrt( data$SD^2/N + .estimateGaussianSDLS(prior, data)^2 )

    }

    output <- list(
      distribution = gettextf("normal (%.2f, %.2f)", pred_mean, pred_SD),
      mean         = pred_mean,
      median       = pred_mean,
      lCI          = stats::qnorm(  (1-CI)/2, pred_mean, pred_SD),
      uCI          = stats::qnorm(1-(1-CI)/2, pred_mean, pred_SD)
    )

    return(output)
  }
}
.normalSupportLS            <- function(data, prior, BF){

  if(is.null(data)){
    return(cbind.data.frame("lCI" = NA, "uCI" = NA))
  }

  prior_mean <- prior[["parMu"]]
  prior_SD   <- prior[["parSigma"]]
  post_mean  <- .estimateGaussianMeanLS(prior, data)
  post_SD    <- .estimateGaussianSDLS(prior, data)

  lSI <- stats::uniroot(
    .normalSupportFunLS,
    BF       = BF,
    mu_prior = prior_mean,
    sd_prior = prior_SD,
    mu_post  = post_mean,
    sd_post  = post_SD,
    lower    = -666 * prior_SD,     # can't use -Inf :(
    upper    = post_mean)$root

  uSI <- stats::uniroot(
    .normalSupportFunLS,
    BF       = BF,
    mu_prior = prior_mean,
    sd_prior = prior_SD,
    mu_post  = post_mean,
    sd_post  = post_SD,
    lower    = post_mean,
    upper    = 666 * prior_SD)$root

  return(cbind.data.frame("lCI" = lSI, "uCI" = uSI))
}
.normalSupportFunLS         <- function(x, BF, mu_prior, sd_prior, mu_post, sd_post){
  exp( stats::dnorm(x, mu_post, sd_post, TRUE) -  stats::dnorm(x, mu_prior, sd_prior, TRUE) ) - BF
}

# plotting functions
.rangeGaussianLS            <- function(data, prior,  type = "parameter", N = 0, prob = .99){

  if(prior[["type"]] == "spike"){

    if(type == "parameter"){

      range <- prior[["parPoint"]] + c(-1, +1)

    }else if(type == "prediction"){

      range <- stats::qnorm(c((1-prob)/2, 1 - (1-prob)/2), prior[["parPoint"]], data$SD/sqrt(N) )

    }


  }else if(prior[["type"]] == "normal"){

    if(is.null(data)){

      if(type == "parameter"){

        range <- stats::qnorm(c((1-prob)/2, 1 - (1-prob)/2), prior[["parMu"]], prior[["parSigma"]])

      }else if(type == "prediction"){

        range <- stats::qnorm(c((1-prob)/2, 1 - (1-prob)/2), prior[["parMu"]], sqrt( data$SD^2/N + prior[["parSigma"]]^2 ))
      }

    }else{

      post_mean <- .estimateGaussianMeanLS(prior, data)
      post_SD   <- .estimateGaussianSDLS(prior, data)

      if(type == "parameter"){

        range <- stats::qnorm(c((1-prob)/2, 1 - (1-prob)/2), post_mean, post_SD)

      }else if(type == "prediction"){

        range <- stats::qnorm(c((1-prob)/2, 1 - (1-prob)/2), post_mean,  sqrt( data$SD^2/N + post_SD^2 ) )
      }
    }
  }

  range <- range(JASPgraphs::getPrettyAxisBreaks(range))
  return(range)
}
.rangeGaussiansLS           <- function(data, priors, type = "parameter", N = 0, prob = .99){

  ranges <- sapply(priors, function(p).rangeGaussianLS(data, p, type, N, prob), simplify = FALSE)
  ranges <- do.call(rbind, ranges)
  range  <- c(min(ranges[,1]), max(ranges[,2]))

  return(range)

}
.rangeGaussianSupportLS     <- function(data, prior,  BF){

  if(prior[["type"]] == "spike"){
    rangesCI <- data.frame("lCI" = prior[["parPoint"]], "uCI" = prior[["parPoint"]])
  }else if(prior[["type"]] == "normal"){
    rangesCI <- .normalSupportLS(data, prior, BF)
  }
  ranges   <- .rangeGaussianLS(data, prior, "parameter")
  ranges   <- rbind(ranges, rangesCI)

  range  <- c(min(ranges[,1], na.rm = T), max(ranges[,2], na.rm = T))
  range  <- range(JASPgraphs::getPrettyAxisBreaks(range))

  return(range)

}
.rangeGaussiansSupportLS    <- function(data, priors, BF){

  ranges <- sapply(priors, function(p).rangeGaussianSupportLS(data, p, BF), simplify = FALSE)
  ranges <- do.call(rbind, ranges)
  range  <- c(min(ranges[,1]), max(ranges[,2]))

  return(range)

}
.dataLinesGaussianLS        <- function(data, prior,  type = "parameter", N = 0, range = NULL, points = 200){

  if(is.null(range)){
    range <- .rangeGaussianLS(data, prior, type, N)
  }

  x_seq   <- seq(range[1], range[2], length.out = points)


  if(type == "parameter"){

    if(prior[["type"]] == "spike"){

      stop("USE '.dataArrowGaussianLS()'")

    }else if(prior[["type"]] == "normal"){

      y_prior <- stats::dnorm(x_seq, prior[["parMu"]], prior[["parSigma"]])

    }


  }else if(type == "prediction"){

    if(prior[["type"]] == "spike"){

      y_prior <- stats::dnorm(x_seq, prior[["parPoint"]], sqrt( data$SD^2/N ))

    }else if(prior[["type"]] == "normal"){

      y_prior <- stats::dnorm(x_seq, prior[["parMu"]], sqrt( data$SD^2/N + prior[["parSigma"]]^2 ))

    }

  }

  if(!is.null(data)){

    if(prior[["type"]] == "spike"){

      if(type == "parameter"){

        stop("USE '.dataArrowGaussianLS()'")

      }else if(type == "prediction"){

        y_post <- stats::dnorm(x_seq, prior[["parPoint"]], sqrt( data$SD^2/N ))

      }


    }else if(prior[["type"]] == "normal"){

      post_mean <- .estimateGaussianMeanLS(prior, data)
      post_SD   <- .estimateGaussianSDLS(prior, data)

      if(type == "parameter"){

        y_post <- stats::dnorm(x_seq, post_mean, post_SD)

      }else if(type == "prediction"){

        y_post <- stats::dnorm(x_seq, post_mean,  sqrt( data$SD^2/N + post_SD^2 ) )
      }

    }

  }else{
    y_post <- y_prior
  }


  linesGroup <- c(y_post, y_prior)
  muGroup    <- c(x_seq,  x_seq)
  nameGroup  <- c(rep("Posterior", length(x_seq)), rep("Prior", length(x_seq)))

  dat        <- data.frame(x = muGroup, y = linesGroup, g = nameGroup)
  return(dat)
}
.dataLinesPredGaussianLS    <- function(data, prior,  type = "parameter", N = 0, range = NULL, points = 200){

  if(is.null(range)){
    range <- .rangeGaussianLS(data, prior, type, N)
  }

  x_seq   <- seq(range[1], range[2], length.out = points)


  if(is.null(data)){
    if(prior[["type"]] == "spike"){

      y <- stats::dnorm(x_seq, prior[["parPoint"]], sqrt( data$SD^2/N ) / sqrt(N))

    }else if(prior[["type"]] == "normal"){

      y <- stats::dnorm(x_seq, prior[["parMu"]], sqrt( data$SD^2/N + prior[["parSigma"]]^2 ) / sqrt(N) )

    }
  }else{
    if(prior[["type"]] == "spike"){

      y <- stats::dnorm(x_seq, prior[["parPoint"]], sqrt( data$SD^2/N ) /sqrt(N) )

    }else if(prior[["type"]] == "normal"){

      y <- stats::dnorm(x_seq, .estimateGaussianMeanLS(prior, data),  sqrt( data$SD^2/N + .estimateGaussianSDLS(prior, data)^2 ) / sqrt(N) )

    }
  }


  dat        <- data.frame(x = x_seq, y = y, g = "Prediction")
  return(dat)
}
.dataArrowGaussianLS        <- function(prior){
  dat       <- data.frame(x = prior[["parPoint"]], y_start = 0, y_end = 1, g = "Prior = Posterior")
  return(dat)
}
.dataObservedGaussianLS     <- function(data){

  dat   <- data.frame(x = data$mean, y = 0, g = "Sample proportion")

  return(dat)
}

.dataCentralGaussianLS      <- function(data, prior, coverage, N = NULL, type = c("parameter", "prediction")){

  if(type == "parameter"){

    if(prior[["type"]] == "spike"){

      x <- c(prior[["parPoint"]], prior[["parPoint"]])

    }else if(prior[["type"]] == "normal"){

      if(is.null(data)){

        x <- stats::qnorm(c((1 - coverage)/2, 1 - (1 - coverage)/2), prior[["parMu"]], prior[["parSigma"]])

      }else{

        x <- stats::qnorm(c((1 - coverage)/2, 1 - (1 - coverage)/2), .estimateGaussianMeanLS(prior, data), .estimateGaussianSDLS(prior, data))

      }

    }

  }else if(type == "prediction"){

    if(is.null(data)){

      if(prior[["type"]] == "spike"){

        x <- stats::qnorm(c((1 - coverage)/2, 1 - (1 - coverage)/2), prior[["parPoint"]], data$SD)

      }else if(prior[["type"]] == "normal"){

        x <- stats::qnorm(c((1 - coverage)/2, 1 - (1 - coverage)/2), prior[["parMu"]], sqrt( data$SD^2/N + prior[["parSigma"]]^2 ) )

      }

    }else{

      if(prior[["type"]] == "spike"){

        x <- stats::qnorm(c((1 - coverage)/2, 1 - (1 - coverage)/2), prior[["parPoint"]], data$SD/sqrt(N))

      }else if(prior[["type"]] == "normal"){

        post_mean <- .estimateGaussianMeanLS(prior, data)
        post_SD   <- .estimateGaussianSDLS(prior, data)

        x <- stats::qnorm(c((1 - coverage)/2, 1 - (1 - coverage)/2), post_mean,  sqrt( data$SD^2/N + post_SD^2 ) )

      }

    }

  }

  dat       <- data.frame(x_start = x[1], x_end = x[2], g = "central", coverage = coverage)
  return(dat)
}
.dataCustomGaussianLS       <- function(data, prior, lCI, uCI, N = NULL, type = c("parameter", "prediction")){

  if(type == "parameter"){

    if(prior[["type"]] == "spike"){

      coverage <- ifelse(lCI <= prior[["parPoint"]] & prior[["parPoint"]] <= uCI, 1, 0)

    }else if(prior[["type"]] == "normal"){

      if(is.null(data)){

        coverage <- stats::pnorm(uCI, prior[["parMu"]], prior[["parSigma"]]) -
          stats::pnorm(lCI, prior[["parMu"]], prior[["parSigma"]])

      }else{

        coverage <- stats::pnorm(uCI, .estimateGaussianMeanLS(prior, data), .estimateGaussianSDLS(prior, data)) -
          stats::pnorm(lCI, .estimateGaussianMeanLS(prior, data), .estimateGaussianSDLS(prior, data))

      }

    }

  }else if(type == "prediction"){

    if(is.null(data)){

      if(prior[["type"]] == "spike"){

        coverage <- stats::pnorm(uCI, prior[["parPoint"]], data$SD) -
          stats::pnorm(lCI, prior[["parPoint"]], data$SD)

      }else if(prior[["type"]] == "normal"){

        coverage <- stats::pnorm(uCI, prior[["parMu"]], sqrt( data$SD^2/N + prior[["parSigma"]]^2 ) ) -
          stats::pnorm(lCI, prior[["parMu"]], sqrt( data$SD^2/N + prior[["parSigma"]]^2 ) )

      }

    }else{

      if(prior[["type"]] == "spike"){

        coverage <- stats::pnorm(uCI, prior[["parPoint"]], data$SD/sqrt(N)) -
          stats::pnorm(lCI, prior[["parPoint"]], data$SD/sqrt(N))

      }else if(prior[["type"]] == "normal"){

        post_mean <- .estimateGaussianMeanLS(prior, data)
        post_SD   <- .estimateGaussianSDLS(prior, data)

        coverage <- stats::pnorm(uCI, post_mean,  sqrt( data$SD^2/N + post_SD^2 ) ) -
          stats::pnorm(lCI, post_mean,  sqrt( data$SD^2/N + post_SD^2 ) )

      }

    }

  }

  dat       <- data.frame(x_start = lCI, x_end = uCI, g = "custom", coverage = coverage)
  return(dat)
}
.dataSupportGaussianLS      <- function(data, prior, BF, range = NULL){

  if(prior[["type"]] == "spike"){

    lCI <- prior[["parPoint"]]
    uCI <- prior[["parPoint"]]

  }else if(prior[["type"]] == "normal"){

    if(is.null(range)){
      range <- .rangeGaussianSupportLS(data, prior, BF)
    }

    x <- .normalSupportLS(data, prior, BF)

    if(nrow(x) > 0){
      lCI      <- x$lCI
      uCI      <- x$uCI
    }else{
      lCI      <- NA
      uCI      <- NA
    }
  }

  dat <- data.frame(x_start = lCI, x_end = uCI, g = "support", BF = BF)
  return(dat)
}


# all settings dependent on data input
.GaussianLS_data_dependencies <- c("dataType", "data_sequence", "selectedVariable", "priors")
