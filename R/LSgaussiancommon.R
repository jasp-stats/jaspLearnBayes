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
.readyGaussianLS       <- function(options) {
  # are data ready
  if (options[["dataInputType"]] == "counts")
    readyData <- TRUE
  else if (options[["dataInputType"]] == "sequence")
    readyData <- nchar(options[["dataSequenceSequenceOfObservations"]]) != 0
  else if (options[["dataInputType"]] == "variable")
    readyData <- options[["dataVariableSelected"]] != ""

  # are priors ready
  ready <- c("data" = readyData, "priors" = length(options[["priors"]]) > 0)

  return(ready)
}
.readDataGaussianLS    <- function(dataset, options) {

  data <- list()

  if (options[["dataInputType"]] == "counts") {

    data$y    <- NULL
    data$mean <- options[["dataCountsMean"]]
    data$SD   <- options[["dataCountsSd"]]
    data$N    <- options[["dataCountsN"]]

  } else{

    if (options[["dataInputType"]] == "sequence") {

      tempY   <- .cleanSequence(options[["dataSequenceSequenceOfObservations"]])
      data$SD <- options[["dataSequenceSequenceSd"]]

    } else if (options[["dataInputType"]] == "variable") {

      # this is stupidly written #rework
      if (!is.null(dataset)) {
        tempY <- dataset
      } else{
        tempY <- .readDataSetToEnd(columns = options[["dataVariableSelected"]])[,1]
      }

      data$SD <- options[["dataVariableSd"]]

    }

    data$y    <- .cleanDataGaussianLS(tempY, options)
    data$mean <- mean(data$y)
    data$N    <- length(data$y)

  }

  return(data)

}
.cleanDataGaussianLS   <- function(x, options) {

  x <- na.omit(x)

  if (anyNA(as.numeric(x))) {
    .quitAnalysis(gettextf("Only numeric values are allowed in the input."))
  }

  return(as.numeric(x))
}
.summaryGaussianLS     <- function(jaspResults, data, options, analysis) {

  if (!options[["dataSummary"]] && !options[["introText"]])
    return()

  if (is.null(jaspResults[["summaryContainer"]])) {
    summaryContainer <- createJaspContainer("Data Input")
    summaryContainer$position <- 1
    jaspResults[["summaryContainer"]] <- summaryContainer
  } else{
    summaryContainer <- jaspResults[["summaryContainer"]]
  }


  if (options[["introText"]] && is.null(summaryContainer[['summaryText']])) {

    summaryText <- createJaspHtml()
    summaryText$dependOn(c("introText", "dataSummary"))
    summaryText$position <- 1

    summaryText[['text']] <- .explanatoryTextLS("data", options, analysis)

    summaryContainer[['summaryText']] <- summaryText
  }


  if (options[["dataSummary"]] && options[["dataInputType"]] != "counts" && is.null(summaryContainer[['summaryTable']])) {

    summaryTable <- createJaspTable(title = gettext("Data Summary"))

    summaryTable$position <- 1
    summaryTable$dependOn(c("dataSummary", .dataDependenciesGaussianLS))

    summaryTable$addColumnInfo(name = "observations",  title = gettext("Observations"),  type = "integer")
    summaryTable$addColumnInfo(name = "mean",          title = gettext("Mean"),          type = "number")
    summaryTable$addColumnInfo(name = "sd",            title = gettext("SD"),            type = "number")

    summaryContainer[["summaryTable"]] <- summaryTable

    if (.readyGaussianLS(options)) {

      summaryTable$addRows(list(observations = data$N,
                                mean         = data$mean,
                                sd           = data$SD))
    }
  }

  return()
}

# computational functions
.estimateGaussianLS         <- function(data, prior, CI = .95) {

  if (prior[["type"]] == "spike") {

    output <- list(
      distribution = gettextf("spike at %s", prior[["parPointInp"]]),
      mean         = prior[["parPoint"]],
      median       = prior[["parPoint"]],
      mode         = prior[["parPoint"]],
      lCI          = prior[["parPoint"]],
      uCI          = prior[["parPoint"]]
    )

    return(output)

  } else if (prior[["type"]] == "normal") {

    if (is.null(data)) {

      output <- list(
        distribution = gettextf("normal (%s, %s)", prior[["parMuInp"]], prior[["parSigmaInp"]]),
        mean         = prior[["parMu"]],
        median       = prior[["parMu"]],
        mode         = prior[["parMu"]],
        lCI          = stats::qnorm(  (1-CI)/2, prior[["parMu"]], prior[["parSigma"]]),
        uCI          = stats::qnorm(1-(1-CI)/2, prior[["parMu"]], prior[["parSigma"]])
      )

    } else{

      estMean <- .estimateGaussianMeanLS(prior, data)
      estSD   <- .estimateGaussianSDLS(prior, data)

      output <- list(
        distribution = gettextf("normal (%.2f, %.2f)", estMean, estSD),
        mean         = estMean,
        median       = estMean,
        lCI          = stats::qnorm(  (1-CI)/2, estMean, estSD),
        uCI          = stats::qnorm(1-(1-CI)/2, estMean, estSD)
      )

    }

    return(output)
  }
}
.estimateGaussianMeanLS     <- function(prior, data) {

  return(
    ( prior[["parSigma"]]^2 * data$mean)         / ( (data$SD^2/data$N) +  prior[["parSigma"]]^2) +
      ( data$SD^2             * prior[["parMu"]] ) / ( (data$SD^2/data$N) +  prior[["parSigma"]]^2)
  )
}
.estimateGaussianSDLS       <- function(prior, data) {
  return(sqrt(
    1/( 1 / prior[["parSigma"]]^2 + data$N / data$SD^2)
  ))
}
.testGaussianLS             <- function(data, priors) {

  names     <- rep(NA, length(priors))
  prior     <- rep(NA, length(priors))
  logLik   <- rep(NA, length(priors))

  for (i in 1:length(priors)) {

    tempPrior <- priors[[i]]
    prior[i]   <- tempPrior$PH
    names[i]   <- tempPrior$name

    if (!is.null(data)) {

      if (tempPrior[["type"]] == "spike") {

        logLik[i]   <- stats::dnorm(data$mean, tempPrior[["parPoint"]], data$SD/sqrt(data$N), log = TRUE)

      } else if (tempPrior[["type"]] == "normal") {

        logLik[i]   <- stats::dnorm(data$mean, prior[["parMu"]], sqrt( data$SD^2 + prior[["parSigma"]]^2 )/sqrt(data$N), log = TRUE)

      }

    }

  }


  logLikPH  <- log(prior) + logLik
  normConst <- log(sum(exp(logLikPH)))
  posterior <- exp(logLikPH - normConst)


  return(data.frame(
    prior       = prior,
    logLik      = logLik,
    posterior   = posterior,
    name        = names
  ))

}
.predictGaussianLS          <- function(data, prior, options, N, CI = .95) {

  if (prior[["type"]] == "spike") {

    output <- list(
      distribution = gettextf("normal (%s, %.2f)", prior[["parPointInp"]], data$SD),
      mean         = prior[["parPoint"]],
      median       = prior[["parPoint"]],
      mode         = prior[["parPoint"]],
      lCI          = stats::qnorm(  (1-CI)/2, prior[["parPoint"]], data$SD),
      uCI          = stats::qnorm(1-(1-CI)/2, prior[["parPoint"]], data$SD)
    )

    return(output)

  } else if (prior[["type"]] == "normal") {

    if (is.null(data)) {

      predMean <- prior[["parMu"]]
      predSD   <- sqrt( data$SD^2 + prior[["parSigma"]]^2 )

    } else{

      predMean <- .estimateGaussianMeanLS(prior, data)
      predSD   <- sqrt( data$SD^2 + .estimateGaussianSDLS(prior, data)^2 )

    }

    output <- list(
      distribution = gettextf("normal (%.2f, %.2f)", predMean, predSD),
      mean         = predMean,
      median       = predMean,
      mode         = predMean,
      lCI          = stats::qnorm(  (1-CI)/2, predMean, predSD),
      uCI          = stats::qnorm(1-(1-CI)/2, predMean, predSD)
    )

    return(output)
  }
}
.normalSupportLS            <- function(data, prior, BF) {

  if (is.null(data)) {
    return(cbind.data.frame("lCI" = NA, "uCI" = NA))
  }

  priorMean <- prior[["parMu"]]
  priorSD   <- prior[["parSigma"]]
  postMean  <- .estimateGaussianMeanLS(prior, data)
  postSD    <- .estimateGaussianSDLS(prior, data)

  lSI <- tryCatch(stats::uniroot(
    .normalSupportFunLS,
    BF       = BF,
    muPrior = priorMean,
    sdPrior = priorSD,
    muPost  = postMean,
    sdPost  = postSD,
    lower    = -666 * priorSD,     # can't use -Inf :(
    upper    = postMean)$root, error = function(e)return(NA))

  uSI <- tryCatch(stats::uniroot(
    .normalSupportFunLS,
    BF       = BF,
    muPrior = priorMean,
    sdPrior = priorSD,
    muPost  = postMean,
    sdPost  = postSD,
    lower    = postMean,
    upper    = 666 * priorSD)$root, error = function(e)return(NA))



  return(cbind.data.frame("lCI" = lSI, "uCI" = uSI))
}
.normalSupportFunLS         <- function(x, BF, muPrior, sdPrior, muPost, sdPost) {
  exp( stats::dnorm(x, muPost, sdPost, TRUE) -  stats::dnorm(x, muPrior, sdPrior, TRUE) ) - BF
}

# plotting functions
.rangeGaussianLS            <- function(data, prior, type = "parameter", N = 0, prob = .99) {

  if (prior[["type"]] == "spike") {

    if (type == "parameter") {

      range <- prior[["parPoint"]] + c(-1, +1)

    } else if (type == "prediction") {

      range <- stats::qnorm(c((1-prob)/2, 1 - (1-prob)/2), prior[["parPoint"]], data$SD )

    }


  } else if (prior[["type"]] == "normal") {

    if (is.null(data)) {

      if (type == "parameter") {

        range <- stats::qnorm(c((1-prob)/2, 1 - (1-prob)/2), prior[["parMu"]], prior[["parSigma"]])

      } else if (type == "prediction") {

        range <- stats::qnorm(c((1-prob)/2, 1 - (1-prob)/2), prior[["parMu"]], sqrt( data$SD^2 + prior[["parSigma"]]^2 ))
      }

    } else{

      postMean <- .estimateGaussianMeanLS(prior, data)
      postSD   <- .estimateGaussianSDLS(prior, data)

      if (type == "parameter") {

        range <- stats::qnorm(c((1-prob)/2, 1 - (1-prob)/2), postMean, postSD)

      } else if (type == "prediction") {

        range <- stats::qnorm(c((1-prob)/2, 1 - (1-prob)/2), postMean,  sqrt( data$SD^2 + postSD^2 ) )
      }
    }
  }

  range <- range(jaspGraphs::getPrettyAxisBreaks(range))
  return(range)
}
.rangeGaussiansLS           <- function(data, priors, type = "parameter", N = 0, prob = .99) {

  ranges <- sapply(priors, function(p).rangeGaussianLS(data, p, type, N, prob), simplify = FALSE)
  ranges <- do.call(rbind, ranges)
  range  <- c(min(ranges[,1]), max(ranges[,2]))

  return(range)

}
.rangeGaussianSupportLS     <- function(data, prior,  BF) {

  if (prior[["type"]] == "spike") {
    rangesCI <- data.frame("lCI" = prior[["parPoint"]], "uCI" = prior[["parPoint"]])
  } else if (prior[["type"]] == "normal") {
    rangesCI <- .normalSupportLS(data, prior, BF)
  }
  ranges   <- .rangeGaussianLS(data, prior, "parameter")
  ranges   <- rbind(ranges, rangesCI)

  range  <- c(min(ranges[,1], na.rm = T), max(ranges[,2], na.rm = T))
  range  <- range(jaspGraphs::getPrettyAxisBreaks(range))

  return(range)
}
.rangeGaussiansSupportLS    <- function(data, priors, BF) {

  ranges <- sapply(priors, function(p).rangeGaussianSupportLS(data, p, BF), simplify = FALSE)
  ranges <- do.call(rbind, ranges)
  range  <- c(min(ranges[,1]), max(ranges[,2]))

  return(range)
}
.dataLinesGaussianLS        <- function(data, prior,  type = "parameter", N = 1, range = NULL, points = 200) {

  if (is.null(range)) {
    range <- .rangeGaussianLS(data, prior, type, N)
  }

  xSeq   <- seq(range[1], range[2], length.out = points)


  if (type == "parameter") {

    if (prior[["type"]] == "spike") {

      stop("USE '.dataArrowGaussianLS()'")

    } else if (prior[["type"]] == "normal") {

      yPrior <- stats::dnorm(xSeq, prior[["parMu"]], prior[["parSigma"]])

    }


  } else if (type == "prediction") {

    if (prior[["type"]] == "spike") {

      yPrior <- stats::dnorm(xSeq, prior[["parPoint"]], data$SD/sqrt(N))

    } else if (prior[["type"]] == "normal") {

      yPrior <- stats::dnorm(xSeq, prior[["parMu"]], sqrt( data$SD^2 + prior[["parSigma"]]^2 )/sqrt(N))

    }

  }

  if (!is.null(data)) {

    if (prior[["type"]] == "spike") {

      if (type == "parameter") {

        stop("USE '.dataArrowGaussianLS()'")

      } else if (type == "prediction") {

        yPost <- stats::dnorm(xSeq, prior[["parPoint"]], sqrt( data$SD^2 )/sqrt(N))

      }


    } else if (prior[["type"]] == "normal") {

      postMean <- .estimateGaussianMeanLS(prior, data)
      postSD   <- .estimateGaussianSDLS(prior, data)

      if (type == "parameter") {

        yPost <- stats::dnorm(xSeq, postMean, postSD)

      } else if (type == "prediction") {

        yPost <- stats::dnorm(xSeq, postMean,  sqrt( data$SD^2 + postSD^2 )/sqrt(N) )
      }

    }

  } else{
    yPost <- yPrior
  }


  linesGroup <- c(yPost, yPrior)
  muGroup    <- c(xSeq,  xSeq)
  nameGroup  <- c(rep(gettext("Posterior"), length(xSeq)), rep(gettext("Prior"), length(xSeq)))

  dat        <- data.frame(x = muGroup, y = linesGroup, g = nameGroup)
  return(dat)
}
.dataLinesPredGaussianLS    <- function(data, prior,  type = "parameter", N = 0, range = NULL, points = 200) {

  if (is.null(range)) {
    range <- .rangeGaussianLS(data, prior, type, N)
  }

  xSeq   <- seq(range[1], range[2], length.out = points)


  if (is.null(data)) {
    if (prior[["type"]] == "spike") {

      y <- stats::dnorm(xSeq, prior[["parPoint"]], data$SD)

    } else if (prior[["type"]] == "normal") {

      y <- stats::dnorm(xSeq, prior[["parMu"]], sqrt( data$SD^2 + prior[["parSigma"]]^2 ) )

    }
  } else{
    if (prior[["type"]] == "spike") {

      y <- stats::dnorm(xSeq, prior[["parPoint"]], data$SD )

    } else if (prior[["type"]] == "normal") {

      y <- stats::dnorm(xSeq, .estimateGaussianMeanLS(prior, data),  sqrt( data$SD^2 + .estimateGaussianSDLS(prior, data)^2 ))

    }
  }


  dat        <- data.frame(x = xSeq, y = y, g = "Prediction")
  return(dat)
}
.dataArrowGaussianLS        <- function(prior) {
  dat       <- data.frame(x = prior[["parPoint"]], yStart = 0, yEnd = 1, g = "Prior = Posterior")
  return(dat)
}
.dataObservedGaussianLS     <- function(data) {

  dat   <- data.frame(x = data$mean, y = 0, g = "Sample proportion")

  return(dat)
}

.dataCentralGaussianLS      <- function(data, prior, coverage, N = 1, type = c("parameter", "prediction")) {

  if (type == "parameter") {

    if (prior[["type"]] == "spike") {

      x <- c(prior[["parPoint"]], prior[["parPoint"]])

    } else if (prior[["type"]] == "normal") {

      if (is.null(data)) {

        x <- stats::qnorm(c((1 - coverage)/2, 1 - (1 - coverage)/2), prior[["parMu"]], prior[["parSigma"]])

      } else{

        x <- stats::qnorm(c((1 - coverage)/2, 1 - (1 - coverage)/2), .estimateGaussianMeanLS(prior, data), .estimateGaussianSDLS(prior, data))

      }

    }

  } else if (type == "prediction") {

    if (is.null(data)) {

      if (prior[["type"]] == "spike") {

        x <- stats::qnorm(c((1 - coverage)/2, 1 - (1 - coverage)/2), prior[["parPoint"]], data$SD /sqrt(N))

      } else if (prior[["type"]] == "normal") {

        x <- stats::qnorm(c((1 - coverage)/2, 1 - (1 - coverage)/2), prior[["parMu"]], sqrt( data$SD^2 + prior[["parSigma"]]^2 ) /sqrt(N) )

      }

    } else{

      if (prior[["type"]] == "spike") {

        x <- stats::qnorm(c((1 - coverage)/2, 1 - (1 - coverage)/2), prior[["parPoint"]], data$SD /sqrt(N))

      } else if (prior[["type"]] == "normal") {

        postMean <- .estimateGaussianMeanLS(prior, data)
        postSD   <- .estimateGaussianSDLS(prior, data)

        x <- stats::qnorm(c((1 - coverage)/2, 1 - (1 - coverage)/2), postMean,  sqrt( data$SD^2 + postSD^2 ) /sqrt(N) )

      }

    }

  }

  dat       <- data.frame(xStart = x[1], xEnd = x[2], g = "central", coverage = coverage)
  return(dat)
}
.dataCustomGaussianLS       <- function(data, prior, lCI, uCI, N = 1, type = c("parameter", "prediction")) {

  if (type == "parameter") {

    if (prior[["type"]] == "spike") {

      coverage <- ifelse (lCI <= prior[["parPoint"]] & prior[["parPoint"]] <= uCI, 1, 0)

    } else if (prior[["type"]] == "normal") {

      if (is.null(data)) {

        coverage <- stats::pnorm(uCI, prior[["parMu"]], prior[["parSigma"]]) -
          stats::pnorm(lCI, prior[["parMu"]], prior[["parSigma"]])

      } else{

        coverage <- stats::pnorm(uCI, .estimateGaussianMeanLS(prior, data), .estimateGaussianSDLS(prior, data)) -
          stats::pnorm(lCI, .estimateGaussianMeanLS(prior, data), .estimateGaussianSDLS(prior, data))

      }

    }

  } else if (type == "prediction") {

    if (is.null(data)) {

      if (prior[["type"]] == "spike") {

        coverage <- stats::pnorm(uCI, prior[["parPoint"]], data$SD /sqrt(N)) -
          stats::pnorm(lCI, prior[["parPoint"]], data$SD /sqrt(N))

      } else if (prior[["type"]] == "normal") {

        coverage <- stats::pnorm(uCI, prior[["parMu"]], sqrt( data$SD^2 + prior[["parSigma"]]^2 ) /sqrt(N) ) -
          stats::pnorm(lCI, prior[["parMu"]], sqrt( data$SD^2 + prior[["parSigma"]]^2 ) /sqrt(N) )

      }

    } else{

      if (prior[["type"]] == "spike") {

        coverage <- stats::pnorm(uCI, prior[["parPoint"]], data$SD /sqrt(N)) -
          stats::pnorm(lCI, prior[["parPoint"]], data$SD /sqrt(N))

      } else if (prior[["type"]] == "normal") {

        postMean <- .estimateGaussianMeanLS(prior, data)
        postSD   <- .estimateGaussianSDLS(prior, data)

        coverage <- stats::pnorm(uCI, postMean,  sqrt( data$SD^2 + postSD^2 ) /sqrt(N) ) -
          stats::pnorm(lCI, postMean,  sqrt( data$SD^2 + postSD^2 ) /sqrt(N) )

      }

    }

  }

  dat       <- data.frame(xStart = lCI, xEnd = uCI, g = "custom", coverage = coverage)
  return(dat)
}
.dataSupportGaussianLS      <- function(data, prior, BF, range = NULL) {

  if (prior[["type"]] == "spike") {

    lCI <- prior[["parPoint"]]
    uCI <- prior[["parPoint"]]

  } else if (prior[["type"]] == "normal") {

    if (is.null(range)) {
      range <- .rangeGaussianSupportLS(data, prior, BF)
    }

    x <- .normalSupportLS(data, prior, BF)

    if (!anyNA(x)) {
      lCI      <- x$lCI
      uCI      <- x$uCI
    } else{
      lCI      <- NA
      uCI      <- NA
    }
  }

  dat <- data.frame(xStart = lCI, xEnd = uCI, g = "support", BF = BF)
  return(dat)
}
.estimateDataPointGaussian  <- function(data, prior, N, type = c("parameter", "prediction"), estimate = c("mean", "median", "mode"), prop) {

  if (type == "parameter") {

    tempEst <- .estimateGaussianLS(data, prior)
    l <- tempEst[[estimate]]

    if (prior[["type"]] == "spike") {

      x <- prior$parPoint
      y <- 1

    } else if (prior[["type"]] == "normal") {

      x <- tempEst[[estimate]]
      y <- stats::dnorm(x, .estimateGaussianMeanLS(prior, data), .estimateGaussianSDLS(prior, data))

    }

  } else if (type == "prediction") {

    options  <- list(predictionN = N)
    tempPred <- .predictGaussianLS(data, prior, options, prop)
    l <- tempPred[[estimate]]

    if (prior[["type"]] == "spike") {

      x <- tempPred[[estimate]]
      if (!prop)
        y <- stats::dnorm(x, prior$parPoint, data$SD)
      else
        y <- stats::dnorm(x, prior$parPoint, data$SD/sqrt(N))

    } else if (prior[["type"]] == "normal") {

      x <- tempPred[[estimate]]
      if (!prop)
        y <- stats::dnorm(x, .estimateGaussianMeanLS(prior, data), sqrt( data$SD^2 + .estimateGaussianSDLS(prior, data)^2 ))
      else
        y <- stats::dnorm(x, .estimateGaussianMeanLS(prior, data), sqrt( data$SD^2 + .estimateGaussianSDLS(prior, data)^2 )/sqrt(N))
    }
  }

  dat <- data.frame(x = x, y = y, estimate = estimate, l = l)
  return(dat)
}

# all settings dependent on data input
.dataDependenciesGaussianLS <- c("dataInputType", "dataCountsMean", "dataCountsSd", "dataCountsN", "dataSequenceSequenceOfObservations", "dataSequenceSequenceSd", "dataVariableSelected", "dataVariableSd", "priors")
