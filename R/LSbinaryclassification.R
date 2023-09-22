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

LSbinaryclassification <- function(jaspResults, dataset, options, state = NULL) {
  .bcIntro(jaspResults, options)

  options <- .bcParseOptions(jaspResults, options)
  ready   <- .bcIsReady     (options)
  dataset <- .bcReadData    (jaspResults, dataset, options, ready)

  results <- .bcComputeResults(jaspResults, dataset, options, ready)
  summary <- .bcComputeSummary(jaspResults, results, options)

  .bcTables(results, summary, jaspResults, dataset, options, ready)
  .bcPlots (results, summary, jaspResults, dataset, options, ready)
}

.bcIntro <- function(jaspResults, options) {
  if(isFALSE(options[["introductoryText"]])) return()
  if(!is.null(jaspResults[["introductoryText"]])) return()

  text <- gettextf('This analysis demonstrates <b>binary classification</b> which is a common statistical procedure where subjects are classified into two groups based on a classification rule.

                   Binary classification is a procedure where data about the subject is dichotomised to reach a binary decision (e.g., yes/no, true/false).

                   Common binary classification applications are:
                   <ul>
                     <li><b>Medical testing and diagnosis</b> where the classification determines whether a patient has a certain disease or not.</li>
                     <li><b>Spam detection</b> where the classification determines whether a message (e.g., an email) is spam or not.</li>
                     <li><b>Quality control</b> where the classification determines whether an industry standard is met or not.</li>
                   </ul>

                   Because binary classification is often used in medical testing, the terminology surrounding binary classification problems is similar to medical terminology (e.g., prevalence, condition, marker).

                   In practice, there is an important distinction between the <i>true</i> state of the subject and the <i>assigned label</i> given by the classification rule.
                   In JASP, the true state is called <b>condition</b> (positive/negative) and the assigned label is called <b>test</b> (positive/negative).
                   In many applications, the conditions or the tests may not be symmetric, therefore various types of errors are of interest in addition to an overall measure of accuracy.
                   These types of errors distinguish, for example, whether a patient has a disease but the test came back negative (false negative), or whether a patient does not have a disease but the test came back positive (false positive).

                   Properties of the test are usually described in terms of its <b>sensitivity</b> and <b>specificity</b>.
                   Sensitivity is the probability of testing positive if the condition is positive.
                   Specificity is the probability of testing negative if the condition is negative.
                   A property of condition is <b>prevalence</b>, which is the proportion of subjects that have a positive condition in the population.
                   Asymmetric characteristics of the test or prevalence can lead to situations that may appear to the untrained eye as paradoxical.
                   For example, in certain situations it is more likely that a patient does not have a particular disease than that they do, even after testing positive for that disease.

                   Formally, the probability that a subject has a positive condition after the test is positive (i.e., the positive predictive value) is obtained by applying <b>Bayes theorem</b>:

                   %s

                   <h5>References</h5>
                   Pepe, M. S. (2003). <i>The statistical evaluation of medical tests for classification and prediction</i>. Oxford University Press.

                   <a href="https://wikipedia.org/wiki/Binary_classification">https://wikipedia.org/wiki/Binary_classification</a>

                   <a href="https://wikipedia.org/wiki/Evaluation_of_binary_classifiers">https://wikipedia.org/wiki/Evaluation_of_binary_classifiers</a>

                   <a href="https://wikipedia.org/wiki/Bayes%%27_theorem">https://wikipedia.org/wiki/Bayes%%27_theorem</a>
                   ', .bcBayesTheoremExpression())

  jaspResults[["introductoryText"]] <- createJaspHtml(title        = gettext("Welcome to binary classification with JASP!"),
                                               text         = text,
                                               dependencies = "introductoryText",
                                               position     = 1)
}

.bcParseOptions <- function(jaspResults, options) {
  options <- .parseAndStoreFormulaOptions(
    jaspResults, options,
    c("prevalence", "sensitivity", "specificity",
    "priorPrevalenceAlpha", "priorPrevalenceBeta",
    "priorSensitivityAlpha", "priorSensitivityBeta",
    "priorSpecificityAlpha", "priorSpecificityBeta",
    "threshold")
    )

  if(options[["inputType"]] == "pointEstimates") options[["ci"]] <- FALSE


  colors <- .bcGetColors(options)[["values"]]

  if(!.bcIsColor(colors))
    jaspBase::.quitAnalysis(gettext("Some of the specified colors are not valid colors."))

  return(options)
}

.bcIsReady <- function(options) {
  if(options[["inputType"]] != "data") return(options[["computeResults"]])

  if(options[["marker"]] != "" && options[["labels"]] != "") {
    return(options[["computeResults"]])
  } else {
    return(FALSE)
  }
}

.bcReadData <- function(jaspResults, dataset, options, ready) {
  if(options[["inputType"]] != "data" || !ready) return(NULL)

  if(is.null(dataset)) {
    dataset <- .readDataSetToEnd(columns = c(options[["marker"]], options[["labels"]]))
    dataset <- dataset[complete.cases(dataset),]

    labels <- dataset[[options[["labels"]]]]
    levels <- levels(as.factor(labels))
    if(length(levels) != 2)    .quitAnalysis(gettext("The 'Positive condition (binary)' variable must have two levels!"))
    if(any(table(labels) < 1)) .quitAnalysis(gettext("Each condition needs at least one observation."))

    dataset <- data.frame(
      marker    = dataset[[options[["marker"]]]],
      condition = labels == levels[2], # second level is "positive" condition
      test      = dataset[[options[["marker"]]]] >= options[["threshold"]]
    )
  }

  if(mean(subset(dataset, condition)$marker) < mean(subset(dataset, !condition)$marker))
    .quitAnalysis(gettextf("Mean of marker in positive condition (%1$s) needs to be larger than the mean of marker in negative condition (%2$s).", levels[2], levels[1]))

  return(dataset)
}

# Results computation and reshaping ----
.bcComputeResults <- function(jaspResults, dataset, options, ready) {
  if(!is.null(jaspResults[["results"]])) return(.bcStatistics(jaspResults[["results"]]$object))

  results <- switch(options[["inputType"]],
                    pointEstimates     = .bcComputeResultsPointEstimates(options),
                    uncertainEstimates = .bcComputeResultsUncertainEstimates(options),
                    data               = .bcComputeResultsFromData(options, dataset, ready)
                    )

  jaspResults[["results"]] <-
    createJaspState(object       = results,
                    dependencies = c("computeResults", "inputType", "marker", "labels", "threshold", "samples",
                                     "truePositive", "falsePositive", "falseNegative", "trueNegative",
                                     "updatePrevalence", "orderConstraint", "positiveTests", "negativeTests",
                                     c("sensitivity", "specificity", "prevalence"),
                                     levels(interaction(c("priorSensitivity", "priorSpecificity", "priorPrevalence"), c("Alpha", "Beta"), sep = ""))
                                     )
                    )
  return(.bcStatistics(results))
}

.bcComputeSummary <- function(jaspResults, results, options) {
  if(!is.null(jaspResults[["summary"]])) return(jaspResults[["summary"]]$object)

  output <- summary(results, ciLevel = options[["ciLevel"]])

  jaspResults[["summary"]] <- createJaspState(object = output)
  jaspResults[["summary"]]$dependOn(optionsFromObject = jaspResults[["results"]], options = "ciLevel")

  return(output)
}

.bcComputeResultsPointEstimates <- function(options) {
  results <- list(
    prevalence  = options[["prevalence"]],
    sensitivity = options[["sensitivity"]],
    specificity = options[["specificity"]]
  )

  class(results) <- c("bcPointEstimates", class(results))
  return(results)
}

summary.bcPointEstimates <- function(results, ...) {
  output <- data.frame(
    variable       = names(results),
    statistic      = .bcTexts("statistic")[names(results)],
    estimate       = unlist(results),
    lowerCI        = NA,
    upperCI        = NA,
    notation       = .bcTexts("notation")[names(results)],
    interpretation = .bcTexts("interpretation")[names(results)],
    stringsAsFactors = FALSE
    )

  return(output)
}

.bcComputeResultsUncertainEstimates <- function(options, progress = TRUE) {
  results <- .bcDrawPosteriorSamples(options, progress = progress)

  class(results) <- c("bcUncertainEstimates", class(results))
  return(results)
}

summary.bcUncertainEstimates <- function(results, ciLevel=0.95) {
  alpha <- 1-ciLevel

  output <- posterior::summarise_draws(
    results,
    estimate = mean,
    median = median,
    sd = sd,
    lowerCI = ~unname(quantile(.x, probs =   alpha/2)),
    upperCI = ~unname(quantile(.x, probs = 1-alpha/2)),
    ssEff = posterior::ess_bulk,
    rhat = posterior::rhat
  )
  output[["statistic"]] <- .bcTexts("statistic")[output[["variable"]]]
  output[["notation"]] <- .bcTexts("notation")[output[["variable"]]]
  output[["interpretation"]] <- .bcTexts("interpretation")[output[["variable"]]]

  return(output)
}

.bcComputeResultsFromData <- function(options, dataset, ready, threshold = NULL) {
  if (!ready) {
    opts <- list(
      truePositive = 0, falsePositive = 0, falseNegative = 0, trueNegative = 0
    )
    tp <- fp <- fn <- tn <- 0
  } else if (options[["inputType"]] == "data") {
    if (!is.null(threshold))
      dataset[["test"]] <- dataset[["marker"]] >= threshold
    opts <- list(
      truePositive = sum( dataset[["condition"]] &  dataset[["test"]]),
      falsePositive = sum(!dataset[["condition"]] &  dataset[["test"]]),
      falseNegative = sum( dataset[["condition"]] & !dataset[["test"]]),
      trueNegative = sum(!dataset[["condition"]] & !dataset[["test"]])
    )
  }
  newOptions <- utils::modifyList(options, opts)
  results <- .bcDrawPosteriorSamples(newOptions)
  class(results) <- c("bcData", "bcUncertainEstimates", class(results))
  return(results)
}


.bcComputeSummaryByThreshold <- function(jaspResults, options, dataset) {
  if(!is.null(jaspResults[["summaryByThreshold"]])) return(jaspResults[["summaryByThreshold"]]$object)

  if(nrow(dataset) < 50) {
    thresholds <- c(dataset$marker, options[["threshold"]])
  } else {
    thresholds <- c(quantile(dataset$marker, seq(0, 1, by = 0.02), na.rm=TRUE), options[["threshold"]])
  }
  threshold <- sort(thresholds)

  mcmcOptions <- list(
    samples = options[["varyingThresholdSamples"]],
    burnin = options[["varyingThresholdBurnin"]],
    thinning = options[["varyingThresholdThinning"]],
    chains = options[["varyingThresholdChains"]]
  )
  mcmcOptions <- utils::modifyList(options, mcmcOptions)

  startProgressbar(expectedTicks = length(thresholds), label = gettext("Estimating parameters per varying thresholds..."))
  results <- lapply(thresholds, function(threshold) {
    res <- .bcComputeResultsFromData(mcmcOptions, dataset, TRUE, threshold)
    res <- .bcStatistics(res)
    class(res) <- c("bcVaryingThresholds", class(res))
    res <- summary(res, ciLevel = mcmcOptions[["ciLevel"]], threshold = threshold)
    progressbarTick()
    return(res)
  })
  results <- do.call(rbind, results)

  jaspResults[["summaryByThreshold"]] <- jaspBase::createJaspState(object = results)
  jaspResults[["summaryByThreshold"]]$dependOn(
    optionsFromObject = jaspResults[["results"]],
    options = c("ciLevel", "varyingThresholdSamples", "varyingThresholdBurnin", "varyingThresholdThinning", "varyingThresholdChains")
    )
  return(results)
}

summary.bcVaryingThresholds <- function(results, ciLevel, threshold) {
  alpha <- 1-ciLevel

  output <- posterior::summarise_draws(
    results,
    estimate = mean,
    lowerCI = ~unname(quantile(.x, probs =   alpha/2)),
    upperCI = ~unname(quantile(.x, probs = 1-alpha/2))
  )
  output[["threshold"]] <- threshold

  return(output)
}

.bcStatistics <- function(results) {
  cls <- class(results)
  results <- within(
    results,
    {
      truePositive            <- prevalence*sensitivity
      falsePositive           <- (1-prevalence)*(1-specificity)
      trueNegative            <- (1-prevalence)*specificity
      falseNegative           <- prevalence*(1-sensitivity)
      positivePredictiveValue <- truePositive / (truePositive + falsePositive)
      negativePredictiveValue <- trueNegative / (trueNegative + falseNegative)
      falseDiscoveryRate      <- 1-positivePredictiveValue
      falseOmissionRate       <- 1-negativePredictiveValue
      falsePositiveRate       <- 1-specificity
      falseNegativeRate       <- 1-sensitivity
      accuracy                <- truePositive + trueNegative
      pretestOdds             <- prevalence / (1-prevalence)
      positiveLikelihoodRatio <- sensitivity / (1-specificity)
      negativeLikelihoodRatio <- (1-sensitivity) / (specificity)
      fMeasure                <- 2 / (sensitivity^(-1) + positivePredictiveValue^{-1})
    }
  )

  statNames <- names(.bcTexts("statistic"))
  columnOrder <- match(statNames, names(results))

  if(posterior::is_draws(results)) {
    columnOrder <- c(columnOrder, match(c(".chain", ".iteration", ".draw"), names(results)))
  }

  results <- results[columnOrder]
  class(results) <- cls
  return(results)
}

.bcGetPosterior <- function(options, dataset = NULL, threshold = NULL) {
  if(!is.null(threshold)) dataset[["test"]] <- dataset[["marker"]] >= threshold
  if(options[["inputType"]] == "uncertainEstimates") {
    tp <- options[["truePositive"]]
    fp <- options[["falsePositive"]]
    fn <- options[["falseNegative"]]
    tn <- options[["trueNegative"]]
  } else if(options[["inputType"]] == "data") {
    tp <- sum( dataset[["condition"]] &  dataset[["test"]])
    fp <- sum(!dataset[["condition"]] &  dataset[["test"]])
    fn <- sum( dataset[["condition"]] & !dataset[["test"]])
    tn <- sum(!dataset[["condition"]] & !dataset[["test"]])
  } else {
    tp <- fp <- fn <- tn <- 0
  }
  results <- list(
    priorPrevalenceAlpha  = options[["priorPrevalenceAlpha"]]  + tp + fn,
    priorPrevalenceBeta   = options[["priorPrevalenceBeta"]]   + fp + tn,
    priorSensitivityAlpha = options[["priorSensitivityAlpha"]] + tp,
    priorSensitivityBeta  = options[["priorSensitivityBeta"]]  + fn,
    priorSpecificityAlpha = options[["priorSpecificityAlpha"]] + tn,
    priorSpecificityBeta  = options[["priorSpecificityBeta"]]  + fp,
    samples  = options[["samples"]]
  )

  class(results) <- "bcPosteriorParams"
  return(results)
}

coef.bcPosteriorParams <- function(results) {
  # .bcStatistics(
  #   prevalence  = results[["priorPrevalenceAlpha"]]  / (results[["priorPrevalenceAlpha"]]  + results[["priorPrevalenceBeta"]] ),
  #   sensitivity = results[["priorSensitivityAlpha"]] / (results[["priorSensitivityAlpha"]] + results[["priorSensitivityBeta"]]),
  #   specificity = results[["priorSpecificityAlpha"]] / (results[["priorSpecificityAlpha"]] + results[["priorSpecificityBeta"]])
  # )
}

.bcExtract <- function(summary, statistic, quantity = "estimate") {
  summary[match(statistic, summary[["variable"]]), quantity, drop = TRUE]
}

.bcDrawPosteriorSamples <- function(options, progress = FALSE) {

  jags_data <- with(options, list(
      z = 1, # ones trick
      tp = truePositive,
      pos = truePositive + falseNegative,
      tn = trueNegative,
      neg = trueNegative + falsePositive,
      total = truePositive + trueNegative + falsePositive + falseNegative,
      positiveTests = positiveTests,
      totalTests = positiveTests + negativeTests,
      updatePrevalence = as.integer(updatePrevalence),
      orderConstraint = as.integer(orderConstraint),
      priorPrevalenceAlpha = priorPrevalenceAlpha,
      priorPrevalenceBeta = priorPrevalenceBeta,
      priorSensitivityAlpha = priorSensitivityAlpha,
      priorSensitivityBeta = priorSensitivityBeta,
      priorSpecificityAlpha = priorSpecificityAlpha,
      priorSpecificityBeta = priorSpecificityBeta
  ))

    samples <- runjags::run.jags(
      model = .bcJagsModel,
      monitor = c("prevalence", "sensitivity", "specificity"),
      data = jags_data,
      summarise = FALSE,
      sample = options[["samples"]],
      burnin = options[["burnin"]],
      thin = options[["thinning"]],
      n.chains = options[["chains"]]
    )
    samples <- posterior::as_draws_df(samples[["mcmc"]])
    return(samples)
}

.bcJagsModel <- "
model{
  # priors
  prevalence  ~ dbeta(priorPrevalenceAlpha,  priorPrevalenceBeta )
  sensitivity ~ dbeta(priorSensitivityAlpha, priorSensitivityBeta)
  specificity ~ dbeta(priorSpecificityAlpha, priorSpecificityBeta)

  # likelihood

  ## update sensitivity and specificity
  tp ~ dbin(sensitivity, pos)
  tn ~ dbin(specificity, neg)

  ## update prevalence
  prevalence2 <- ifelse(updatePrevalence==1, prevalence, 0.5)
  pos ~ dbin(prevalence2, total)

  ## correction as per Winkler & Smith
  theta <- prevalence*sensitivity + (1-prevalence)*(1-specificity)
  positiveTests ~ dbin(theta, totalTests)

  # order constraints
  constraint <- ifelse(orderConstraint, step(sensitivity-(1-specificity)), 1)
  z ~ dbern(constraint)
}
"

# Output tables ----
.bcTables <- function(results, summary, jaspResults, dataset, options, ready) {

  if(is.null(jaspResults[["tables"]])) {
    tablesContainer <- createJaspContainer(title = gettext("Tables"))
    tablesContainer$dependOn(optionsFromObject = jaspResults[["results"]], options = c("ci", "ciLevel"))
    tablesContainer$position <- 2
    jaspResults[["tables"]] <- tablesContainer
  } else {
    tablesContainer <- jaspResults[["tables"]]
  }

  .bcTableStatistics    (results, summary, tablesContainer, dataset, options, ready)
  .bcTableConfusion     (results, summary, tablesContainer, dataset, options, ready)
  return()
  .bcTablePriorPosterior(results, tablesContainer, dataset, options, ready)
}

.bcTableStatistics <- function(results, summary, tablesContainer, dataset, options, ready) {
  if( isFALSE(options[["statistics"]])     ) return()
  if(!is.null(tablesContainer[["statistics"]]) ) return()

  table <- createJaspTable(title = gettext("Statistics"), position = 1)
  table$dependOn(options = c("statistics", "statisticsAdditional"))
  table$showSpecifiedColumnsOnly <- TRUE

  table$addColumnInfo(name = "statistic",  title = "")

  if(options[["inputType"]] == "pointEstimates") {
    table$addColumnInfo(name = "estimate", title = gettext("Estimate"), type = "number")
  } else {
    table$addColumnInfo(name = "estimate", title = gettext("Mean"),   type = "number")
    table$addColumnInfo(name = "median",   title = gettext("Median"), type = "number")
    table$addColumnInfo(name = "sd",       title = gettext("SD"),     type = "number")
    if(options[["ci"]]) {
      ciLevelPercent <- gettextf("%s%% Credible Interval", 100*options[["ciLevel"]])
      table$addColumnInfo(name = "lowerCI", title = gettext("Lower"), overtitle = ciLevelPercent, type = "number")
      table$addColumnInfo(name = "upperCI", title = gettext("Upper"), overtitle = ciLevelPercent, type = "number")
    }
    table$addColumnInfo(name = "rhat", title = gettext("Rhat"),              overtitle = gettext("Diagnostics"), type = "number")
    table$addColumnInfo(name = "ssEff", title = gettext("Eff. Sample Size"), overtitle = gettext("Diagnostics"), type = "number")

    table$addFootnote(message = .bcTexts("footnote", options = options)[["posteriorEstimate"]])
  }

  table$addColumnInfo(name = "notation", title = gettext("Notation"))
  table$addColumnInfo(name = "interpretation", title = gettext("Interpretation"))

  if(ready && !is.null(summary)) {
    data <- if(options[["statisticsAdditional"]]) summary else summary[1:14,,drop=FALSE]
    table$setData(data)
  }

  tablesContainer[["statistics"]] <- table
}

.bcTableConfusion <- function(results, summary, tablesContainer, dataset, options, ready) {
  if( isFALSE(options[["confusionMatrix"]])     ) return()
  if(!is.null(tablesContainer[["confusionMatrix"]]) ) return()

  table <- createJaspTable(title = gettext("Confusion Matrix"), position = 2)
  table$dependOn(options = c("confusionMatrix", "confusionMatrixType", "confusionMatrixAdditionalInfo"))
  table$showSpecifiedColumnsOnly <- TRUE
  table$transpose <- TRUE

  ciText <- gettextf("%s%% CI", 100*options[["ciLevel"]])

  table$addColumnInfo(name = "head", title = "")

  if(options[["confusionMatrixType"]] == "text") {
    table$addColumnInfo(name = "pos",  title = gettext("Positive test"))
  } else if(options[["confusionMatrixType"]] == "number") {
    table$addColumnInfo(name = "pos_value",  title = gettext("Positive test"), type = "number")
  } else {
    table$addColumnInfo(name = "pos",  title = gettext("Outcome"), overtitle = gettext("Positive test"))
    table$addColumnInfo(name = "pos_value", title = gettext("Estimate"), overtitle = gettext("Positive test"), type = "number")
  }

  if(options[["ci"]] && options[["confusionMatrixType"]] != "text") {
    table$addColumnInfo(name = "pos_lower", title = gettextf("Lower %s", ciText), type = "number")
    table$addColumnInfo(name = "pos_upper", title = gettextf("Upper %s", ciText), type = "number")
  }

  if(options[["confusionMatrixType"]] == "text") {
    table$addColumnInfo(name = "neg",  title = gettext("Negative test"))
  } else if(options[["confusionMatrixType"]] == "number") {
    table$addColumnInfo(name = "neg_value",  title = gettext("Negative test"), type = "number")
  } else {
    table$addColumnInfo(name = "neg",  title = gettext("Outcome"), overtitle = gettext("Negative test"))
    table$addColumnInfo(name = "neg_value", title = gettext("Estimate"), overtitle = gettext("Negative test"), type = "number")
  }

  if(options[["ci"]] && options[["confusionMatrixType"]] != "text") {
    table$addColumnInfo(name = "neg_lower", title = gettextf("Lower %s", ciText), type = "number")
    table$addColumnInfo(name = "neg_upper", title = gettextf("Upper %s", ciText), type = "number")
  }

  if(isTRUE(options[["confusionMatrixAdditionalInfo"]])) {
    if(options[["confusionMatrixType"]] == "text") {
      table$addColumnInfo(name = "add1", title = gettext("Positive/Total"))
    } else if(options[["confusionMatrixType"]] == "number") {
      table$addColumnInfo(name = "add1_value", title = gettext("Positive/Total"), type = "number")
    } else {
      table$addColumnInfo(name = "add1", title = gettext("Outcome"), overtitle = gettext("Positive/Total"))
      table$addColumnInfo(name = "add1_value", title = gettext("Estimate"), overtitle = gettext("Positive/Total"), type = "number")
    }

    if(options[["ci"]] && options[["confusionMatrixType"]] != "text") {
      table$addColumnInfo(name = "add1_lower", title = gettextf("Lower %s", ciText), type = "number")
      table$addColumnInfo(name = "add1_upper", title = gettextf("Upper %s", ciText), type = "number")
    }


    if(options[["confusionMatrixType"]] == "text") {
      table$addColumnInfo(name = "add2", title = gettext("Negative/Total"))
    } else if(options[["confusionMatrixType"]] == "number") {
      table$addColumnInfo(name = "add2_value", title = gettext("Negative/Total"), type = "number")
    } else {
      table$addColumnInfo(name = "add2", title = gettext("Outcome"), overtitle = gettext("Negative/Total"))
      table$addColumnInfo(name = "add2_value", title = gettext("Estimate"), overtitle = gettext("Negative/Total"), type = "number")
    }

    if(options[["ci"]] && options[["confusionMatrixType"]] != "text") {
      table$addColumnInfo(name = "add2_lower", title = gettextf("Lower %s", ciText), type = "number")
      table$addColumnInfo(name = "add2_upper", title = gettextf("Upper %s", ciText), type = "number")
    }

    if(options[["confusionMatrixType"]] == "text") {
      table$addColumnInfo(name = "tot", title = gettext("Total"))
    } else if(options[["confusionMatrixType"]] == "number") {
      table$addColumnInfo(name = "tot_value", title = gettext("Total"), type = "number")
    } else {
      table$addColumnInfo(name = "tot", title = gettext("Outcome"), overtitle = gettext("Total"))
      table$addColumnInfo(name = "tot_value", title = gettext("Estimate"), overtitle = gettext("Total"), type = "number")
    }

    if(options[["ci"]] && options[["confusionMatrixType"]] != "text") {
      table$addColumnInfo(name = "tot_lower", title = gettextf("Lower %s", ciText), type = "number")
      table$addColumnInfo(name = "tot_upper", title = gettextf("Upper %s", ciText), type = "number")
    }
  }

  if(ready) .bcFillTableConfusion(summary, table, options)

  # footnotes
  if(options[["inputType"]] != "pointEstimates" && options[["confusionMatrixType"]] != "text")
    table$addFootnote(message = .bcTexts("footnote", options = options)[["posteriorEstimate"]])

  tablesContainer[["confusionMatrix"]] <- table
}

.bcFillTableConfusion <- function(summary, table, options) {
  tab <- summary[, c("statistic", "estimate", "lowerCI", "upperCI")]
  rownames(tab) <- summary[["variable"]]

  df <- data.frame(
    head       = gettext(c("Positive condition", "Negative condition", "True/Total")),

    pos        = tab[c("truePositive", "falsePositive", "positivePredictiveValue"), "statistic", drop=TRUE],
    pos_value  = tab[c("truePositive", "falsePositive", "positivePredictiveValue"), "estimate",  drop=TRUE],
    pos_lower  = tab[c("truePositive", "falsePositive", "positivePredictiveValue"), "lowerCI",   drop=TRUE],
    pos_upper  = tab[c("truePositive", "falsePositive", "positivePredictiveValue"), "upperCI",   drop=TRUE],

    neg        = tab[c("falseNegative", "trueNegative", "negativePredictiveValue"), "statistic", drop=TRUE],
    neg_value  = tab[c("falseNegative", "trueNegative", "negativePredictiveValue"), "estimate",  drop=TRUE],
    neg_lower  = tab[c("falseNegative", "trueNegative", "negativePredictiveValue"), "lowerCI",   drop=TRUE],
    neg_upper  = tab[c("falseNegative", "trueNegative", "negativePredictiveValue"), "upperCI",   drop=TRUE],

    add1       = tab[c("sensitivity", "falsePositiveRate", "accuracy"), "statistic", drop=TRUE],
    add1_value = tab[c("sensitivity", "falsePositiveRate", "accuracy"), "estimate",  drop=TRUE],
    add1_lower = tab[c("sensitivity", "falsePositiveRate", "accuracy"), "lowerCI",   drop=TRUE],
    add1_upper = tab[c("sensitivity", "falsePositiveRate", "accuracy"), "upperCI",   drop=TRUE],

    add2       = c(tab[c("falseNegativeRate", "specificity"), "statistic", drop=TRUE], ""),
    add2_value = c(tab[c("falseNegativeRate", "specificity"), "estimate",  drop=TRUE], NA),
    add2_lower = c(tab[c("falseNegativeRate", "specificity"), "lowerCI",   drop=TRUE], NA),
    add2_upper = c(tab[c("falseNegativeRate", "specificity"), "upperCI",   drop=TRUE], NA),

    tot        = c(gettext(c("Prevalence", "Rareness")), ""),
    tot_value  = c(tab["prevalence", "estimate", drop=TRUE], 1-tab["prevalence", "estimate", drop=TRUE], NA),
    tot_lower  = c(tab["prevalence", "lowerCI",  drop=TRUE], 1-tab["prevalence", "lowerCI",  drop=TRUE], NA),
    tot_upper  = c(tab["prevalence", "upperCI",  drop=TRUE], 1-tab["prevalence", "upperCI",  drop=TRUE], NA)
  )

  if(isFALSE(options[["confusionMatrixAdditionalInfo"]]))
    df <- df[1:2,]

  table$setData(df)
}

.bcTablePriorPosterior <- function(results, tablesContainer, dataset, options, ready) {
  if( isFALSE(options[["priorPosterior"]])     ) return()
  if(!is.null(tablesContainer[["priorPosterior"]]) ) return()

  table <- createJaspTable(title = gettext("Priors and Posteriors"), position = 3)
  table$dependOn(options = "priorPosterior")

  table$addColumnInfo(name = "parameter", title = gettext("Parameter"), format = "string")
  table$addColumnInfo(name = "prior",     title = gettext("Prior"),     format = "string")
  table$addColumnInfo(name = "posterior", title = gettext("Posterior"), format = "string")

  tablesContainer[["priorPosterior"]] <- table

  if(!ready) return()

  post <- .bcGetPosterior(options, dataset)
  data <- data.frame(
    parameter = gettext(c("Prevalence", "Sensitivity", "Specificity")),
    prior     = gettextf("beta(%1$s,%2$s)",
                         options[c("priorPrevalenceAlpha", "priorSensitivityAlpha", "priorSpecificityAlpha")],
                         options[c("priorPrevalenceBeta",  "priorSensitivityBeta",  "priorSpecificityBeta")]),
    posterior = gettextf("beta(%1$s,%2$s)",
                         post[c("priorPrevalenceAlpha", "priorSensitivityAlpha", "priorSpecificityAlpha")],
                         post[c("priorPrevalenceBeta",  "priorSensitivityBeta",  "priorSpecificityBeta")])
  )

  table$setData(data)
}

# Output plots ----
.bcPlots <- function(results, summary, jaspResults, dataset, options, ready) {

  if(is.null(jaspResults[["plots"]])) {
    plotsContainer <- createJaspContainer(title = gettext("Plots"))
    plotsContainer$dependOn(optionsFromObject = jaspResults[["results"]])
    plotsContainer$position <- 3
    jaspResults[["plots"]] <- plotsContainer
  } else {
    plotsContainer <- jaspResults[["plots"]]
  }

  .bcPlotPriorPosteriorPositive(results, summary, plotsContainer, dataset, options, ready, position = 1)
  .bcPlotIconPlot              (results, summary, plotsContainer, dataset, options, ready, position = 2)
  .bcPlotROC                   (results, summary, plotsContainer, dataset, options, ready, position = 3, jaspResults)
  .bcPlotTOC                   (results, summary, plotsContainer, dataset, options, ready, position = 4, jaspResults)
  .bcPlotPR                    (results, summary, plotsContainer, dataset, options, ready, position = 5, jaspResults)
  .bcPlotTestCharacteristics   (results, summary, plotsContainer, dataset, options, ready, position = 6, jaspResults)
  .bcPlotVaryingPrevalence     (results, summary, plotsContainer, dataset, options, ready, position = 7)
  .bcPlotAlluvial              (results, summary, plotsContainer, dataset, options, ready, position = 8)
  .bcPlotSignal                (results, summary, plotsContainer, dataset, options, ready, position = 9)
  .bcPlotEstimates             (results, summary, plotsContainer, dataset, options, ready, position = 10)
}

## Prior posterior plot ----
.bcPlotPriorPosteriorPositive <- function(results, summary, plotsContainer, dataset, options, ready, position) {
  if( isFALSE(options[["probabilityPositivePlot"]])     ) return()
  if(!is.null(plotsContainer[["probabilityPositivePlot"]]) ) return()

  plotsContainer[["probabilityPositivePlot"]] <-
    createJaspPlot(title        = gettext("Probability Positive"),
                   dependencies = c("probabilityPositivePlot", "ci", "ciLevel", "probabilityPositivePlotEntireDistribution"),
                   position     = position,
                   width        = 500,
                   height       = 500,
    )

  if(ready) plotsContainer[["probabilityPositivePlot"]]$plotObject <-
    .bcFillPlotPriorPosteriorPositive(results, summary, dataset, options) +
      ggplot2::theme(legend.text = ggplot2::element_text(margin = ggplot2::margin(r = 0.2, unit = "npc")))
}

.bcFillPlotPriorPosteriorPositive <- function(results, summary, dataset, options) {
  UseMethod(".bcFillPlotPriorPosteriorPositive")
}

.bcFillPlotPriorPosteriorPositive.bcPointEstimates <- function(results, summary, dataset, options) {

  data <- expand.grid(test   = gettext(c("Not tested", "Tested")),
                      result = gettext(c("Negative", "Positive")))
  data$probPositive <- numeric(length = nrow(data))

  for(i in 1:nrow(data)) {
    if(data$test[i] == gettext("Not tested")) {
      data$probPositive[i] <- results[["prevalence"]]
    } else if(data$result[i] == gettext("Negative")) {
      data$probPositive[i] <- results[["falseOmissionRate"]]
    } else {
      data$probPositive[i] <- results[["positivePredictiveValue"]]
    }
  }

  plot <- ggplot2::ggplot(data    = data,
                          mapping = ggplot2::aes(x=result, y=probPositive, fill=test)
                          ) +
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(), col = "black") +
    ggplot2::xlab(gettext("Test Result")) +
    ggplot2::ylab(gettext("P(Condition = positive)")) +
    ggplot2::scale_fill_discrete(name = NULL) +
    ggplot2::scale_y_continuous(breaks = jaspGraphs::getPrettyAxisBreaks(c(0, data$probPositive)),
                                limits = range(jaspGraphs::getPrettyAxisBreaks(c(0, data$probPositive))))

  plot <- jaspGraphs::themeJasp(plot, legend.position = "bottom")

  return(plot)
}

.bcFillPlotPriorPosteriorPositive.bcUncertainEstimates <- function(results, summary, dataset, options) {
  if(options[["probabilityPositivePlotEntireDistribution"]]) {
    data <- expand.grid(test   = gettext(c("Not tested", "Tested")),
                        result = gettext(c("Negative", "Positive")),
                        iter   = seq_len(options[["samples"]]))
    data$value <- NA
    for(i in 1:nrow(data)) {
      if(data$test[i] == gettext("Not tested")) {
        data$value[i] <- results[["prevalence"]][data[i, "iter"]]
      } else if(data$result[i] == gettext("Negative")) {
        data$value[i] <- results[["falseOmissionRate"]][data[i, "iter"]]
      } else {
        data$value[i] <- results[["positivePredictiveValue"]][data[i, "iter"]]
      }
    }

    plot <- ggplot2::ggplot(data = data,
                            mapping = ggplot2::aes(x=result, y=value, fill=test)) +
      #ggplot2::geom_violin(position = ggplot2::position_dodge(width = 0.7), col = "black", width = 0.7) +
      ggdist::stat_eye(position = ggplot2::position_dodge(width = 0.7), width = 1) +
      ggplot2::xlab(gettext("Test Result")) +
      ggplot2::ylab(gettext("P(Condition = positive)")) +
      ggplot2::scale_fill_discrete(name = NULL)

  } else {
    data <- expand.grid(test   = gettext(c("Not tested", "Tested")),
                        result = gettext(c("Negative", "Positive")))
    data$mean <- data$lower <- data$upper <- numeric(length(nrow(data)))
    alpha <- 1-options[["ciLevel"]]

    for(i in 1:nrow(data)) {
      if(data$test[i] == gettext("Not tested")) {
        varName <- "prevalence"
      } else if(data$result[i] == gettext("Negative")) {
        varName <- "falseOmissionRate"
      } else {
        varName <- "positivePredictiveValue"
      }

      data$mean [i] <- .bcExtract(summary, varName)
      data$lower[i] <- .bcExtract(summary, varName, "lowerCI")
      data$upper[i] <- .bcExtract(summary, varName, "upperCI")
    }

    plot <- ggplot2::ggplot(data    = data,
                            mapping = ggplot2::aes(x=result, y=mean, fill=test, ymin=lower, ymax=upper)) +
      ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(width=0.7), col = "black", width=0.7) +
      ggplot2::xlab(gettext("Test Result")) +
      ggplot2::ylab(gettext("P(Condition = positive)")) +
      ggplot2::scale_fill_discrete(name = NULL)

    if(options[["ci"]]) {
      plot <- plot +
        ggplot2::geom_errorbar(position = ggplot2::position_dodge(width=0.7), size = 1, width = 0.5) +
        ggplot2::scale_y_continuous(breaks = jaspGraphs::getPrettyAxisBreaks(c(0, data$upper)),
                                    limits = range(jaspGraphs::getPrettyAxisBreaks(c(0, data$upper))))
    } else {
      plot <- plot +
        ggplot2::scale_y_continuous(breaks = jaspGraphs::getPrettyAxisBreaks(c(0, data$mean)),
                                    limits = range(jaspGraphs::getPrettyAxisBreaks(c(0, data$mean))))
    }
  }


  plot <- jaspGraphs::themeJasp(plot, legend.position = "bottom")

  return(plot)
}

## Icon plot ----
.bcPlotIconPlot <- function(results, summary, plotsContainer, dataset, options, ready, position) {
  if( isFALSE(options[["iconPlot"]])     ) return()
  if(!is.null(plotsContainer[["iconPlot"]]) ) return()

  plotsContainer[["iconPlot"]] <-
    createJaspPlot(title        = gettext("Icon Plot"),
                   dependencies = c("iconPlot", .bcGetColors(options)[["names"]]),
                   position     = position,
                   width        = 500,
                   height       = 500
    )

  if(ready) plotsContainer[["iconPlot"]]$plotObject <-
    .bcFillPlotIconPlot(results, summary, dataset, options) +
      ggplot2::theme(legend.text = ggplot2::element_text(margin = ggplot2::margin(r = 0.2, unit = "npc")))

}

.bcFillPlotIconPlot <- function(results, summary, dataset, options) {
  UseMethod(".bcFillPlotIconPlot")
}

.bcFillPlotIconPlot.default <- function(results, summary, dataset, options) {

  data <- expand.grid(cond = gettext(c("Positive", "Negative")),
                      test = gettext(c("Positive", "Negative")),
                      KEEP.OUT.ATTRS = FALSE)
  data$out <- factor(
    gettext(c("True positive", "False positive", "False negative", "True negative")),
    levels = gettext(c("True positive", "False positive", "False negative", "True negative"))
  )
  #data$prop <- summary[match(c("truePositive", "falsePositive", "falseNegative", "trueNegative"), summary$variable), "estimate", drop=TRUE]
  data$prop <- .bcExtract(summary, c("truePositive", "falsePositive", "falseNegative", "trueNegative"))
  if(any(data$prop < 1e-4)) {
    npoints <- 1e6
    xside <- yside <- 1e3
  } else if(any(data$prop < 1e-2)) {
    npoints <- 1e4
    xside <- yside <- 1e2
  } else {
    npoints <- 1e2
    xside <- yside <- 10
  }

  data$n <- round(npoints*data$prop, digits = 0)
  data <- data.frame(
    outcome = rep(data$out, data$n),
    x = rep(c(1:xside,xside:1), times = yside/2),
    y = rep(seq_len(yside), each = xside),
    img = system.file("icons", "person.png", package = "jaspLearnBayes")
  )

  plot <- ggplot2::ggplot(data=data, mapping = ggplot2::aes(x=x,y=y))

  if(npoints <= 1e2) {
    plot <- plot +
      ggplot2::geom_raster(mapping = ggplot2::aes(fill=outcome), alpha = 0) +
      geom_png(mapping = ggplot2::aes(w=0.75,h=0.75,img=img,col=outcome)) +
      ggplot2::scale_fill_manual (name = "", values = .bcGetColors(options)[["values"]], labels = .bcGetColors(options)[["labels"]]) +
      ggplot2::scale_color_manual(name = "", values = .bcGetColors(options)[["values"]], labels = .bcGetColors(options)[["labels"]]) +
      ggplot2::guides(fill=ggplot2::guide_legend(ncol=2, override.aes = list(alpha = 1)), col = FALSE)
  } else {
    plot <- plot +
      ggplot2::geom_tile(mapping = ggplot2::aes(fill=outcome)) +
      ggplot2::scale_fill_manual (name = "", values = .bcGetColors(options)[["values"]], labels = .bcGetColors(options)[["labels"]]) +
      ggplot2::guides(fill=ggplot2::guide_legend(ncol=2))
  }

  plot <- plot +
    ggplot2::coord_fixed(ratio = xside/yside) +
    ggplot2::ylab(NULL) + ggplot2::xlab(NULL) +
    ggplot2::scale_x_discrete(labels = NULL, breaks = NULL) +
    ggplot2::scale_y_discrete(labels = NULL, breaks = NULL)

  plot <- jaspGraphs::themeJasp(plot, legend.position = "bottom")

  return(plot)
}

## ROC curve plot ----
.bcPlotROC <- function(results, summary, plotsContainer, dataset, options, ready, position, jaspResults) {
  if( isFALSE(options[["rocPlot"]])     ) return()
  if(!is.null(plotsContainer[["rocPlot"]]) ) return()

  plotsContainer[["rocPlot"]] <-
    createJaspPlot(
      title        = gettext("Receiving Operating Characteristic Curve"),
      dependencies = c("rocPlot", "ci", "ciLevel", "rocPlotPosteriorRealizations", "rocPlotPosteriorRealizationsNumber"),
      position     = position,
      width        = 500,
      height       = 500,
      )

  if(ready) plotsContainer[["rocPlot"]]$plotObject <-
    .bcFillPlotROC(results, summary, dataset, options, jaspResults=jaspResults) +
      ggplot2::xlim(c(0,1)) + ggplot2::ylim(c(0,1))
}

.bcFillPlotROC <- function(results, summary, dataset, options, ...) {
  UseMethod(".bcFillPlotROC")
}

.bcFillPlotROC.default <- function(results, summary, dataset, options, ...) {

  threshold    <- qnorm(.bcExtract(summary, "specificity"))
  meanPositive <- qnorm(.bcExtract(summary, "sensitivity"), mean = threshold)

  varyingThreshold <- seq(qnorm(0.01), qnorm(0.99, meanPositive), length.out = 101)
  data <- data.frame(
    fpr = c(pnorm(varyingThreshold,                      lower.tail = FALSE), 0, 1),
    tpr = c(pnorm(varyingThreshold, mean = meanPositive, lower.tail = FALSE), 0, 1)
  )

  pointData <- data.frame(fpr = .bcExtract(summary, "falsePositiveRate"),
                          tpr = .bcExtract(summary, "sensitivity"))

  plot <- ggplot2::ggplot(data    = data,
                          mapping = ggplot2::aes(x    = fpr,
                                                 y    = tpr)
                          )

  if(options[["ci"]]) {
    densityData <- data.frame(
      fpr = results[["falsePositiveRate"]],
      tpr = results[["sensitivity"]]
    )
    breakLevel <- .bcGetLevel2D(densityData$fpr, densityData$tpr, options[["ciLevel"]])

    plot <- plot +
      ggplot2::stat_density2d(data = densityData, geom = "polygon",
                              mapping = ggplot2::aes(fill = ggplot2::after_stat(level)),
                              bins = 5) +
      ggplot2::scale_fill_distiller(palette = "Blues", direction = 1) +
      ggplot2::geom_density2d(data = densityData, breaks = breakLevel,
                              size = 1, col = "black", linetype = 2)
  }

  if(options[["inputType"]] == "uncertainEstimates" && options[["rocPlotPosteriorRealizations"]]) {
    for(i in seq_len(options[["rocPlotPosteriorRealizationsNumber"]])) {
      threshold    <- qnorm(results[["specificity"]][i])
      meanPositive <- qnorm(results[["sensitivity"]][i], mean = threshold)

      varyingThreshold <- seq(qnorm(0.01), qnorm(0.99, meanPositive), length.out = 101)
      iterData <- data.frame(
        fpr = c(pnorm(varyingThreshold,                      lower.tail = FALSE), 0, 1),
        tpr = c(pnorm(varyingThreshold, mean = meanPositive, lower.tail = FALSE), 0, 1)
      )

      plot <- plot +
        ggplot2::geom_line(data = iterData, size = 1, alpha = 0.05)
    }
  }


  plot <- plot +
    jaspGraphs::geom_abline2(slope = 1, intercept = 0, linetype = 2) +
    ggplot2::geom_line(size = 1.5) +
    jaspGraphs::geom_point(data = pointData, size = 5) +
    ggplot2::xlab(gettext("False Positive Rate (1-Specificity)")) +
    ggplot2::ylab(gettext("True Positive Rate (Sensitivity)"))


  plot <- jaspGraphs::themeJasp(plot)

  return(plot)
}

.bcFillPlotROC.bcData <- function(results, summary, dataset, options, jaspResults) {
  thresholds <- .bcComputeSummaryByThreshold(jaspResults = jaspResults, options = options, dataset = dataset)

  data <- data.frame(
    fpr = c(subset(thresholds, variable == "falsePositiveRate")[["estimate"]], 0, 1),
    tpr = c(subset(thresholds, variable == "sensitivity")[["estimate"]], 0, 1)
  )

  pointData <- data.frame(fpr = .bcExtract(summary, "falsePositiveRate"),
                          tpr = .bcExtract(summary, "sensitivity"))

  plot <- ggplot2::ggplot(data    = data,
                          mapping = ggplot2::aes(x = fpr, y = tpr)
                          )

  if(options[["ci"]]) {
    densityData <- data.frame(
      fpr = results[["falsePositiveRate"]],
      tpr = results[["sensitivity"]]
    )
    breakLevel <- .bcGetLevel2D(densityData$fpr, densityData$tpr, options[["ciLevel"]])

    plot <- plot +
      ggplot2::stat_density2d(data = densityData, geom = "polygon",
                              mapping = ggplot2::aes(fill = ggplot2::after_stat(level)),
                              bins = 5) +
      ggplot2::scale_fill_distiller(palette = "Blues", direction = 1) +
      ggplot2::geom_density2d(data = densityData, breaks = breakLevel,
                              size = 1, col = "black", linetype = 2)

  }

  if(options[["rocPlotPosteriorRealizations"]]) {
    for(i in seq_len(options[["rocPlotPosteriorRealizationsNumber"]])) {
      threshold    <- qnorm(results[["specificity"]][i])
      meanPositive <- qnorm(results[["sensitivity"]][i], mean = threshold)

      varyingThreshold <- seq(qnorm(0.01), qnorm(0.99, meanPositive), length.out = 101)
      iterData <- data.frame(
        fpr = c(pnorm(varyingThreshold,                      lower.tail = FALSE), 0, 1),
        tpr = c(pnorm(varyingThreshold, mean = meanPositive, lower.tail = FALSE), 0, 1)
      )

      plot <- plot +
        ggplot2::geom_line(data = iterData, size = 1, alpha = 0.05)
    }
  }

  plot <- plot +
    jaspGraphs::geom_abline2(slope = 1, intercept = 0, linetype = 2) +
    ggplot2::geom_step(size = 1.5) +
    jaspGraphs::geom_point(data = pointData, size = 5) +
    ggplot2::xlab(gettext("False Positive Rate (1-Specificity)")) +
    ggplot2::ylab(gettext("True Positive Rate (Sensitivity)"))


  plot <- jaspGraphs::themeJasp(plot)

  return(plot)
}

## TOC curve plot ----
.bcPlotTOC <- function(results, summary, plotsContainer, dataset, options, ready, position, jaspResults) {
  if( isFALSE(options[["tocPlot"]])     ) return()
  if(!is.null(plotsContainer[["tocPlot"]]) ) return()

  plotsContainer[["tocPlot"]] <-
    createJaspPlot(
      title        = gettext("Total Operating Characteristic Curve"),
      dependencies = "tocPlot",
      position     = position,
      width        = 500,
      height       = 500,
    )

  if(ready) plotsContainer[["tocPlot"]]$plotObject <-
    .bcFillPlotTOC(results, summary, dataset, options, jaspResults=jaspResults)
}

.bcFillPlotTOC <- function(results, summary, dataset, options, ...) {
  UseMethod(".bcFillPlotTOC")
}

.bcFillPlotTOC.default <- function(results, summary, dataset, options, ...) {
  threshold    <- qnorm(.bcExtract(summary, "specificity"))
  meanPositive <- qnorm(.bcExtract(summary, "sensitivity"), mean = threshold)
  prevalence <- .bcExtract(summary, "prevalence")

  varyingThreshold <- seq(qnorm(0.01), qnorm(0.99, meanPositive), length.out = 101)
  fpr <- c(pnorm(varyingThreshold,                      lower.tail = FALSE), 0, 1)
  tpr <- c(pnorm(varyingThreshold, mean = meanPositive, lower.tail = FALSE), 0, 1)

  curveData <- data.frame(
    tp = prevalence * tpr,
    fp = (1-prevalence) * fpr
  )

  pointData <- data.frame(
    tp = .bcExtract(summary, "truePositive"),
    fp = .bcExtract(summary, "falsePositive")
  )

  boxData <- data.frame(x = c(0, prevalence, 1, 1-prevalence),
                        y = c(0, prevalence, prevalence, 0))

  diagLineData <- data.frame(x = c(0, 1), y = c(0, prevalence))

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, 1))
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, .bcExtract(summary, "prevalence")))
  xLimits <- range(xBreaks)
  yLimits <- range(yBreaks)

  plot <- ggplot2::ggplot() +
    ggplot2::geom_polygon(data = boxData, mapping = ggplot2::aes(x = x, y = y), alpha = 0.0, color = "black", linetype = 2, linewidth = 1) +
    ggplot2::geom_line(data = diagLineData, mapping = ggplot2::aes(x = x, y = y), linetype = 2) +
    ggplot2::geom_line(data = curveData, mapping = ggplot2::aes(x = tp+fp, y = tp), size = 1.5) +
    jaspGraphs::geom_point(data = pointData, mapping = ggplot2::aes(x = tp+fp, y = tp), size = 5) +
    ggplot2::xlab(gettext("True positives + False positives")) +
    ggplot2::ylab(gettext("True positives")) +
    ggplot2::scale_x_continuous(breaks = xBreaks, limits = xLimits) +
    ggplot2::scale_y_continuous(breaks = yBreaks, limits = yLimits)


  plot <- jaspGraphs::themeJasp(plot)

  return(plot)
}

.bcFillPlotTOC.bcData <- function(results, summary, dataset, options, jaspResults) {
  thresholds <- .bcComputeSummaryByThreshold(jaspResults = jaspResults, options = options, dataset = dataset)

  prevalence <- .bcExtract(summary, "prevalence")
  curveData <- data.frame(
    tp = c(subset(thresholds, variable == "truePositive")[["estimate"]], 0, prevalence),
    fp = c(subset(thresholds, variable == "falsePositive")[["estimate"]], 0, 1-prevalence)
  )
  pointData <- data.frame(
    tp = .bcExtract(summary, "truePositive"),
    fp = .bcExtract(summary, "falsePositive")
  )

  boxData <- data.frame(x = c(0, prevalence, 1, 1-prevalence),
                        y = c(0, prevalence, prevalence, 0))

  diagLineData <- data.frame(x = c(0, 1), y = c(0, prevalence))

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, 1))
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, .bcExtract(summary, "prevalence")))
  xLimits <- range(xBreaks)
  yLimits <- range(yBreaks)

  plot <- ggplot2::ggplot() +
    ggplot2::geom_polygon(data = boxData, mapping = ggplot2::aes(x = x, y = y), alpha = 0.0, color = "black", linetype = 2, linewidth = 1) +
    ggplot2::geom_line(data = diagLineData, mapping = ggplot2::aes(x = x, y = y), linetype = 2) +
    ggplot2::geom_line(data = curveData, mapping = ggplot2::aes(x = tp+fp, y = tp), size = 1.5) +
    jaspGraphs::geom_point(data = pointData, mapping = ggplot2::aes(x = tp+fp, y = tp), size = 5) +
    ggplot2::xlab(gettext("True positives + False positives")) +
    ggplot2::ylab(gettext("True positives")) +
    ggplot2::scale_x_continuous(breaks = xBreaks, limits = xLimits) +
    ggplot2::scale_y_continuous(breaks = yBreaks, limits = yLimits)


  plot <- jaspGraphs::themeJasp(plot)

  return(plot)
}

## PR curve plot ----
.bcPlotPR <- function(results, summary, plotsContainer, dataset, options, ready, position, jaspResults) {
  if( isFALSE(options[["prPlot"]])     ) return()
  if(!is.null(plotsContainer[["prPlot"]]) ) return()

  plotsContainer[["prPlot"]] <-
    createJaspPlot(
      title        = gettext("Precision-recall"),
      dependencies = c("prPlot", "ci", "ciLevel", "prPlotPosteriorRealizations", "prPlotPosteriorRealizationsNumber"),
      position     = position,
      width        = 500,
      height       = 500,
    )

  if(ready) plotsContainer[["prPlot"]]$plotObject <-
    .bcFillPlotPR(results, summary, dataset, options, jaspResults=jaspResults) +
    ggplot2::xlim(c(0,1)) + ggplot2::ylim(c(0,1))
}

.bcFillPlotPR <- function(results, summary, dataset, options, ...) {
  UseMethod(".bcFillPlotPR")
}

.bcFillPlotPR.default <- function(results, summary, dataset, options, ...) {

  prevalence  <- .bcExtract(summary, "prevalence")
  sensitivity <- .bcExtract(summary, "sensitivity")
  specificity <- .bcExtract(summary, "specificity")


  threshold    <- qnorm(specificity)
  meanPositive <- qnorm(sensitivity, mean = threshold)
  varyingThreshold <- c(threshold, seq(qnorm(0.01), qnorm(0.99, meanPositive), length.out = 101))

  curveData <- data.frame(
    prevalence = prevalence,
    sensitivity = c(pnorm(varyingThreshold, mean = meanPositive, lower.tail = FALSE), 0, 1),
    specificity = c(pnorm(varyingThreshold,                      lower.tail = TRUE ), 1, 0)
  )
  curveData <- .bcStatistics(curveData)

  pointData <- data.frame(sensitivity = sensitivity,
                          positivePredictiveValue = curveData[["positivePredictiveValue"]][1])#.bcExtract(summary, "positivePredictiveValue"))

  plot <- ggplot2::ggplot(mapping = ggplot2::aes(x    = sensitivity,
                                                 y    = positivePredictiveValue))

  if(options[["ci"]]) {
    densityData <- data.frame(
      sensitivity = results[["sensitivity"]],
      positivePredictiveValue = results[["positivePredictiveValue"]]
    )
    breakLevel <- .bcGetLevel2D(densityData$sensitivity, densityData$positivePredictiveValue, options[["ciLevel"]])

    plot <- plot +
      ggplot2::stat_density2d(data = densityData, geom = "polygon",
                              mapping = ggplot2::aes(fill = ggplot2::after_stat(level)),
                              bins = 5) +
      ggplot2::scale_fill_distiller(palette = "Blues", direction = 1) +
      ggplot2::geom_density2d(data = densityData, breaks = breakLevel,
                              size = 1, col = "black", linetype = 2)

    prevalenceCiData <- data.frame(x = c(0, 1), lower = .bcExtract(summary, "prevalence", "lowerCI"), upper = .bcExtract(summary, "prevalence", "upperCI"))
    plot <- plot +
      ggplot2::geom_ribbon(
        data = prevalenceCiData,
        mapping = ggplot2::aes(x=x, ymin = lower, ymax = upper),
        inherit.aes = FALSE,
        fill = "firebrick", alpha = 0.3)
  }

  if(options[["inputType"]] == "uncertainEstimates" && options[["prPlotPosteriorRealizations"]]) {
    for(i in seq_len(options[["prPlotPosteriorRealizationsNumber"]])) {
      threshold    <- qnorm(results[["specificity"]][i])
      meanPositive <- qnorm(results[["sensitivity"]][i], mean = threshold)

      varyingThreshold <- seq(qnorm(0.01), qnorm(0.99, meanPositive), length.out = 101)
      iterData <- data.frame(
        prevalence = results[["prevalence"]][i],
        sensitivity = c(pnorm(varyingThreshold, mean = meanPositive, lower.tail = FALSE), 0, 1),
        specificity = c(pnorm(varyingThreshold,                      lower.tail = TRUE ), 1, 0)
      )
      iterData <- .bcStatistics(iterData)

      plot <- plot +
        ggplot2::geom_line(data = iterData, size = 1, alpha = 0.05)
    }
  }


  plot <- plot +
    jaspGraphs::geom_abline2(slope = 0, intercept = prevalence, linetype = 2) +
    ggplot2::geom_line(data = curveData, size = 1.5) +
    jaspGraphs::geom_point(data = pointData, size = 5) +
    ggplot2::xlab(gettext("Sensitivity (recall)")) +
    ggplot2::ylab(gettext("Positive Predictive Value"))


  plot <- jaspGraphs::themeJasp(plot)

  return(plot)
}

.bcFillPlotPR.bcData <- function(results, summary, dataset, options, jaspResults) {
  prevalence <- .bcExtract(summary, "prevalence")
  thresholds <- .bcComputeSummaryByThreshold(jaspResults = jaspResults, options = options, dataset = dataset)

  curveData <- data.frame(
    sensitivity = c(subset(thresholds, variable == "sensitivity")[["estimate"]], 0, 1),
    positivePredictiveValue = c(subset(thresholds, variable == "positivePredictiveValue")[["estimate"]], 1, prevalence)
  )

  pointData <- data.frame(sensitivity = .bcExtract(summary, "sensitivity"),
                          positivePredictiveValue = .bcExtract(summary, "positivePredictiveValue"))

  plot <- ggplot2::ggplot(mapping = ggplot2::aes(x = sensitivity, y = positivePredictiveValue))

  if(options[["ci"]]) {
    densityData <- data.frame(
      sensitivity = results[["sensitivity"]],
      positivePredictiveValue = results[["positivePredictiveValue"]]
    )
    breakLevel <- .bcGetLevel2D(densityData$sensitivity, densityData$positivePredictiveValue, options[["ciLevel"]])

    plot <- plot +
      ggplot2::stat_density2d(data = densityData, geom = "polygon",
                              mapping = ggplot2::aes(fill = ggplot2::after_stat(level)),
                              bins = 5) +
      ggplot2::scale_fill_distiller(palette = "Blues", direction = 1) +
      ggplot2::geom_density2d(data = densityData, breaks = breakLevel,
                              size = 1, col = "black", linetype = 2)

    prevalenceCiData <- data.frame(x = c(0, 1), lower = .bcExtract(summary, "prevalence", "lowerCI"), upper = .bcExtract(summary, "prevalence", "upperCI"))
    plot <- plot +
      ggplot2::geom_ribbon(
        data = prevalenceCiData,
        mapping = ggplot2::aes(x=x, ymin = lower, ymax = upper),
        inherit.aes = FALSE,
        fill = "firebrick", alpha = 0.3)
  }

  if(options[["inputType"]] != "pointEstimates" && options[["prPlotPosteriorRealizations"]]) {
    for(i in seq_len(options[["prPlotPosteriorRealizationsNumber"]])) {
      threshold    <- qnorm(results[["specificity"]][i])
      meanPositive <- qnorm(results[["sensitivity"]][i], mean = threshold)
      prev <- results[["prevalence"]][i]
      varyingThreshold <- seq(qnorm(0.01), qnorm(0.99, meanPositive), length.out = 101)
      iterData <- data.frame(
        prevalence = prev,
        sensitivity = c(pnorm(varyingThreshold, mean = meanPositive, lower.tail = FALSE), 0, 1),
        specificity = c(pnorm(varyingThreshold,                      lower.tail = TRUE ), 1, 0)
      )
      iterData <- .bcStatistics(iterData)

      plot <- plot +
        ggplot2::geom_line(data = iterData, size = 1, alpha = 0.05)
    }
  }


  plot <- plot +
    jaspGraphs::geom_abline2(slope = 0, intercept = prevalence, linetype = 2) +
    ggplot2::geom_line(data = curveData, size = 1.5) +
    jaspGraphs::geom_point(data = pointData, size = 5) +
    ggplot2::xlab(gettext("Sensitivity (recall)")) +
    ggplot2::ylab(gettext("Positive Predictive Value"))


  plot <- jaspGraphs::themeJasp(plot)

}

## Test characteristics ----
.bcPlotTestCharacteristics <- function(results, summary, plotsContainer, dataset, options, ready, position, jaspResults) {
  if( isFALSE(options[["testCharacteristicsPlot"]])     ) return()
  if(!is.null(plotsContainer[["testCharacteristicsPlot"]]) ) return()

  plotsContainer[["testCharacteristicsPlot"]] <-
    createJaspPlot(title        = gettext("Test Characteristics"),
                   dependencies = c("testCharacteristicsPlot", "ci", "ciLevel"),
                   position     = position,
                   width        = 500,
                   height       = 500,
    )


  if(ready) plotsContainer[["testCharacteristicsPlot"]]$plotObject <-
    .bcFillPlotTestCharacteristics(results, summary, dataset, options, jaspResults=jaspResults) +
      ggplot2::theme(legend.text = ggplot2::element_text(margin = ggplot2::margin(r = 0.2, unit = "npc")))
}


.bcFillPlotTestCharacteristics <- function(results, summary, dataset, options, ...) {
  UseMethod(".bcFillPlotTestCharacteristics")
}

.bcFillPlotTestCharacteristics.bcPointEstimates <- function(results, summary, dataset, options, ...) {
  threshold    <- qnorm(.bcExtract(summary, "specificity"))
  meanPositive <- qnorm(.bcExtract(summary, "sensitivity"), mean = threshold)

  varyingThreshold <- seq(qnorm(0.01), qnorm(0.99, meanPositive), length.out = 101)
  data <- data.frame(
    threshold = varyingThreshold,
    tpr = pnorm(varyingThreshold, mean = meanPositive, lower.tail = FALSE),
    tnr = pnorm(varyingThreshold, lower.tail = TRUE)
  )
  pointData <- data.frame(
    x = threshold,
    y = .bcExtract(summary, c("sensitivity", "specificity"))
  )
  segmentData <- data.frame(x = threshold, xend = threshold, y = 0, yend = 1)

  plot <- ggplot2::ggplot(data = data, mapping = ggplot2::aes(x=threshold)) +
    ggplot2::geom_segment(data = segmentData, ggplot2::aes(x=x, y=y, xend=xend, yend=yend), linetype = 2, size = 1) +
    ggplot2::geom_line(mapping = ggplot2::aes(y=tpr, color = gettext("Sensitivity")), size = 2) +
    ggplot2::geom_line(mapping = ggplot2::aes(y=tnr, color = gettext("Specificity")), size = 2) +
    jaspGraphs::geom_point(data = pointData, mapping = ggplot2::aes(x=x,y=y), size = 5) +
    ggplot2::xlab(gettext("Test Threshold")) +
    ggplot2::ylab(NULL) +
    ggplot2::ylim(c(0, 1)) +
    ggplot2::scale_x_continuous(breaks = jaspGraphs::getPrettyAxisBreaks(varyingThreshold),
                                limits = range(jaspGraphs::getPrettyAxisBreaks(varyingThreshold))) +
    ggplot2::scale_color_manual(
      name   = gettext("Characteristic"),
      values = c("steelblue", "firebrick")
    )

  plot <- jaspGraphs::themeJasp(plot, legend.position = "bottom")

  return(plot)
}

.bcFillPlotTestCharacteristics.bcUncertainEstimates <- function(results, summary, dataset, options, ...) {
  alpha <- 1-options[["ciLevel"]]

  nPoints <- 101
  data <- data.frame(
    threshold = numeric(nPoints),
    tpr       = numeric(nPoints),
    tprLower  = numeric(nPoints),
    tprUpper  = numeric(nPoints),
    tnr       = numeric(nPoints),
    tnrLower  = numeric(nPoints),
    tnrUpper  = numeric(nPoints)
  )

  thresholdPosterior    <- qnorm(results[["specificity"]])
  meanPositivePosterior <- qnorm(results[["sensitivity"]], mean = thresholdPosterior)

  varyingThreshold <- matrix(data = NA, nrow = nPoints, ncol = options[["samples"]])
  for(i in seq_len(options[["samples"]]))
    varyingThreshold[,i] <- seq(qnorm(0.01), qnorm(0.99, meanPositivePosterior[i]), length.out = nPoints)

  for(i in seq_len(nPoints)) {
    tpr <- pnorm(varyingThreshold[i,], mean = meanPositivePosterior, lower.tail = FALSE)
    tnr <- pnorm(varyingThreshold[i,], mean = 0,                     lower.tail = TRUE)

    data[i, "threshold"] <- mean(varyingThreshold[i,], na.rm=TRUE)
    data[i, "tpr"]       <- mean(tpr, na.rm=TRUE)
    data[i, "tprLower"]  <- quantile(tpr, p =   alpha/2, na.rm=TRUE)
    data[i, "tprUpper"]  <- quantile(tpr, p = 1-alpha/2, na.rm=TRUE)
    data[i, "tnr"]       <- mean(tnr, na.rm=TRUE)
    data[i, "tnrLower"]  <- quantile(tnr, p =   alpha/2, na.rm=TRUE)
    data[i, "tnrUpper"]  <- quantile(tnr, p = 1-alpha/2, na.rm=TRUE)
  }


  threshold <- mean(qnorm(results[["specificity"]]), na.rm=TRUE)
  pointData <- data.frame(
    x = threshold,
    y = .bcExtract(summary, c("sensitivity", "specificity"))
  )

  plot <- ggplot2::ggplot(data = data, mapping = ggplot2::aes(x=threshold)) +
    ggplot2::geom_vline(xintercept = threshold, linetype = 2, size = 1)

  if(options[["ci"]]) {
    plot <- plot +
      ggplot2::geom_ribbon(mapping = ggplot2::aes(ymin=tprLower,ymax=tprUpper, fill = gettext("Sensitivity")), alpha = 0.5) +
      ggplot2::geom_ribbon(mapping = ggplot2::aes(ymin=tnrLower,ymax=tnrUpper, fill = gettext("Specificity")), alpha = 0.5)
  }

  plot <- plot +
    ggplot2::geom_line(mapping = ggplot2::aes(y=tpr, color = gettext("Sensitivity")), size = 2, alpha = 0.8) +
    ggplot2::geom_line(mapping = ggplot2::aes(y=tnr, color = gettext("Specificity")), size = 2, alpha = 0.8) +
    jaspGraphs::geom_point(data = pointData, mapping = ggplot2::aes(x=x,y=y), size = 5) +
    ggplot2::xlab(gettext("Test Threshold")) +
    ggplot2::ylab(NULL) +
    ggplot2::ylim(c(0, 1)) +
    ggplot2::scale_x_continuous(breaks = jaspGraphs::getPrettyAxisBreaks(data$threshold),
                                limits = range(jaspGraphs::getPrettyAxisBreaks(data$threshold))) +
    ggplot2::scale_color_manual(
      name   = gettext("Characteristic"),
      values = c("steelblue", "firebrick")
    ) +
    ggplot2::scale_fill_manual(
      name   = gettext("Characteristic"),
      values = c("steelblue", "firebrick")
    )

  plot <- jaspGraphs::themeJasp(plot, legend.position = "bottom")

  return(plot)
}

.bcFillPlotTestCharacteristics.bcData <- function(results, summary, dataset, options, jaspResults) {
  thresholds <- .bcComputeSummaryByThreshold(jaspResults = jaspResults, options = options, dataset = dataset)
  data <- data.frame(
    threshold = subset(thresholds, variable == "sensitivity")[["threshold"]],
    tpr       = subset(thresholds, variable == "sensitivity")[["estimate"]],
    tprLower  = subset(thresholds, variable == "sensitivity")[["lowerCI"]],
    tprUpper  = subset(thresholds, variable == "sensitivity")[["upperCI"]],
    tnr       = subset(thresholds, variable == "specificity")[["estimate"]],
    tnrLower  = subset(thresholds, variable == "specificity")[["lowerCI"]],
    tnrUpper  = subset(thresholds, variable == "specificity")[["upperCI"]]
  )

  pointData <- data.frame(
    x = options[["threshold"]],
    y = .bcExtract(summary, c("sensitivity", "specificity"))
  )

  plot <- ggplot2::ggplot(data = data, mapping = ggplot2::aes(x=threshold)) +
    ggplot2::geom_vline(xintercept = options[["threshold"]], linetype = 2, size = 1)

  if(options[["ci"]]) {
    plot <- plot +
      ggplot2::geom_ribbon(mapping = ggplot2::aes(ymin=tprLower,ymax=tprUpper, fill = gettext("Sensitivity")), alpha = 0.5) +
      ggplot2::geom_ribbon(mapping = ggplot2::aes(ymin=tnrLower,ymax=tnrUpper, fill = gettext("Specificity")), alpha = 0.5)
  }

  plot <- plot +
    ggplot2::geom_line(mapping = ggplot2::aes(y=tpr, color = gettext("Sensitivity")), size = 2, alpha = 0.8) +
    ggplot2::geom_line(mapping = ggplot2::aes(y=tnr, color = gettext("Specificity")), size = 2, alpha = 0.8) +
    jaspGraphs::geom_point(data = pointData, mapping = ggplot2::aes(x=x,y=y), size = 5) +
    ggplot2::xlab(gettext("Test Threshold")) +
    ggplot2::ylab(NULL) +
    ggplot2::ylim(c(0, 1)) +
    ggplot2::scale_x_continuous(breaks = jaspGraphs::getPrettyAxisBreaks(data$threshold),
                                limits = range(jaspGraphs::getPrettyAxisBreaks(data$threshold))) +
    ggplot2::scale_color_manual(
      name   = gettext("Characteristic"),
      values = c("steelblue", "firebrick")
    ) +
    ggplot2::scale_fill_manual(
      name   = gettext("Characteristic"),
      values = c("steelblue", "firebrick")
    )

  plot <- jaspGraphs::themeJasp(plot, legend.position = "bottom")

  return(plot)
}

## Varying prevalence plot ----
.bcPlotVaryingPrevalence <- function(results, summary, plotsContainer, dataset, options, ready, position) {
  if( isFALSE(options[["predictiveValuesByPrevalence"]])     ) return()
  if(!is.null(plotsContainer[["predictiveValuesByPrevalence"]]) ) return()

  plotsContainer[["predictiveValuesByPrevalence"]] <-
    createJaspPlot(title        = gettext("PPV and NPV by Prevalence"),
                   dependencies = c("predictiveValuesByPrevalence", "ci", "ciLevel"),
                   position     = position,
                   width        = 500,
                   height       = 500,
    )

  if(ready) plotsContainer[["predictiveValuesByPrevalence"]]$plotObject <-
    .bcFillPlotVaryingPrevalence(results, summary, dataset, options) +
      ggplot2::theme(legend.text = ggplot2::element_text(margin = ggplot2::margin(r = 0.2, unit = "npc")))
}

.bcFillPlotVaryingPrevalence <- function(results, summary, dataset, options) {
  UseMethod(".bcFillPlotVaryingPrevalence")
}

.bcFillPlotVaryingPrevalence.bcPointEstimates <- function(results, summary, dataset, options) {
  data <- .bcStatistics(
    data.frame(
      prevalence = seq(0, 1, by = 0.01),
      sensitivity = results[["sensitivity"]],
      specificity = results[["specificity"]])
  )

  pointData <- data.frame(
    x = rep(results[["prevalence"]], 2),
    y = c(results[["positivePredictiveValue"]], results[["negativePredictiveValue"]])
  )

  plot <- ggplot2::ggplot(data = data) +
    ggplot2::geom_segment(ggplot2::aes(x = results[["prevalence"]], y = 0, xend = results[["prevalence"]], yend = 1), linetype = 2, size = 1) +
    ggplot2::geom_line(mapping = ggplot2::aes(x = prevalence, y = positivePredictiveValue, color = gettext("Positive")), size = 2) +
    ggplot2::geom_line(mapping = ggplot2::aes(x = prevalence, y = negativePredictiveValue, color = gettext("Negative")), size = 2) +
    jaspGraphs::geom_point(data = pointData, mapping = ggplot2::aes(x = x, y = y), size = 5) +
    ggplot2::xlab(gettext("Prevalence")) +
    ggplot2::ylab(gettext("Predictive Value")) +
    ggplot2::scale_color_manual(
      name   = gettext("Predictive Value"),
      breaks = c(gettext("Positive"), gettext("Negative")),
      values = c("firebrick", "steelblue")
      )

  plot <- jaspGraphs::themeJasp(plot, legend.position = "bottom")

  return(plot)
}

.bcFillPlotVaryingPrevalence.bcUncertainEstimates <- function(results, summary, dataset, options) {
  alpha <- 1-options[["ciLevel"]]
  sensitivity <- results[["sensitivity"]]
  specificity <- results[["specificity"]]

  prevalence <- seq(0, 1, by = 0.01)
  data <- data.frame(prevalence = prevalence,
                     meanPPV = NA, lowerPPV = NA, upperPPV = NA,
                     meanNPV = NA, lowerNPV = NA, upperNPV = NA)

  for(i in seq_along(prevalence)) {
    truePositive            <- prevalence[i]*sensitivity
    falsePositive           <- (1-prevalence[i])*(1-specificity)
    trueNegative            <- (1-prevalence[i])*specificity
    falseNegative           <- prevalence[i]*(1-sensitivity)
    positivePredictiveValue <- truePositive / (truePositive + falsePositive)
    negativePredictiveValue <- trueNegative / (trueNegative + falseNegative)

    data[i, "meanPPV"]  <- mean    (positivePredictiveValue, na.rm=TRUE)
    data[i, "lowerPPV"] <- quantile(positivePredictiveValue, p =   alpha/2, na.rm=TRUE)
    data[i, "upperPPV"] <- quantile(positivePredictiveValue, p = 1-alpha/2, na.rm=TRUE)

    data[i, "meanNPV"]  <- mean    (negativePredictiveValue, na.rm=TRUE)
    data[i, "lowerNPV"] <- quantile(negativePredictiveValue, p =   alpha/2, na.rm=TRUE)
    data[i, "upperNPV"] <- quantile(negativePredictiveValue, p = 1-alpha/2, na.rm=TRUE)
  }

  plot <- ggplot2::ggplot(data = data)

  if(options[["ci"]]) {
    plot <- plot +
      ggplot2::geom_ribbon(mapping = ggplot2::aes(x = prevalence, ymin = lowerPPV, ymax = upperPPV, fill = gettext("Positive")), alpha = 0.5) +
      ggplot2::geom_ribbon(mapping = ggplot2::aes(x = prevalence, ymin = lowerNPV, ymax = upperNPV, fill = gettext("Negative")), alpha = 0.5)
  }


  plot <- plot +
    ggplot2::geom_line(mapping = ggplot2::aes(x = prevalence, y = meanPPV, color = gettext("Positive")), size = 2) +
    ggplot2::geom_line(mapping = ggplot2::aes(x = prevalence, y = meanNPV, color = gettext("Negative")), size = 2) +
    ggplot2::xlab(gettext("Prevalence")) +
    ggplot2::ylab(gettext("Predictive Value")) +
    ggplot2::scale_color_manual(
      name   = gettext("Predictive Value"),
      breaks = c(gettext("Positive"), gettext("Negative")),
      values = c("firebrick", "steelblue")
    ) +
    ggplot2::scale_fill_manual(
      name   = gettext("Predictive Value"),
      breaks = c(gettext("Positive"), gettext("Negative")),
      values = c("firebrick", "steelblue")
    )

  plot <- jaspGraphs::themeJasp(plot, legend.position = "bottom")

  return(plot)
}

## Alluvial plot ----
.bcPlotAlluvial <- function(results, summary, plotsContainer, dataset, options, ready, position) {
  if( isFALSE(options[["alluvialPlot"]])     ) return()
  if(!is.null(plotsContainer[["alluvialPlot"]]) ) return()

  plotsContainer[["alluvialPlot"]] <-
    createJaspPlot(title        = gettext("Alluvial Plot"),
                   dependencies = c("alluvialPlot", .bcGetColors(options)[["names"]]),
                   position     = position,
                   width        = 500,
                   height       = 500
    )

  if(ready) plotsContainer[["alluvialPlot"]]$plotObject <-
    .bcFillPlotAlluvial(results, summary, dataset, options) +
      ggplot2::theme(legend.text = ggplot2::element_text(margin = ggplot2::margin(r = 0.2, unit = "npc")))
}

.bcFillPlotAlluvial <- function(results, summary, dataset, options) {
  UseMethod(".bcFillPlotAlluvial")
}

.bcFillPlotAlluvial.default <- function(results, summary, dataset, options) {

  data <- expand.grid(cond = gettext(c("Positive", "Negative")),
                      test = gettext(c("Positive", "Negative")),
                      KEEP.OUT.ATTRS = FALSE)
  data$out <- factor(
    gettext(c("True positive", "False positive", "False negative", "True negative")),
    levels = gettext(c("True positive", "False positive", "False negative", "True negative"))
  )
  data$prop <- .bcExtract(summary, c("truePositive", "falsePositive", "falseNegative", "trueNegative"))
  plot <- ggplot2::ggplot(data = data,
                          mapping = ggplot2::aes(y = prop, axis1 = cond, axis2 = test)) +
    ggalluvial::geom_alluvium(mapping = ggplot2::aes(fill = out)) +
    ggalluvial::geom_stratum(width = 0.5) +
    ggalluvial::stat_stratum(geom = "text", ggplot2::aes(label = ggplot2::after_stat(stratum)), size = 6) +
    ggplot2::scale_x_discrete(limits = c("cond", "test"),
                              labels = gettext(c("Condition", "Test"))) +
    ggplot2::scale_fill_manual (name = "", values = .bcGetColors(options)[["values"]], labels = .bcGetColors(options)[["labels"]]) +
    ggplot2::guides(fill=ggplot2::guide_legend(ncol=2, override.aes = list(alpha = 1))) +
    ggplot2::ylab(gettext("Proportion of Population"))

  plot <- jaspGraphs::themeJasp(plot, legend.position = "bottom")

  return(plot)
}

## Signal detection plot ----
.bcPlotSignal <- function(results, summary, plotsContainer, dataset, options, ready, position) {
  if( isFALSE(options[["signalDetectionPlot"]])     ) return()
  if(!is.null(plotsContainer[["signalDetectionPlot"]]) ) return()

  plotsContainer[["signalDetectionPlot"]] <-
    createJaspPlot(title        = gettext("Signal Detection"),
                   dependencies = c("signalDetectionPlot", .bcGetColors(options)[["names"]]),
                   position     = position,
                   width        = 500,
                   height       = 500
    )

  if(ready) plotsContainer[["signalDetectionPlot"]]$plotObject <-
    .bcFillPlotSignal(results, summary, dataset, options) +
      ggplot2::theme(legend.text = ggplot2::element_text(margin = ggplot2::margin(r = 0.2, unit = "npc")))

}

.bcFillPlotSignal <- function(results, summary, dataset, options) {
  UseMethod(".bcFillPlotSignal")
}

.bcFillPlotSignal.default <- function(results, summary, dataset, options) {
  threshold    <- qnorm(.bcExtract(summary, "specificity"))
  meanPositive <- qnorm(.bcExtract(summary, "sensitivity"), mean = threshold)

  lowerLimitX <- min(qnorm(0.001, c(0, meanPositive)))
  upperLimitX <- max(qnorm(0.999, c(0, meanPositive)))
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(lowerLimitX, upperLimitX))
  xLimits <- range(xBreaks)

  prevalence <- .bcExtract(summary, "prevalence")
  plot <- ggplot2::ggplot() +
    ggplot2::stat_function(fun = .bcwdnorm, args = list(mean = 0, sd = 1, w = 1-prevalence), xlim = c(lowerLimitX, threshold), geom = "area", mapping = ggplot2::aes(fill = "steelblue"), alpha = 0.7)  +
    ggplot2::stat_function(fun = .bcwdnorm, args = list(mean = 0, sd = 1, w = 1-prevalence), xlim = c(threshold, upperLimitX), geom = "area", mapping = ggplot2::aes(fill = "darkorange"), alpha = 0.7) +
    ggplot2::stat_function(fun = .bcwdnorm, args = list(mean = meanPositive, sd = 1, w = prevalence), xlim = c(lowerLimitX, threshold), geom = "area", mapping = ggplot2::aes(fill = "red"), alpha = 0.7) +
    ggplot2::stat_function(fun = .bcwdnorm, args = list(mean = meanPositive, sd = 1, w = prevalence), xlim = c(threshold, upperLimitX), geom = "area", mapping = ggplot2::aes(fill = "darkgreen"), alpha = 0.7) +
    ggplot2::stat_function(fun = .bcwdnorm, args = list(mean = 0, sd = 1, w = 1-prevalence), size = 1) +
    ggplot2::stat_function(fun = .bcwdnorm, args = list(mean = meanPositive, sd = 1, w = prevalence), size = 1) +
    ggplot2::geom_segment(ggplot2::aes(x = threshold, y = 0, xend = threshold, yend = Inf), linetype = 2, size = 1.5) +
    ggplot2::scale_x_continuous(breaks = xBreaks,
                                limits = xLimits) +
    ggplot2::scale_y_continuous(breaks = jaspGraphs::getPrettyAxisBreaks(c(0, prevalence * dnorm(0), (1-prevalence) * dnorm(0))),
                                limits = range(jaspGraphs::getPrettyAxisBreaks(c(0, prevalence * dnorm(0), (1-prevalence) * dnorm(0))))) +
    ggplot2::scale_fill_manual (name = "", values = .bcGetColors(options)[["values"]], labels = .bcGetColors(options)[["labels"]]) +
    ggplot2::guides(fill=ggplot2::guide_legend(ncol=2)) +
    ggplot2::xlab(gettext("Marker")) +
    ggplot2::ylab(gettext("Density"))

  plot <- jaspGraphs::themeJasp(plot, legend.position = "bottom")

  return(plot)
}

.bcFillPlotSignal.bcData <- function(results, summary, dataset, options) {
  histogram <- hist(dataset[["marker"]], plot = FALSE)
  counts    <- histogram[["counts"]]
  breaks    <- histogram[["breaks"]]
  binWidth  <- breaks[2] - breaks[1]
  group     <- character(nrow(dataset))
  group[ dataset$condition &  dataset$test] <- gettext("True positive")
  group[!dataset$condition &  dataset$test] <- gettext("False positive")
  group[ dataset$condition & !dataset$test] <- gettext("False negative")
  group[!dataset$condition & !dataset$test] <- gettext("True negative")

  dataset$group <- factor(group, levels = gettext(c("True positive", "False positive", "False negative", "True negative")))
  plot <- ggplot2::ggplot(data = dataset, mapping = ggplot2::aes(x = marker, fill = group)) +
    ggplot2::geom_histogram(position = "identity", alpha = 0.4, color = "black", binwidth = binWidth, boundary = options[["threshold"]]) +
    ggplot2::geom_segment(ggplot2::aes(x = options[["threshold"]], y = 0, xend = options[["threshold"]], yend = Inf), linetype = 2, size = 1.5) +
    ggplot2::scale_x_continuous(breaks = jaspGraphs::getPrettyAxisBreaks(dataset[["marker"]]),
                                limits = range(jaspGraphs::getPrettyAxisBreaks(dataset[["marker"]]))) +
    ggplot2::scale_y_continuous(breaks = jaspGraphs::getPrettyAxisBreaks(counts),
                                limits = range(jaspGraphs::getPrettyAxisBreaks(counts))) +
    ggplot2::scale_fill_manual (name = "", values = .bcGetColors(options)[["values"]], labels = .bcGetColors(options)[["labels"]]) +
    ggplot2::guides(fill=ggplot2::guide_legend(ncol=2)) +
    ggplot2::xlab(gettext("Marker")) +
    ggplot2::ylab(gettext("Count"))

  plot <- jaspGraphs::themeJasp(plot, legend.position = "bottom")

  return(plot)
}

## Estimates plot ----
.bcPlotEstimates <- function(results, summary, plotsContainer, dataset, options, ready, position) {
  if( isFALSE(options[["estimatesPlot"]]) ) return()
  if(!is.null(plotsContainer[["estimatesPlot"]]) ) return()

  plots <- c("estimatesPlotPrevalence",
             "estimatesPlotSensitivity",             "estimatesPlotSpecificity",
             "estimatesPlotTruePositive",        "estimatesPlotFalsePositive",
             "estimatesPlotTrueNegative",        "estimatesPlotFalseNegative",
             "estimatesPlotPositivePredictiveValue", "estimatesPlotNegativePredictiveValue",
             "estimatesPlotFalseDiscoveryRate",      "estimatesPlotFalseOmissionRate",
             "estimatesPlotFalsePositiveRate",       "estimatesPlotFalseNegativeRate",
             "estimatesPlotAccuracy")
  selectedPlots <- unlist(options[plots])

  plotsContainer[["estimatesPlot"]] <-
    createJaspPlot(title        = gettext("Estimates"),
                   dependencies = c("estimatesPlot", plots, "plotEstimatesType"),
                   position     = position,
                   width        = 500,
                   height       = 50 + 50 * sum(selectedPlots)
    )

  if(ready && any(selectedPlots)) plotsContainer[["estimatesPlot"]]$plotObject <-
    .bcFillPlotEstimates(results, summary, dataset, options, selectedPlots)

}

.bcFillPlotEstimates <- function(results, summary, dataset, options, selectedPlots) {
  UseMethod(".bcFillPlotEstimates")
}

.bcFillPlotEstimates.default <- function(results, summary, dataset, options, selectedPlots) {
  if(options[["plotEstimatesType"]] == "interval") {
    rows <- c("prevalence",
              "sensitivity",             "specificity",
              "truePositive",            "falsePositive",
              "trueNegative",            "falseNegative",
              "positivePredictiveValue", "negativePredictiveValue",
              "falseDiscoveryRate",      "falseOmissionRate",
              "falsePositiveRate",   "falseNegativeRate",
              "accuracy")[selectedPlots]

    data <- .bcExtract(summary, rows, c("statistic", "estimate", "lowerCI", "upperCI"))
    data[["statistic"]] <- factor(data[["statistic"]], levels = rev(data[["statistic"]]))

    plot <- ggplot2::ggplot(data = data, mapping = ggplot2::aes(x=estimate,y=statistic))

    if(options[["ci"]])
      plot <- plot + ggplot2::geom_errorbarh(mapping = ggplot2::aes(xmin=lowerCI,xmax=upperCI),
                                             height = 0.25, size = 1)

    plot <- plot + jaspGraphs::geom_point(size = 6)

  } else {
    cols <- c("prevalence",
              "sensitivity",             "specificity",
              "truePositive",            "falsePositive",
              "trueNegative",            "falseNegative",
              "positivePredictiveValue", "negativePredictiveValue",
              "falseDiscoveryRate",      "falseOmissionRate",
              "falsePositiveRate",   "falseNegativeRate",
              "accuracy")[selectedPlots]
    data <- as.data.frame(results[cols])
    colNames <- .bcExtract(summary, cols, "statistic")
    colnames(data) <- colNames
    data <- tidyr::pivot_longer(data = data, cols = tidyr::everything(),
                                names_to = "statistic", values_to = "estimate")
    data[["statistic"]] <- factor(data[["statistic"]], levels = rev(colNames))
    plot <- ggplot2::ggplot(data = data, mapping = ggplot2::aes(x=estimate,y=statistic)) +
      ggdist::stat_halfeye()
  }

  plot <- plot + ggplot2::xlim(c(0,1)) +
    ggplot2::xlab(gettext("Estimate")) +
    ggplot2::ylab(NULL)

  plot <- jaspGraphs::themeJasp(plot)

  return(plot)
}

.bcFillPlotEstimates.bcPointEstimates <- function(results, summary, dataset, options, selectedPlots) {
  data <- summary
  rows <- c("prevalence",
            "sensitivity",             "specificity",
            "truePositive",            "falsePositive",
            "trueNegative",            "falseNegative",
            "positivePredictiveValue", "negativePredictiveValue",
            "falseDiscoveryRate",      "falseOmissionRate",
            "falsePositiveRate",   "falseNegativeRate",
            "accuracy")[selectedPlots]

  data <- data[rows,]
  data[["statistic"]] <- factor(data[["statistic"]], levels = rev(data[["statistic"]]))

  plot <- ggplot2::ggplot(data = data, mapping = ggplot2::aes(x=estimate,y=statistic)) +
    jaspGraphs::geom_point(size = 6)

  plot <- plot + ggplot2::xlim(c(0,1)) +
    ggplot2::xlab(gettext("Estimate")) +
    ggplot2::ylab(NULL)

  plot <- jaspGraphs::themeJasp(plot)

  return(plot)
}

# Helpers ----
.bcTexts <- function(what = c("statistic", "interpretation", "notation", "footnote"), options = NULL) {
  what <- match.arg(what)
  out <- switch(what,
    statistic = c(
      prevalence              = gettext("Prevalence"),
      sensitivity             = gettext("Sensitivity"),
      specificity             = gettext("Specificity"),
      truePositive            = gettext("True positive"),
      falsePositive           = gettext("False positive"),
      trueNegative            = gettext("True negative"),
      falseNegative           = gettext("False negative"),
      positivePredictiveValue = gettext("Positive predictive value"),
      negativePredictiveValue = gettext("Negative predictive value"),
      falseDiscoveryRate      = gettext("False discovery rate"),
      falseOmissionRate       = gettext("False omission rate"),
      falsePositiveRate       = gettext("False positive rate"),
      falseNegativeRate       = gettext("False negative rate"),
      accuracy                = gettext("Accuracy"),
      pretestOdds             = gettext("Pretest odds"),
      positiveLikelihoodRatio = gettext("Positive likelihood ratio"),
      negativeLikelihoodRatio = gettext("Negative likelihood ratio"),
      fMeasure                = gettext("F-measure")
    ),
    interpretation = c(
      prevalence              = gettext("Proportion of a population affected by the condition."),
      sensitivity             = gettext("(True positive rate) Proportion of those who are affected by the condition and are correctly tested positive."),
      specificity             = gettext("(True negative rate) Proportion of those who are not affected by the condition and are correctly tested negative."),
      truePositive            = gettext("Proportion of a population affected by a condition and correctly tested positive."),
      falsePositive           = gettext("Proportion of a population not affected by a condition and incorrectly tested positive."),
      trueNegative            = gettext("Proportion of a population not affected by a condition and correctly tested negative."),
      falseNegative           = gettext("Proportion of a population affected by a condition and incorrectly tested negative."),
      positivePredictiveValue = gettext("Proportion of those who tested positive and are affected by the condition."),
      negativePredictiveValue = gettext("Proportion of those who tested negative and are not affected by the condition."),
      falseDiscoveryRate      = gettext("Proportion of false positives in the pool of those that test positive."),
      falseOmissionRate       = gettext("Proportion of false negatives in the pool of those that test negative."),
      falsePositiveRate       = gettext("Complement proportion to specificity."),
      falseNegativeRate       = gettext("Complement proportion to sensitivity."),
      accuracy                = gettext("Proportion of the population that is true positive or true negative.")
   ),
   notation = c(
     prevalence              = gettext("P(Condition = positive)"),
     sensitivity             = gettext("P(Test = positive | Condition = positive)"),
     specificity             = gettext("P(Test = negative | Condition = negative)"),
     truePositive            = gettextf("P(Condition = positive %s Test = positive)", "\u2227"),
     falsePositive           = gettextf("P(Condition = negative %s Test = positive)", "\u2227"),
     trueNegative            = gettextf("P(Condition = negative %s Test = negative)", "\u2227"),
     falseNegative           = gettextf("P(Condition = positive %s Test = negative)", "\u2227"),
     positivePredictiveValue = gettext("P(Condition = positive | Test = positive)"),
     negativePredictiveValue = gettext("P(Condition = negative | Test = negative)"),
     falseDiscoveryRate      = gettext("P(Condition = negative | Test = positive)"),
     falseOmissionRate       = gettext("P(Condition = positive | Test = negative)"),
     falsePositiveRate       = gettext("P(Test = positive | Condition = negative)"),
     falseNegativeRate       = gettext("P(Test = negative | Condition = positive)"),
     accuracy                = gettextf("P(Condition = positive %1$s Test = positive %2$s Condition = negative %1$s Test = negative)", "\u2227", "\u2228")
   ),
   footnote = c(
     ci  = gettextf("Central credible intervals are based on %i samples.", options[["samples"]]*options[["chains"]]),
     posteriorEstimate = gettextf("Parameter estimates are based on %i MCMC samples.", options[["samples"]]*options[["chains"]])
   )
  )

  return(out)
}

.bcBayesTheoremExpression <- function() {
  condition <- gettext("Condition")
  test <- gettext("Test")
  positive <- gettext("positive")
  negative <- gettext("negative")
  prevalence <- gettext("Prevalence")
  sensitivity <- gettext("Sensitivity")
  specificity <- gettext("Specificity")

  theorem <- jaspBase::mathExpression(
    r"{
    \begin{aligned}
p(& \text{Condition} = \text{positive} \mid \text{Test} = \text{positive}) \\
& = \frac{p(\text{Condition}=\text{positive}) \times p(\text{Test} = \text{positive} \mid \text{Condition}=\text{positive})}{p(\text{Test} = \text{positive})}\\
& = \frac{\text{Prevalence} \times \text{Sensitivity}}{\text{Prevalence} \times \text{Sensitivity} + (1-\text{Prevalence})\times(1-\text{Specificity})}
\end{aligned}
    }",
    inline = FALSE
  )
}

.bcwdnorm <- function(x, mean = 0, sd = 1, w = 1) {
  w * dnorm(x, mean = mean, sd = sd)
}


.bcGetLevel2D <- function(x, y, confLevel) {
  # https://stackoverflow.com/a/23448933/7997788
  kk <- MASS::kde2d(x,y, n = 25)
  dx <- diff(kk$x[1:2])
  dy <- diff(kk$y[1:2])
  sz <- sort(kk$z)
  c1 <- cumsum(sz) * dx * dy
  approx(c1, sz, xout = 1-confLevel)$y
}

GeomPNG <- ggplot2::ggproto("GeomPNG", ggplot2::Geom,
                            required_aes = c("x", "y", "w", "h", "img"),
                            default_aes = ggplot2::aes(col = "grey"),
                            draw_group = function(data, panel_params, coord) {
                              img <- png::readPNG(as.character(data$img))
                              fillColor <- grDevices::col2rgb(data$col[1])
                              img[,,1] <- fillColor["red",  ]*img[,,4]/255
                              img[,,2] <- fillColor["green",]*img[,,4]/255
                              img[,,3] <- fillColor["blue",] *img[,,4]/255
                              size <- coord$transform(data.frame(x=data$w, y=data$h), panel_params)
                              data <- coord$transform(data, panel_params)

                              grid::rasterGrob(image = img, x = data$x, y = data$y, width = size$x, height = size$y,
                                               just = c(0.5, 0.5), default.units = "native")
                            }
)

geom_png <- function(mapping = NULL, data = NULL) {
  ggplot2::layer(data = data, mapping = mapping, geom = GeomPNG,
                 stat = ggplot2::StatIdentity, position = ggplot2::PositionIdentity,
                 show.legend = FALSE)
}

.bcGetColors <- function(options) {
  names <- c("colorTruePositive", "colorFalsePositive", "colorFalseNegative", "colorTrueNegative")
  list(
    names = names,
    values = unname(unlist(options[names])),
    labels = gettext(c("True positive", "False positive", "False negative", "True negative"))
  )
}

.bcIsColor <- function(x) {
  for (color in x) {
    res <- try(col2rgb(x),silent=TRUE)
    if (jaspBase::isTryError(res))
      return(FALSE)
  }
  return(TRUE)
}
