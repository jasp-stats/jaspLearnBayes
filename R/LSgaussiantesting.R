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

LSgaussiantesting  <- function(jaspResults, dataset, options, state = NULL) {

  # introductory text
  if (options[["introductoryText"]]).introductoryTextLS(jaspResults, options, "gaussTest")

  # a vector of two, first for data, second for hypotheses
  ready <- .readyGaussianLS(options)

  # evaluate the expressions in priors
  if (ready[2])options[["priors"]] <- .evaluatePriors(options[["priors"]])

  # load, check, transform and process data
  if (ready[1])data <- .readDataGaussianLS(dataset, options)

  # data summary table if requested (but not if the data counts were added directly)
  .summaryGaussianLS(jaspResults, data, options, "gaussTest")

  ### inference
  # summary table
  .testsGaussianLS(jaspResults, data, ready, options)

  return()
}


.testsGaussianLS              <- function(jaspResults, data, ready, options) {

  if (is.null(jaspResults[["testsContainer"]])) {
    testsContainer <- createJaspContainer("Model")
    testsContainer$position <- 2
    jaspResults[["testsContainer"]] <- testsContainer
  } else{
    testsContainer <- jaspResults[["testsContainer"]]
  }


  if (options[["introductoryText"]] && is.null(testsContainer[['introText']])) {

    introText <- createJaspHtml()
    introText$dependOn("introductoryText")
    introText$position <- 1

    introText[['text']] <- .explanatoryTextLS("tests", options, "gaussTest")

    testsContainer[['introText']] <- introText
  }


  if (is.null(testsContainer[['testsTable']])) {

    testsTable <- createJaspTable(title = gettext("Testing Summary"))

    testsTable$position <- 2
    testsTable$dependOn(c(.GaussianLS_data_dependencies, "priorPredictivePerformanceBfType", "priorPredictivePerformanceBfVsHypothesis"))

    bfType_name <- switch(
      options[["priorPredictivePerformanceBfType"]],
      "inclusion" = gettext("Inclusion BF"),
      "best"      = gettextf("BF%s","\u2081\u2080"),
      "vs"        = gettextf("BF%s","\u2081\u2080")
    )

    testsTable$addColumnInfo(name = "hypothesis",   title = gettext("Hypothesis"),          type = "string")
    testsTable$addColumnInfo(name = "prior",        title = gettext("P(H)"),                type = "number")
    testsTable$addColumnInfo(name = "log_lik",      title = gettext("log(likelihood)"),     type = "number")
    testsTable$addColumnInfo(name = "posterior",    title = gettext("P(H|data)"),           type = "number")
    testsTable$addColumnInfo(name = "bf",           title = bfType_name,                    type = "number")

    testsTable$setExpectedSize(length(options[["priors"]]))

    testsContainer[["testsTable"]] <- testsTable


  }

  return()
}
