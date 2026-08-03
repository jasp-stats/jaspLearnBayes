context("Learn Bayes - Buffon's Needle Manipulation")

## default settings
options <- jaspTools::analysisOptions("LSBuffonsneedlemanipulation")
options$numberOfCrosses                <- 10
options$numberOfThrows                 <- 100
options$priorAlpha                     <- 1
options$priorBeta                      <- 1
options$lengthToDistanceProportion     <- 80
options$ciLevel                        <- 0.95
options$min                            <- 3
options$max                            <- 3.2
options$highlight                      <- TRUE
options$priorPosteriorProportion       <- TRUE
options$priorPosteriorProportionLegend <- TRUE
options$priorPosteriorPiCi             <- TRUE
options$priorPosteriorProportionLegend <- TRUE
options$priorPosteriorProportionCi                 <- TRUE
options$priorPosteriorPiLegend         <- TRUE

set.seed(1)
dataset <- NULL
results <- jaspTools::runAnalysis("LSBuffonsneedlemanipulation", dataset, options)#, makeTests = TRUE)


test_that("Implied Prior and Posterior for π plot matches", {
  plotName <- results[["results"]][["piDistPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "implied-prior-and-posterior-for-π")
})

test_that("Prior and Posterior for Proportion of Crosses plot matches", {
  plotName <- results[["results"]][["propDistPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "prior-and-posterior-for-proportion-of-crosses")
})

test_that("Summary Table results match", {
  table <- results[["results"]][["summaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(16, 0, 16.4381357547624, 10, 100, 9.66364618026701, 32.3161343887626
                                 ))
})


test_that("Formula input preserves percentage behavior", {
  formulaOptions <- options
  formulaOptions$lengthToDistanceProportion <- "40 * 2"

  formulaResults <- jaspTools::runAnalysis("LSBuffonsneedlemanipulation", dataset, formulaOptions)
  table <- formulaResults[["results"]][["summaryTable"]][["data"]]

  jaspTools::expect_equal_tables(table,
                                 list(16, 0, 16.4381357547624, 10, 100, 9.66364618026701, 32.3161343887626
                                 ))
})

test_that("Invalid length-to-distance percentage sets a summary table error", {
  invalidOptions <- options
  invalidOptions$lengthToDistanceProportion <- "101"

  invalidResults <- jaspTools::runAnalysis("LSBuffonsneedlemanipulation", dataset, invalidOptions)
  summaryTable <- invalidResults[["results"]][["summaryTable"]]

  testthat::expect_identical(summaryTable[["status"]], "error")
  testthat::expect_identical(
    summaryTable[["error"]][["errorMessage"]],
    "The proportion of needle length to interline distance must be between 1% and 100%."
  )
  testthat::expect_null(invalidResults[["results"]][["propDistPlot"]])
  testthat::expect_null(invalidResults[["results"]][["piDistPlot"]])
})
