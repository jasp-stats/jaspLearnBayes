context("Learn Bayes - Buffon's Needle Manipulation")

## default settings
options <- analysisOptions("LSBuffonsneedlemanipulation")
options$numberOfCrosses                <- 50
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
